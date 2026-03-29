;;; -*- Gerbil -*-
;;; Qt parity commands (part 8) — surround-mode, goto-last-change,
;;; crosshair-mode, quickrun, eros-mode, auto-dim-other-buffers,
;;; rainbow-identifiers, outline-minor-mode, string-edit-at-point,
;;; persistent-scratch.
;;; Chain position: after commands-parity7.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/sort
        :std/misc/ports
        :gemacs/core
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/commands-core
        :gemacs/qt/commands-parity5
        :gemacs/echo)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (qt-echo-message app msg)
  "Show message in the Qt echo area."
  (echo-message! (app-state-echo app) msg))

;;;============================================================================
;;; Surround-mode (Qt) — add/change/delete surrounding delimiters
;;;============================================================================

(def *qt-surround-pairs* (make-hash-table))
(begin
  (hash-put! *qt-surround-pairs* #\( #\))
  (hash-put! *qt-surround-pairs* #\[ #\])
  (hash-put! *qt-surround-pairs* #\{ #\})
  (hash-put! *qt-surround-pairs* #\< #\>)
  (hash-put! *qt-surround-pairs* #\" #\")
  (hash-put! *qt-surround-pairs* #\' #\')
  (hash-put! *qt-surround-pairs* #\` #\`))

(def (qt-surround-closing ch)
  (hash-ref *qt-surround-pairs* ch ch))

(def (qt-surround-find-enclosing ed pos ch)
  "Find enclosing pair positions around POS for char CH."
  (let* ((close (qt-surround-closing ch))
         (same? (char=? ch close))
         (len (sci-send ed SCI_GETLENGTH 0)))
    (if same?
      (let loop-back ((p (- pos 1)))
        (if (< p 0) #f
          (let ((c (integer->char (sci-send ed SCI_GETCHARAT p))))
            (if (char=? c ch)
              (let loop-fwd ((q (+ pos 1)))
                (if (>= q len) #f
                  (let ((c2 (integer->char (sci-send ed SCI_GETCHARAT q))))
                    (if (char=? c2 close) (cons p (+ q 1))
                      (loop-fwd (+ q 1))))))
              (loop-back (- p 1))))))
      (let loop-back ((p (- pos 1)) (depth 0))
        (if (< p 0) #f
          (let ((c (integer->char (sci-send ed SCI_GETCHARAT p))))
            (cond
              ((char=? c close) (loop-back (- p 1) (+ depth 1)))
              ((and (char=? c ch) (= depth 0))
               (let loop-fwd ((q (+ pos 1)) (d 0))
                 (if (>= q len) #f
                   (let ((c2 (integer->char (sci-send ed SCI_GETCHARAT q))))
                     (cond
                       ((char=? c2 ch) (loop-fwd (+ q 1) (+ d 1)))
                       ((and (char=? c2 close) (= d 0)) (cons p (+ q 1)))
                       ((char=? c2 close) (loop-fwd (+ q 1) (- d 1)))
                       (else (loop-fwd (+ q 1) d)))))))
              ((char=? c ch) (loop-back (- p 1) (- depth 1)))
              (else (loop-back (- p 1) depth)))))))))

(def (cmd-qt-surround-add app)
  "Add surrounding delimiters around region."
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART 0))
         (end (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= start end)
      (qt-echo-message app "No region selected. Select text first.")
      (let ((input (qt-echo-read-string app "Surround with: ")))
        (when (and input (>= (string-length input) 1))
          (let* ((ch (string-ref input 0))
                 (open-str (string ch))
                 (close-str (string (qt-surround-closing ch))))
            (sci-send ed SCI_BEGINUNDOACTION 0)
            (sci-send/string ed SCI_INSERTTEXT close-str end)  ; wparam=end
            (sci-send/string ed SCI_INSERTTEXT open-str start)  ; wparam=start
            (sci-send ed SCI_ENDUNDOACTION 0)
            (qt-echo-message app
              (string-append "Surrounded with " open-str "..." close-str))))))))

(def (cmd-qt-surround-delete app)
  "Delete surrounding delimiters."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS 0))
         (input (qt-echo-read-string app "Delete surround: ")))
    (when (and input (>= (string-length input) 1))
      (let* ((ch (string-ref input 0))
             (pair (qt-surround-find-enclosing ed pos ch)))
        (if (not pair)
          (qt-echo-message app (string-append "No enclosing " (string ch) " found"))
          (let ((start (car pair))
                (end (- (cdr pair) 1)))
            (sci-send ed SCI_BEGINUNDOACTION 0)
            (sci-send ed SCI_DELETERANGE end 1)
            (sci-send ed SCI_DELETERANGE start 1)
            (sci-send ed SCI_ENDUNDOACTION 0)
            (qt-echo-message app
              (string-append "Removed surrounding " (string ch)))))))))

(def (cmd-qt-surround-change app)
  "Change surrounding delimiters."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS 0))
         (old-input (qt-echo-read-string app "Change surround from: ")))
    (when (and old-input (>= (string-length old-input) 1))
      (let* ((old-ch (string-ref old-input 0))
             (pair (qt-surround-find-enclosing ed pos old-ch)))
        (if (not pair)
          (qt-echo-message app (string-append "No enclosing " (string old-ch) " found"))
          (let ((new-input (qt-echo-read-string app "Change surround to: ")))
            (when (and new-input (>= (string-length new-input) 1))
              (let* ((new-ch (string-ref new-input 0))
                     (start (car pair))
                     (end (- (cdr pair) 1))
                     (new-open (string new-ch))
                     (new-close (string (qt-surround-closing new-ch))))
                (sci-send ed SCI_BEGINUNDOACTION 0)
                (sci-send ed SCI_SETTARGETSTART end)
                (sci-send ed SCI_SETTARGETEND (+ end 1))
                (sci-send/string ed SCI_REPLACETARGET new-close -1)
                (sci-send ed SCI_SETTARGETSTART start)
                (sci-send ed SCI_SETTARGETEND (+ start 1))
                (sci-send/string ed SCI_REPLACETARGET new-open -1)
                (sci-send ed SCI_ENDUNDOACTION 0)
                (qt-echo-message app
                  (string-append "Changed " (string old-ch) " → " new-open))))))))))

;;;============================================================================
;;; Goto-last-change (Qt) — navigate edit positions
;;;============================================================================

(def *qt-edit-ring* (make-hash-table))
(def *qt-edit-index* (make-hash-table))
(def *qt-edit-ring-max* 32)

(def (qt-p8-record-edit-position! ed pos)
  (let* ((key (object->serial-number ed))
         (ring (or (hash-get *qt-edit-ring* key) '()))
         (dominated? (and (pair? ring) (< (abs (- pos (car ring))) 20))))
    (unless dominated?
      (let ((new-ring (qt-take (cons pos ring)
                        (min *qt-edit-ring-max* (+ (length ring) 1)))))
        (hash-put! *qt-edit-ring* key new-ring)
        (hash-put! *qt-edit-index* key 0)))))

(def (qt-take lst n)
  (if (or (<= n 0) (null? lst)) '()
    (cons (car lst) (qt-take (cdr lst) (- n 1)))))

(def (cmd-qt-goto-last-change app)
  "Jump to the location of the previous edit."
  (let* ((ed (current-qt-editor app))
         (key (object->serial-number ed))
         (ring (or (hash-get *qt-edit-ring* key) '()))
         (idx (or (hash-get *qt-edit-index* key) 0)))
    (if (null? ring)
      (qt-echo-message app "No edit history in this buffer")
      (let* ((pos (list-ref ring (min idx (- (length ring) 1))))
             (len (sci-send ed SCI_GETLENGTH 0))
             (safe-pos (min pos len)))
        (sci-send ed SCI_GOTOPOS safe-pos)
        (sci-send ed SCI_SCROLLCARET 0)
        (hash-put! *qt-edit-index* key (min (+ idx 1) (- (length ring) 1)))
        (let ((line (+ 1 (sci-send ed SCI_LINEFROMPOSITION safe-pos))))
          (qt-echo-message app
            (string-append "Edit " (number->string (+ idx 1))
                           "/" (number->string (length ring))
                           " (line " (number->string line) ")")))))))

(def (cmd-qt-goto-last-change-reverse app)
  "Jump forward in edit history."
  (let* ((ed (current-qt-editor app))
         (key (object->serial-number ed))
         (ring (or (hash-get *qt-edit-ring* key) '()))
         (idx (or (hash-get *qt-edit-index* key) 0)))
    (if (or (null? ring) (<= idx 0))
      (qt-echo-message app "At newest edit position")
      (let* ((new-idx (- idx 1))
             (pos (list-ref ring new-idx))
             (len (sci-send ed SCI_GETLENGTH 0))
             (safe-pos (min pos len)))
        (sci-send ed SCI_GOTOPOS safe-pos)
        (sci-send ed SCI_SCROLLCARET 0)
        (hash-put! *qt-edit-index* key new-idx)
        (let ((line (+ 1 (sci-send ed SCI_LINEFROMPOSITION safe-pos))))
          (qt-echo-message app
            (string-append "Edit " (number->string (+ new-idx 1))
                           "/" (number->string (length ring))
                           " (line " (number->string line) ")")))))))

;;;============================================================================
;;; Crosshair-mode (Qt) — highlight current line AND column
;;;============================================================================

(def *qt-crosshair-active* #f)
(def *qt-crosshair-indicator* 6)

(def (cmd-qt-crosshair-mode app)
  "Toggle crosshair: highlight current line and column."
  (let* ((ed (current-qt-editor app)))
    (set! *qt-crosshair-active* (not *qt-crosshair-active*))
    (if *qt-crosshair-active*
      (begin
        (sci-send ed SCI_SETCARETLINEVISIBLE 1)
        (sci-send ed SCI_SETCARETLINEBACK #x333333)
        (sci-send ed SCI_INDICSETSTYLE *qt-crosshair-indicator* 12)
        (sci-send ed SCI_INDICSETFORE *qt-crosshair-indicator* #x555555)
        (sci-send ed 2523 *qt-crosshair-indicator* 60) ; SCI_INDICSETALPHA
        (qt-echo-message app "Crosshair mode enabled"))
      (begin
        (sci-send ed SCI_SETCARETLINEVISIBLE 0)
        (sci-send ed SCI_SETINDICATORCURRENT *qt-crosshair-indicator*)
        (sci-send ed SCI_INDICATORCLEARRANGE 0 (sci-send ed SCI_GETLENGTH 0))
        (qt-echo-message app "Crosshair mode disabled")))))

;;;============================================================================
;;; Quickrun (Qt) — execute buffer with appropriate interpreter
;;;============================================================================

(def *qt-quickrun-interpreters* (make-hash-table))
(begin
  (hash-put! *qt-quickrun-interpreters* "ss" "gxi")
  (hash-put! *qt-quickrun-interpreters* "scm" "gxi")
  (hash-put! *qt-quickrun-interpreters* "py" "python3")
  (hash-put! *qt-quickrun-interpreters* "rb" "ruby")
  (hash-put! *qt-quickrun-interpreters* "js" "node")
  (hash-put! *qt-quickrun-interpreters* "ts" "npx ts-node")
  (hash-put! *qt-quickrun-interpreters* "sh" "bash")
  (hash-put! *qt-quickrun-interpreters* "bash" "bash")
  (hash-put! *qt-quickrun-interpreters* "zsh" "zsh")
  (hash-put! *qt-quickrun-interpreters* "lua" "lua")
  (hash-put! *qt-quickrun-interpreters* "pl" "perl")
  (hash-put! *qt-quickrun-interpreters* "go" "go run")
  (hash-put! *qt-quickrun-interpreters* "hs" "runghc")
  (hash-put! *qt-quickrun-interpreters* "ml" "ocaml")
  (hash-put! *qt-quickrun-interpreters* "php" "php")
  (hash-put! *qt-quickrun-interpreters* "r" "Rscript")
  (hash-put! *qt-quickrun-interpreters* "jl" "julia")
  (hash-put! *qt-quickrun-interpreters* "el" "emacs --batch -l"))

(def (qt-quickrun-ext name)
  (let ((dot (string-index-right name #\.)))
    (if dot (substring name (+ dot 1) (string-length name)) "")))

(def (cmd-qt-quickrun app)
  "Execute the current buffer with the appropriate interpreter."
  (let* ((buf (current-qt-buffer app))
         (name (or (buffer-file-path buf) (buffer-name buf)))
         (ext (qt-quickrun-ext name))
         (interp (hash-get *qt-quickrun-interpreters* ext)))
    (if (not interp)
      (qt-echo-message app (string-append "No interpreter for ." ext))
      (let* ((file (buffer-file-path buf))
             (cmd (if file (string-append interp " " file)
                    "echo 'Save file first'"))
             (output (with-exception-catcher
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                     (lambda () (display-exception e)))))
                      (lambda ()
                        (with-input-from-process
                         (list path: "/bin/bash" arguments: (list "-c" cmd))
                         (lambda () (read-string 65536)))))))
        (qt-open-output-buffer app "*Quickrun*" output)
        (qt-echo-message app (string-append "Ran: " interp " " (or file name)))))))

(def (cmd-qt-quickrun-with app)
  "Execute buffer with user-specified command."
  (let* ((buf (current-qt-buffer app))
         (file (buffer-file-path buf))
         (cmd (qt-echo-read-string app "Command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((full-cmd (if file (string-append cmd " " file) cmd))
             (output (with-exception-catcher
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                     (lambda () (display-exception e)))))
                      (lambda ()
                        (with-input-from-process
                         (list path: "/bin/bash" arguments: (list "-c" full-cmd))
                         (lambda () (read-string 65536)))))))
        (qt-open-output-buffer app "*Quickrun*" output)
        (qt-echo-message app (string-append "Ran: " full-cmd))))))

;;;============================================================================
;;; Eros-mode (Qt) — eval result overlays
;;;============================================================================

(def *qt-eros-active* #f)

(def (cmd-qt-eros-mode app)
  "Toggle eros-mode: show evaluation results inline."
  (set! *qt-eros-active* (not *qt-eros-active*))
  (qt-echo-message app (if *qt-eros-active*
                          "Eros mode enabled (eval results shown inline)"
                          "Eros mode disabled")))

(def (cmd-qt-eros-eval-last-sexp app)
  "Evaluate the sexp before point and show result."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS 0))
         (brace-pos (sci-send ed SCI_BRACEMATCH (- pos 1)))
         (start (if (>= brace-pos 0) brace-pos
                  (sci-send ed SCI_WORDSTARTPOSITION pos 1)))
         (text-len (- pos start)))
    (if (<= text-len 0)
      (qt-echo-message app "No expression at point")
      (let* ((text (qt-plain-text-edit-text-range (current-qt-editor app) start pos))
             (result (with-exception-catcher
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                     (lambda () (display-exception e)))))
                      (lambda ()
                        (with-output-to-string
                         (lambda () (write (eval (with-input-from-string text read)))))))))
        (if *qt-eros-active*
          (let ((line (sci-send ed SCI_LINEFROMPOSITION pos 0)))
            (sci-send ed 2548 2)  ; SCI_ANNOTATIONSETVISIBLE = ANNOTATION_BOXED
            (sci-send/string ed 2540 (string-append " => " result) line)  ; SCI_ANNOTATIONSETTEXT
            (qt-echo-message app result))
          (qt-echo-message app (string-append "=> " result)))))))

;;;============================================================================
;;; Auto-dim-other-buffers (Qt)
;;;============================================================================

(def *qt-auto-dim-active* #f)

(def (cmd-qt-auto-dim-other-buffers app)
  "Toggle dimming of non-focused windows."
  (set! *qt-auto-dim-active* (not *qt-auto-dim-active*))
  (let ((ed (current-qt-editor app)))
    (if *qt-auto-dim-active*
      (begin
        (sci-send ed SCI_SETCARETLINEVISIBLE 1)
        (sci-send ed SCI_SETCARETLINEBACK #x333333)
        (qt-echo-message app "Auto-dim other buffers enabled"))
      (begin
        (sci-send ed SCI_SETCARETLINEVISIBLE 0)
        (qt-echo-message app "Auto-dim other buffers disabled")))))

;;;============================================================================
;;; Rainbow-identifiers (Qt)
;;;============================================================================

(def *qt-rainbow-id-active* #f)
(def *qt-rainbow-colors*
  (vector #xE06C75 #x98C379 #xE5C07B #x61AFEF
          #xC678DD #x56B6C2 #xD19A66 #xABB2BF
          #xBE5046 #x7EC16E #xD4B074 #x519FDF
          #xB76CC8 #x48A8B4 #xC28B57 #x9CA2AB))

(def (cmd-qt-rainbow-identifiers-mode app)
  "Toggle rainbow identifier coloring."
  (let* ((ed (current-qt-editor app)))
    (set! *qt-rainbow-id-active* (not *qt-rainbow-id-active*))
    (if *qt-rainbow-id-active*
      (begin
        (let ((styles '(11 5 6 9 16)))
          (for-each
           (lambda (s)
             (sci-send ed SCI_STYLESETFORE s
               (vector-ref *qt-rainbow-colors*
                 (modulo s (vector-length *qt-rainbow-colors*)))))
           styles))
        (sci-send ed SCI_COLOURISE 0 -1)
        (qt-echo-message app "Rainbow identifiers enabled"))
      (begin
        (let ((styles '(11 5 6 9 16)))
          (for-each
           (lambda (s) (sci-send ed SCI_STYLESETFORE s #xD4D4D4))
           styles))
        (sci-send ed SCI_COLOURISE 0 -1)
        (qt-echo-message app "Rainbow identifiers disabled")))))

;;;============================================================================
;;; Outline-minor-mode (Qt)
;;;============================================================================

(def *qt-outline-active* #f)

(def (cmd-qt-outline-minor-mode app)
  "Toggle outline-minor-mode: code folding for all file types."
  (let* ((ed (current-qt-editor app)))
    (set! *qt-outline-active* (not *qt-outline-active*))
    (if *qt-outline-active*
      (begin
        (sci-send ed SCI_SETMARGINWIDTHN 2 16)
        (sci-send ed SCI_SETMARGINTYPEN 2 SC_MARGIN_SYMBOL)
        (sci-send ed SCI_SETMARGINMASKN 2 SC_MASK_FOLDERS)
        (sci-send ed SCI_SETMARGINSENSITIVEN 2 1)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPEN SC_MARK_BOXMINUS)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDER SC_MARK_BOXPLUS)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERSUB SC_MARK_VLINE)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERTAIL SC_MARK_LCORNER)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEREND SC_MARK_BOXPLUSCONNECTED)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPENMID SC_MARK_BOXMINUSCONNECTED)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERMIDTAIL SC_MARK_TCORNER)
        (for-each (lambda (m)
                    (sci-send ed SCI_MARKERSETFORE m #x808080)
                    (sci-send ed SCI_MARKERSETBACK m #x282828))
                  (list SC_MARKNUM_FOLDEROPEN SC_MARKNUM_FOLDER
                        SC_MARKNUM_FOLDERSUB SC_MARKNUM_FOLDERTAIL
                        SC_MARKNUM_FOLDEREND SC_MARKNUM_FOLDEROPENMID
                        SC_MARKNUM_FOLDERMIDTAIL))
        (sci-send ed SCI_SETFOLDFLAGS 16)
        (qt-echo-message app "Outline-minor-mode enabled"))
      (begin
        (sci-send ed SCI_SETMARGINWIDTHN 2 0)
        (sci-send ed SCI_SETFOLDFLAGS 0)
        (sci-send ed SCI_FOLDALL 1)
        (qt-echo-message app "Outline-minor-mode disabled")))))

(def (cmd-qt-outline-toggle-children app)
  "Toggle fold at current line."
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION
                 (sci-send ed SCI_GETCURRENTPOS 0) 0)))
    (sci-send ed SCI_TOGGLEFOLD line)
    (qt-echo-message app
      (if (= (sci-send ed SCI_GETFOLDEXPANDED line) 1) "Unfolded" "Folded"))))

(def (cmd-qt-outline-fold-all app)
  "Fold all regions."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 0)
    (qt-echo-message app "All folds collapsed")))

(def (cmd-qt-outline-unfold-all app)
  "Unfold all regions."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 1)
    (qt-echo-message app "All folds expanded")))

;;;============================================================================
;;; String-edit-at-point (Qt)
;;;============================================================================

(def (qt-string-at-point ed pos)
  (let* ((style (sci-send ed SCI_GETSTYLEAT pos))
         (len (sci-send ed SCI_GETLENGTH 0)))
    (if (or (= style 6) (= style 7) (= style 3) (= style 12) (= style 13))
      (let loop-back ((p (- pos 1)))
        (if (< p 0) (cons 0 pos)
          (if (not (= (sci-send ed SCI_GETSTYLEAT p) style))
            (let loop-fwd ((q (+ pos 1)))
              (if (>= q len) (cons (+ p 1) len)
                (if (not (= (sci-send ed SCI_GETSTYLEAT q) style))
                  (cons (+ p 1) q)
                  (loop-fwd (+ q 1)))))
            (loop-back (- p 1)))))
      #f)))

(def (cmd-qt-string-edit-at-point app)
  "Edit string literal at point in a temporary buffer."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS 0))
         (bounds (qt-string-at-point ed pos)))
    (if (not bounds)
      (qt-echo-message app "No string literal at point")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (text (qt-plain-text-edit-text-range ed start end)))
        (qt-open-output-buffer app "*String Edit*" text)
        (qt-echo-message app
          (string-append "Editing string (" (number->string (- end start)) " chars)"))))))

;;;============================================================================
;;; Persistent-scratch (Qt)
;;;============================================================================

(def *qt-persistent-scratch-active* #f)

(def (qt-scratch-path)
  (string-append (getenv "HOME" "/tmp") "/.gemacs-scratch"))

(def (cmd-qt-persistent-scratch-mode app)
  "Toggle persistent scratch mode."
  (set! *qt-persistent-scratch-active* (not *qt-persistent-scratch-active*))
  (qt-echo-message app
    (if *qt-persistent-scratch-active*
      (string-append "Persistent scratch enabled (" (qt-scratch-path) ")")
      "Persistent scratch disabled")))

(def (cmd-qt-persistent-scratch-save app)
  "Save *scratch* buffer to disk."
  (let ((path (qt-scratch-path))
        (buf (find (lambda (b) (string=? (buffer-name b) "*scratch*"))
                   (buffer-list))))
    (if (not buf)
      (qt-echo-message app "No *scratch* buffer found")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (with-output-to-file path (lambda () (display text)))
        (qt-echo-message app
          (string-append "Scratch saved (" (number->string (string-length text)) " bytes)"))))))

(def (cmd-qt-persistent-scratch-restore app)
  "Restore *scratch* buffer from disk."
  (let ((path (qt-scratch-path)))
    (if (not (file-exists? path))
      (qt-echo-message app "No saved scratch file found")
      (let ((text (with-input-from-file path (lambda () (read-string 1048576)))))
        (qt-open-output-buffer app "*scratch*" text)
        (qt-echo-message app
          (string-append "Scratch restored (" (number->string (string-length text)) " bytes)"))))))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity8-commands!)
  ;; Surround
  (register-command! 'surround-add cmd-qt-surround-add)
  (register-command! 'surround-delete cmd-qt-surround-delete)
  (register-command! 'surround-change cmd-qt-surround-change)
  ;; Goto-last-change
  (register-command! 'goto-last-change cmd-qt-goto-last-change)
  (register-command! 'goto-last-change-reverse cmd-qt-goto-last-change-reverse)
  ;; Crosshair
  (register-command! 'crosshair-mode cmd-qt-crosshair-mode)
  ;; Quickrun
  (register-command! 'quickrun cmd-qt-quickrun)
  (register-command! 'quickrun-with cmd-qt-quickrun-with)
  ;; Eros
  (register-command! 'eros-mode cmd-qt-eros-mode)
  (register-command! 'eros-eval-last-sexp cmd-qt-eros-eval-last-sexp)
  ;; Auto-dim
  (register-command! 'auto-dim-other-buffers cmd-qt-auto-dim-other-buffers)
  ;; Rainbow identifiers
  (register-command! 'rainbow-identifiers-mode cmd-qt-rainbow-identifiers-mode)
  ;; Outline
  (register-command! 'outline-minor-mode cmd-qt-outline-minor-mode)
  (register-command! 'outline-toggle-children cmd-qt-outline-toggle-children)
  (register-command! 'outline-fold-all cmd-qt-outline-fold-all)
  (register-command! 'outline-unfold-all cmd-qt-outline-unfold-all)
  ;; String edit
  (register-command! 'string-edit-at-point cmd-qt-string-edit-at-point)
  ;; Persistent scratch
  (register-command! 'persistent-scratch-mode cmd-qt-persistent-scratch-mode)
  (register-command! 'persistent-scratch-save cmd-qt-persistent-scratch-save)
  (register-command! 'persistent-scratch-restore cmd-qt-persistent-scratch-restore))
