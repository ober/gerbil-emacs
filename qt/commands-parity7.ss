;;; -*- Gerbil -*-
;;; Qt parity commands (part 7) — corfu-mode, minimap, orderless,
;;; marginalia, smooth-scroll, context-menu, ligature-mode,
;;; nano-theme, page-break-lines, doom-modeline.
;;; Chain position: after commands-parity6.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/sort
        :gemacs/core
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/commands-core
        :gemacs/qt/commands-parity5)

;;;============================================================================
;;; Corfu-mode (Qt) — in-buffer completion popup
;;;============================================================================

(def *qt-corfu-active* #f)
(def *qt-corfu-max-candidates* 15)

(def *qt-corfu-lang-keywords* (make-hash-table))

(hash-put! *qt-corfu-lang-keywords* 'scheme
  '("define" "lambda" "let" "let*" "letrec" "begin" "cond" "case"
    "if" "when" "unless" "and" "or" "not" "set!" "do" "import" "export"
    "def" "defstruct" "defmethod" "defrule" "defvar!" "defonce"
    "match" "with-catch" "try" "catch" "finally" "for-each" "map"
    "filter" "fold" "values" "call-with-values" "display" "displayln"
    "string-append" "string-length" "string-ref" "substring"
    "number->string" "string->number" "symbol->string" "string->symbol"
    "hash-put!" "hash-get" "hash-ref" "hash-key?" "hash-keys"
    "make-hash-table" "hash->list" "hash-for-each" "hash-remove!"))

(hash-put! *qt-corfu-lang-keywords* 'python
  '("def" "class" "import" "from" "return" "if" "elif" "else" "for"
    "while" "try" "except" "finally" "with" "as" "yield" "lambda"
    "pass" "break" "continue" "raise" "True" "False" "None"
    "print" "len" "range" "list" "dict" "set" "tuple" "str" "int"))

(hash-put! *qt-corfu-lang-keywords* 'javascript
  '("function" "const" "let" "var" "return" "if" "else" "for" "while"
    "do" "switch" "case" "break" "continue" "try" "catch" "finally"
    "throw" "class" "extends" "new" "this" "super" "import" "export"
    "default" "async" "await" "typeof" "instanceof"
    "console" "document" "window" "Array" "Object" "Promise"
    "null" "undefined" "true" "false"))

(hash-put! *qt-corfu-lang-keywords* 'c
  '("int" "char" "float" "double" "void" "long" "short" "unsigned"
    "const" "static" "extern" "struct" "union" "enum" "typedef"
    "sizeof" "return" "if" "else" "for" "while" "do" "switch" "case"
    "break" "continue" "NULL" "malloc" "free" "printf" "strlen"))

(hash-put! *qt-corfu-lang-keywords* 'rust
  '("fn" "let" "mut" "const" "static" "struct" "enum" "impl" "trait"
    "pub" "use" "mod" "if" "else" "match" "for" "while" "loop"
    "break" "continue" "return" "async" "await" "unsafe"
    "String" "Vec" "Option" "Result" "Some" "None" "Ok" "Err"))

(def (qt-corfu-word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-) (char=? ch #\?) (char=? ch #\!)))

(def (qt-corfu-lang buf)
  (let ((lang (buffer-lexer-lang buf)))
    (cond
      ((not lang) #f)
      ((memq lang '(scheme gerbil lisp)) 'scheme)
      ((memq lang '(python)) 'python)
      ((memq lang '(javascript typescript)) 'javascript)
      ((memq lang '(c cpp)) 'c)
      ((memq lang '(rust)) 'rust)
      (else #f))))

(def (cmd-qt-corfu-complete app)
  "Trigger completion-at-point popup with buffer words + language keywords."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (word-start (sci-send ed SCI_WORDSTARTPOSITION pos 1))
         (plen (- pos word-start)))
    (if (= plen 0)
      (echo-message! echo "No prefix to complete")
      (let* ((prefix (qt-plain-text-edit-text-range ed word-start pos))
             (text (qt-plain-text-edit-text ed))
             (len (string-length text))
             (candidates (make-hash-table)))
        ;; Scan buffer for words
        (let scan ((i 0))
          (when (< i len)
            (if (qt-corfu-word-char? (string-ref text i))
              (let word-end ((k i))
                (if (or (>= k len) (not (qt-corfu-word-char? (string-ref text k))))
                  (begin
                    (let ((word (substring text i k)))
                      (when (and (> (string-length word) plen)
                                 (string-prefix? prefix word)
                                 (not (= i word-start)))
                        (hash-put! candidates word #t)))
                    (scan k))
                  (word-end (+ k 1))))
              (scan (+ i 1)))))
        ;; Add language keywords
        (let* ((lang (qt-corfu-lang buf))
               (kws (and lang (hash-get *qt-corfu-lang-keywords* lang))))
          (when kws
            (for-each (lambda (w)
                        (when (string-prefix? prefix w)
                          (hash-put! candidates w #t)))
                      kws)))
        ;; Show popup
        (let* ((words (sort (hash-keys candidates) string<?))
               (limited (if (> (length words) *qt-corfu-max-candidates*)
                          (take words *qt-corfu-max-candidates*)
                          words)))
          (if (null? limited)
            (echo-message! echo (string-append "No completions for \"" prefix "\""))
            (begin
              (sci-send ed SCI_AUTOCSETSEPARATOR (char->integer #\newline) 0)
              (sci-send ed SCI_AUTOCSETIGNORECASE 0 0)
              (sci-send ed SCI_AUTOCSETMAXHEIGHT *qt-corfu-max-candidates* 0)
              (sci-send ed SCI_AUTOCSETDROPRESTOFWORD 1 0)
              (sci-send ed SCI_AUTOCSETORDER 1 0)
              (sci-send/string ed SCI_AUTOCSHOW
                (string-join limited "\n") plen)
              (echo-message! echo
                (string-append (number->string (length limited)) " completions")))))))))

(def (cmd-qt-corfu-mode-real app)
  "Toggle corfu-mode — in-buffer completion popup with language awareness."
  (set! *qt-corfu-active* (not *qt-corfu-active*))
  (echo-message! (app-state-echo app)
    (if *qt-corfu-active*
      "Corfu mode: on (use completion-at-point or C-M-i)"
      "Corfu mode: off")))

;;;============================================================================
;;; Minimap-mode (Qt)
;;;============================================================================

(def *qt-minimap-active* #f)

(def (cmd-qt-minimap-mode-real app)
  "Toggle minimap — zoomed-out code overview."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app))
         (wins (qt-frame-windows fr)))
    (set! *qt-minimap-active* (not *qt-minimap-active*))
    (if *qt-minimap-active*
      (begin
        (when (>= (length wins) 2)
          (let ((map-ed (qt-edit-window-editor (list-ref wins (- (length wins) 1)))))
            (let ((main-buf (current-qt-buffer app)))
              (when main-buf
                (qt-buffer-attach! map-ed main-buf)
                (set! (qt-edit-window-buffer (list-ref wins (- (length wins) 1))) main-buf)))
            (sci-send map-ed SCI_SETZOOM -10)
            (sci-send map-ed SCI_SETMARGINWIDTHN 0 0)
            (sci-send map-ed SCI_SETREADONLY 1)))
        (echo-message! echo "Minimap: on"))
      (begin
        (when (>= (length wins) 2)
          (let ((map-ed (qt-edit-window-editor (list-ref wins (- (length wins) 1)))))
            (sci-send map-ed SCI_SETZOOM 0)
            (sci-send map-ed SCI_SETREADONLY 0)))
        (echo-message! echo "Minimap: off")))))

;;;============================================================================
;;; Orderless completion (Qt)
;;;============================================================================

(def *qt-orderless-active* #f)

(def (qt-orderless-match? pattern candidate)
  "Orderless matching: space-separated tokens, any order."
  (let* ((tokens (string-split pattern #\space))
         (tokens (filter (lambda (t) (> (string-length t) 0)) tokens))
         (cand-lower (string-downcase candidate)))
    (let loop ((toks tokens))
      (if (null? toks) #t
        (let ((tok (car toks)))
          (cond
            ((and (> (string-length tok) 1) (char=? (string-ref tok 0) #\!))
             (let ((rest-tok (string-downcase (substring tok 1 (string-length tok)))))
               (if (string-contains cand-lower rest-tok) #f (loop (cdr toks)))))
            ((and (> (string-length tok) 1) (char=? (string-ref tok 0) #\^))
             (let ((rest-tok (string-downcase (substring tok 1 (string-length tok)))))
               (if (string-prefix? rest-tok cand-lower) (loop (cdr toks)) #f)))
            (else
             (if (string-contains cand-lower (string-downcase tok))
               (loop (cdr toks)) #f))))))))

(def (cmd-qt-orderless-mode-real app)
  "Toggle orderless completion matching."
  (set! *qt-orderless-active* (not *qt-orderless-active*))
  (echo-message! (app-state-echo app)
    (if *qt-orderless-active*
      "Orderless: on (space separates tokens, ! negates, ^ prefix)"
      "Orderless: off")))

(def (cmd-qt-orderless-filter-demo app)
  "Demo orderless filtering on commands."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Orderless pattern: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((all-cmds (map symbol->string (hash-keys *all-commands*)))
             (matches (filter (lambda (c) (qt-orderless-match? pattern c)) all-cmds))
             (sorted (sort matches string<?))
             (limited (if (> (length sorted) 20) (take sorted 20) sorted)))
        (echo-message! echo
          (string-append (number->string (length matches)) " matches: "
                         (string-join limited ", ")))))))

;;;============================================================================
;;; Marginalia (Qt)
;;;============================================================================

(def *qt-marginalia-active2* #f)

(def (qt-marginalia-annotate-command cmd-name)
  (let* ((sym (if (string? cmd-name) (string->symbol cmd-name) cmd-name))
         (binding (find-keybinding-for-command sym))
         (doc (command-doc sym))
         (parts '()))
    (when (and doc (> (string-length doc) 0))
      (let ((short-doc (if (> (string-length doc) 40)
                         (string-append (substring doc 0 37) "...")
                         doc)))
        (set! parts (cons short-doc parts))))
    (when (and binding (> (string-length binding) 0))
      (set! parts (cons (string-append "(" binding ")") parts)))
    (if (null? parts) ""
      (string-append "  — " (string-join (reverse parts) " ")))))

(def (cmd-qt-marginalia-mode-real app)
  "Toggle marginalia annotations in completions."
  (set! *qt-marginalia-active2* (not *qt-marginalia-active2*))
  (echo-message! (app-state-echo app)
    (if *qt-marginalia-active2* "Marginalia: on" "Marginalia: off")))

(def (cmd-qt-marginalia-describe-commands app)
  "Show all commands with annotations."
  (let* ((all-cmds (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (annotated (map (lambda (c)
                           (string-append c (qt-marginalia-annotate-command c)))
                         all-cmds))
         (text (string-join annotated "\n")))
    (qt-open-output-buffer app "*Marginalia: Commands*" text)
    (echo-message! (app-state-echo app)
      (string-append (number->string (length all-cmds)) " commands"))))

;;;============================================================================
;;; Smooth scroll (Qt)
;;;============================================================================

(def *qt-pixel-scroll-active* #f)

(def (cmd-qt-pixel-scroll-real app)
  "Toggle smooth scrolling with scroll margin."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (set! *qt-pixel-scroll-active* (not *qt-pixel-scroll-active*))
    (if *qt-pixel-scroll-active*
      (begin
        (sci-send ed SCI_SETYCARETPOLICY (+ 1 4 8) 3)
        (echo-message! echo "Smooth scroll: on"))
      (begin
        (sci-send ed SCI_SETYCARETPOLICY 0 0)
        (echo-message! echo "Smooth scroll: off")))))

;;;============================================================================
;;; Context menu (Qt)
;;;============================================================================

(def (cmd-qt-context-menu app)
  "Show context menu with common actions."
  (let* ((echo (app-state-echo app))
         (items '("Cut" "Copy" "Paste" "Undo" "Redo"
                  "Select All" "Find" "Go to Line"
                  "Toggle Comment" "Indent Region"))
         (choice (qt-echo-read-string app
                   (string-append "Context: " (string-join items " | ") " > "))))
    (when (and choice (> (string-length choice) 0))
      (let ((cmd (cond
                   ((string-prefix? "Cu" choice) 'kill-region)
                   ((string-prefix? "Co" choice) 'kill-ring-save)
                   ((string-prefix? "Pa" choice) 'yank)
                   ((string-prefix? "Un" choice) 'undo)
                   ((string-prefix? "Re" choice) 'redo)
                   ((string-prefix? "Se" choice) 'mark-whole-buffer)
                   ((string-prefix? "Fi" choice) 'isearch-forward)
                   ((string-prefix? "Go" choice) 'goto-line)
                   ((string-prefix? "To" choice) 'comment-dwim)
                   ((string-prefix? "In" choice) 'indent-region)
                   (else #f))))
        (if cmd (execute-command! app cmd)
          (echo-message! echo "Unknown action"))))))

(def (cmd-qt-context-menu-mode-real app)
  "Toggle context-menu-mode."
  (set! *context-menu-active* (not *context-menu-active*))
  (echo-message! (app-state-echo app) "Context menu mode toggled"))

(def *context-menu-active* #f)

;;;============================================================================
;;; Ligature mode (Qt)
;;;============================================================================

(def (cmd-qt-ligature-mode-real app)
  "Toggle ligature display mode."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (current (sci-send ed 2034)))
    (sci-send ed 2035 (if (= current 1) 0 1))
    (echo-message! echo
      (if (= current 1) "Ligature mode: on" "Ligature mode: off"))))

;;;============================================================================
;;; Nano theme (Qt)
;;;============================================================================

(def (cmd-qt-nano-theme-real app)
  "Toggle nano-theme — minimalist appearance."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (margin-w (sci-send ed SCI_GETMARGINWIDTHN 0)))
    (if (> margin-w 0)
      (begin
        (sci-send ed SCI_SETMARGINWIDTHN 0 0)
        (sci-send ed SCI_SETMARGINWIDTHN 2 0)
        (sci-send ed 2155 0 16)
        (sci-send ed SCI_SETHSCROLLBAR 0)
        (sci-send ed SCI_SETCARETWIDTH 1)
        (sci-send ed SCI_SETEDGEMODE 0)
        (echo-message! echo "Nano theme: on"))
      (begin
        (sci-send ed SCI_SETMARGINWIDTHN 0 40)
        (sci-send ed SCI_SETMARGINWIDTHN 2 12)
        (sci-send ed 2155 0 4)
        (sci-send ed SCI_SETHSCROLLBAR 1)
        (sci-send ed SCI_SETCARETWIDTH 2)
        (echo-message! echo "Nano theme: off")))))

;;;============================================================================
;;; Page-break-lines (Qt)
;;;============================================================================

(def (cmd-qt-page-break-lines-real app)
  "Toggle page-break-lines — display ^L as horizontal rule."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (sci-send ed SCI_INDICSETSTYLE 5 6)  ;; INDIC_STRIKE
    (sci-send ed SCI_INDICSETFORE 5 #x808080)
    (let loop ((i 0) (count 0))
      (if (>= i len)
        (echo-message! echo
          (string-append "Page-break-lines: " (number->string count) " markers"))
        (if (char=? (string-ref text i) #\page)
          (let* ((line (sci-send ed SCI_LINEFROMPOSITION i))
                 (ls (sci-send ed SCI_POSITIONFROMLINE line))
                 (le (sci-send ed SCI_GETLINEENDPOSITION line)))
            (sci-send ed SCI_SETINDICATORCURRENT 5)
            (sci-send ed SCI_INDICATORFILLRANGE ls (- le ls))
            (loop (+ i 1) (+ count 1)))
          (loop (+ i 1) count))))))

;;;============================================================================
;;; Doom modeline (Qt)
;;;============================================================================

(def *qt-doom-modeline-active* #f)

(def (cmd-qt-doom-modeline-real app)
  "Toggle doom-modeline — enhanced mode line."
  (set! *qt-doom-modeline-active* (not *qt-doom-modeline-active*))
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (line (+ 1 (sci-send ed SCI_LINEFROMPOSITION pos)))
         (col (sci-send ed SCI_GETCOLUMN pos))
         (total (sci-send ed SCI_GETLINECOUNT))
         (pct (if (> total 0) (inexact->exact (round (* 100 (/ line total)))) 0))
         (name (if buf (buffer-name buf) "*scratch*"))
         (modified? (and buf (buffer-modified buf)))
         (lang (and buf (buffer-lexer-lang buf))))
    (echo-message! (app-state-echo app)
      (if *qt-doom-modeline-active*
        (string-append "Doom modeline: on — "
          (if modified? "* " "")
          name " L" (number->string line) ":C" (number->string col)
          " " (number->string pct) "%"
          (if lang (string-append " (" (symbol->string lang) ")") ""))
        "Doom modeline: off"))))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity7-commands!)
  (for-each (lambda (p) (register-command! (car p) (cdr p)))
    (list
      ;; Corfu
      (cons 'corfu-complete cmd-qt-corfu-complete)
      (cons 'corfu-mode-real cmd-qt-corfu-mode-real)
      ;; Minimap
      (cons 'minimap-mode-real cmd-qt-minimap-mode-real)
      ;; Orderless
      (cons 'orderless-mode-real cmd-qt-orderless-mode-real)
      (cons 'orderless-filter-demo cmd-qt-orderless-filter-demo)
      ;; Marginalia
      (cons 'marginalia-mode-real cmd-qt-marginalia-mode-real)
      (cons 'marginalia-describe-commands cmd-qt-marginalia-describe-commands)
      ;; Smooth scroll
      (cons 'pixel-scroll-precision-mode-real cmd-qt-pixel-scroll-real)
      ;; Context menu
      (cons 'context-menu cmd-qt-context-menu)
      (cons 'context-menu-mode-real cmd-qt-context-menu-mode-real)
      ;; Ligature
      (cons 'ligature-mode-real cmd-qt-ligature-mode-real)
      ;; Nano theme
      (cons 'nano-theme-real cmd-qt-nano-theme-real)
      ;; Page-break-lines
      (cons 'page-break-lines-real cmd-qt-page-break-lines-real)
      ;; Doom modeline
      (cons 'doom-modeline-real cmd-qt-doom-modeline-real))))
