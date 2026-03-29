;;; -*- Gerbil -*-
;;; Corfu-mode (completion popup), minimap, orderless completion,
;;; marginalia annotations, smooth scroll, context menu, ligature,
;;; nano-theme, page-break-lines, doom-modeline.
;;; New module to keep other editor-extra files under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
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
;;; Corfu-mode — in-buffer completion-at-point popup
;;;============================================================================

;; State
(def *corfu-mode-active* #f)
(def *corfu-idle-delay* 0.5)
(def *corfu-min-prefix* 3)
(def *corfu-max-candidates* 15)

;; Language keyword tables for richer completion
(def *corfu-lang-keywords* (make-hash-table))

(hash-put! *corfu-lang-keywords* 'scheme
  '("define" "lambda" "let" "let*" "letrec" "begin" "cond" "case"
    "if" "when" "unless" "and" "or" "not" "set!" "do" "import" "export"
    "def" "defstruct" "defmethod" "defrule" "defvar!" "defonce"
    "match" "with-catch" "try" "catch" "finally" "for-each" "map"
    "filter" "fold" "values" "call-with-values" "display" "displayln"
    "string-append" "string-length" "string-ref" "substring"
    "number->string" "string->number" "symbol->string" "string->symbol"
    "hash-put!" "hash-get" "hash-ref" "hash-key?" "hash-keys"
    "make-hash-table" "hash->list" "hash-for-each" "hash-remove!"))

(hash-put! *corfu-lang-keywords* 'python
  '("def" "class" "import" "from" "return" "if" "elif" "else" "for"
    "while" "try" "except" "finally" "with" "as" "yield" "lambda"
    "pass" "break" "continue" "raise" "True" "False" "None"
    "print" "len" "range" "list" "dict" "set" "tuple" "str" "int"
    "float" "bool" "open" "self" "super" "__init__" "__str__"))

(hash-put! *corfu-lang-keywords* 'javascript
  '("function" "const" "let" "var" "return" "if" "else" "for" "while"
    "do" "switch" "case" "break" "continue" "try" "catch" "finally"
    "throw" "class" "extends" "new" "this" "super" "import" "export"
    "default" "async" "await" "yield" "typeof" "instanceof"
    "console" "document" "window" "Array" "Object" "Promise"
    "Map" "Set" "JSON" "Math" "String" "Number" "Boolean" "null"
    "undefined" "true" "false"))

(hash-put! *corfu-lang-keywords* 'c
  '("int" "char" "float" "double" "void" "long" "short" "unsigned"
    "signed" "const" "static" "extern" "struct" "union" "enum"
    "typedef" "sizeof" "return" "if" "else" "for" "while" "do"
    "switch" "case" "break" "continue" "goto" "default" "include"
    "define" "ifdef" "ifndef" "endif" "NULL" "malloc" "free"
    "printf" "fprintf" "sprintf" "strlen" "strcmp" "strcpy" "memcpy"))

(hash-put! *corfu-lang-keywords* 'rust
  '("fn" "let" "mut" "const" "static" "struct" "enum" "impl" "trait"
    "pub" "use" "mod" "crate" "self" "super" "where" "type" "as"
    "if" "else" "match" "for" "while" "loop" "break" "continue"
    "return" "move" "async" "await" "unsafe" "dyn" "ref"
    "String" "Vec" "Option" "Result" "Some" "None" "Ok" "Err"
    "Box" "Rc" "Arc" "HashMap" "HashSet" "println!" "format!"))

(def (corfu-word-char? ch)
  "Is CH a word character for completion purposes?"
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-) (char=? ch #\?) (char=? ch #\!)))

(def (corfu-collect-buffer-words text prefix pos)
  "Collect unique word candidates from TEXT matching PREFIX, excluding word at POS."
  (let ((candidates (make-hash-table))
        (len (string-length text))
        (plen (string-length prefix)))
    (let scan ((i 0))
      (when (< i len)
        (if (corfu-word-char? (string-ref text i))
          (let word-end ((k i))
            (if (or (>= k len) (not (corfu-word-char? (string-ref text k))))
              (begin
                (let ((word (substring text i k)))
                  (when (and (> (string-length word) plen)
                             (string-prefix? prefix word)
                             (not (= i (- pos plen))))
                    (hash-put! candidates word #t)))
                (scan k))
              (word-end (+ k 1))))
          (scan (+ i 1)))))
    (hash-keys candidates)))

(def (corfu-collect-keywords lang prefix)
  "Get language keywords matching PREFIX."
  (let ((kws (hash-get *corfu-lang-keywords* lang)))
    (if kws
      (filter (lambda (w) (string-prefix? prefix w)) kws)
      '())))

(def (corfu-lang-from-buffer buf)
  "Determine language symbol from buffer's lexer-lang."
  (let ((lang (buffer-lexer-lang buf)))
    (cond
      ((not lang) #f)
      ((memq lang '(scheme gerbil lisp)) 'scheme)
      ((memq lang '(python)) 'python)
      ((memq lang '(javascript typescript coffeescript)) 'javascript)
      ((memq lang '(c cpp objc)) 'c)
      ((memq lang '(rust)) 'rust)
      (else #f))))

(def (cmd-corfu-complete app)
  "Trigger completion-at-point popup with buffer words + language keywords."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0))
         (len (string-length text)))
    (let* ((prefix-start
            (let loop ((i (- pos 1)))
              (if (or (< i 0) (not (corfu-word-char? (string-ref text i))))
                (+ i 1) (loop (- i 1)))))
           (prefix (substring text prefix-start pos))
           (plen (string-length prefix)))
      (if (= plen 0)
        (echo-message! echo "No prefix to complete")
        (let* ((buf-words (corfu-collect-buffer-words text prefix pos))
               (lang (corfu-lang-from-buffer buf))
               (kw-words (corfu-collect-keywords lang prefix))
               (all-ht (make-hash-table)))
          (for-each (lambda (w) (hash-put! all-ht w 'keyword)) kw-words)
          (for-each (lambda (w) (hash-put! all-ht w 'buffer)) buf-words)
          (let* ((words (sort (hash-keys all-ht) string<?))
                 (limited (if (> (length words) *corfu-max-candidates*)
                            (take words *corfu-max-candidates*)
                            words)))
            (if (null? limited)
              (echo-message! echo (string-append "No completions for \"" prefix "\""))
              (begin
                (send-message ed SCI_AUTOCSETSEPARATOR (char->integer #\newline) 0)
                (send-message ed SCI_AUTOCSETIGNORECASE 0 0)
                (send-message ed SCI_AUTOCSETMAXHEIGHT *corfu-max-candidates* 0)
                (send-message ed SCI_AUTOCSETDROPRESTOFWORD 1 0)
                (send-message ed SCI_AUTOCSETORDER 1 0)
                (let ((item-list (string-join limited "\n")))
                  (scintilla-send-message-string
                    (scintilla-editor-handle ed) SCI_AUTOCSHOW plen item-list))
                (echo-message! echo
                  (string-append (number->string (length limited)) " completions"
                                 (if lang (string-append " [" (symbol->string lang) "]") "")))))))))))

(def (cmd-corfu-mode-real app)
  "Toggle corfu-mode — in-buffer completion-at-point with language awareness."
  (set! *corfu-mode-active* (not *corfu-mode-active*))
  (echo-message! (app-state-echo app)
    (if *corfu-mode-active*
      "Corfu mode: on (use completion-at-point or C-M-i)"
      "Corfu mode: off")))

;;;============================================================================
;;; Minimap-mode — zoomed-out code overview
;;;============================================================================

(def *minimap-active* #f)
(def *minimap-zoom-level* -10)

(def (cmd-minimap-mode-real app)
  "Toggle minimap-mode — show a zoomed-out code overview in a narrow split.
   Uses the second window as a minimap with reduced zoom."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (wins (frame-windows fr)))
    (set! *minimap-active* (not *minimap-active*))
    (if *minimap-active*
      (begin
        ;; Configure minimap on rightmost window (if split exists)
        (when (>= (length wins) 2)
          (let* ((map-win (list-ref wins (- (length wins) 1)))
                 (map-ed (edit-window-editor map-win)))
            ;; Attach same buffer
            (let ((main-buf (edit-window-buffer (list-ref wins 0))))
              (when main-buf
                (buffer-attach! map-ed main-buf)
                (set! (edit-window-buffer map-win) main-buf)))
            ;; Zoom out
            (send-message map-ed SCI_SETZOOM *minimap-zoom-level* 0)
            ;; Disable line numbers
            (send-message map-ed SCI_SETMARGINWIDTHN 0 0)
            ;; Read-only
            (send-message map-ed SCI_SETREADONLY 1 0)
            ;; Narrow width
            (set! (edit-window-size-bias map-win) -3)
            (frame-layout! fr)))
        (echo-message! echo "Minimap: on (split window to see overview)"))
      (begin
        (when (>= (length wins) 2)
          (let* ((map-win (list-ref wins (- (length wins) 1)))
                 (map-ed (edit-window-editor map-win)))
            (send-message map-ed SCI_SETZOOM 0 0)
            (send-message map-ed SCI_SETREADONLY 0 0)
            (set! (edit-window-size-bias map-win) 0)
            (frame-layout! fr)))
        (echo-message! echo "Minimap: off")))))

;;;============================================================================
;;; Orderless completion — space-separated flexible matching
;;;============================================================================

(def *orderless-active* #f)

(def (orderless-match? pattern candidate)
  "Check if CANDIDATE matches PATTERN using orderless matching.
   Space-separated tokens match in any order.
   Supports: plain tokens (substring), !token (negation), ^token (prefix)."
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
             (let ((tok-lower (string-downcase tok)))
               (if (string-contains cand-lower tok-lower)
                 (loop (cdr toks)) #f)))))))))

(def (orderless-filter pattern candidates)
  "Filter CANDIDATES by orderless PATTERN."
  (filter (lambda (c) (orderless-match? pattern c)) candidates))

(def (cmd-orderless-mode-real app)
  "Toggle orderless completion matching — space-separated tokens in any order."
  (set! *orderless-active* (not *orderless-active*))
  (echo-message! (app-state-echo app)
    (if *orderless-active*
      "Orderless matching: on (space separates tokens, ! negates, ^ anchors prefix)"
      "Orderless matching: off")))

(def (cmd-orderless-filter-demo app)
  "Demonstrate orderless filtering on registered commands."
  (let* ((echo (app-state-echo app))
         (pattern (app-read-string app "Orderless pattern: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((all-cmds (map symbol->string (hash-keys *all-commands*)))
             (matches (orderless-filter pattern all-cmds))
             (sorted (sort matches string<?))
             (limited (if (> (length sorted) 20) (take sorted 20) sorted)))
        (echo-message! echo
          (string-append (number->string (length matches)) " matches: "
                         (string-join limited ", ")))))))

;;;============================================================================
;;; Marginalia — annotations in completion candidates
;;;============================================================================

(def *marginalia-active2* #f)

(def (marginalia-annotate-command cmd-name)
  "Annotate a command name with its keybinding and doc."
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

(def (marginalia-annotate-buffer buf-name)
  "Annotate a buffer name with file path and mode."
  (let ((buf (buffer-by-name buf-name)))
    (if buf
      (let* ((path (buffer-file-path buf))
             (lang (buffer-lexer-lang buf))
             (modified? (buffer-modified buf))
             (parts '()))
        (when modified? (set! parts (cons "[modified]" parts)))
        (when lang (set! parts (cons (symbol->string lang) parts)))
        (when path
          (let ((dir (path-directory path)))
            (set! parts (cons dir parts))))
        (if (null? parts) ""
          (string-append "  — " (string-join (reverse parts) " "))))
      "")))

(def (cmd-marginalia-mode-real app)
  "Toggle marginalia-mode — annotations in completion candidates."
  (set! *marginalia-active2* (not *marginalia-active2*))
  (echo-message! (app-state-echo app)
    (if *marginalia-active2*
      "Marginalia: on (completions show annotations)"
      "Marginalia: off")))

(def (cmd-marginalia-describe-commands app)
  "Show all commands with marginalia annotations."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (all-cmds (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (annotated (map (lambda (c)
                           (string-append c (marginalia-annotate-command c)))
                         all-cmds))
         (text (string-join annotated "\n"))
         (buf (buffer-create! "*Marginalia: Commands*" ed #f)))
    (editor-set-text ed text)
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (echo-message! echo
      (string-append (number->string (length all-cmds)) " commands with annotations"))))

;;;============================================================================
;;; Pixel-scroll-precision-mode (smooth scrolling) — TUI
;;;============================================================================

(def *pixel-scroll-active* #f)
(def *scroll-margin* 3)

(def (cmd-pixel-scroll-precision-mode-real app)
  "Toggle smooth scrolling with scroll margin."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *pixel-scroll-active* (not *pixel-scroll-active*))
    (if *pixel-scroll-active*
      (begin
        ;; CARET_SLOP=1, CARET_STRICT=4, CARET_EVEN=8
        (send-message ed SCI_SETYCARETPOLICY (+ 1 4 8) *scroll-margin*)
        (echo-message! echo "Smooth scroll: on (margin: 3 lines)"))
      (begin
        (send-message ed SCI_SETYCARETPOLICY 0 0)
        (echo-message! echo "Smooth scroll: off")))))

;;;============================================================================
;;; Context-menu-mode — right-click menu (TUI simulation)
;;;============================================================================

(def *context-menu-active* #f)

(def (cmd-context-menu app)
  "Show a context menu with common actions (simulates right-click)."
  (let* ((echo (app-state-echo app))
         (items '("Cut" "Copy" "Paste" "---" "Undo" "Redo" "---"
                  "Select All" "Find..." "Go to Line..." "---"
                  "Toggle Comment" "Indent Region" "Format Buffer"))
         (choice (app-read-string app
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
                   ((string-prefix? "Fo" choice) 'format-buffer)
                   (else #f))))
        (if cmd
          (execute-command! app cmd)
          (echo-message! echo "Unknown action"))))))

(def (cmd-context-menu-mode-real app)
  "Toggle context-menu-mode."
  (set! *context-menu-active* (not *context-menu-active*))
  (echo-message! (app-state-echo app)
    (if *context-menu-active*
      "Context menu: on (use M-x context-menu)"
      "Context menu: off")))

;;;============================================================================
;;; Ligature-mode — font ligature display via Scintilla technology
;;;============================================================================

(def *ligature-mode-active* #f)

(def (cmd-ligature-mode-real app)
  "Toggle ligature display hints. When on, disables buffered draw
   which can help with font ligature rendering in supported terminals."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *ligature-mode-active* (not *ligature-mode-active*))
    (if *ligature-mode-active*
      (begin
        (send-message ed 2035 0 0)
        (echo-message! echo "Ligature mode: on (font ligatures enabled)"))
      (begin
        (send-message ed 2035 1 0)
        (echo-message! echo "Ligature mode: off")))))

;;;============================================================================
;;; Nano-theme-mode — minimalist appearance
;;;============================================================================

(def *nano-theme-active* #f)

(def (cmd-nano-theme-real app)
  "Toggle nano-theme — minimalist appearance with hidden margins and clean look."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *nano-theme-active* (not *nano-theme-active*))
    (if *nano-theme-active*
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 0)
        (send-message ed SCI_SETMARGINWIDTHN 2 0)
        (send-message ed 2155 0 16)  ;; SCI_SETMARGINLEFT
        (send-message ed SCI_SETHSCROLLBAR 0 0)
        (send-message ed SCI_SETCARETWIDTH 1 0)
        (send-message ed SCI_SETEDGEMODE 0 0)
        (echo-message! echo "Nano theme: on (minimalist appearance)"))
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 40)
        (send-message ed SCI_SETMARGINWIDTHN 2 12)
        (send-message ed 2155 0 4)
        (send-message ed SCI_SETHSCROLLBAR 1 0)
        (send-message ed SCI_SETCARETWIDTH 2 0)
        (echo-message! echo "Nano theme: off")))))

;;;============================================================================
;;; Page-break-lines-mode — display ^L as horizontal rule
;;;============================================================================

(def *page-break-lines-active* #f)

(def (cmd-page-break-lines-real app)
  "Toggle page-break-lines-mode — display ^L as horizontal rule."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (len (string-length text)))
    (set! *page-break-lines-active* (not *page-break-lines-active*))
    (if *page-break-lines-active*
      (begin
        ;; Use indicator 5 for page breaks
        (send-message ed SCI_INDICSETSTYLE 5 6)  ;; INDIC_STRIKE=6
        (send-message ed SCI_INDICSETFORE 5 #x808080)
        (let loop ((i 0) (count 0))
          (if (>= i len)
            (echo-message! echo
              (string-append "Page-break-lines: on (" (number->string count) " markers)"))
            (if (char=? (string-ref text i) #\page)
              (let* ((line (send-message ed SCI_LINEFROMPOSITION i 0))
                     (ls (send-message ed SCI_POSITIONFROMLINE line 0))
                     (le (send-message ed SCI_GETLINEENDPOSITION line 0)))
                (send-message ed SCI_SETINDICATORCURRENT 5 0)
                (send-message ed SCI_INDICATORFILLRANGE ls (- le ls))
                (loop (+ i 1) (+ count 1)))
              (loop (+ i 1) count)))))
      (begin
        (send-message ed SCI_SETINDICATORCURRENT 5 0)
        (send-message ed SCI_INDICATORCLEARRANGE 0 len)
        (echo-message! echo "Page-break-lines: off")))))

;;;============================================================================
;;; Doom-modeline-mode — enhanced mode line
;;;============================================================================

(def *doom-modeline-active* #f)

(def (doom-modeline-format app)
  "Format a doom-style mode line string."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (name (if buf (buffer-name buf) "*scratch*"))
         (modified? (and buf (buffer-modified buf)))
         (lang (and buf (buffer-lexer-lang buf)))
         (ed (edit-window-editor win))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0))
         (line (+ 1 (send-message ed SCI_LINEFROMPOSITION pos 0)))
         (col (send-message ed SCI_GETCOLUMN pos 0))
         (total-lines (send-message ed SCI_GETLINECOUNT 0 0))
         (pct (if (> total-lines 0)
                (inexact->exact (round (* 100 (/ line total-lines))))
                0)))
    (string-append
      (if modified? " * " "   ")
      name
      "  L" (number->string line)
      ":C" (number->string col)
      "  " (number->string pct) "%"
      (if lang (string-append "  (" (symbol->string lang) ")") ""))))

(def (cmd-doom-modeline-real app)
  "Toggle doom-modeline — enhanced mode line with extra info."
  (set! *doom-modeline-active* (not *doom-modeline-active*))
  (if *doom-modeline-active*
    (echo-message! (app-state-echo app)
      (string-append "Doom modeline: on — " (doom-modeline-format app)))
    (echo-message! (app-state-echo app) "Doom modeline: off")))
