;;; -*- Gerbil -*-
;;; Status line rendering for gerbil-emacs
;;;
;;; Format: -UUU:**-  buffer-name    (mode) L42 C1  Top
;;; Drawn with reversed colors via tui-print!

(export modeline-draw!)

(import :std/sugar
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/highlight)

;;;============================================================================
;;; Mode name detection
;;;============================================================================

(def (buffer-mode-name buf)
  "Return the major mode name for a buffer."
  (let ((lang (buffer-lexer-lang buf)))
    (case lang
      ((scheme gerbil) "Gerbil")
      ((lisp) "Lisp")
      ((python) "Python")
      ((c) "C")
      ((cpp) "C++")
      ((javascript) "JS")
      ((typescript) "TS")
      ((rust) "Rust")
      ((go) "Go")
      ((java) "Java")
      ((ruby) "Ruby")
      ((shell bash) "Shell")
      ((markdown) "Markdown")
      ((org) "Org")
      ((json) "JSON")
      ((yaml) "YAML")
      ((toml) "TOML")
      ((html xml) "HTML")
      ((css) "CSS")
      ((sql) "SQL")
      ((lua) "Lua")
      ((zig) "Zig")
      ((nix) "Nix")
      ((dired) "Dired")
      ((repl) "REPL")
      ((eshell) "Eshell")
      ((shell-mode) "Shell")
      ((terminal) "Term")
      (else "Text"))))

;;;============================================================================
;;; Line ending detection
;;;============================================================================

(def (eol-indicator ed)
  "Return EOL indicator string based on Scintilla's EOL mode."
  (let ((mode (send-message ed SCI_GETEOLMODE 0 0)))
    (cond
      ((= mode SC_EOL_LF) "LF")
      ((= mode SC_EOL_CRLF) "CRLF")
      ((= mode SC_EOL_CR) "CR")
      (else "LF"))))

;;;============================================================================
;;; Position percentage
;;;============================================================================

(def (buffer-position-percent ed)
  "Return position as percentage string (Top/Bot/All/NN%)."
  (let* ((pos (editor-get-current-pos ed))
         (len (editor-get-text-length ed))
         (first-vis (editor-get-first-visible-line ed))
         (total (editor-get-line-count ed)))
    (cond
      ((= len 0) "All")
      ((= first-vis 0) "Top")
      ((>= (+ first-vis 1) total) "Bot")
      (else
       (let ((pct (quotient (* pos 100) (max len 1))))
         (string-append (number->string pct) "%"))))))

;;;============================================================================
;;; Modeline rendering
;;;============================================================================

(def (modeline-draw! win is-current)
  "Draw the modeline for an edit-window at its bottom row."
  (let* ((buf (edit-window-buffer win))
         (ed  (edit-window-editor win))
         (y   (+ (edit-window-y win) (- (edit-window-h win) 1)))
         (w   (edit-window-w win))
         (pos  (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (col  (+ 1 (editor-get-column ed pos)))
         (mod? (editor-get-modify? ed))
         (ro?  (= 1 (send-message ed SCI_GETREADONLY 0 0)))
         (name (buffer-name buf))
         (mode (buffer-mode-name buf))
         (pct  (buffer-position-percent ed))
         (eol  (eol-indicator ed))
         ;; Modified/read-only indicator
         (state-str (cond
                      ((and ro? mod?) "%*")
                      (ro? "%%")
                      (mod? "**")
                      (else "--")))
         (left (string-append
                "-U:" state-str "-  " name "  "))
         (right (string-append
                 "(" mode " " eol ") "
                 "L" (number->string line)
                 " C" (number->string col)
                 "  " pct))
         ;; Compute padding between left and right
         (total-len (+ (string-length left) (string-length right)))
         (info (if (< total-len w)
                 (string-append left
                                (make-string (- w total-len) #\-)
                                right)
                 (let ((combined (string-append left right)))
                   (if (> (string-length combined) w)
                     (substring combined 0 w)
                     (string-append combined
                                    (make-string (- w (string-length combined)) #\-))))))
         ;; Active window: dark on light; inactive: dimmer
         (fg (if is-current #x000000 #x808080))
         (bg (if is-current #xd8d8d8 #x282828)))
    (tui-print! 0 y fg bg info)))
