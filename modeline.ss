;;; -*- Gerbil -*-
;;; Status line rendering for gerbil-emacs
;;;
;;; Format: -UU-:**-  buffer-name    (line,col)
;;; Drawn with reversed colors via tui-print!

(export modeline-draw!)

(import :std/sugar
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/highlight)

;;;============================================================================
;;; Modeline rendering
;;;============================================================================

(def (buffer-mode-name buf)
  "Return the major mode name for a buffer."
  (let ((lang (buffer-lexer-lang buf)))
    (cond
      ((eq? lang 'dired)  "Dired")
      ((eq? lang 'repl)   "REPL")
      ((eq? lang 'eshell) "Eshell")
      ((eq? lang 'shell)  "Shell")
      ;; Check if file has a Gerbil extension
      ((and (buffer-file-path buf)
            (gerbil-file-extension? (buffer-file-path buf)))
       "Gerbil")
      (else "Fundamental"))))

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
         (name (buffer-name buf))
         (mode (buffer-mode-name buf))
         (pct  (buffer-position-percent ed))
         ;; Build the info string
         ;; -UU-:**-  name    (mode) L42 C1  Top
         (mod-str (if mod? "**" "--"))
         (left (string-append
                "-UU-:" mod-str "-  " name "  "))
         (right (string-append
                 "(" mode ") "
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
