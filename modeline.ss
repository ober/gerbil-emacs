;;; -*- Gerbil -*-
;;; Status line rendering for gerbil-emacs
;;;
;;; Format: -UU-:**-  buffer-name    (line,col)
;;; Drawn with reversed colors via tui-print!

(export modeline-draw!)

(import :std/sugar
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/buffer
        :gerbil-emacs/window)

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
         (name (buffer-name buf))
         ;; Build the info string
         (mod-str (if mod? "**" "--"))
         (info (string-append
                "-UU-:" mod-str "-  " name
                "    (" (number->string line)
                "," (number->string col) ")"))
         ;; Pad with dashes to fill width
         (padded (if (< (string-length info) w)
                   (string-append info
                                  (make-string (- w (string-length info)) #\-))
                   (substring info 0 w)))
         ;; Active window: white on black; inactive: dimmer
         (fg (if is-current
               (bitwise-ior TB_BLACK TB_BOLD)
               TB_WHITE))
         (bg (if is-current TB_WHITE TB_BLACK)))
    (tui-print! 0 y fg bg padded)))
