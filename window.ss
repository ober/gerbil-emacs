;;; -*- Gerbil -*-
;;; Frame and window layout for gerbil-emacs
;;;
;;; Layout model (vertical splitting):
;;;   | edit-window-0 (edit area)   | rows y..y+h-2
;;;   | modeline-0 (1 row)          | row y+h-1
;;;   | edit-window-1 (edit area)   |
;;;   | modeline-1 (1 row)          |
;;;   | echo area (1 row)           | last row

(export
  (struct-out edit-window)
  (struct-out frame)
  current-window
  frame-init!
  frame-shutdown!
  frame-resize!
  frame-layout!
  frame-refresh!
  frame-split!
  frame-split-right!
  frame-delete-window!
  frame-delete-other-windows!
  frame-other-window!
  frame-draw-dividers!)

(import :std/sugar
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/buffer)

;;;============================================================================
;;; Structures
;;;============================================================================

(defstruct edit-window
  (editor   ; scintilla-editor instance
   buffer   ; buffer struct
   x y w h) ; position and size in terminal cells
  transparent: #t)

(defstruct frame
  (windows        ; list of edit-window
   current-idx    ; index of active window
   width          ; terminal width
   height         ; terminal height
   split-direction) ; 'vertical (stacked) or 'horizontal (side-by-side)
  transparent: #t)

;;;============================================================================
;;; Accessors
;;;============================================================================

(def (current-window fr)
  (list-ref (frame-windows fr) (frame-current-idx fr)))

;;;============================================================================
;;; Frame initialization and shutdown
;;;============================================================================

(def (frame-init! width height)
  "Create initial frame with one window and a scratch buffer."
  (let* ((edit-h (max 1 (- height 2)))  ; 1 row modeline, 1 row echo
         (ed (create-scintilla-editor width: width height: edit-h))
         (buf (buffer-create-from-editor! buffer-scratch-name ed))
         (win (make-edit-window ed buf 0 0 width (- height 1))))
    (make-frame (list win) 0 width height 'vertical)))

(def (frame-shutdown! fr)
  "Destroy all editors in the frame."
  (for-each (lambda (win) (editor-destroy (edit-window-editor win)))
            (frame-windows fr)))

;;;============================================================================
;;; Layout: distribute available rows among windows
;;;============================================================================

(def (frame-layout! fr)
  (if (eq? (frame-split-direction fr) 'horizontal)
    (frame-layout-horizontal! fr)
    (frame-layout-vertical! fr)))

(def (frame-layout-vertical! fr)
  (let* ((width (frame-width fr))
         (height (frame-height fr))
         (windows (frame-windows fr))
         (n (length windows))
         (avail (- height 1))          ; 1 row for echo area
         (per-win (quotient avail n))
         (extra (remainder avail n)))
    (let loop ((wins windows) (y 0) (i 0))
      (when (pair? wins)
        (let* ((win (car wins))
               (h (+ per-win (if (< i extra) 1 0)))
               (edit-h (max 1 (- h 1))))  ; at least 1 row edit, 1 for modeline
          (set! (edit-window-x win) 0)
          (set! (edit-window-y win) y)
          (set! (edit-window-w win) width)
          (set! (edit-window-h win) h)
          (editor-resize (edit-window-editor win) width edit-h)
          (editor-move (edit-window-editor win) 0 y)
          (loop (cdr wins) (+ y h) (+ i 1)))))))

(def (frame-layout-horizontal! fr)
  "Side-by-side layout: each window gets a share of the width."
  (let* ((width (frame-width fr))
         (height (frame-height fr))
         (windows (frame-windows fr))
         (n (length windows))
         (win-h (- height 1))          ; 1 row for echo area
         (edit-h (max 1 (- win-h 1)))  ; 1 row for modeline per window
         ;; n-1 divider columns between windows
         (dividers (max 0 (- n 1)))
         (avail-w (- width dividers))
         (per-win (quotient avail-w n))
         (extra (remainder avail-w n)))
    (let loop ((wins windows) (x 0) (i 0))
      (when (pair? wins)
        (let* ((win (car wins))
               (w (+ per-win (if (< i extra) 1 0))))
          (set! (edit-window-x win) x)
          (set! (edit-window-y win) 0)
          (set! (edit-window-w win) w)
          (set! (edit-window-h win) win-h)
          (editor-resize (edit-window-editor win) w edit-h)
          (editor-move (edit-window-editor win) x 0)
          ;; Skip past window width + 1 divider column
          (loop (cdr wins) (+ x w 1) (+ i 1)))))))

;;;============================================================================
;;; Resize (terminal size changed)
;;;============================================================================

(def (frame-resize! fr width height)
  (set! (frame-width fr) width)
  (set! (frame-height fr) height)
  (frame-layout! fr))

;;;============================================================================
;;; Refresh all editors
;;;============================================================================

(def (frame-refresh! fr)
  (for-each (lambda (win)
              (editor-refresh (edit-window-editor win)))
            (frame-windows fr)))

;;;============================================================================
;;; Window management
;;;============================================================================

(def (frame-split! fr)
  "Split vertically: add a new window below. Returns the new editor."
  (set! (frame-split-direction fr) 'vertical)
  (let* ((cur (current-window fr))
         (buf (edit-window-buffer cur))
         (new-ed (create-scintilla-editor))
         (new-win (make-edit-window new-ed buf 0 0 0 0)))
    (buffer-attach! new-ed buf)
    (set! (frame-windows fr)
          (append (frame-windows fr) (list new-win)))
    (frame-layout! fr)
    new-ed))

(def (frame-split-right! fr)
  "Split horizontally: add a new window to the right. Returns the new editor."
  (set! (frame-split-direction fr) 'horizontal)
  (let* ((cur (current-window fr))
         (buf (edit-window-buffer cur))
         (new-ed (create-scintilla-editor))
         (new-win (make-edit-window new-ed buf 0 0 0 0)))
    (buffer-attach! new-ed buf)
    (set! (frame-windows fr)
          (append (frame-windows fr) (list new-win)))
    (frame-layout! fr)
    new-ed))

(def (frame-delete-window! fr)
  "Delete the current window (if more than one)."
  (when (> (length (frame-windows fr)) 1)
    (let* ((idx (frame-current-idx fr))
           (win (list-ref (frame-windows fr) idx)))
      (editor-destroy (edit-window-editor win))
      (set! (frame-windows fr) (list-remove-idx (frame-windows fr) idx))
      (when (>= (frame-current-idx fr) (length (frame-windows fr)))
        (set! (frame-current-idx fr) (- (length (frame-windows fr)) 1)))
      (frame-layout! fr))))

(def (frame-delete-other-windows! fr)
  "Keep only the current window, destroy all others."
  (let ((cur (current-window fr)))
    (for-each (lambda (win)
                (unless (eq? win cur)
                  (editor-destroy (edit-window-editor win))))
              (frame-windows fr))
    (set! (frame-windows fr) (list cur))
    (set! (frame-current-idx fr) 0)
    (frame-layout! fr)))

(def (frame-other-window! fr)
  "Switch to the next window."
  (let ((n (length (frame-windows fr))))
    (set! (frame-current-idx fr)
          (modulo (+ (frame-current-idx fr) 1) n))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (list-remove-idx lst idx)
  (let loop ((l lst) (i 0) (acc []))
    (cond
      ((null? l) (reverse acc))
      ((= i idx) (append (reverse acc) (cdr l)))
      (else (loop (cdr l) (+ i 1) (cons (car l) acc))))))

;;;============================================================================
;;; Divider drawing (for horizontal splits)
;;;============================================================================

(def (frame-draw-dividers! fr)
  "Draw vertical divider lines between side-by-side windows."
  (when (eq? (frame-split-direction fr) 'horizontal)
    (let* ((windows (frame-windows fr))
           (height (- (frame-height fr) 1))  ; don't draw over echo area
           (fg #x808080)
           (bg #x181818))
      ;; Draw a vertical line at x = (window-x + window-w) for each window
      ;; except the last one
      (let loop ((wins windows))
        (when (and (pair? wins) (pair? (cdr wins)))
          (let ((x (+ (edit-window-x (car wins))
                       (edit-window-w (car wins)))))
            (let yloop ((y 0))
              (when (< y height)
                (tui-change-cell! x y (char->integer #\│) fg bg)
                (yloop (+ y 1)))))
          (loop (cdr wins)))))))
