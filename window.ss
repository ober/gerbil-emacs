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
  frame-draw-dividers!
  frame-enlarge-window!
  frame-shrink-window!
  frame-enlarge-window-horizontally!
  frame-shrink-window-horizontally!)

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
   x y w h  ; position and size in terminal cells
   size-bias) ; integer offset for window resize (default 0)
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
         (win (make-edit-window ed buf 0 0 width (- height 1) 0)))
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
         (extra (remainder avail n))
         ;; Compute bias-adjusted sizes, ensuring minimum of 2 rows per window
         (raw-sizes (let loop ((ws windows) (i 0) (acc []))
                      (if (null? ws) (reverse acc)
                        (let ((win (car ws)))
                          (loop (cdr ws) (+ i 1)
                                (cons (+ per-win
                                         (if (< i extra) 1 0)
                                         (or (edit-window-size-bias win) 0))
                                      acc))))))
         ;; Clamp to minimum 2 (1 edit + 1 modeline)
         (clamped (map (lambda (s) (max 2 s)) raw-sizes))
         ;; Normalize: ensure total equals avail
         (total (apply + clamped))
         (sizes (if (= total avail)
                  clamped
                  ;; Adjust last window to make total correct
                  (let ((delta (- avail total)))
                    (let loop2 ((ss (reverse clamped)) (acc []) (d delta))
                      (if (null? ss) (reverse acc)
                        (if (= d 0) (append (reverse ss) acc)
                          (let ((new-s (max 2 (+ (car ss) d))))
                            (loop2 (cdr ss) (cons new-s acc) 0)))))))))
    (let loop ((wins windows) (szs sizes) (y 0))
      (when (and (pair? wins) (pair? szs))
        (let* ((win (car wins))
               (h (car szs))
               (edit-h (max 1 (- h 1))))
          (set! (edit-window-x win) 0)
          (set! (edit-window-y win) y)
          (set! (edit-window-w win) width)
          (set! (edit-window-h win) h)
          (editor-resize (edit-window-editor win) width edit-h)
          (editor-move (edit-window-editor win) 0 y)
          (loop (cdr wins) (cdr szs) (+ y h)))))))

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
         (extra (remainder avail-w n))
         ;; Compute bias-adjusted widths
         (raw-widths (let loop ((ws windows) (i 0) (acc []))
                       (if (null? ws) (reverse acc)
                         (let ((win (car ws)))
                           (loop (cdr ws) (+ i 1)
                                 (cons (+ per-win
                                          (if (< i extra) 1 0)
                                          (or (edit-window-size-bias win) 0))
                                       acc))))))
         (clamped (map (lambda (w) (max 4 w)) raw-widths))
         (total (apply + clamped))
         (sizes (if (= total avail-w)
                  clamped
                  (let ((delta (- avail-w total)))
                    (let loop2 ((ss (reverse clamped)) (acc []) (d delta))
                      (if (null? ss) (reverse acc)
                        (if (= d 0) (append (reverse ss) acc)
                          (let ((new-s (max 4 (+ (car ss) d))))
                            (loop2 (cdr ss) (cons new-s acc) 0)))))))))
    (let loop ((wins windows) (szs sizes) (x 0))
      (when (and (pair? wins) (pair? szs))
        (let* ((win (car wins))
               (w (car szs)))
          (set! (edit-window-x win) x)
          (set! (edit-window-y win) 0)
          (set! (edit-window-w win) w)
          (set! (edit-window-h win) win-h)
          (editor-resize (edit-window-editor win) w edit-h)
          (editor-move (edit-window-editor win) x 0)
          (loop (cdr wins) (cdr szs) (+ x w 1)))))))

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
         (new-win (make-edit-window new-ed buf 0 0 0 0 0)))
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
         (new-win (make-edit-window new-ed buf 0 0 0 0 0)))
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

;;;============================================================================
;;; Window resizing
;;;============================================================================

(def (frame-enlarge-window! fr (delta 1))
  "Make the current window taller (vertical) or wider (horizontal) by delta rows/cols.
   Steals space from the next window (or previous if current is last)."
  (let* ((windows (frame-windows fr))
         (n (length windows)))
    (when (> n 1)
      (let* ((idx (frame-current-idx fr))
             (cur (list-ref windows idx))
             ;; Steal from neighbor
             (neighbor-idx (if (< idx (- n 1)) (+ idx 1) (- idx 1)))
             (neighbor (list-ref windows neighbor-idx)))
        (set! (edit-window-size-bias cur) (+ (or (edit-window-size-bias cur) 0) delta))
        (set! (edit-window-size-bias neighbor) (- (or (edit-window-size-bias neighbor) 0) delta))
        (frame-layout! fr)))))

(def (frame-shrink-window! fr (delta 1))
  "Make the current window smaller by delta rows/cols."
  (frame-enlarge-window! fr (- delta)))

(def (frame-enlarge-window-horizontally! fr (delta 1))
  "Enlarge current window horizontally (alias for enlarge when in h-split)."
  (frame-enlarge-window! fr delta))

(def (frame-shrink-window-horizontally! fr (delta 1))
  "Shrink current window horizontally."
  (frame-shrink-window! fr delta))
