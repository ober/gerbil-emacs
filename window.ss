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
  frame-delete-window!
  frame-delete-other-windows!
  frame-other-window!)

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
  (windows      ; list of edit-window
   current-idx  ; index of active window
   width        ; terminal width
   height)      ; terminal height
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
    (make-frame (list win) 0 width height)))

(def (frame-shutdown! fr)
  "Destroy all editors in the frame."
  (for-each (lambda (win) (editor-destroy (edit-window-editor win)))
            (frame-windows fr)))

;;;============================================================================
;;; Layout: distribute available rows among windows
;;;============================================================================

(def (frame-layout! fr)
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
  "Split: add a new window showing the same buffer. Returns the new editor."
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
