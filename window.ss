;;; -*- Gerbil -*-
;;; Frame and window layout for gemacs
;;;
;;; Architecture (now matches Qt's tree model):
;;;   frame holds:
;;;     - root       : the logical split tree (split-leaf | split-node)
;;;     - windows    : flat ordered list of edit-window (derived, kept in sync)
;;;     - current-idx: index of active window in windows list
;;;     - width/height: terminal dimensions
;;;
;;;   split-leaf(win)  : a single editor pane
;;;   split-node(orientation, children) : a logical split with 2+ children
;;;
;;; Splits always insert the new window AFTER the current one in the flat
;;; list. Layout is computed recursively from the tree.

(export
  (struct-out edit-window)
  (struct-out frame)
  (struct-out split-leaf)
  (struct-out split-node)
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
  frame-shrink-window-horizontally!
  ;; Tree helpers (for tests)
  split-tree-flatten
  split-tree-find-parent
  split-tree-find-leaf)

(import :std/sugar
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/buffer)

;;;============================================================================
;;; Structures
;;;============================================================================

(defstruct edit-window
  (editor   ; scintilla-editor instance
   buffer   ; buffer struct
   x y w h  ; position and size in terminal cells
   size-bias) ; integer offset for window resize (default 0)
  transparent: #t)

;;; A split-leaf wraps a single edit window.
(defstruct split-leaf (edit-window) transparent: #t)

;;; A split-node is a logical split with 2+ children (each a split-leaf or split-node).
;;; orientation is 'horizontal or 'vertical.
(defstruct split-node
  (orientation   ; 'horizontal | 'vertical
   children)     ; list of split-leaf | split-node
  transparent: #t)

;;; The main frame struct.
;;; root     = logical split tree (split-leaf when single pane).
;;; windows  = flat ordered list derived from root, kept in sync.
;;; current-idx = index of active window in windows list.
(defstruct frame
  (root           ; split-leaf | split-node
   windows        ; list of edit-window
   current-idx    ; index of active window
   width          ; terminal width
   height)        ; terminal height
  transparent: #t)

;;;============================================================================
;;; Accessors
;;;============================================================================

(def (current-window fr)
  (list-ref (frame-windows fr) (frame-current-idx fr)))

;;;============================================================================
;;; Split tree helpers
;;;============================================================================

(def (split-tree-flatten node)
  "Return ordered flat list of edit-window from tree, left-to-right depth-first."
  (cond
    ((split-leaf? node) [(split-leaf-edit-window node)])
    ((split-node? node)
     (apply append (map split-tree-flatten (split-node-children node))))))

(def (split-tree-find-parent root win)
  "Return the split-node whose direct children include the leaf for WIN, or #f."
  (cond
    ((split-leaf? root) #f)
    ((split-node? root)
     (let loop ((children (split-node-children root)))
       (cond
         ((null? children) #f)
         ((and (split-leaf? (car children))
               (eq? (split-leaf-edit-window (car children)) win))
          root)
         (else
          (let ((found (split-tree-find-parent (car children) win)))
            (or found (loop (cdr children))))))))))

(def (split-tree-find-leaf root win)
  "Return the split-leaf node for WIN, or #f."
  (cond
    ((split-leaf? root)
     (if (eq? (split-leaf-edit-window root) win) root #f))
    ((split-node? root)
     (let loop ((children (split-node-children root)))
       (if (null? children) #f
         (or (split-tree-find-leaf (car children) win)
             (loop (cdr children))))))))

(def (split-tree-find-parent-of-node root child-node)
  "Return the split-node that contains CHILD-NODE as a direct child, or #f."
  (cond
    ((split-leaf? root) #f)
    ((split-node? root)
     (if (any (lambda (c) (eq? c child-node)) (split-node-children root))
       root
       (let loop ((children (split-node-children root)))
         (if (null? children) #f
           (or (split-tree-find-parent-of-node (car children) child-node)
               (loop (cdr children)))))))))

(def (split-tree-replace-child! node old-child new-child)
  "Replace OLD-CHILD with NEW-CHILD in NODE's children list."
  (set! (split-node-children node)
        (map (lambda (c) (if (eq? c old-child) new-child c))
             (split-node-children node))))

(def (split-tree-remove-child! node child)
  "Remove CHILD from NODE's children list."
  (set! (split-node-children node)
        (filter (lambda (c) (not (eq? c child)))
                (split-node-children node))))

;;;============================================================================
;;; Frame initialization and shutdown
;;;============================================================================

(def (frame-init! width height)
  "Create initial frame with one window and a scratch buffer."
  (let* ((edit-h (max 1 (- height 2)))  ; 1 row modeline, 1 row echo
         (ed (create-scintilla-editor width: width height: edit-h))
         (buf (buffer-create-from-editor! buffer-scratch-name ed))
         (win (make-edit-window ed buf 0 0 width (- height 1) 0))
         (root (make-split-leaf win)))
    (make-frame root (list win) 0 width height)))

(def (frame-shutdown! fr)
  "Destroy all editors in the frame."
  (for-each (lambda (win) (editor-destroy (edit-window-editor win)))
            (frame-windows fr)))

;;;============================================================================
;;; Layout: recursively compute positions and sizes from tree
;;;============================================================================

(def (frame-layout! fr)
  "Recursively layout the split tree within the frame bounds."
  (let* ((width (frame-width fr))
         (height (frame-height fr))
         (avail-h (- height 1)))  ; 1 row for echo area
    (split-tree-layout! (frame-root fr) 0 0 width avail-h)))

(def (split-tree-layout! node x y w h)
  "Recursively layout NODE within bounding box (x, y, w, h)."
  (cond
    ((split-leaf? node)
     ;; Leaf: set window position directly
     (let* ((win (split-leaf-edit-window node))
            (edit-h (max 1 (- h 1))))  ; 1 row for modeline
       (set! (edit-window-x win) x)
       (set! (edit-window-y win) y)
       (set! (edit-window-w win) w)
       (set! (edit-window-h win) h)
       (editor-resize (edit-window-editor win) w edit-h)
       (editor-move (edit-window-editor win) x y)))

    ((split-node? node)
     ;; Node: divide space among children based on orientation
     (let* ((orientation (split-node-orientation node))
            (children (split-node-children node))
            (n (length children)))
       (if (eq? orientation 'vertical)
         ;; Vertical: stack children top to bottom
         (let* ((avail h)
                (per-child (quotient avail n))
                (extra (remainder avail n))
                ;; Compute sizes (even distribution + bias)
                (sizes (let loop ((cs children) (i 0) (acc []))
                         (if (null? cs) (reverse acc)
                           (let* ((child (car cs))
                                  (bias (if (split-leaf? child)
                                          (or (edit-window-size-bias
                                                (split-leaf-edit-window child)) 0)
                                          0))
                                  (size (+ per-child
                                           (if (< i extra) 1 0)
                                           bias)))
                             (loop (cdr cs) (+ i 1) (cons (max 2 size) acc))))))
                ;; Normalize to ensure total = avail
                (total (apply + sizes))
                (final-sizes (if (= total avail)
                               sizes
                               (let ((delta (- avail total)))
                                 (cons (+ (car sizes) delta) (cdr sizes))))))
           ;; Layout children recursively
           (let loop ((cs children) (szs final-sizes) (cy y))
             (when (and (pair? cs) (pair? szs))
               (let ((ch (car szs)))
                 (split-tree-layout! (car cs) x cy w ch)
                 (loop (cdr cs) (cdr szs) (+ cy ch)))))))

         ;; Horizontal: place children left to right
         (let* ((dividers (max 0 (- n 1)))  ; vertical dividers between children
                (avail (- w dividers))
                (per-child (quotient avail n))
                (extra (remainder avail n))
                (sizes (let loop ((cs children) (i 0) (acc []))
                         (if (null? cs) (reverse acc)
                           (let* ((child (car cs))
                                  (bias (if (split-leaf? child)
                                          (or (edit-window-size-bias
                                                (split-leaf-edit-window child)) 0)
                                          0))
                                  (size (+ per-child
                                           (if (< i extra) 1 0)
                                           bias)))
                             (loop (cdr cs) (+ i 1) (cons (max 4 size) acc))))))
                (total (apply + sizes))
                (final-sizes (if (= total avail)
                               sizes
                               (let ((delta (- avail total)))
                                 (cons (+ (car sizes) delta) (cdr sizes))))))
           ;; Layout children recursively
           (let loop ((cs children) (szs final-sizes) (cx x))
             (when (and (pair? cs) (pair? szs))
               (let ((cw (car szs)))
                 (split-tree-layout! (car cs) cx y cw h)
                 (loop (cdr cs) (cdr szs) (+ cx cw 1))))))))))  ; +1 for divider

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
;;; Window splitting — tree-based architecture (matches Qt logic)
;;;============================================================================

(def (frame-do-split! fr orientation)
  "Split the currently focused window with ORIENTATION ('horizontal | 'vertical).
   Returns the new editor widget.

   Cases:
   A. Parent has SAME orientation → append new sibling.
   B. Root is a leaf (first split) → create root node.
   C. No parent or parent has DIFFERENT orientation → create nested node."
  (let* ((cur-win   (current-window fr))
         (cur-leaf  (split-tree-find-leaf (frame-root fr) cur-win))
         (parent    (split-tree-find-parent (frame-root fr) cur-win))
         (cur-buf   (edit-window-buffer cur-win))
         (new-ed    (create-scintilla-editor))
         (new-win   (make-edit-window new-ed cur-buf 0 0 0 0 0)))

    (buffer-attach! new-ed cur-buf)

    (cond
      ;; ── Case A: parent has same orientation — add sibling ─────────────────
      ((and parent (eq? (split-node-orientation parent) orientation))
       (let ((new-leaf (make-split-leaf new-win)))
         ;; Insert new-leaf after cur-leaf in parent's children
         (set! (split-node-children parent)
               (let loop ((cs (split-node-children parent)) (acc []))
                 (cond
                   ((null? cs) (reverse (cons new-leaf acc)))
                   ((eq? (car cs) cur-leaf)
                    (append (reverse (cons new-leaf (cons cur-leaf acc))) (cdr cs)))
                   (else (loop (cdr cs) (cons (car cs) acc))))))
         ;; Rebuild flat list from tree
         (set! (frame-windows fr) (split-tree-flatten (frame-root fr)))
         ;; Find new window's index
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (frame-windows fr))))
           (set! (frame-current-idx fr) (or new-idx 0)))
         (frame-layout! fr)
         new-ed))

      ;; ── Case B: root is a leaf (very first split) ─────────────────────────
      ((split-leaf? (frame-root fr))
       (let* ((new-leaf (make-split-leaf new-win))
              (new-node (make-split-node orientation (list cur-leaf new-leaf))))
         (set! (frame-root fr) new-node)
         ;; Rebuild flat list from tree
         (set! (frame-windows fr) (split-tree-flatten (frame-root fr)))
         ;; Find new window's index
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (frame-windows fr))))
           (set! (frame-current-idx fr) (or new-idx 0)))
         (frame-layout! fr)
         new-ed))

      ;; ── Case C: no parent or different orientation — nest ─────────────────
      (else
       (let* ((new-leaf (make-split-leaf new-win))
              (new-node (make-split-node orientation (list cur-leaf new-leaf))))
         ;; Replace cur-leaf with new-node in parent (or set as root)
         (cond
           (parent (split-tree-replace-child! parent cur-leaf new-node))
           (else   (set! (frame-root fr) new-node)))
         ;; Rebuild flat list from tree
         (set! (frame-windows fr) (split-tree-flatten (frame-root fr)))
         ;; Find new window's index
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (frame-windows fr))))
           (set! (frame-current-idx fr) (or new-idx 0)))
         (frame-layout! fr)
         new-ed)))))

(def (frame-split! fr)
  "Split vertically: add a new window below. Returns the new editor."
  (frame-do-split! fr 'vertical))

(def (frame-split-right! fr)
  "Split horizontally: add a new window to the right. Returns the new editor."
  (frame-do-split! fr 'horizontal))

(def (frame-delete-window! fr)
  "Delete the current window (if more than one).
   Automatically unwraps single-child split nodes when they arise."
  (when (> (length (frame-windows fr)) 1)
    (let* ((idx       (frame-current-idx fr))
           (win       (list-ref (frame-windows fr) idx))
           (root      (frame-root fr))
           (cur-leaf  (split-tree-find-leaf root win))
           (parent    (split-tree-find-parent root win)))

      ;; Destroy the editor
      (editor-destroy (edit-window-editor win))

      ;; Update tree: remove cur-leaf from parent
      (when parent
        (split-tree-remove-child! parent cur-leaf)
        (let ((remaining (split-node-children parent)))
          ;; If parent now has exactly 1 child, unwrap it
          (when (and remaining (= 1 (length remaining)))
            (let* ((only-child  (car remaining))
                   (grandparent (split-tree-find-parent-of-node root parent)))
              ;; Replace parent with only-child
              (if grandparent
                (split-tree-replace-child! grandparent parent only-child)
                (set! (frame-root fr) only-child))))))

      ;; Rebuild flat windows list and current-idx
      (set! (frame-windows fr) (split-tree-flatten (frame-root fr)))
      (when (>= idx (length (frame-windows fr)))
        (set! (frame-current-idx fr) (- (length (frame-windows fr)) 1)))
      (frame-layout! fr))))

(def (frame-delete-other-windows! fr)
  "Keep only the current window, destroy all others."
  (let ((cur (current-window fr)))
    ;; Destroy all other editors
    (for-each (lambda (win)
                (unless (eq? win cur)
                  (editor-destroy (edit-window-editor win))))
              (frame-windows fr))
    ;; Reset tree to single leaf
    (set! (frame-root fr) (make-split-leaf cur))
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

(def (list-index pred lst)
  "Find the index of the first element matching pred, or #f."
  (let loop ((l lst) (i 0))
    (cond
      ((null? l) #f)
      ((pred (car l)) i)
      (else (loop (cdr l) (+ i 1))))))

(def (list-remove-idx lst idx)
  (let loop ((l lst) (i 0) (acc []))
    (cond
      ((null? l) (reverse acc))
      ((= i idx) (append (reverse acc) (cdr l)))
      (else (loop (cdr l) (+ i 1) (cons (car l) acc))))))

(def (last lst)
  "Return the last element of a non-empty list."
  (if (null? (cdr lst))
    (car lst)
    (last (cdr lst))))

;;;============================================================================
;;; Divider drawing (for horizontal splits)
;;;============================================================================

(def (frame-draw-dividers! fr)
  "Recursively draw vertical divider lines between horizontally split windows."
  (let ((fg #x808080)
        (bg #x181818)
        (height (- (frame-height fr) 1)))
    (split-tree-draw-dividers! (frame-root fr) height fg bg)))

(def (split-tree-draw-dividers! node height fg bg)
  "Recursively draw dividers for horizontal splits in the tree."
  (when (split-node? node)
    (when (eq? (split-node-orientation node) 'horizontal)
      ;; For each child except the last, draw a divider after it
      (let loop ((children (split-node-children node)))
        (when (and (pair? children) (pair? (cdr children)))
          (let* ((child (car children))
                 ;; Get rightmost window in this child subtree
                 (wins (split-tree-flatten child))
                 (rightmost (if (null? wins) #f (last wins))))
            (when rightmost
              (let ((x (+ (edit-window-x rightmost)
                          (edit-window-w rightmost))))
                (let yloop ((y 0))
                  (when (< y height)
                    (tui-change-cell! x y (char->integer #\│) fg bg)
                    (yloop (+ y 1))))))
            (loop (cdr children))))))
    ;; Recurse into children
    (for-each (lambda (child) (split-tree-draw-dividers! child height fg bg))
              (split-node-children node))))

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
