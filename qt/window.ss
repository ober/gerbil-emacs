;;; -*- Gerbil -*-
;;; Qt frame/window management for gemacs
;;;
;;; Uses nested QSplitters to hold multiple QPlainTextEdit panes in a
;;; recursive binary tree. Each split can have a different orientation,
;;; so horizontal-then-vertical nesting works correctly.
;;;
;;; Architecture:
;;;   qt-frame holds:
;;;     - splitter   : the permanent root QSplitter (QMainWindow's central widget)
;;;     - root       : the logical split tree (split-leaf | split-node)
;;;     - windows    : flat ordered list of qt-edit-window (derived, kept in sync)
;;;     - current-idx: index of active window in windows list
;;;     - main-win   : QMainWindow pointer
;;;
;;;   split-leaf(win)  : a single editor pane
;;;   split-node(orientation, splitter, children) : a QSplitter with children
;;;
;;; Splits always append the new window AFTER the current one in the flat
;;; list. This invariant lets winner-undo simply delete the last window.

(export (struct-out qt-edit-window)
        (struct-out qt-frame)
        (struct-out split-leaf)
        (struct-out split-node)
        qt-current-window
        qt-current-editor
        qt-current-buffer
        qt-frame-init!
        qt-frame-split!
        qt-frame-split-right!
        qt-frame-delete-window!
        qt-frame-delete-other-windows!
        qt-frame-other-window!
        ;; Tree helpers (used by commands-sexp.ss, tests)
        split-tree-flatten
        split-tree-find-parent
        split-tree-find-leaf
        split-tree-collect-sub-splitters)

(import :std/sugar
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/qt/buffer)

;;;============================================================================
;;; Structures
;;;============================================================================

(defstruct qt-edit-window
  (editor           ; QScintilla editor pointer
   container        ; QStackedWidget wrapping editor + image widget
   buffer           ; buffer struct
   line-number-area ; line-number-area pointer or #f
   image-scroll     ; QScrollArea for image display, or #f (lazy)
   image-label)     ; QLabel inside scroll area, or #f (lazy)
  transparent: #t)

;;; A split-leaf wraps a single edit window.
(defstruct split-leaf (edit-window) transparent: #t)

;;; A split-node is a QSplitter with 2+ children (each a split-leaf or split-node).
;;; orientation is QT_HORIZONTAL or QT_VERTICAL.
(defstruct split-node
  (orientation   ; QT_HORIZONTAL | QT_VERTICAL
   splitter      ; QSplitter widget pointer
   children)     ; list of split-leaf | split-node
  transparent: #t)

;;; The main frame struct.
;;; splitter = permanent root QSplitter (QMainWindow central widget).
;;; root     = logical split tree (split-leaf when single pane).
;;; windows  = flat ordered list derived from root, kept in sync.
;;; current-idx = index of active window in windows list.
(defstruct qt-frame
  (splitter     ; QSplitter pointer (permanent root splitter)
   root         ; split-leaf | split-node
   windows      ; list of qt-edit-window
   current-idx  ; index of active window
   main-win)    ; QMainWindow pointer
  transparent: #t)

;;;============================================================================
;;; Accessors
;;;============================================================================

(def (qt-current-window fr)
  (list-ref (qt-frame-windows fr) (qt-frame-current-idx fr)))

(def (qt-current-editor fr)
  (qt-edit-window-editor (qt-current-window fr)))

(def (qt-current-buffer fr)
  (qt-edit-window-buffer (qt-current-window fr)))

;;;============================================================================
;;; Split tree helpers
;;;============================================================================

(def (split-tree-flatten node)
  "Return ordered flat list of qt-edit-window from tree, left-to-right."
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

(def (split-tree-collect-sub-splitters node root-spl)
  "Return list of all QSplitter widgets in tree, excluding ROOT-SPL.
   Returned in depth-first order (innermost first)."
  (cond
    ((split-leaf? node) [])
    ((split-node? node)
     (let ((spl (split-node-splitter node))
           (children-spls
            (apply append
                   (map (lambda (c) (split-tree-collect-sub-splitters c root-spl))
                        (split-node-children node)))))
       (if (eq? spl root-spl)
         children-spls
         (append children-spls (list spl)))))))

;;;============================================================================
;;; QScintilla editor setup
;;;============================================================================

(def (qt-scintilla-setup-editor! ed)
  "Configure QScintilla editor: dark theme, margins, caret, save-point signals."
  ;; Base dark theme
  (sci-send ed SCI_STYLESETBACK STYLE_DEFAULT (rgb->sci #x1e #x1e #x2e))
  (sci-send ed SCI_STYLESETFORE STYLE_DEFAULT (rgb->sci #xd4 #xd4 #xd4))
  ;; Set monospace font on STYLE_DEFAULT before STYLECLEARALL so all styles inherit it.
  ;; This is critical for org-table column alignment — CSS font-family alone doesn't
  ;; affect Scintilla's internal text renderer.
  (sci-send/string ed SCI_STYLESETFONT "Monospace" STYLE_DEFAULT)
  (sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT 11)
  (sci-send ed SCI_STYLECLEARALL)
  ;; Line number margin
  (sci-send ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
  (sci-send ed SCI_SETMARGINWIDTHN 0 50)
  (sci-send ed SCI_STYLESETBACK STYLE_LINENUMBER (rgb->sci #x20 #x20 #x20))
  (sci-send ed SCI_STYLESETFORE STYLE_LINENUMBER (rgb->sci #x8c #x8c #x8c))
  ;; Caret line highlight
  (sci-send ed SCI_SETCARETLINEVISIBLE 1)
  (sci-send ed SCI_SETCARETLINEBACK (rgb->sci #x22 #x22 #x28))
  (sci-send ed SCI_SETCARETFORE (rgb->sci #xd4 #xd4 #xd4))
  ;; Tab settings
  (sci-send ed SCI_SETTABWIDTH 4)
  (sci-send ed SCI_SETINDENT 4)
  ;; Save-point signals for modified state tracking
  (qt-on-scintilla-save-point-reached! ed
    (lambda ()
      (let* ((doc (sci-send ed SCI_GETDOCPOINTER))
             (buf (hash-get *doc-buffer-map* doc)))
        (when buf (set! (buffer-modified buf) #f)))))
  (qt-on-scintilla-save-point-left! ed
    (lambda ()
      (let* ((doc (sci-send ed SCI_GETDOCPOINTER))
             (buf (hash-get *doc-buffer-map* doc)))
        (when buf (set! (buffer-modified buf) #t))))))

(def (qt-make-new-window! container-parent buf)
  "Create a new qt-edit-window with a fresh editor in a new container.
   CONTAINER-PARENT is the QSplitter that will own the container."
  (let* ((container (qt-stacked-widget-create parent: container-parent))
         (new-ed (qt-plain-text-edit-create parent: container))
         (lna (qt-line-number-area-create new-ed))
         (new-win (make-qt-edit-window new-ed container buf lna #f #f)))
    (qt-scintilla-setup-editor! new-ed)
    (qt-buffer-attach! new-ed buf)
    (qt-stacked-widget-add-widget! container new-ed)
    (qt-splitter-add-widget! container-parent container)
    (hash-put! *editor-window-map* new-ed new-win)
    new-win))

;;;============================================================================
;;; Frame initialization
;;;============================================================================

(def (qt-frame-init! main-win splitter)
  "Create frame with one QScintilla editor in a QStackedWidget in a QSplitter.
   Returns the frame struct."
  (let* ((container (qt-stacked-widget-create parent: splitter))
         (editor (qt-plain-text-edit-create parent: container))
         (buf (qt-buffer-create! buffer-scratch-name editor))
         (lna (qt-line-number-area-create editor))
         (win (make-qt-edit-window editor container buf lna #f #f)))
    (qt-scintilla-setup-editor! editor)
    (qt-buffer-attach! editor buf)
    (qt-stacked-widget-add-widget! container editor)
    (qt-splitter-add-widget! splitter container)
    (hash-put! *editor-window-map* editor win)
    (let ((root (make-split-leaf win)))
      (make-qt-frame splitter root (list win) 0 main-win))))

;;;============================================================================
;;; Window splitting — the core new architecture
;;;============================================================================

(def (qt-frame-do-split! fr orientation)
  "Split the currently focused window with ORIENTATION (QT_HORIZONTAL|QT_VERTICAL).
   Returns the new editor widget.

   Cases:
   A. Parent has SAME orientation → append new sibling in same splitter.
   B. Root is a leaf (first split) → use root splitter, set its orientation.
   C. No parent or parent has DIFFERENT orientation → create nested QSplitter."
  (let* ((cur-win   (qt-current-window fr))
         (root-spl  (qt-frame-splitter fr))
         (cur-leaf  (split-tree-find-leaf (qt-frame-root fr) cur-win))
         (parent    (split-tree-find-parent (qt-frame-root fr) cur-win))
         (cur-buf   (qt-edit-window-buffer cur-win)))

    (cond
      ;; ── Case A: parent has same orientation — add sibling ─────────────────
      ((and parent (= (split-node-orientation parent) orientation))
       (let* ((parent-spl (split-node-splitter parent))
              (new-win    (qt-make-new-window! parent-spl cur-buf))
              (new-leaf   (make-split-leaf new-win)))
         ;; Insert new-leaf after cur-leaf in parent's children
         (set! (split-node-children parent)
               (let loop ((cs (split-node-children parent)) (acc []))
                 (cond
                   ((null? cs) (reverse (cons new-leaf acc)))
                   ((eq? (car cs) cur-leaf)
                    (append (reverse (cons new-leaf (cons cur-leaf acc))) (cdr cs)))
                   (else (loop (cdr cs) (cons (car cs) acc))))))
         ;; Rebuild flat list from tree to maintain depth-first order
         (set! (qt-frame-windows fr) (split-tree-flatten (qt-frame-root fr)))
         ;; Find new window's index in the rebuilt list
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (qt-frame-windows fr))))
           (set! (qt-frame-current-idx fr) (or new-idx 0)))
         (qt-edit-window-editor new-win)))

      ;; ── Case B: root is a leaf (very first split) ─────────────────────────
      ((split-leaf? (qt-frame-root fr))
       (qt-splitter-set-orientation! root-spl orientation)
       (let* ((new-win  (qt-make-new-window! root-spl cur-buf))
              (new-leaf (make-split-leaf new-win))
              (new-node (make-split-node orientation root-spl (list cur-leaf new-leaf))))
         (set! (qt-frame-root fr) new-node)
         ;; Rebuild flat list from tree to maintain depth-first order
         (set! (qt-frame-windows fr) (split-tree-flatten (qt-frame-root fr)))
         ;; Find new window's index in the rebuilt list
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (qt-frame-windows fr))))
           (set! (qt-frame-current-idx fr) (or new-idx 0)))
         (qt-edit-window-editor new-win)))

      ;; ── Case C: no parent or different orientation — nest with new splitter ─
      (else
       (let* ((parent-spl   (if parent
                              (split-node-splitter parent)
                              root-spl))
              ;; Create sub-splitter with the desired orientation
              (new-spl      (qt-splitter-create orientation parent: parent-spl))
              (_ (qt-splitter-set-handle-width! new-spl 3))
              (_ (qt-widget-set-style-sheet! new-spl
                   "QSplitter::handle { background: #51afef; }"))
              ;; Reparent cur-win's container into the new splitter
              (cur-container (qt-edit-window-container cur-win))
              (_ (qt-splitter-add-widget! new-spl cur-container))
              ;; Create new window in the new splitter
              (new-win      (qt-make-new-window! new-spl cur-buf))
              (new-leaf     (make-split-leaf new-win))
              (new-node     (make-split-node orientation new-spl
                                             (list cur-leaf new-leaf))))
         ;; Replace cur-leaf with new-node in the parent (or set as root)
         (cond
           (parent (split-tree-replace-child! parent cur-leaf new-node))
           (else   (set! (qt-frame-root fr) new-node)))
         ;; Rebuild flat list from tree to maintain depth-first order
         (set! (qt-frame-windows fr) (split-tree-flatten (qt-frame-root fr)))
         ;; Find new window's index in the rebuilt list
         (let ((new-idx (list-index (lambda (w) (eq? w new-win)) (qt-frame-windows fr))))
           (set! (qt-frame-current-idx fr) (or new-idx 0)))
         (qt-edit-window-editor new-win))))))

(def (qt-frame-split! fr)
  "Split vertically: add a new window below. Returns the new editor."
  (qt-frame-do-split! fr QT_VERTICAL))

(def (qt-frame-split-right! fr)
  "Split horizontally: add a new window to the right. Returns the new editor."
  (qt-frame-do-split! fr QT_HORIZONTAL))

;;;============================================================================
;;; Window deletion
;;;============================================================================

(def (qt-frame-delete-window! fr)
  "Delete the current window (if more than one).
   Automatically unwraps single-child split nodes when they arise."
  (when (> (length (qt-frame-windows fr)) 1)
    (let* ((idx       (qt-frame-current-idx fr))
           (win       (list-ref (qt-frame-windows fr) idx))
           (ed        (qt-edit-window-editor win))
           (container (qt-edit-window-container win))
           (root      (qt-frame-root fr))
           (cur-leaf  (split-tree-find-leaf root win))
           (parent    (split-tree-find-parent root win)))

      ;; Safety check
      (unless cur-leaf
        (error "qt-frame-delete-window!: could not find leaf for window"))

      ;; Remove the window from tracking before Qt widget destruction
      (hash-remove! *editor-window-map* ed)

      ;; Update tree: remove cur-leaf from parent
      (when parent
        (split-tree-remove-child! parent cur-leaf)
        (let ((remaining (split-node-children parent)))
          ;; If parent now has exactly 1 child, unwrap it
          (when (and remaining (= 1 (length remaining)))
            (let* ((only-child  (car remaining))
                   (parent-spl  (split-node-splitter parent))
                   (grandparent (split-tree-find-parent-of-node root parent))
                   ;; The Qt widget to add to the grandparent's splitter
                   (only-qt-w   (if (split-leaf? only-child)
                                  (qt-edit-window-container
                                   (split-leaf-edit-window only-child))
                                  (split-node-splitter only-child)))
                   ;; Destination splitter
                   (dest-spl    (if grandparent
                                  (split-node-splitter grandparent)
                                  (qt-frame-splitter fr))))
              ;; Safety checks
              (unless only-qt-w
                (error "qt-frame-delete-window!: only-qt-w is null"))
              (unless dest-spl
                (error "qt-frame-delete-window!: dest-spl is null"))
              ;; Move only-child's Qt widget to dest-spl (reparents it)
              (qt-splitter-add-widget! dest-spl only-qt-w)
              ;; Update tree: replace parent with only-child
              (if grandparent
                (split-tree-replace-child! grandparent parent only-child)
                (set! (qt-frame-root fr) only-child))
              ;; Destroy the now-redundant sub-splitter (not root-spl)
              (when (and parent-spl (not (eq? parent-spl (qt-frame-splitter fr))))
                (qt-widget-destroy! parent-spl))))))

      ;; Destroy the deleted window's Qt container
      (when container
        (qt-widget-hide! container)
        (qt-widget-destroy! container))

      ;; Update flat windows list and current-idx
      (set! (qt-frame-windows fr) (list-remove-idx (qt-frame-windows fr) idx))
      (when (>= (qt-frame-current-idx fr) (length (qt-frame-windows fr)))
        (set! (qt-frame-current-idx fr) (- (length (qt-frame-windows fr)) 1))))))

(def (qt-frame-delete-other-windows! fr)
  "Keep only the current window, destroy all others and all sub-splitters."
  (let* ((cur     (qt-current-window fr))
         (root-spl (qt-frame-splitter fr))
         (all-wins (qt-frame-windows fr))
         (sub-spls (split-tree-collect-sub-splitters (qt-frame-root fr) root-spl)))
    ;; 1. Move current window's container to root-spl (reparents it)
    (qt-splitter-add-widget! root-spl (qt-edit-window-container cur))
    ;; 2. Destroy all other windows' containers
    (for-each (lambda (win)
                (unless (eq? win cur)
                  (let ((ed        (qt-edit-window-editor win))
                        (container (qt-edit-window-container win)))
                    (hash-remove! *editor-window-map* ed)
                    (qt-widget-hide! container)
                    (qt-widget-destroy! container))))
              all-wins)
    ;; 3. Destroy sub-splitters (innermost first, avoiding double-destroy)
    (for-each qt-widget-destroy! sub-spls)
    ;; 4. Update logical tree and flat list
    (set! (qt-frame-root fr) (make-split-leaf cur))
    (set! (qt-frame-windows fr) (list cur))
    (set! (qt-frame-current-idx fr) 0)))

;;;============================================================================
;;; Window navigation
;;;============================================================================

(def (qt-frame-other-window! fr)
  "Switch to the next window (wraps around)."
  (let ((n (length (qt-frame-windows fr))))
    (set! (qt-frame-current-idx fr)
          (modulo (+ (qt-frame-current-idx fr) 1) n))))

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
