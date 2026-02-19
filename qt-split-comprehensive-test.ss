;;; -*- Gerbil -*-
;;; COMPREHENSIVE Window Split Testing
;;; Tests REAL user workflows with navigation and splitting
;;;
;;; Run: QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-split-comprehensive-test

(import :std/sugar
        :std/format
        (only-in :gerbil-qt/qt
                 with-qt-app
                 qt-widget-create
                 qt-widget-destroy!
                 qt-scintilla-create
                 qt-splitter-create
                 QT_VERTICAL
                 QT_HORIZONTAL)
        :gerbil-scintilla/constants
        (only-in :gemacs/qt/sci-shim
                 sci-send
                 sci-send/string
                 qt-plain-text-edit-text
                 qt-plain-text-edit-set-text!
                 qt-scintilla-destroy!)
        (only-in :gemacs/core
                 new-app-state
                 execute-command!
                 find-command
                 make-buffer
                 buffer-name
                 app-state-frame)
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 make-split-leaf
                 make-split-node
                 split-leaf?
                 split-node?
                 split-leaf-edit-window
                 split-node-orientation
                 split-node-children
                 qt-edit-window-buffer
                 qt-edit-window-editor
                 qt-frame-root
                 qt-frame-windows
                 qt-frame-current-idx
                 qt-current-window
                 qt-frame-init!)
        (only-in :gemacs/qt/commands
                 qt-register-all-commands!))

(export main)

;;;============================================================================
;;; Test Infrastructure
;;;============================================================================

(def *passes* 0)
(def *failures* 0)

(def (pass! msg)
  (set! *passes* (+ *passes* 1))
  (displayln "  ✓ " msg))

(def (fail! msg got expected)
  (set! *failures* (+ *failures* 1))
  (displayln "  ✗ FAIL: " msg)
  (displayln "    Got:      " got)
  (displayln "    Expected: " expected))

;;;============================================================================
;;; Helper: Singleton widget (same pattern as qt-functional-test.ss)
;;;============================================================================

(def *test-singleton-ed* #f)
(def *test-singleton-w*  #f)

(def (test-singleton-init!)
  (unless *test-singleton-ed*
    (set! *test-singleton-w*  (qt-widget-create))
    (set! *test-singleton-ed* (qt-scintilla-create parent: *test-singleton-w*))))

(def (make-test-frame-with-buffers!)
  "Create a Qt frame with a single window showing buffer 'BufA'."
  (test-singleton-init!)
  (let* ((ed  *test-singleton-ed*)
         (w   *test-singleton-w*)
         (doc (sci-send ed SCI_GETDOCPOINTER))
         (buf (make-buffer "BufA" #f doc #f #f #f #f))
         (win (make-qt-edit-window ed #f buf #f #f #f))
         (fr  (make-qt-frame #f (make-split-leaf win) [win] 0 #f))
         (app (new-app-state fr)))
    (values fr app)))

(def (assign-buffer-to-current-window! app name)
  "Assign a buffer with the given name to the current window."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (make-buffer name #f #f #f #f #f #f)))
    (set! (qt-edit-window-buffer win) buf)))

(def (get-window-buffer-names fr)
  "Return list of buffer names for all windows, in window-list order."
  (map (lambda (w) (buffer-name (qt-edit-window-buffer w)))
       (qt-frame-windows fr)))

(def (get-current-buffer-name app)
  "Return the buffer name of the current window."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr)))
    (buffer-name (qt-edit-window-buffer win))))

;;;============================================================================
;;; Tree Structure Helpers
;;;============================================================================

(def (describe-tree node)
  "Return a human-readable s-expression describing the split tree."
  (cond
    ((split-leaf? node)
     (let ((buf (qt-edit-window-buffer (split-leaf-edit-window node))))
       (buffer-name buf)))
    ((split-node? node)
     (let ((orient (if (= (split-node-orientation node) QT_HORIZONTAL) 'H 'V))
           (children (map describe-tree (split-node-children node))))
       (cons orient children)))
    (else '???)))

(def (count-windows-in-tree node)
  "Count leaf nodes in tree."
  (cond
    ((split-leaf? node) 1)
    ((split-node? node)
     (apply + (map count-windows-in-tree (split-node-children node))))
    (else 0)))

;;;============================================================================
;;; COMPREHENSIVE TESTS
;;;============================================================================

(def (run-comprehensive-split-tests)
  (displayln "\n═══════════════════════════════════════════════════════")
  (displayln "  COMPREHENSIVE Window Split Tests")
  (displayln "═══════════════════════════════════════════════════════\n")

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 1: Horizontal split → jump to other window → vertical split
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "TEST 1: Horizontal split → other-window → vertical split")
  (displayln "  Initial:  [BufA]")
  (displayln "  Expected: (BufA1 over BufA2) | BufB")
  (displayln "")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    ;; Initial: BufA
    (displayln "  Step 1: split-window-right")
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB")  ; Right window now shows BufB

    (let ((bufs (get-window-buffer-names fr))
          (idx (qt-frame-current-idx fr)))
      (displayln "    Windows: " bufs "  Current: " idx)
      (if (equal? bufs ["BufA" "BufB"])
        (pass! "After split-right: windows are [BufA, BufB]")
        (fail! "After split-right: window order" bufs ["BufA" "BufB"]))
      (if (= idx 1)
        (pass! "After split-right: cursor at idx 1 (BufB)")
        (fail! "After split-right: cursor" idx 1)))

    (displayln "  Step 2: other-window (back to BufA)")
    (execute-command! app 'other-window)

    (let ((idx (qt-frame-current-idx fr))
          (cur-buf (get-current-buffer-name app)))
      (displayln "    Current: idx=" idx " buf=" cur-buf)
      (if (= idx 0)
        (pass! "After other-window: cursor at idx 0")
        (fail! "After other-window: cursor" idx 0))
      (if (equal? cur-buf "BufA")
        (pass! "After other-window: current buffer is BufA")
        (fail! "After other-window: current buffer" cur-buf "BufA")))

    (displayln "  Step 3: split-window-below (split BufA vertically)")
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufA2")  ; Bottom-left window

    (let ((bufs (get-window-buffer-names fr))
          (idx (qt-frame-current-idx fr))
          (tree (describe-tree (qt-frame-root fr))))
      (displayln "    Windows: " bufs "  Current: " idx)
      (displayln "    Tree: " tree)

      ;; Expected: [BufA, BufA2, BufB] or [BufA1, BufA2, BufB]
      ;; Tree: (H (V BufA BufA2) BufB)
      (if (= (length bufs) 3)
        (pass! "After split-below: 3 windows")
        (fail! "After split-below: window count" (length bufs) 3))

      ;; Check tree structure
      (if (and (pair? tree) (eq? (car tree) 'H))
        (pass! "Root is horizontal")
        (fail! "Root orientation" tree "starts with H"))

      ;; Left child should be vertical with 2 windows
      (let ((left-child (and (pair? tree) (pair? (cdr tree)) (cadr tree))))
        (if (and (pair? left-child) (eq? (car left-child) 'V))
          (pass! "Left child is vertical node")
          (fail! "Left child structure" left-child "(V ...)")))

      ;; Right child should be BufB leaf
      (let ((right-child (and (pair? tree) (pair? (cdr tree)) (pair? (cddr tree)) (caddr tree))))
        (if (equal? right-child "BufB")
          (pass! "Right child is BufB (untouched)")
          (fail! "Right child" right-child "BufB")))

      ;; Verify window order: split-below should insert AFTER the split window
      ;; So order should be [BufA, BufA2, BufB] where BufA2 is the new bottom-left window
      (if (and (>= (length bufs) 2)
               (equal? (car bufs) "BufA")
               (equal? (cadr bufs) "BufA2"))
        (pass! "Window order: BufA and BufA2 are first two (left pane)")
        (fail! "Window order: first two windows" (list (car bufs) (cadr bufs)) '("BufA" "BufA2")))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 2: Vertical split → other-window → horizontal split
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 2: Vertical split → other-window → horizontal split")
  (displayln "  Initial:  [BufA]")
  (displayln "  Expected: (BufA1 | BufA2) over BufB")
  (displayln "")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (displayln "  Step 1: split-window-below")
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufB")

    (displayln "  Step 2: other-window (back to top)")
    (execute-command! app 'other-window)

    (let ((idx (qt-frame-current-idx fr))
          (cur-buf (get-current-buffer-name app)))
      (if (and (= idx 0) (equal? cur-buf "BufA"))
        (pass! "After other-window: at idx 0, BufA")
        (fail! "After other-window" (list idx cur-buf) '(0 "BufA"))))

    (displayln "  Step 3: split-window-right (split top pane)")
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufA2")

    (let ((bufs (get-window-buffer-names fr))
          (tree (describe-tree (qt-frame-root fr))))
      (displayln "    Windows: " bufs)
      (displayln "    Tree: " tree)

      (if (= (length bufs) 3)
        (pass! "After split-right: 3 windows")
        (fail! "After split-right: count" (length bufs) 3))

      ;; Expected tree: (V (H BufA BufA2) BufB)
      (if (and (pair? tree) (eq? (car tree) 'V))
        (pass! "Root is vertical")
        (fail! "Root" tree "starts with V"))

      (let ((top-child (and (pair? tree) (pair? (cdr tree)) (cadr tree))))
        (if (and (pair? top-child) (eq? (car top-child) 'H))
          (pass! "Top child is horizontal node")
          (fail! "Top child" top-child "(H ...)")))

      (let ((bottom-child (and (pair? tree) (pair? (cdr tree)) (pair? (cddr tree)) (caddr tree))))
        (if (equal? bottom-child "BufB")
          (pass! "Bottom child is BufB (untouched)")
          (fail! "Bottom child" bottom-child "BufB")))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 3: Horizontal → vertical → horizontal (zigzag pattern)
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 3: Zigzag - H → V → H with navigation")
  (displayln "  Build: A | B, navigate to A, split A vertically, navigate to B, split B horizontally")
  (displayln "")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB")

    (execute-command! app 'other-window)  ; to A
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufA2")

    (execute-command! app 'other-window)  ; Should go to BufB
    (execute-command! app 'other-window)  ; Might need 2 jumps to get to BufB

    (let ((cur-buf (get-current-buffer-name app)))
      (displayln "  Current buffer after navigation: " cur-buf)
      ;; Navigate until we hit BufB
      (let loop ((count 0))
        (cond
          ((equal? (get-current-buffer-name app) "BufB")
           (pass! "Found BufB after navigation"))
          ((> count 5)
           (fail! "Could not navigate to BufB" (get-current-buffer-name app) "BufB"))
          (else
           (execute-command! app 'other-window)
           (loop (+ count 1))))))

    (displayln "  Split BufB horizontally")
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB2")

    (let ((bufs (get-window-buffer-names fr))
          (tree (describe-tree (qt-frame-root fr))))
      (displayln "    Windows: " bufs)
      (displayln "    Tree: " tree)

      (if (= (length bufs) 4)
        (pass! "Zigzag: 4 windows")
        (fail! "Zigzag: count" (length bufs) 4))

      ;; Expected: (H (V BufA BufA2) (H BufB BufB2))
      (if (and (pair? tree) (eq? (car tree) 'H))
        (pass! "Zigzag: root is horizontal")
        (fail! "Zigzag: root" tree "H"))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 4: Delete window after complex split
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 4: Delete window after complex navigation and splits")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB")
    (execute-command! app 'other-window)
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufA2")

    (displayln "  Before delete: " (get-window-buffer-names fr))
    (displayln "  Tree: " (describe-tree (qt-frame-root fr)))

    ;; Current window is BufA2 (bottom-left), delete it
    (execute-command! app 'delete-window)

    (let ((bufs (get-window-buffer-names fr))
          (tree (describe-tree (qt-frame-root fr))))
      (displayln "  After delete: " bufs)
      (displayln "  Tree: " tree)

      (if (= (length bufs) 2)
        (pass! "After delete: 2 windows remain")
        (fail! "After delete: count" (length bufs) 2))

      ;; Should collapse back to flat horizontal: (H BufA BufB)
      (if (and (pair? tree) (eq? (car tree) 'H) (= (length (cdr tree)) 2))
        (pass! "After delete: flat horizontal with 2 children")
        (fail! "After delete: tree" tree "(H ... ...)"))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 5: Stress test - many splits with navigation
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 5: Stress test - 6 windows with mixed navigation")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "Buf1")

    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "Buf2")

    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "Buf3")

    (execute-command! app 'other-window)
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "Buf4")

    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "Buf5")

    (let ((bufs (get-window-buffer-names fr))
          (count (count-windows-in-tree (qt-frame-root fr))))
      (displayln "  Final windows: " bufs)
      (displayln "  Window count: " count)
      (displayln "  Tree: " (describe-tree (qt-frame-root fr)))

      (if (= count (length bufs))
        (pass! "Stress: window count matches tree count")
        (fail! "Stress: count mismatch" (list count (length bufs)) "equal"))

      (if (>= (length bufs) 5)
        (pass! "Stress: at least 5 windows created")
        (fail! "Stress: window count" (length bufs) "≥ 5"))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 6: Cursor position tracking through complex splits
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 6: Verify cursor follows splits correctly")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB")
    (let ((idx1 (qt-frame-current-idx fr)))
      (if (= idx1 1)
        (pass! "After split-right: cursor at new window (idx 1)")
        (fail! "After split-right: cursor" idx1 1)))

    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufB2")
    (let ((idx2 (qt-frame-current-idx fr)))
      (if (= idx2 2)
        (pass! "After split-below: cursor at new window (idx 2)")
        (fail! "After split-below: cursor" idx2 2)))

    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    (let ((idx3 (qt-frame-current-idx fr)))
      (if (= idx3 0)
        (pass! "After 3x other-window: wrapped back to idx 0")
        (fail! "After 3x other-window: cursor" idx3 0))))

  ;; ══════════════════════════════════════════════════════════════════════
  ;; TEST 7: User's exact bug scenario
  ;; ══════════════════════════════════════════════════════════════════════
  (displayln "\nTEST 7: USER'S BUG - horizontal split → jump to other → vertical split")
  (displayln "  THIS IS THE ACTUAL BUG THE USER REPORTED")
  (let-values (((fr app) (make-test-frame-with-buffers!)))
    (displayln "  Initial: single window [BufA]")

    (displayln "  Action: split-window-right → creates A | B")
    (execute-command! app 'split-window-right)
    (assign-buffer-to-current-window! app "BufB")
    (let ((bufs1 (get-window-buffer-names fr)))
      (displayln "    State: " bufs1 " (cursor should be at BufB)")
      (if (equal? bufs1 ["BufA" "BufB"])
        (pass! "User bug: after h-split, have [BufA BufB]")
        (fail! "User bug: after h-split" bufs1 ["BufA" "BufB"])))

    (displayln "  Action: jump to other buffer (BufA)")
    (execute-command! app 'other-window)
    (let ((cur (get-current-buffer-name app)))
      (displayln "    Current: " cur)
      (if (equal? cur "BufA")
        (pass! "User bug: jumped to BufA")
        (fail! "User bug: current after jump" cur "BufA")))

    (displayln "  Action: split-window-below → should split BufA into (A1 over A2)")
    (execute-command! app 'split-window-below)
    (assign-buffer-to-current-window! app "BufA-bottom")

    (let ((bufs2 (get-window-buffer-names fr))
          (tree (describe-tree (qt-frame-root fr))))
      (displayln "    State: " bufs2)
      (displayln "    Tree:  " tree)

      (if (= (length bufs2) 3)
        (pass! "User bug: 3 windows after v-split")
        (fail! "User bug: window count" (length bufs2) 3))

      ;; CRITICAL: Check that BufB is UNTOUCHED
      (if (member "BufB" bufs2)
        (pass! "User bug: BufB still exists (not replaced)")
        (fail! "User bug: BufB missing" bufs2 "contains BufB"))

      ;; CRITICAL: Tree should be (H (V ...) BufB), NOT (V ... BufB)
      (if (and (pair? tree) (eq? (car tree) 'H))
        (pass! "User bug: root is HORIZONTAL (correct)")
        (fail! "User bug: ROOT IS WRONG ORIENTATION" tree "should start with H"))

      ;; CRITICAL: BufB should be in the RIGHT child (untouched)
      (let ((right-child (and (pair? tree) (>= (length tree) 3) (caddr tree))))
        (if (equal? right-child "BufB")
          (pass! "User bug: BufB is RIGHT child (UNTOUCHED - correct)")
          (fail! "User bug: BufB POSITION WRONG" right-child "should be 'BufB'")))

      ;; CRITICAL: Left child should be vertical with BufA and BufA-bottom
      (let ((left-child (and (pair? tree) (>= (length tree) 2) (cadr tree))))
        (if (and (pair? left-child) (eq? (car left-child) 'V))
          (pass! "User bug: LEFT child is vertical (BufA split)")
          (fail! "User bug: LEFT CHILD STRUCTURE WRONG" left-child "should be (V ...)")))))

  (displayln "\n═══════════════════════════════════════════════════════")
  (displayln "  End of Comprehensive Split Tests")
  (displayln "═══════════════════════════════════════════════════════"))

;;;============================================================================
;;; Main
;;;============================================================================

(def (main . args)
  (with-qt-app _app
    (qt-register-all-commands!)
    (run-comprehensive-split-tests)

    (displayln "\n───────────────────────────────────────────────────────")
    (displayln "RESULTS: " *passes* " passed, " *failures* " failed")
    (displayln "───────────────────────────────────────────────────────\n")

    (if (= *failures* 0)
      (begin
        (displayln "✓ All comprehensive split tests PASSED!")
        (exit 0))
      (begin
        (displayln "✗ " *failures* " test(s) FAILED - IMPLEMENTATION HAS BUGS")
        (exit *failures*)))))
