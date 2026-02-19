;;; -*- Gerbil -*-
;;; SIMPLE split test - just test the user's exact bug
;;; Pattern copied exactly from qt-functional-test.ss

(import :std/sugar
        (only-in :gerbil-qt/qt
                 with-qt-app
                 qt-widget-create
                 qt-scintilla-create
                 QT_VERTICAL
                 QT_HORIZONTAL)
        :gerbil-scintilla/constants
        (only-in :gemacs/qt/sci-shim
                 sci-send
                 qt-plain-text-edit-text)
        (only-in :gemacs/core
                 new-app-state
                 execute-command!
                 make-buffer)
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 make-split-leaf
                 split-node?
                 split-node-orientation
                 split-node-children
                 qt-frame-root
                 qt-frame-windows
                 qt-frame-current-idx)
        (only-in :gemacs/qt/commands
                 qt-register-all-commands!))

(export main)

(def *passes* 0)
(def *failures* 0)

(def (pass! label)
  (set! *passes* (+ *passes* 1))
  (displayln "  ✓ " label))

(def (fail! label actual expected)
  (set! *failures* (+ *failures* 1))
  (displayln "  ✗ FAIL: " label)
  (displayln "    Got:      " actual)
  (displayln "    Expected: " expected))

;; Singleton pattern from qt-functional-test.ss
(def *singleton-ed* #f)
(def *singleton-w*  #f)

(def (singleton-init!)
  (unless *singleton-ed*
    (set! *singleton-w*  (qt-widget-create))
    (set! *singleton-ed* (qt-scintilla-create parent: *singleton-w*))))

(def (make-test-app name)
  (singleton-init!)
  (let* ((ed  *singleton-ed*)
         (w   *singleton-w*)
         (doc (sci-send ed SCI_GETDOCPOINTER))
         (buf (make-buffer name #f doc #f #f #f #f))
         (win (make-qt-edit-window ed #f buf #f #f #f))
         (fr  (make-qt-frame #f (make-split-leaf win) [win] 0 #f))
         (app (new-app-state fr)))
    (values fr app)))

(def (run-tests)
  (displayln "\n═══ Simple Split Tests ═══\n")

  ;; TEST 1: Basic horizontal split
  (displayln "TEST 1: split-window-right creates 2 windows")
  (let-values (((fr app) (make-test-app "test.txt")))
    (execute-command! app 'split-window-right)
    (let ((count (length (qt-frame-windows fr))))
      (if (= count 2)
        (pass! "2 windows after split-right")
        (fail! "window count" count 2))))

  ;; TEST 2: Horizontal → navigate → vertical (THE USER'S BUG)
  (displayln "\nTEST 2: USER'S BUG - h-split → other-window → v-split")
  (let-values (((fr app) (make-test-app "test.txt")))
    (displayln "  Step 1: split-window-right")
    (execute-command! app 'split-window-right)
    (let ((idx1 (qt-frame-current-idx fr)))
      (displayln "    Current window index: " idx1)
      (if (= idx1 1)
        (pass! "Cursor at idx 1 after split-right")
        (fail! "Cursor after split-right" idx1 1)))

    (displayln "  Step 2: other-window (jump back to left)")
    (execute-command! app 'other-window)
    (let ((idx2 (qt-frame-current-idx fr)))
      (displayln "    Current window index: " idx2)
      (if (= idx2 0)
        (pass! "Cursor at idx 0 after other-window")
        (fail! "Cursor after other-window" idx2 0)))

    (displayln "  Step 3: split-window-below (split LEFT window)")
    (execute-command! app 'split-window-below)
    (let* ((count (length (qt-frame-windows fr)))
           (root (qt-frame-root fr)))
      (displayln "    Total windows: " count)
      (if (= count 3)
        (pass! "3 windows total")
        (fail! "Window count after v-split" count 3))

      ;; CRITICAL CHECK: Root should be HORIZONTAL
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_HORIZONTAL))
        (pass! "Root is HORIZONTAL (correct)")
        (fail! "Root orientation"
               (if (split-node? root) (split-node-orientation root) "not a node")
               QT_HORIZONTAL))

      ;; CRITICAL CHECK: Left child should be VERTICAL
      (when (and (split-node? root) (>= (length (split-node-children root)) 2))
        (let ((left-child (car (split-node-children root))))
          (if (and (split-node? left-child)
                   (= (split-node-orientation left-child) QT_VERTICAL))
            (pass! "Left child is VERTICAL (left pane was split)")
            (fail! "Left child structure"
                   (if (split-node? left-child) "vertical node" "not a node")
                   "vertical node")))))))

(def (main . args)
  (with-qt-app _app
    (qt-register-all-commands!)
    (run-tests)

    (displayln "\n───────────────────────────────")
    (displayln "RESULTS: " *passes* " passed, " *failures* " failed")
    (displayln "───────────────────────────────\n")

    (if (= *failures* 0)
      (begin
        (displayln "✓ All tests PASSED!")
        (exit 0))
      (begin
        (displayln "✗ " *failures* " test(s) FAILED - BUG CONFIRMED")
        (exit *failures*)))))
