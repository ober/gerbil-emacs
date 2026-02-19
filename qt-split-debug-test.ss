;;; -*- Gerbil -*-
;;; Debug test - find where the segfault happens

(import :std/sugar
        (only-in :gerbil-qt/qt
                 with-qt-app
                 qt-widget-create
                 qt-scintilla-create)
        :gerbil-scintilla/constants
        (only-in :gemacs/qt/sci-shim sci-send)
        (only-in :gemacs/core
                 new-app-state
                 execute-command!
                 make-buffer
                 buffer-name
                 app-state-frame)
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 make-split-leaf
                 qt-frame-windows
                 qt-current-window
                 qt-edit-window-buffer)
        (only-in :gemacs/qt/commands qt-register-all-commands!))

(export main)

(def (main . args)
  (displayln "═══ Qt Split Debug Test ═══\n")

  (with-qt-app _app
    (displayln "Step 1: Qt app created")

    (qt-register-all-commands!)
    (displayln "Step 2: Commands registered")

    (let ((w (qt-widget-create)))
      (displayln "Step 3: Widget created: " w)

      (let ((ed (qt-scintilla-create parent: w)))
        (displayln "Step 4: Scintilla editor created: " ed)

        (let ((doc (sci-send ed SCI_GETDOCPOINTER)))
          (displayln "Step 5: Got document pointer: " doc)

          (let ((buf (make-buffer "TestBuf" #f doc #f #f #f #f)))
            (displayln "Step 6: Buffer created: " (buffer-name buf))

            (let ((win (make-qt-edit-window ed #f buf #f #f #f)))
              (displayln "Step 7: Qt edit window created")

              (let ((fr (make-qt-frame #f (make-split-leaf win) [win] 0 #f)))
                (displayln "Step 8: Qt frame created")

                (let ((app (new-app-state fr)))
                  (displayln "Step 9: App state created")
                  (displayln "        Frame has " (length (qt-frame-windows fr)) " window(s)")

                  (displayln "\nStep 10: Attempting split-window-right...")
                  (with-catch
                    (lambda (e)
                      (displayln "ERROR during split: " e)
                      (exit 1))
                    (lambda ()
                      (execute-command! app 'split-window-right)
                      (displayln "SUCCESS: split-window-right completed")
                      (displayln "        Frame now has " (length (qt-frame-windows fr)) " window(s)")

                      (displayln "\nStep 11: Attempting other-window...")
                      (execute-command! app 'other-window)
                      (displayln "SUCCESS: other-window completed")

                      (let ((cur-buf (buffer-name (qt-edit-window-buffer (qt-current-window fr)))))
                        (displayln "        Current buffer: " cur-buf))

                      (displayln "\nStep 12: Attempting split-window-below...")
                      (execute-command! app 'split-window-below)
                      (displayln "SUCCESS: split-window-below completed")
                      (displayln "        Frame now has " (length (qt-frame-windows fr)) " window(s)")

                      (displayln "\n✓ All steps completed successfully!")
                      (exit 0))))))))))

    (displayln "\n✗ Reached end without completing test")
    (exit 1)))
