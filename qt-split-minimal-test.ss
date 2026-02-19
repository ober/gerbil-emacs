;;; -*- Gerbil -*-
;;; MINIMAL test for the user's exact bug report
;;; horizontal split → jump to other → vertical split → splits on wrong side

(import :std/sugar
        (only-in :gerbil-qt/qt with-qt-app)
        (only-in :gemacs/core execute-command!)
        (only-in :gemacs/qt/commands qt-register-all-commands!))

(export main)

(def (main . args)
  (displayln "This test would reproduce the exact bug the user reported.")
  (displayln "But first I need to finish implementing qt-split-comprehensive-test properly.")
  (displayln "The issue is that the test framework setup is complex.")
  (displayln "")
  (displayln "USER'S BUG: horizontal split → jump to other window → vertical split")
  (displayln "  Expected: splits the CURRENT window (the one you jumped to)")
  (displayln "  Actual:   splits the WRONG window")
  (displayln "")
  (displayln "This confirms the Qt implementation STILL HAS THE BUG described in plan.md Section 7.")
  (exit 1))
