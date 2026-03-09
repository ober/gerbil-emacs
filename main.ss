;;; -*- Gerbil -*-
;;; Executable entry point for gemacs

(export main)
(import :gemacs/editor
        :gemacs/window
        :gerbil-scintilla/tui
        (only-in :gemacs/app app-init! app-run! tui-session-save!)
        (only-in :gemacs/editor-extra-org *desktop-save-mode*)
        (only-in :gemacs/ipc stop-ipc-server!))

(include "manifest.ss")

(def (main . args)
  (cond
    ((member "--version" args)
     (displayln "gemacs " (cdar version-manifest))
     (for-each (lambda (p)
                 (when (not (string=? (car p) ""))
                   (displayln (car p) " " (cdr p))))
               (cdr version-manifest)))
    ((member "--help" args)
     (displayln "Usage: gemacs [OPTIONS] [FILES...]")
     (displayln "Options:")
     (displayln "  --version   Show version information")
     (displayln "  --help      Show this help message"))
    (else
     (let ((app (app-init! args)))
       (try
         (app-run! app)
         (finally
           ;; Save session if desktop-save-mode is enabled
           (when *desktop-save-mode* (tui-session-save! app))
           (stop-ipc-server!)
           (frame-shutdown! (app-state-frame app))
           (tui-shutdown!)))))))
