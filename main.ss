;;; -*- Gerbil -*-
;;; Executable entry point for gerbil-emacs

(export main)
(import :gerbil-emacs/editor
        :gerbil-emacs/window
        :gerbil-scintilla/tui
        (only-in :gerbil-emacs/app app-init! app-run!)
        (only-in :gerbil-emacs/ipc stop-ipc-server!))

(include "manifest.ss")

(def (main . args)
  (cond
    ((member "--version" args)
     (displayln "gerbil-emacs " (cdar version-manifest))
     (for-each (lambda (p)
                 (when (not (string=? (car p) ""))
                   (displayln (car p) " " (cdr p))))
               (cdr version-manifest)))
    ((member "--help" args)
     (displayln "Usage: gerbil-emacs [OPTIONS] [FILES...]")
     (displayln "Options:")
     (displayln "  --version   Show version information")
     (displayln "  --help      Show this help message"))
    (else
     (let ((app (app-init! args)))
       (try
         (app-run! app)
         (finally
           (stop-ipc-server!)
           (frame-shutdown! (app-state-frame app))
           (tui-shutdown!)))))))
