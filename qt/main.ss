;;; -*- Gerbil -*-
;;; Executable entry point for gerbil-emacs Qt backend

(export main)
(import (only-in :gerbil-emacs/qt/app qt-main))

(include "../manifest.ss")

(def (main . args)
  (cond
    ((member "--version" args)
     (displayln "gerbil-emacs " (cdar version-manifest))
     (for-each (lambda (p)
                 (when (not (string=? (car p) ""))
                   (displayln (car p) " " (cdr p))))
               (cdr version-manifest)))
    ((member "--help" args)
     (displayln "Usage: gerbil-emacs-qt [OPTIONS] [FILES...]")
     (displayln "Options:")
     (displayln "  --version   Show version information")
     (displayln "  --help      Show this help message"))
    (else
     (apply qt-main args))))
