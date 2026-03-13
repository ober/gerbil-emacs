;;; -*- Gerbil -*-
;;; Executable entry point for gemacs Qt backend

(export main)
(import (only-in :gemacs/qt/app qt-main))

(include "../manifest.ss")

(def (main . args)
  ;; Single-processor mode: GAMBCOPT=,-:p1 must be set in the environment
  ;; BEFORE launching the binary (Gambit reads it at startup, before main).
  ;; Set it here for child processes spawned by gemacs.
  (setenv "GAMBCOPT" ",-:p1")
  (cond
    ((member "--version" args)
     (displayln "gemacs " (cdar version-manifest))
     (for-each (lambda (p)
                 (when (not (string=? (car p) ""))
                   (displayln (car p) " " (cdr p))))
               (cdr version-manifest)))
    ((member "--help" args)
     (displayln "Usage: gemacs-qt [OPTIONS] [FILES...]")
     (displayln "Options:")
     (displayln "  --version        Show version information")
     (displayln "  --help           Show this help message")
     (displayln "  --verbose        Log all Qt calls and commands to ~/.gemacs-verbose.log")
     (displayln "  --repl <port>    Start TCP debug REPL on given port (0=auto)"))
    (else
     (apply qt-main args))))
