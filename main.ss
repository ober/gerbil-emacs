;;; -*- Gerbil -*-
;;; Executable entry point for gemacs

(export main)
(import :gemacs/editor
        :gemacs/window
        :gerbil-scintilla/tui
        (only-in :gemacs/app app-init! app-run! tui-session-save!)
        (only-in :gemacs/editor-extra-org *desktop-save-mode*)
        (only-in :gemacs/ipc stop-ipc-server!)
        (only-in :gemacs/debug-repl start-debug-repl! stop-debug-repl!))

(include "manifest.ss")

(def (parse-repl-port args)
  "Return (port-num . filtered-args) if --repl <port> is present, else #f."
  (let loop ((rest args) (acc []))
    (cond
      ((null? rest) #f)
      ((and (string=? (car rest) "--repl")
            (pair? (cdr rest))
            (string->number (cadr rest)))
       (cons (string->number (cadr rest))
             (append (reverse acc) (cddr rest))))
      (else (loop (cdr rest) (cons (car rest) acc))))))

(def (main . args)
  ;; Single-processor mode: GAMBCOPT=,-:p1 must be set in the environment
  ;; BEFORE launching (Gambit reads it at startup). Set here for child processes.
  (setenv "GAMBCOPT" ",-:p1")
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
     (displayln "  --version        Show version information")
     (displayln "  --help           Show this help message")
     (displayln "  --repl <port>    Start TCP debug REPL on given port (0=auto)"))
    (else
     (let* ((repl-port-env (getenv "GEMACS_REPL_PORT" #f))
            (repl-info     (or (parse-repl-port args)
                               (and repl-port-env
                                    (cons (string->number repl-port-env) args))))
            (clean-args    (if repl-info (cdr repl-info) args))
            (app           (app-init! clean-args)))
       (when repl-info
         (start-debug-repl! (car repl-info)))
       (try
         (app-run! app)
         (finally
           ;; Save session if desktop-save-mode is enabled
           (when *desktop-save-mode* (tui-session-save! app))
           (when repl-info (stop-debug-repl!))
           (stop-ipc-server!)
           (frame-shutdown! (app-state-frame app))
           (tui-shutdown!)))))))
