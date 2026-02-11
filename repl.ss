;;; -*- Gerbil -*-
;;; REPL subprocess management for gerbil-emacs
;;;
;;; Manages a gxi subprocess: spawn, send input, read output, stop.
;;; No backend imports — shared between TUI and Qt.

(export
  (struct-out repl-state)
  repl-start!
  repl-send!
  repl-read-available
  repl-stop!
  repl-prompt)

(import :std/sugar
        :gerbil-emacs/core)

;;;============================================================================
;;; REPL state
;;;============================================================================

(defstruct repl-state
  (process       ; Gambit process port (bidirectional)
   prompt-pos    ; integer: byte position where current input starts
   history)      ; list of previous inputs
  transparent: #t)

(def repl-prompt "gerbil> ")

;;;============================================================================
;;; Lifecycle
;;;============================================================================

(def (repl-start!)
  "Spawn a gxi subprocess and return a repl-state."
  (let ((proc (open-process
                (list path: "gxi"
                      arguments: '()
                      stdin-redirection: #t
                      stdout-redirection: #t
                      stderr-redirection: #t
                      pseudo-terminal: #f))))
    (make-repl-state proc 0 [])))

(def (repl-send! rs input)
  "Send a line of input to the gxi process."
  (let ((proc (repl-state-process rs)))
    (display input proc)
    (newline proc)
    (force-output proc))
  ;; Add to history
  (set! (repl-state-history rs)
    (cons input (repl-state-history rs))))

(def (repl-read-available rs)
  "Read all available output from the gxi process (non-blocking).
   Returns a string, or #f if nothing available."
  (let ((proc (repl-state-process rs)))
    (if (char-ready? proc)
      (let ((out (open-output-string)))
        (let loop ()
          (when (char-ready? proc)
            (let ((ch (read-char proc)))
              (unless (eof-object? ch)
                (write-char ch out)
                (loop)))))
        (let ((s (get-output-string out)))
          (if (string-empty? s) #f s)))
      #f)))

(def (repl-stop! rs)
  "Shut down the gxi subprocess."
  (let ((proc (repl-state-process rs)))
    (with-catch void (lambda () (close-output-port proc)))
    (with-catch void (lambda () (process-status proc)))))
