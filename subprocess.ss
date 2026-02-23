;;; -*- Gerbil -*-
;;; Interruptible subprocess execution for gemacs.
;;; Provides non-blocking process I/O that can be interrupted by C-g.

(export run-process-interruptible
        run-process-interruptible/qt
        *active-subprocess*
        kill-active-subprocess!)

(import :std/sugar
        :gemacs/core)

;;;============================================================================
;;; Active subprocess tracking
;;;============================================================================

(def *active-subprocess* #f)

(def (kill-active-subprocess!)
  "Kill the currently tracked subprocess, if any."
  (when *active-subprocess*
    (with-catch void
      (lambda () (close-port *active-subprocess*)))
    (set! *active-subprocess* #f)))

;;;============================================================================
;;; Internal: drain available chars from port into string-port
;;;============================================================================

(def (drain-available! proc out)
  "Read all immediately available chars from PROC into OUT.
   Returns #t if EOF was reached, #f otherwise."
  (let loop ()
    (if (char-ready? proc)
      (let ((ch (read-char proc)))
        (if (eof-object? ch)
          #t
          (begin (write-char ch out) (loop))))
      #f)))

;;;============================================================================
;;; TUI variant: polls tui-peek-event for C-g
;;;============================================================================

(def (run-process-interruptible cmd
                                peek-event  ;; (lambda (timeout-ms) -> event-or-#f)
                                event-key?  ;; (lambda (ev) -> bool)
                                event-key   ;; (lambda (ev) -> key-code)
                                stdin-text: (stdin-text #f)
                                poll-ms: (poll-ms 50))
  "Run CMD via /bin/sh, reading output non-blockingly.
   PEEK-EVENT is called with poll-ms to check for C-g (key code 7).
   Returns (values output-string exit-status).
   Raises keyboard-quit-exception on C-g."
  (let ((proc (open-process
                (list path: "/bin/sh"
                      arguments: ["-c" cmd]
                      stdin-redirection: (if stdin-text #t #f)
                      stdout-redirection: #t
                      stderr-redirection: #t
                      pseudo-terminal: #f))))
    (dynamic-wind
      (lambda () (set! *active-subprocess* proc))
      (lambda ()
        ;; Write stdin if provided
        (when stdin-text
          (display stdin-text proc)
          (force-output proc)
          (close-output-port proc))
        ;; Poll loop: check for C-g, drain output, repeat
        (let ((out (open-output-string)))
          (let loop ()
            ;; Check for C-g via peek-event
            (let ((ev (peek-event poll-ms)))
              (when (and ev (event-key? ev) (= (event-key ev) 7))
                (with-catch void (lambda () (close-port proc)))
                (raise (make-keyboard-quit-exception))))
            ;; Drain whatever's available
            (if (drain-available! proc out)
              ;; EOF reached — process finished
              (let ((status (with-catch (lambda (_) #f)
                              (lambda () (process-status proc)))))
                (close-port proc)
                (values (get-output-string out) status))
              ;; Not done yet — loop
              (loop)))))
      (lambda () (set! *active-subprocess* #f)))))

;;;============================================================================
;;; Qt variant: pumps Qt event loop to let C-g handler set quit flag
;;;============================================================================

(def (run-process-interruptible/qt cmd
                                   process-events!  ;; (lambda () -> void)
                                   stdin-text: (stdin-text #f)
                                   poll-ms: (poll-ms 50))
  "Run CMD via /bin/sh, reading output non-blockingly.
   PROCESS-EVENTS! pumps the Qt event loop so keystrokes set *quit-flag*.
   Returns (values output-string #f).
   Raises keyboard-quit-exception on C-g.
   NOTE: Omits process-status (hangs in Qt due to SIGCHLD race)."
  (let ((proc (open-process
                (list path: "/bin/sh"
                      arguments: ["-c" cmd]
                      stdin-redirection: (if stdin-text #t #f)
                      stdout-redirection: #t
                      stderr-redirection: #t
                      pseudo-terminal: #f))))
    (dynamic-wind
      (lambda () (set! *active-subprocess* proc))
      (lambda ()
        ;; Write stdin if provided
        (when stdin-text
          (display stdin-text proc)
          (force-output proc)
          (close-output-port proc))
        ;; Poll loop: pump events, check quit flag, drain output
        (let ((out (open-output-string)))
          (let loop ()
            ;; Pump Qt event loop
            (process-events!)
            ;; Check quit flag
            (when (quit-flag?)
              (quit-flag-clear!)
              (with-catch void (lambda () (close-port proc)))
              (raise (make-keyboard-quit-exception)))
            ;; Drain whatever's available
            (if (drain-available! proc out)
              ;; EOF reached — process finished
              (begin
                (close-port proc)
                (values (get-output-string out) #f))
              ;; Not done yet — sleep briefly to avoid busy-wait, then loop
              (begin
                (thread-sleep! (/ poll-ms 1000.0))
                (loop))))))
      (lambda () (set! *active-subprocess* #f)))))
