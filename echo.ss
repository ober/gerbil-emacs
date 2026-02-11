;;; -*- Gerbil -*-
;;; TUI echo area / minibuffer for gerbil-emacs
;;;
;;; The echo area occupies the last terminal row.
;;; It displays messages and handles simple line input for prompts.
;;;
;;; Echo state and message functions are in core.ss.

(export
  (struct-out echo-state)
  make-initial-echo-state
  echo-message!
  echo-error!
  echo-clear!
  echo-draw!
  echo-read-string)

(import :std/sugar
        :gerbil-scintilla/tui
        :gerbil-emacs/core)

;;;============================================================================
;;; Draw the echo area (TUI-specific)
;;;============================================================================

(def (echo-draw! echo row width)
  "Draw the echo area at the given row."
  ;; Clear the row (dark background matching editor theme)
  (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
  ;; Draw message if any
  (let ((msg (echo-state-message echo)))
    (when msg
      (let ((fg (if (echo-state-error? echo)
                  #xff4040    ; bright red for errors
                  #xd8d8d8)) ; light gray for normal messages
            (display-msg (if (> (string-length msg) width)
                           (substring msg 0 width)
                           msg)))
        (tui-print! 0 row fg #x181818 display-msg)))))

;;;============================================================================
;;; Read a string from the user in the echo area (TUI-specific)
;;; Runs a blocking sub-event-loop.
;;; Returns the input string, or #f if cancelled (C-g).
;;;============================================================================

(def (echo-read-string echo prompt row width)
  (echo-clear! echo)
  (let loop ((input ""))
    ;; Draw prompt + input
    (let* ((display-str (string-append prompt input))
           (display-len (string-length display-str)))
      (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
      (tui-print! 0 row #xd8d8d8 #x181818
                  (if (> display-len width)
                    (substring display-str 0 width)
                    display-str))
      (tui-set-cursor! (min display-len (- width 1)) row)
      (tui-present!))
    ;; Wait for key
    (let ((ev (tui-poll-event)))
      (cond
        ((not ev) (loop input))
        ((not (tui-event-key? ev)) (loop input))
        (else
         (let ((key (tui-event-key ev))
               (ch  (tui-event-ch ev)))
           (cond
             ;; C-g (0x07) -> cancel
             ((= key #x07)
              (echo-message! echo "Quit")
              #f)
             ;; Enter (0x0D) -> accept
             ((= key #x0D) input)
             ;; Backspace (0x08 or 0x7F) -> delete last char
             ((or (= key #x08) (= key #x7F))
              (if (> (string-length input) 0)
                (loop (substring input 0 (- (string-length input) 1)))
                (loop input)))
             ;; Printable char -> append
             ((> ch 31)
              (loop (string-append input (string (integer->char ch)))))
             ;; Ignore other keys
             (else (loop input)))))))))
