;;; -*- Gerbil -*-
;;; Shell mode: run $SHELL in a buffer with subprocess I/O
;;;
;;; Uses Gambit's open-process with pseudo-terminal for proper
;;; terminal emulation. ANSI escape codes are stripped before display.

(export shell-buffer?
        *shell-state*
        (struct-out shell-state)
        shell-start!
        shell-send!
        shell-read-available
        shell-stop!
        shell-prompt
        strip-ansi-codes)

(import :std/sugar
        :gerbil-emacs/core)

;;;============================================================================
;;; Shell state
;;;============================================================================

(def (shell-buffer? buf)
  "Check if this buffer is a shell buffer."
  (eq? (buffer-lexer-lang buf) 'shell))

;; Maps shell buffers to their shell-state structs
(def *shell-state* (make-hash-table))

(defstruct shell-state
  (process      ; Gambit process port (bidirectional)
   prompt-pos)  ; integer: byte position where current input starts
  transparent: #t)

(def shell-prompt "")  ; Shell provides its own prompt via output

;;;============================================================================
;;; ANSI escape code stripping
;;;============================================================================

(def (strip-ansi-codes str)
  "Remove ANSI escape sequences from a string.
   Handles CSI sequences (ESC [ ... letter) and OSC sequences (ESC ] ... BEL)."
  (let* ((len (string-length str))
         (esc (integer->char 27))
         (bel (integer->char 7)))
    (let loop ((i 0) (acc []))
      (if (>= i len)
        (list->string (reverse acc))
        (let ((ch (string-ref str i)))
          (if (char=? ch esc)
            ;; Start of escape sequence
            (if (< (+ i 1) len)
              (let ((next (string-ref str (+ i 1))))
                (cond
                  ;; CSI: ESC [ ... letter
                  ((char=? next #\[)
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (let ((c (string-ref str j)))
                         (if (and (char>=? c #\@) (char<=? c #\~))
                           (loop (+ j 1) acc)  ; skip the final char too
                           (skip (+ j 1)))))))
                  ;; OSC: ESC ] ... BEL
                  ((char=? next #\])
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (if (char=? (string-ref str j) bel)
                         (loop (+ j 1) acc)
                         (skip (+ j 1))))))
                  ;; Other: ESC + single char
                  (else (loop (+ i 2) acc))))
              (loop (+ i 1) acc))
            ;; Regular character — keep it (but skip carriage returns)
            (if (char=? ch #\return)
              (loop (+ i 1) acc)
              (loop (+ i 1) (cons ch acc)))))))))

;;;============================================================================
;;; Shell lifecycle
;;;============================================================================

(def (shell-start!)
  "Spawn $SHELL and return a shell-state."
  (let* ((shell-path (getenv "SHELL" "/bin/bash"))
         (proc (open-process
                 (list path: shell-path
                       arguments: '()
                       stdin-redirection: #t
                       stdout-redirection: #t
                       stderr-redirection: #t
                       pseudo-terminal: #f))))
    (make-shell-state proc 0)))

(def (shell-send! ss input)
  "Send a line of input to the shell."
  (let ((proc (shell-state-process ss)))
    (display input proc)
    (newline proc)
    (force-output proc)))

(def (shell-read-available ss)
  "Read all available output from the shell (non-blocking).
   Returns stripped text, or #f if nothing available."
  (let ((proc (shell-state-process ss)))
    (if (char-ready? proc)
      (let ((out (open-output-string)))
        (let loop ()
          (when (char-ready? proc)
            (let ((ch (read-char proc)))
              (unless (eof-object? ch)
                (write-char ch out)
                (loop)))))
        (let* ((raw (get-output-string out))
               (cleaned (strip-ansi-codes raw)))
          (if (string=? cleaned "") #f cleaned)))
      #f)))

(def (shell-stop! ss)
  "Shut down the shell subprocess."
  (let ((proc (shell-state-process ss)))
    (with-catch void (lambda () (close-output-port proc)))
    (with-catch void (lambda () (process-status proc)))))
