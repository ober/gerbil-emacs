;;; -*- Gerbil -*-
;;; Shell mode: gsh-backed in-process shell
;;;
;;; Uses gerbil-shell (gsh) for in-process POSIX shell execution.
;;; No subprocess or PTY — commands are executed synchronously via gsh-capture.

(export shell-buffer?
        *shell-state*
        (struct-out shell-state)
        shell-start!
        shell-execute!
        shell-stop!
        shell-prompt
        strip-ansi-codes)

(import :std/sugar
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gemacs/core)

;;;============================================================================
;;; Shell state
;;;============================================================================

(def (shell-buffer? buf)
  "Check if this buffer is a shell buffer."
  (eq? (buffer-lexer-lang buf) 'shell))

;; Maps shell buffers to their shell-state structs
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *shell-state* (make-hash-table-eq))

(defstruct shell-state
  (env          ; gsh shell-environment
   prompt-pos)  ; integer: byte position where current input starts
  transparent: #t)

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
;;; Shell lifecycle (gsh-backed)
;;;============================================================================

(def (shell-start!)
  "Create a gsh-backed shell and return a shell-state."
  (let ((env (gsh-init!)))
    (env-set! env "SHELL" "gsh")
    (make-shell-state env 0)))

(def (shell-prompt ss)
  "Return the shell prompt string showing cwd."
  (let* ((env (shell-state-env ss))
         (cwd (or (env-get env "PWD") (current-directory)))
         (home (getenv "HOME" ""))
         (display-cwd (if (and (> (string-length home) 0)
                               (string-prefix? home cwd))
                        (string-append "~" (substring cwd (string-length home)
                                                          (string-length cwd)))
                        cwd)))
    (string-append display-cwd " $ ")))

(def (shell-execute! input ss)
  "Execute a command via gsh, return (values output-string new-cwd).
   Output may be a string, 'clear, or 'exit."
  (let ((env (shell-state-env ss))
        (trimmed (string-trim-both input)))
    (cond
      ((string=? trimmed "")
       (values "" (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "clear")
       (values 'clear (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "exit")
       (values 'exit (or (env-get env "PWD") (current-directory))))
      (else
       (with-catch
         (lambda (e)
           (values (string-append "gsh: "
                     (with-output-to-string (lambda () (display-exception e)))
                     "\n")
                   (or (env-get env "PWD") (current-directory))))
         (lambda ()
           (let-values (((output status) (gsh-capture trimmed env)))
             (values (or output "")
                     (or (env-get env "PWD") (current-directory))))))))))

(def (shell-stop! ss)
  "Clean up the shell state (no-op for gsh-backed shells)."
  (void))
