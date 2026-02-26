;;; -*- Gerbil -*-
;;; Shell mode: gsh-backed in-process shell
;;;
;;; Uses gerbil-shell (gsh) for in-process POSIX shell execution.
;;; Sources ~/.gshrc, honors PS1, captures both stdout and stderr.

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
        :gsh/startup
        (only-in :gsh/prompt expand-prompt)
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
  "Create a gsh-backed shell and return a shell-state.
   Sources ~/.gshrc for PS1, aliases, etc."
  (let ((env (gsh-init! #t)))  ; interactive? = #t for alias expansion
    (env-set! env "SHELL" "gsh")
    ;; Set default PS1 before sourcing rc (rc may override)
    (unless (env-get env "PS1")
      (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
    ;; Source ~/.gshrc for interactive shells
    (with-catch
      (lambda (e) (void))  ; ignore errors in rc file
      (lambda () (load-startup-files! env #f #t)))  ; login?=#f interactive?=#t
    (make-shell-state env 0)))

(def (shell-prompt ss)
  "Return the expanded PS1 prompt string."
  (let* ((env (shell-state-env ss))
         (ps1 (or (env-get env "PS1") "$ "))
         (env-getter (lambda (name) (env-get env name))))
    (strip-ansi-codes
      (expand-prompt ps1 env-getter
                     0  ; job-count
                     (shell-environment-cmd-number env)))))

(def (shell-execute! input ss)
  "Execute a command via gsh, return (values output-string new-cwd).
   Output may be a string, 'clear, or 'exit.
   Captures both stdout and stderr."
  (let ((env (shell-state-env ss))
        (trimmed (string-trim-both input)))
    (env-inc-cmd-number! env)
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
           ;; Capture both stdout and stderr
           (let* ((err-port (open-output-string))
                  (result (parameterize ((current-error-port err-port))
                            (gsh-capture trimmed env))))
             (let-values (((stdout status) result))
               (let ((stderr (get-output-string err-port))
                     (cwd (or (env-get env "PWD") (current-directory))))
                 (values (string-append (or stdout "")
                                        (if (> (string-length stderr) 0) stderr ""))
                         cwd))))))))))

(def (shell-stop! ss)
  "Clean up the shell state (no-op for gsh-backed shells)."
  (void))
