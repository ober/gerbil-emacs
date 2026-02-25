;;; -*- Gerbil -*-
;;; gsh-eshell: gsh-powered eshell replacing the toy eshell.ss
;;;
;;; Each eshell buffer gets its own shell-environment with full POSIX shell
;;; semantics: pipes, redirections, variables, globs, quoting, etc.

(export gsh-eshell-buffer?
        *gsh-eshell-state*
        gsh-eshell-prompt
        gsh-eshell-init-buffer!
        gsh-eshell-process-input)

(import :std/sugar
        :std/format
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gemacs/core)

;;;============================================================================
;;; State management
;;;============================================================================

;; Maps eshell buffers to their gsh shell-environment
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *gsh-eshell-state* (make-hash-table-eq))

;; Shared gsh initialization flag — call gsh-init! once
(def *gsh-initialized* #f)
(def *gsh-shared-env* #f)

(def gsh-eshell-prompt "gsh> ")

(def (gsh-eshell-buffer? buf)
  "Check if this buffer is a gsh eshell buffer."
  (eq? (buffer-lexer-lang buf) 'eshell))

(def (ensure-gsh-initialized!)
  "Initialize the gsh engine once (idempotent)."
  (unless *gsh-initialized*
    (set! *gsh-shared-env* (gsh-init!))
    (env-set! *gsh-shared-env* "SHELL" "gsh")
    (set! *gsh-initialized* #t)))

(def (gsh-eshell-init-buffer! buf)
  "Initialize a gsh environment for an eshell buffer.
   Returns the shell-environment for this buffer."
  (ensure-gsh-initialized!)
  ;; Each buffer gets a fresh environment so cd, variables, etc. are independent
  (let ((env (gsh-init!)))
    (hash-put! *gsh-eshell-state* buf env)
    env))

;;;============================================================================
;;; Input processing
;;;============================================================================

(def (gsh-eshell-process-input input buf)
  "Process an eshell input line via gsh.
   Returns (values output new-cwd).
   output can be:
     - a string to display
     - 'clear to clear the buffer
     - 'exit to close the eshell"
  (let ((env (hash-get *gsh-eshell-state* buf))
        (trimmed (string-trim-both input)))
    (cond
      ;; No environment — shouldn't happen
      ((not env)
       (values "Error: no gsh environment for this buffer\n"
               (current-directory)))

      ;; Empty input
      ((string=? trimmed "")
       (values "" (or (env-get env "PWD") (current-directory))))

      ;; Clear command
      ((string=? trimmed "clear")
       (values 'clear (or (env-get env "PWD") (current-directory))))

      ;; Exit command
      ((string=? trimmed "exit")
       (values 'exit (or (env-get env "PWD") (current-directory))))

      ;; Everything else goes through gsh
      (else
       (gsh-execute-and-capture trimmed env)))))

(def (gsh-execute-and-capture input env)
  "Execute INPUT via gsh, capturing stdout+stderr.
   Returns (values output-string cwd-string)."
  (with-catch
    (lambda (e)
      (values (string-append "gsh: "
                (with-output-to-string (lambda () (display-exception e)))
                "\n")
              (or (env-get env "PWD") (current-directory))))
    (lambda ()
      (let-values (((output status) (gsh-capture input env)))
        ;; gsh-capture strips trailing newlines (command substitution behavior)
        ;; but for display we want a trailing newline if there's output
        (let ((display-output (if (and (string? output)
                                       (> (string-length output) 0))
                                (string-append output "\n")
                                "")))
          (values display-output
                  (or (env-get env "PWD") (current-directory))))))))
