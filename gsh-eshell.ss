;;; -*- Gerbil -*-
;;; gsh-eshell: gsh-powered eshell replacing the toy eshell.ss
;;;
;;; Each eshell buffer gets its own shell-environment with full POSIX shell
;;; semantics: pipes, redirections, variables, globs, quoting, etc.

(export gsh-eshell-buffer?
        *gsh-eshell-state*
        gsh-eshell-prompt
        gsh-eshell-get-prompt
        gsh-eshell-init-buffer!
        gsh-eshell-process-input)

(import :std/sugar
        :std/format
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gsh/startup
        (only-in :gsh/prompt expand-prompt)
        :gemacs/core)

;;;============================================================================
;;; State management
;;;============================================================================

;; Maps eshell buffers to their gsh shell-environment
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *gsh-eshell-state* (make-hash-table-eq))

(def gsh-eshell-prompt "gsh> ")

(def (gsh-eshell-buffer? buf)
  "Check if this buffer is a gsh eshell buffer."
  (eq? (buffer-lexer-lang buf) 'eshell))

(def (gsh-eshell-init-buffer! buf)
  "Initialize a gsh environment for an eshell buffer.
   Sources ~/.gshrc for aliases, PS1, etc."
  (let ((env (gsh-init! #t)))  ; interactive? = #t
    (env-set! env "SHELL" "gsh")
    (unless (env-get env "PS1")
      (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
    (with-catch
      (lambda (e) (void))
      (lambda () (load-startup-files! env #f #t)))
    (hash-put! *gsh-eshell-state* buf env)
    ;; Update the prompt from PS1
    (let* ((ps1 (or (env-get env "PS1") "gsh> "))
           (env-getter (lambda (name) (env-get env name))))
      (set! gsh-eshell-prompt
        (with-catch
          (lambda (e) "gsh> ")
          (lambda () (strip-ansi-codes
                       (expand-prompt ps1 env-getter))))))
    env))

(def (gsh-eshell-get-prompt buf)
  "Return the expanded PS1 prompt for this eshell buffer."
  (let ((env (hash-get *gsh-eshell-state* buf)))
    (if env
      (let* ((ps1 (or (env-get env "PS1") "gsh> "))
             (env-getter (lambda (name) (env-get env name))))
        (with-catch
          (lambda (e) "gsh> ")
          (lambda ()
            (strip-ansi-codes
              (expand-prompt ps1 env-getter
                             0  ; job-count
                             (shell-environment-cmd-number env))))))
      "gsh> ")))

(def (strip-ansi-codes str)
  "Remove ANSI escape sequences from a string."
  (let* ((len (string-length str))
         (esc (integer->char 27))
         (bel (integer->char 7)))
    (let loop ((i 0) (acc []))
      (if (>= i len)
        (list->string (reverse acc))
        (let ((ch (string-ref str i)))
          (if (char=? ch esc)
            (if (< (+ i 1) len)
              (let ((next (string-ref str (+ i 1))))
                (cond
                  ((char=? next #\[)
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (let ((c (string-ref str j)))
                         (if (and (char>=? c #\@) (char<=? c #\~))
                           (loop (+ j 1) acc)
                           (skip (+ j 1)))))))
                  ((char=? next #\])
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (if (char=? (string-ref str j) bel)
                         (loop (+ j 1) acc)
                         (skip (+ j 1))))))
                  (else (loop (+ i 2) acc))))
              (loop (+ i 1) acc))
            (if (char=? ch #\return)
              (loop (+ i 1) acc)
              (loop (+ i 1) (cons ch acc)))))))))

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
  (env-inc-cmd-number! env)
  (with-catch
    (lambda (e)
      (values (string-append "gsh: "
                (with-output-to-string (lambda () (display-exception e)))
                "\n")
              (or (env-get env "PWD") (current-directory))))
    (lambda ()
      (let* ((err-port (open-output-string))
             (result (parameterize ((current-error-port err-port))
                       (gsh-capture input env))))
        (let-values (((stdout status) result))
          (let ((stderr (get-output-string err-port))
                (display-output (if (and (string? stdout)
                                         (> (string-length stdout) 0))
                                  (string-append stdout "\n")
                                  "")))
            (values (string-append display-output
                                  (if (> (string-length stderr) 0) stderr ""))
                    (or (env-get env "PWD") (current-directory)))))))))
