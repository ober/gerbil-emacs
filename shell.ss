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
        shell-execute-async!
        shell-poll-output
        shell-pty-busy?
        shell-interrupt!
        shell-send-input!
        shell-cleanup-pty!
        shell-stop!
        shell-prompt
        strip-ansi-codes)

(import :std/sugar
        :std/srfi/13
        :std/misc/channel
        :gsh/lib
        :gsh/environment
        :gsh/startup
        (only-in :gsh/prompt expand-prompt)
        (only-in :gsh/registry builtin?)
        :gemacs/core
        :gemacs/pty)

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
   prompt-pos   ; integer: byte position where current input starts
   pty-master   ; PTY master fd (or #f)
   pty-pid      ; child process PID (or #f)
   pty-channel  ; channel for PTY output messages (or #f)
   pty-thread)  ; reader thread (or #f)
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
                  ;; Character set designation: ESC ( X, ESC ) X, etc. (3 bytes)
                  ((and (memv next '(#\( #\) #\* #\+))
                        (< (+ i 2) len))
                   (loop (+ i 3) acc))
                  ;; Other: ESC + single char
                  (else (loop (+ i 2) acc))))
              (loop (+ i 1) acc))
            ;; Regular character — keep it (but skip carriage returns)
            (if (or (char=? ch #\return)
                    (char=? ch (integer->char 1))   ; RL_PROMPT_START_IGNORE
                    (char=? ch (integer->char 2)))   ; RL_PROMPT_END_IGNORE
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
    (make-shell-state env 0 #f #f #f #f)))

(def (make-cmd-exec-fn env)
  "Create a command-execution function for PS1 $(...) expansion."
  (lambda (cmd)
    (with-catch
      (lambda (e) "")
      (lambda ()
        (let-values (((output status) (gsh-capture cmd env)))
          (let ((s (or output "")))
            ;; Strip trailing newline (command substitution convention)
            (if (and (> (string-length s) 0)
                     (char=? (string-ref s (- (string-length s) 1)) #\newline))
              (substring s 0 (- (string-length s) 1))
              s)))))))

(def (shell-prompt ss)
  "Return the expanded PS1 prompt string."
  (let* ((env (shell-state-env ss))
         (ps1 (or (env-get env "PS1") "$ "))
         (env-getter (lambda (name) (env-get env name))))
    (strip-ansi-codes
      (expand-prompt ps1 env-getter
                     0  ; job-count
                     (shell-environment-cmd-number env)
                     0  ; history-number
                     (make-cmd-exec-fn env)))))

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
  "Clean up the shell state."
  (shell-cleanup-pty! ss))

;;;============================================================================
;;; Async PTY execution
;;;============================================================================

(def (shell-pure-simple-command? input)
  "Check if input is a simple command without shell metacharacters."
  (let ((len (string-length input)))
    (let loop ((i 0) (in-quote #f))
      (if (>= i len) #t
        (let ((ch (string-ref input i)))
          (cond
            ((and (not in-quote) (memv ch '(#\| #\; #\& #\`)))
             #f)
            ((char=? ch #\') (loop (+ i 1) (not in-quote)))
            (else (loop (+ i 1) in-quote))))))))

(def (shell-extract-first-word input)
  "Extract the first command word, skipping leading VAR=val assignments."
  (let ((len (string-length input)))
    (let loop ((i 0))
      (if (>= i len) #f
        (let ((ch (string-ref input i)))
          (cond
            ((char-whitespace? ch) (loop (+ i 1)))
            ((not (char-whitespace? ch))
             (let end ((j i))
               (if (or (>= j len) (char-whitespace? (string-ref input j)))
                 (let ((word (substring input i j)))
                   (if (string-index word #\=)
                     (loop j)  ; skip VAR=val
                     word))
                 (end (+ j 1)))))
            (else (loop (+ i 1)))))))))

(def (shell-pty-waitpid-status pid nohang?)
  "Wait for child and return just the exit status integer."
  (let-values (((status exited?) (pty-waitpid pid nohang?)))
    status))

(def (shell-pty-reader-loop mfd pid ch)
  "Reader thread body: poll PTY output and post to channel."
  (with-catch
    (lambda (e)
      (gemacs-log! "Shell PTY reader error: "
        (with-output-to-string
          (lambda () (display-exception e))))
      (channel-put ch (cons 'done -1)))
    (lambda ()
      (let loop ()
        (thread-sleep! 0.05)
        (let ((data (pty-read mfd)))
          (cond
            ((string? data)
             (channel-put ch (cons 'data data))
             (loop))
            ((eq? data 'eof)
             (let ((status (shell-pty-waitpid-status pid #f)))
               (channel-put ch (cons 'done status))))
            (else
             (if (pty-child-alive? pid)
               (loop)
               (let drain ()
                 (let ((d (pty-read mfd)))
                   (cond
                     ((string? d)
                      (channel-put ch (cons 'data d))
                      (drain))
                     (else
                      (let ((status (shell-pty-waitpid-status pid #t)))
                        (channel-put ch (cons 'done status)))))))))))))))

(def (shell-execute-async! input ss)
  "Execute command: builtins go through gsh-capture (sync),
   external commands go through PTY subprocess (async).
   Returns:
   - (values 'sync output cwd) for sync builtins
   - (values 'async #f #f) when command dispatched to PTY
   - (values 'special 'clear|'exit cwd) for clear/exit"
  (let ((env (shell-state-env ss))
        (trimmed (string-trim-both input)))
    (env-inc-cmd-number! env)
    (cond
      ((string=? trimmed "")
       (values 'sync "" (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "clear")
       (values 'special 'clear (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "exit")
       (values 'special 'exit (or (env-get env "PWD") (current-directory))))
      (else
       (let ((first-word (shell-extract-first-word trimmed)))
         (if (and first-word
                  (builtin? first-word)
                  (shell-pure-simple-command? trimmed))
           ;; Builtin: run in-process via gsh-capture (synchronous)
           (let-values (((output cwd) (shell-execute! input ss)))
             (values 'sync output cwd))
           ;; External/compound: spawn PTY subprocess (async)
           (let* ((env-alist (env-exported-alist env))
                  (rows 24) (cols 80))
             (let-values (((mfd pid) (pty-spawn trimmed env-alist rows cols)))
               (if (and mfd pid)
                 (let ((ch (make-channel)))
                   (set! (shell-state-pty-master ss) mfd)
                   (set! (shell-state-pty-pid ss) pid)
                   (set! (shell-state-pty-channel ss) ch)
                   (let ((thread (spawn (lambda () (shell-pty-reader-loop mfd pid ch)))))
                     (set! (shell-state-pty-thread ss) thread)
                     (values 'async #f #f)))
                 ;; forkpty failed, fall back to sync
                 (let-values (((output cwd) (shell-execute! input ss)))
                   (values 'sync output cwd)))))))))))

(def (shell-poll-output ss)
  "Non-blocking check for PTY output. Returns:
   - (cons 'data string) — output chunk
   - (cons 'done exit-status) — child exited
   - #f — nothing available"
  (let ((ch (shell-state-pty-channel ss)))
    (and ch (channel-try-get ch))))

(def (shell-pty-busy? ss)
  "Check if a PTY command is currently running."
  (and (shell-state-pty-pid ss) #t))

(def (shell-interrupt! ss)
  "Send SIGINT to the PTY child process group."
  (let ((pid (shell-state-pty-pid ss)))
    (when pid
      (pty-kill! pid 2))))

(def (shell-send-input! ss str)
  "Send string to the PTY child's stdin."
  (let ((mfd (shell-state-pty-master ss)))
    (when mfd
      (pty-write mfd str))))

(def (shell-cleanup-pty! ss)
  "Clean up PTY state after command finishes or on stop."
  (let ((mfd (shell-state-pty-master ss))
        (pid (shell-state-pty-pid ss)))
    (when mfd
      (pty-close! mfd pid))
    (set! (shell-state-pty-master ss) #f)
    (set! (shell-state-pty-pid ss) #f)
    (set! (shell-state-pty-channel ss) #f)
    (set! (shell-state-pty-thread ss) #f)))
