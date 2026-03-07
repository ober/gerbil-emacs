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
        gsh-eshell-process-input
        gsh-eshell-strip-ansi
        interactive-command?
        eshell-history-prev
        eshell-history-next
        eshell-history-reset!)

(import :std/sugar
        :std/format
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gsh/startup
        (only-in :gsh/prompt expand-prompt)
        :gemacs/core
        :gemacs/shell-history)

;;;============================================================================
;;; State management
;;;============================================================================

;; Maps eshell buffers to their gsh shell-environment
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *gsh-eshell-state* (make-hash-table-eq))

(def gsh-eshell-prompt "gsh> ")

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

(def (gsh-eshell-buffer? buf)
  "Check if this buffer is a gsh eshell buffer."
  (eq? (buffer-lexer-lang buf) 'eshell))

(def (gsh-eshell-init-buffer! buf)
  "Initialize a gsh environment for an eshell buffer.
   Sources ~/.gshrc for aliases, PS1, etc."
  (let ((env (gsh-init! #t)))  ; interactive? = #t
    (env-set! env "SHELL" "gsh")
    ;; Clear inherited bash PS1 BEFORE sourcing .gshrc — bash PS1 has
    ;; \[...\], $(cmd) syntax that gsh can't parse. .gshrc will set its own.
    (env-set! env "PS1" "\\u@\\h:\\w\\$ ")
    (with-catch
      (lambda (e)
        (gemacs-log! "gsh-eshell: startup file error: "
          (with-output-to-string "" (lambda () (display-exception e (current-output-port))))))
      (lambda () (load-startup-files! env #f #t)))
    (gemacs-log! "gsh-eshell: PS1=" (or (env-get env "PS1") "<none>"))
    (hash-put! *gsh-eshell-state* buf env)
    ;; Update the prompt from PS1
    (let* ((ps1 (or (env-get env "PS1") "gsh> "))
           (env-getter (lambda (name) (env-get env name)))
           (cmd-exec (make-cmd-exec-fn env)))
      (set! gsh-eshell-prompt
        (with-catch
          (lambda (e) "gsh> ")
          (lambda () (gsh-eshell-strip-ansi
                       (expand-prompt ps1 env-getter
                                      0  ; job-count
                                      (shell-environment-cmd-number env)
                                      0  ; history-number
                                      cmd-exec))))))
    env))

(def (gsh-eshell-get-prompt buf)
  "Return the expanded PS1 prompt for this eshell buffer."
  (let ((env (hash-get *gsh-eshell-state* buf)))
    (if env
      (let* ((ps1 (or (env-get env "PS1") "gsh> "))
             (env-getter (lambda (name) (env-get env name)))
             (cmd-exec (make-cmd-exec-fn env)))
        (with-catch
          (lambda (e) "gsh> ")
          (lambda ()
            (gsh-eshell-strip-ansi
              (expand-prompt ps1 env-getter
                             0  ; job-count
                             (shell-environment-cmd-number env)
                             0  ; history-number
                             cmd-exec)))))
      "gsh> ")))

(def (gsh-eshell-strip-ansi str)
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
                  ;; Character set designation: ESC ( X, ESC ) X, etc. (3 bytes)
                  ((and (memv next '(#\( #\) #\* #\+))
                        (< (+ i 2) len))
                   (loop (+ i 3) acc))
                  (else (loop (+ i 2) acc))))
              (loop (+ i 1) acc))
            (if (or (char=? ch #\return)
                    (char=? ch (integer->char 1))   ; RL_PROMPT_START_IGNORE
                    (char=? ch (integer->char 2)))   ; RL_PROMPT_END_IGNORE
              (loop (+ i 1) acc)
              (loop (+ i 1) (cons ch acc)))))))))

;;;============================================================================
;;; History navigation (up/down arrow in eshell)
;;;============================================================================

;; Per-buffer history navigation index (-1 = not navigating)
(def *eshell-history-index* (make-hash-table-eq))
;; Per-buffer saved input (what user typed before starting history navigation)
(def *eshell-saved-input* (make-hash-table-eq))

(def (eshell-history-prev buf current-input)
  "Navigate to the previous (older) history entry.
   Returns the history command string, or #f if no more history.
   On first call, saves current-input so it can be restored."
  (let* ((idx (or (hash-get *eshell-history-index* buf) -1))
         (history *gsh-history*)
         (hlen (length history))
         (new-idx (+ idx 1)))
    (if (>= new-idx hlen)
      #f  ; no more history
      (begin
        ;; Save the original input on first navigation
        (when (= idx -1)
          (hash-put! *eshell-saved-input* buf current-input))
        (hash-put! *eshell-history-index* buf new-idx)
        (caddr (list-ref history new-idx))))))

(def (eshell-history-next buf)
  "Navigate to the next (newer) history entry.
   Returns the history command string, the saved input, or #f if already at newest."
  (let* ((idx (or (hash-get *eshell-history-index* buf) -1)))
    (cond
      ;; Not navigating
      ((< idx 0) #f)
      ;; At newest entry — restore saved input
      ((= idx 0)
       (hash-put! *eshell-history-index* buf -1)
       (let ((saved (or (hash-get *eshell-saved-input* buf) "")))
         (hash-remove! *eshell-saved-input* buf)
         saved))
      ;; Move to newer entry
      (else
       (let ((new-idx (- idx 1)))
         (hash-put! *eshell-history-index* buf new-idx)
         (caddr (list-ref *gsh-history* new-idx)))))))

(def (eshell-history-reset! buf)
  "Reset history navigation state (called when input is submitted)."
  (hash-remove! *eshell-history-index* buf)
  (hash-remove! *eshell-saved-input* buf))

;;;============================================================================
;;; Interactive command detection (eshell-visual-commands equivalent)
;;;============================================================================

(def *interactive-commands*
  '("top" "htop" "btop" "vim" "vi" "nvim" "nano" "emacs" "less" "more"
    "man" "screen" "tmux" "ssh" "telnet" "ftp" "sftp" "python" "python3"
    "ipython" "node" "irb" "ghci" "gdb" "lldb" "mysql" "psql" "sqlite3"
    "mongosh" "redis-cli" "nmon" "atop" "iotop" "nethogs" "watch"
    "tail -f" "journalctl -f" "dmesg -w"))

(def (interactive-command? input)
  "Check if input starts with a known interactive/full-screen program."
  (let ((first-word (let* ((s (string-trim input))
                           (sp (string-index s #\space)))
                      (if sp (substring s 0 sp) s))))
    (member first-word *interactive-commands*)))

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
  ;; Reset history navigation on input submission
  (eshell-history-reset! buf)
  (let ((env (hash-get *gsh-eshell-state* buf))
        (trimmed (safe-string-trim-both input)))
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

      ;; Block interactive/full-screen programs that would hang eshell
      ((interactive-command? trimmed)
       (values (string-append trimmed ": use M-x vterm for interactive programs\n")
               (or (env-get env "PWD") (current-directory))))

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
            (values (gsh-eshell-strip-ansi
                      (string-append display-output
                                     (if (> (string-length stderr) 0) stderr "")))
                    (or (env-get env "PWD") (current-directory)))))))))
