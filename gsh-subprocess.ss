;;; -*- Gerbil -*-
;;; gsh-subprocess: gsh-powered command execution for M-!, grep, compile, etc.
;;;
;;; Replaces subprocess.ss's bash-forking approach with in-process gsh execution.
;;; Commands are parsed and executed by gsh — builtins run in-process,
;;; external commands are fork-exec'd by gsh's executor.

(export gsh-run-command
        gsh-run-command/qt
        *gsh-active-process*
        gsh-kill-active-process!)

(import :std/sugar
        :std/format
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gemacs/core)

;;;============================================================================
;;; Active process tracking
;;;============================================================================

(def *gsh-active-process* #f)

(def (gsh-kill-active-process!)
  "Kill the currently tracked gsh subprocess, if any."
  ;; gsh manages its own child processes; we just clear our flag
  (set! *gsh-active-process* #f))

;;;============================================================================
;;; Heredoc delimiter safety
;;;============================================================================

(def (make-safe-heredoc-delimiter text)
  "Generate a heredoc delimiter that does not appear in TEXT.
   Avoids injection when TEXT contains the delimiter string."
  (let loop ((suffix 0))
    (let ((delim (if (= suffix 0)
                   "__GSH_STDIN__"
                   (string-append "__GSH_STDIN_" (number->string suffix) "__"))))
      (if (string-contains text delim)
        (loop (+ suffix 1))
        delim))))

;;;============================================================================
;;; Shared gsh environment for one-shot commands
;;;============================================================================

(def *gsh-cmd-env* #f)

(def (ensure-gsh-cmd-env!)
  "Lazily initialize a shared gsh environment for one-shot commands."
  (unless *gsh-cmd-env*
    (set! *gsh-cmd-env* (gsh-init!)))
  *gsh-cmd-env*)

;;;============================================================================
;;; TUI variant: polls tui-peek-event for C-g
;;;============================================================================

(def (gsh-run-command cmd
                      peek-event  ;; (lambda (timeout-ms) -> event-or-#f)
                      event-key?  ;; (lambda (ev) -> bool)
                      event-key   ;; (lambda (ev) -> key-code)
                      stdin-text: (stdin-text #f)
                      cwd: (cwd #f))
  "Run CMD via gsh, capturing output.
   PEEK-EVENT is called to check for C-g (key code 7).
   Returns (values output-string exit-status).
   Raises keyboard-quit-exception on C-g."
  ;; Check for C-g before starting
  (let ((ev (peek-event 0)))
    (when (and ev (event-key? ev) (= (event-key ev) 7))
      (raise (make-keyboard-quit-exception))))
  (let ((env (ensure-gsh-cmd-env!)))
    ;; Set cwd if specified
    (when cwd
      (env-set! env "PWD" cwd)
      (with-catch void (lambda () (current-directory cwd))))
    (dynamic-wind
      (lambda () (set! *gsh-active-process* #t))
      (lambda ()
        ;; Handle stdin-text by prepending heredoc.
        ;; Use a safe delimiter that doesn't appear in stdin-text.
        (let ((effective-cmd
               (if stdin-text
                 (let ((delim (make-safe-heredoc-delimiter stdin-text)))
                   (string-append "cat <<'" delim "'\n"
                                  stdin-text
                                  "\n" delim "\n | " cmd))
                 cmd)))
          (let-values (((output status) (gsh-capture effective-cmd env)))
            (values (or output "") (or status 0)))))
      (lambda () (set! *gsh-active-process* #f)))))

;;;============================================================================
;;; Qt variant: pumps Qt event loop for C-g
;;;============================================================================

(def (gsh-run-command/qt cmd
                         process-events!  ;; (lambda () -> void)
                         stdin-text: (stdin-text #f)
                         cwd: (cwd #f))
  "Run CMD via gsh, capturing output.
   PROCESS-EVENTS! pumps the Qt event loop so keystrokes set *quit-flag*.
   Returns (values output-string exit-status).
   Raises keyboard-quit-exception on C-g."
  ;; Check quit flag before starting
  (process-events!)
  (when (quit-flag?)
    (quit-flag-clear!)
    (raise (make-keyboard-quit-exception)))
  (let ((env (ensure-gsh-cmd-env!)))
    ;; Set cwd if specified
    (when cwd
      (env-set! env "PWD" cwd)
      (with-catch void (lambda () (current-directory cwd))))
    (dynamic-wind
      (lambda () (set! *gsh-active-process* #t))
      (lambda ()
        (let ((effective-cmd
               (if stdin-text
                 (let ((delim (make-safe-heredoc-delimiter stdin-text)))
                   (string-append "cat <<'" delim "'\n"
                                  stdin-text
                                  "\n" delim "\n | " cmd))
                 cmd)))
          (let-values (((output status) (gsh-capture effective-cmd env)))
            (values (or output "") (or status 0)))))
      (lambda () (set! *gsh-active-process* #f)))))
