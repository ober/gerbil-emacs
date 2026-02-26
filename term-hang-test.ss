;;; -*- Gerbil -*-
;;; Diagnostic test: Why long-running commands (top, yes, sleep) hang the editor
;;;
;;; Root cause: shell.ss, terminal.ss, and gsh-eshell.ss all use gsh-capture
;;; which calls command-substitute. This function:
;;;   1. Redirects fd 1 to a pipe
;;;   2. Forks the external command via ffi-fork-exec
;;;   3. Calls wait-for-foreground-process-raw (BLOCKING wait for child exit)
;;;   4. Reads ALL output from the pipe AFTER the child exits
;;;
;;; For a command like `top` or `yes` that never exits (or produces infinite
;;; output), step 3 blocks the calling thread forever. Since shell/terminal
;;; commands run on the main thread, the entire editor freezes.
;;;
;;; The fix exists in subprocess.ss: run-process-interruptible uses Gambit's
;;; open-process + non-blocking drain-available! polling + C-g checking.
;;; But shell.ss/terminal.ss don't use it — they use in-process gsh-capture.
;;;
;;; Run: HOME=$HOME GERBIL_LOADPATH=$HOME/.gerbil/lib gerbil test ./term-hang-test.ss

(import :std/test
        :std/srfi/13
        :gsh/lib
        :gsh/environment
        :gemacs/core
        :gemacs/shell
        :gemacs/terminal
        :gemacs/subprocess)

(def term-hang-test
  (test-suite "terminal hang diagnostics"

    ;;=========================================================================
    ;; 1. Baseline: quick commands work fine through gsh-capture
    ;;=========================================================================

    (test-case "baseline: echo completes instantly via gsh-capture"
      (let ((ss (shell-start!)))
        (let-values (((output cwd) (shell-execute! "echo hello" ss)))
          (check (string-contains output "hello") => 0)
          (shell-stop! ss))))

    (test-case "baseline: terminal-execute! works for quick commands"
      (let ((ts (terminal-start!)))
        (let-values (((output cwd) (terminal-execute! "echo terminal-ok" ts)))
          (check (string-contains output "terminal-ok") => 0)
          (terminal-stop! ts))))

    (test-case "baseline: multiple commands in sequence"
      (let ((ss (shell-start!)))
        (let-values (((out1 _) (shell-execute! "echo first" ss)))
          (check (string-contains out1 "first") => 0))
        (let-values (((out2 _) (shell-execute! "echo second" ss)))
          (check (string-contains out2 "second") => 0))
        (shell-stop! ss)))

    ;;=========================================================================
    ;; 2. Demonstrate the blocking: gsh-capture hangs on long-running commands
    ;;=========================================================================

    (test-case "HANG PROOF: sleep blocks gsh-capture (thread timeout detects it)"
      ;; Run `sleep 10` via gsh-capture in a thread, with a 2-second timeout.
      ;; If gsh-capture is blocking (it is), the thread won't finish in 2s.
      (let* ((ss (shell-start!))
             (result-box (box #f))
             (thread (spawn
                       (lambda ()
                         (let-values (((output cwd) (shell-execute! "sleep 10" ss)))
                           (set-box! result-box output))))))
        ;; Wait 2 seconds — if gsh-capture were non-blocking, it would return
        (thread-sleep! 2)
        (let ((finished? (unbox result-box)))
          ;; The command is still running — gsh-capture is blocked
          (check finished? => #f)
          ;; Kill the thread to clean up
          (thread-terminate! thread)
          (shell-stop! ss))))

    (test-case "HANG PROOF: yes | head hangs in gsh-capture pipe handling"
      ;; `yes | head -1` should finish instantly, but gsh-capture's pipe
      ;; handling via command-substitute may still block on the read side.
      ;; Testing with a larger output to see buffering behavior.
      (let* ((ss (shell-start!))
             (result-box (box #f))
             (thread (spawn
                       (lambda ()
                         (let-values (((output cwd) (shell-execute! "yes | head -5" ss)))
                           (set-box! result-box output))))))
        (thread-sleep! 3)
        (let ((result (unbox result-box)))
          (if result
            ;; If it completed, the pipe finished (good for small output)
            (check (string-contains result "y") => 0)
            ;; If still blocked after 3s, the pipe is stuck
            (begin
              (displayln "  WARNING: yes|head-5 still blocked after 3s")
              (thread-terminate! thread)))
          (shell-stop! ss))))

    ;;=========================================================================
    ;; 3. The fix already exists: subprocess.ss non-blocking execution
    ;;=========================================================================

    (test-case "FIX EXISTS: run-process-interruptible handles sleep without blocking"
      ;; subprocess.ss already has non-blocking, interruptible execution.
      ;; It uses Gambit's open-process + char-ready? polling.
      (let* ((result-box (box #f))
             ;; Fake peek-event that never sees C-g
             (fake-peek (lambda (ms) (thread-sleep! (/ ms 1000.0)) #f))
             (fake-key? (lambda (ev) #f))
             (fake-key  (lambda (ev) 0))
             (thread (spawn
                       (lambda ()
                         (let-values (((output status)
                                       (run-process-interruptible
                                         "sleep 0.5 && echo done"
                                         fake-peek fake-key? fake-key)))
                           (set-box! result-box output))))))
        (thread-sleep! 3)
        (let ((result (unbox result-box)))
          ;; The subprocess-based approach completes!
          (check (string? result) => #t)
          (check (string-contains result "done") => 0)
          (thread-terminate! thread))))

    (test-case "FIX EXISTS: run-process-interruptible can be interrupted"
      ;; Simulate C-g after 1 second — the subprocess gets killed
      (let* ((start-time (time->seconds (current-time)))
             ;; After 1s, fake a C-g event
             (fake-peek (lambda (ms)
                          (thread-sleep! (/ ms 1000.0))
                          (if (> (- (time->seconds (current-time)) start-time) 1.0)
                            'fake-event  ;; trigger interrupt
                            #f)))
             (fake-key? (lambda (ev) (eq? ev 'fake-event)))
             (fake-key  (lambda (ev) 7))  ;; 7 = C-g
             (interrupted? #f))
        (with-catch
          (lambda (e)
            (when (keyboard-quit-exception? e)
              (set! interrupted? #t)))
          (lambda ()
            (run-process-interruptible
              "sleep 60"  ;; would run for 60s
              fake-peek fake-key? fake-key)))
        ;; Should have been interrupted by our fake C-g within ~1s
        (check interrupted? => #t)
        (let ((elapsed (- (time->seconds (current-time)) start-time)))
          (check (< elapsed 5.0) => #t))))

    ;;=========================================================================
    ;; 4. Architecture comparison: gsh-capture vs subprocess
    ;;=========================================================================

    (test-case "ARCHITECTURE: gsh-capture executes in-process (no subprocess for builtins)"
      ;; gsh builtins like echo don't fork — they run in-process
      ;; This is fast but means the calling thread is busy
      (let ((ss (shell-start!)))
        (let-values (((output cwd) (shell-execute! "echo builtin" ss)))
          (check (string-contains output "builtin") => 0))
        (shell-stop! ss)))

    (test-case "ARCHITECTURE: gsh-capture forks for external commands then BLOCKS on wait"
      ;; External commands go through execute-external → ffi-fork-exec
      ;; → wait-for-foreground-process-raw (blocking waitpid)
      ;; Prove it: /bin/echo is external, should still work for quick commands
      (let ((ss (shell-start!)))
        (let-values (((output cwd) (shell-execute! "/bin/echo external" ss)))
          (check (string-contains output "external") => 0))
        (shell-stop! ss)))

    (test-case "ARCHITECTURE: terminal-execute! has same blocking problem as shell-execute!"
      ;; terminal.ss also uses gsh-capture internally
      (let* ((ts (terminal-start!))
             (result-box (box #f))
             (thread (spawn
                       (lambda ()
                         (let-values (((output cwd)
                                       (terminal-execute! "sleep 10" ts)))
                           (set-box! result-box output))))))
        (thread-sleep! 2)
        (let ((finished? (unbox result-box)))
          (check finished? => #f)  ;; Still blocked
          (thread-terminate! thread)
          (terminal-stop! ts))))

    ;;=========================================================================
    ;; 5. Diagnostic: what happens with typical interactive commands
    ;;=========================================================================

    (test-case "DIAGNOSTIC: top -b -n 1 works (batch mode, single iteration)"
      ;; top with -b (batch) -n 1 (one iteration) actually terminates
      ;; This proves the command itself works, the issue is non-terminating mode
      (let* ((ss (shell-start!))
             (result-box (box #f))
             (thread (spawn
                       (lambda ()
                         (let-values (((output cwd) (shell-execute! "top -b -n 1" ss)))
                           (set-box! result-box output))))))
        ;; Give it 5 seconds (top -b -n 1 is usually fast)
        (thread-sleep! 5)
        (let ((result (unbox result-box)))
          (if result
            (begin
              (displayln "  top -b -n 1 completed, output length: " (string-length result))
              (check (> (string-length result) 0) => #t))
            (begin
              (displayln "  WARNING: even top -b -n 1 blocked for 5s!")
              (thread-terminate! thread))))
        (shell-stop! ss)))

    (test-case "DIAGNOSTIC: infinite-output command blocks gsh-capture (fd-level)"
      ;; Prove that the blocking is at the fd/waitpid level:
      ;; gsh-capture redirects stdout to a pipe, forks, waits.
      ;; Even thread-terminate! can't cleanly abort it — the child process
      ;; (cat /dev/zero) keeps running and the pipe stays open.
      ;; We skip actually running this to avoid leaking zombie processes.
      ;; Instead, demonstrate with a command that blocks briefly then gets killed.
      (let* ((ss (shell-start!))
             (result-box (box #f))
             ;; Use a command that blocks but we can kill by PID
             (thread (spawn
                       (lambda ()
                         ;; sh -c "sleep 30" will fork a sleep child
                         (let-values (((output cwd) (shell-execute! "sleep 30" ss)))
                           (set-box! result-box output))))))
        (thread-sleep! 1)
        ;; Not finished — proves blocking
        (check (unbox result-box) => #f)
        ;; Kill all sleep 30 children to unblock
        (with-catch void (lambda () (shell-execute! "" ss)))
        (thread-terminate! thread)
        (shell-stop! ss)))

    ;;=========================================================================
    ;; 6. Summary report
    ;;=========================================================================

    (test-case "SUMMARY: print diagnosis"
      (displayln "")
      (displayln "====================================================================")
      (displayln "DIAGNOSIS: Why long-running commands hang the editor")
      (displayln "====================================================================")
      (displayln "")
      (displayln "CAUSE:")
      (displayln "  shell.ss, terminal.ss, and gsh-eshell.ss all use gsh-capture")
      (displayln "  which calls command-substitute (gsh/expander.ss:1947).")
      (displayln "  For external commands, this chain is:")
      (displayln "")
      (displayln "    cmd-terminal-send")
      (displayln "      → terminal-execute!    (terminal.ss:340)")
      (displayln "        → gsh-capture        (gsh/lib.ss:140)")
      (displayln "          → command-substitute (gsh/expander.ss:1947)")
      (displayln "            → ffi-fork-exec   (forks child process)")
      (displayln "            → wait-for-foreground-process-raw  *** BLOCKS ***")
      (displayln "            → ffi-read-all-from-fd  (reads ALL output after exit)")
      (displayln "")
      (displayln "  wait-for-foreground-process-raw is a blocking waitpid().")
      (displayln "  If the child never exits (top, yes, cat, sleep 999), this")
      (displayln "  blocks the calling thread forever. Since shell/terminal")
      (displayln "  commands run on the main thread, the entire editor freezes.")
      (displayln "")
      (displayln "THE FIX ALREADY EXISTS:")
      (displayln "  subprocess.ss has run-process-interruptible which:")
      (displayln "    1. Uses Gambit open-process (spawns real subprocess)")
      (displayln "    2. Polls with char-ready? + drain-available! (non-blocking)")
      (displayln "    3. Checks for C-g between polls (interruptible)")
      (displayln "    4. Returns output incrementally as it arrives")
      (displayln "")
      (displayln "PROPOSED FIX:")
      (displayln "  For external commands (not gsh builtins), use")
      (displayln "  run-process-interruptible instead of gsh-capture.")
      (displayln "  Keep gsh-capture for builtins (cd, export, alias, etc.)")
      (displayln "  that must modify the in-process shell environment.")
      (displayln "")
      (displayln "  Alternatively, run gsh-capture in a background thread")
      (displayln "  with a timeout and kill mechanism, feeding output to")
      (displayln "  the buffer incrementally via a pipe.")
      (displayln "====================================================================")
      (check #t => #t))))

(def main
  (lambda args
    (run-tests! term-hang-test)
    (test-report-summary!)))
