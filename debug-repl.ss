;;; -*- Gerbil -*-
;;; TCP REPL server for debugging a running gemacs instance.
;;; Connect with: nc 127.0.0.1 <port>
;;; Stays responsive even when the main thread is hung (green thread isolation).

(export start-debug-repl! stop-debug-repl! debug-repl-port)

(import :std/sugar
        :std/misc/threads
        :std/srfi/13
        :gemacs/core)

;;;============================================================================
;;; State
;;;============================================================================

(def *debug-repl-server* #f)
(def *debug-repl-actual-port* #f)
(def *debug-repl-client-threads* [])
(def *debug-repl-port-file*
  (path-expand ".gemacs-repl-port" (getenv "HOME")))

;;;============================================================================
;;; Port file
;;;============================================================================

(def (write-repl-port-file! port-num)
  (call-with-output-file *debug-repl-port-file*
    (lambda (p)
      (display "PORT=" p) (display port-num p) (newline p)
      (display "PID=" p)  (display (##os-getpid) p) (newline p))))

(def (delete-repl-port-file!)
  (when (file-exists? *debug-repl-port-file*)
    (with-exception-catcher void
      (lambda () (delete-file *debug-repl-port-file*)))))

;;;============================================================================
;;; Comma command helpers
;;;============================================================================

(def (thread-state-string st)
  (cond
    ((thread-state-running? st)  "running")
    ((thread-state-waiting? st)  "waiting")
    ((thread-state-normally-terminated? st)
     (string-append "terminated(" (object->string (thread-state-normally-terminated-result st)) ")"))
    ((thread-state-abnormally-terminated? st)
     (string-append "aborted(" (object->string (thread-state-abnormally-terminated-reason st)) ")"))
    (else (object->string st))))

(def (cmd-list-threads port)
  (for-each
    (lambda (t)
      (let* ((name (thread-name t))
             (st   (thread-state t)))
        (display (string-append
                   "  " (object->string t)
                   "  name=" (object->string (or name "(unnamed)"))
                   "  state=" (thread-state-string st)
                   "\n")
                 port)))
    (all-threads)))

(def (cmd-show-backtrace thread-name-str port)
  (let ((ts (filter (lambda (t)
                      (equal? (object->string (thread-name t)) thread-name-str))
                    (all-threads))))
    (if (null? ts)
      (display (string-append "No thread named: " thread-name-str "\n") port)
      (for-each
        (lambda (t)
          (display (string-append "=== Thread: " (object->string (thread-name t)) " ===\n") port)
          (with-exception-catcher
            (lambda (e) (display "  (backtrace unavailable)\n" port))
            (lambda ()
              (##display-continuation-backtrace
                (##thread-state t) port #t #t 50 0))))
        ts))))

(def (cmd-show-bt-all port)
  (for-each
    (lambda (t)
      (display (string-append "=== Thread: " (object->string (thread-name t)) " ===\n") port)
      (with-exception-catcher
        (lambda (e) (display "  (backtrace unavailable)\n" port))
        (lambda ()
          (##display-continuation-backtrace
            (##thread-state t) port #t #t 30 0))))
    (all-threads)))

(def (cmd-list-buffers port)
  (for-each
    (lambda (buf)
      (let ((name (buffer-name buf))
            (path (buffer-file-path buf))
            (mod  (buffer-modified buf)))
        (display (string-append
                   "  " name
                   (if mod " [modified]" "")
                   (if path (string-append "  " path) "  (no file)")
                   "\n")
                 port)))
    (buffer-list)))

(def (cmd-show-state port)
  (let ((bufs (buffer-list)))
    (display
      (string-append
        "  buffers:   " (number->string (length bufs)) " buffer(s)\n"
        "  kill-ring: (not accessible without app)\n"
        "  threads:   " (number->string (length (all-threads))) " thread(s)\n")
      port)))

(def (cmd-show-vars port)
  (let ((bufs (buffer-list)))
    (display
      (string-append
        "  *buffer-list*: " (object->string bufs) "\n")
      port)))

(def (cmd-force-gc port)
  (##gc)
  (let ((stats (##process-statistics)))
    (display (string-append
               "  GC done.\n"
               "  heap-size:    " (object->string (##f64vector-ref stats 5)) "\n"
               "  alloc:        " (object->string (##f64vector-ref stats 6)) "\n")
             port)))

(def help-text
  "  ,help           This help message
  ,threads        List all threads (name, state)
  ,bt <name>      Show backtrace for a named thread
  ,bt-all         Show backtraces for ALL threads
  ,buffers        List all open buffers
  ,state          Show key state summary
  ,vars           Show global variables
  ,gc             Force GC and show heap stats
  ,eval-in <thread-name> <expr>  Inject eval via thread-async!
  ,quit           Close this REPL connection
  ,shutdown       Gracefully quit gemacs (emergency)
  <expr>          Evaluate arbitrary Gerbil expression
")

;;;============================================================================
;;; Client handler
;;;============================================================================

(def (debug-repl-handle-client! client token)
  ;; Token auth: if a token is required, read first line and verify
  (when token
    (let ((line (with-exception-catcher (lambda (e) #f) (lambda () (read-line client)))))
      (unless (and (string? line) (string=? (string-trim-both line) token))
        (display "Access denied.\n" client)
        (force-output client)
        (close-port client)
        (##thread-terminate! (current-thread)))))
  ;; Banner
  (display "gemacs debug REPL — type ,help for commands\n" client)
  (force-output client)
  (let loop ()
    (display "gemacs-dbg> " client)
    (force-output client)
    (let ((line (with-exception-catcher (lambda (e) #f) (lambda () (read-line client)))))
      (when (and line (not (eof-object? line)))
        (let ((cmd (string-trim-both line)))
          (cond
            ((string=? cmd "") (loop))
            ((string=? cmd ",quit")
             (display "Connection closed.\n" client)
             (force-output client))
            ((string=? cmd ",help")
             (display help-text client)
             (force-output client)
             (loop))
            ((string=? cmd ",threads")
             (cmd-list-threads client)
             (force-output client)
             (loop))
            ((string=? cmd ",bt-all")
             (cmd-show-bt-all client)
             (force-output client)
             (loop))
            ((string-prefix? ",bt " cmd)
             (let ((tname (string-trim (substring cmd 4 (string-length cmd)))))
               (cmd-show-backtrace tname client))
             (force-output client)
             (loop))
            ((string=? cmd ",buffers")
             (cmd-list-buffers client)
             (force-output client)
             (loop))
            ((string=? cmd ",state")
             (cmd-show-state client)
             (force-output client)
             (loop))
            ((string=? cmd ",vars")
             (cmd-show-vars client)
             (force-output client)
             (loop))
            ((string=? cmd ",gc")
             (cmd-force-gc client)
             (force-output client)
             (loop))
            ((string-prefix? ",eval-in " cmd)
             ;; Format: ,eval-in <thread-name> <expr>
             ;; Find first space after thread name
             (let* ((rest (substring cmd 9 (string-length cmd)))
                    (sp   (string-index rest #\space)))
               (if (not sp)
                 (display "Usage: ,eval-in <thread-name> <expr>\n" client)
                 (let* ((tname (substring rest 0 sp))
                        (expr-str (string-trim (substring rest sp (string-length rest))))
                        (ts (filter (lambda (t)
                                      (equal? (object->string (thread-name t)) tname))
                                    (all-threads))))
                   (if (null? ts)
                     (display (string-append "No thread: " tname "\n") client)
                     (with-exception-catcher
                       (lambda (e)
                         (display "Error scheduling eval: " client)
                         (display-exception e client)
                         (newline client))
                       (lambda ()
                         (thread-async! (car ts)
                           (lambda ()
                             (with-exception-catcher
                               (lambda (e) (void))
                               (lambda ()
                                 (eval (read (open-input-string expr-str)))))))
                         (display "Eval injected into thread " client)
                         (display tname client)
                         (newline client)))))))
             (force-output client)
             (loop))
            ((string=? cmd ",shutdown")
             (display "Initiating graceful shutdown...\n" client)
             (force-output client)
             (with-exception-catcher void
               (lambda () (##thread-terminate! ##primordial-thread))))
            (else
             ;; Evaluate as Gerbil expression
             (with-exception-catcher
               (lambda (e)
                 (display "ERROR: " client)
                 (display-exception e client))
               (lambda ()
                 (let ((result (eval (read (open-input-string cmd)))))
                   (write result client)
                   (newline client))))
             (force-output client)
             (loop))))))))

;;;============================================================================
;;; Accept loop
;;;============================================================================

(def *client-counter* 0)

(def (debug-repl-accept-loop srv token)
  (let loop ()
    (let ((client (with-exception-catcher
                    (lambda (e) #f)
                    (lambda () (read srv)))))
      (when (and client (not (eof-object? client)))
        (set! *client-counter* (+ *client-counter* 1))
        (let ((n *client-counter*))
          (let ((t (spawn/name (string->symbol (string-append "debug-repl-client-" (number->string n)))
                     (lambda ()
                       (with-exception-catcher
                         (lambda (e) (void))
                         (lambda ()
                           (debug-repl-handle-client! client token)
                           (with-exception-catcher void (lambda () (close-port client)))))))))
            (set! *debug-repl-client-threads* (cons t *debug-repl-client-threads*))))
        (loop)))))

;;;============================================================================
;;; Public API
;;;============================================================================

(def (start-debug-repl! port-num token: (token #f))
  "Start the TCP debug REPL on 127.0.0.1:port-num.
   If token: is provided, clients must send it as the first line.
   Use port 0 for OS-assigned ephemeral port.
   Writes ~/.gemacs-repl-port for discovery."
  (let ((srv (open-tcp-server
               (list server-address: "127.0.0.1"
                     port-number: port-num
                     reuse-address: #t
                     backlog: 8))))
    (set! *debug-repl-server* srv)
    (set! *debug-repl-client-threads* [])
    (let ((actual-port (socket-info-port-number (tcp-server-socket-info srv))))
      (set! *debug-repl-actual-port* actual-port)
      (write-repl-port-file! actual-port)
      (spawn/name 'debug-repl-accept
        (lambda ()
          (debug-repl-accept-loop srv token)))
      actual-port)))

(def (stop-debug-repl!)
  "Stop the debug REPL server and clean up."
  (when *debug-repl-server*
    (with-exception-catcher void
      (lambda () (close-port *debug-repl-server*)))
    (set! *debug-repl-server* #f)
    (set! *debug-repl-actual-port* #f))
  ;; Terminate all client threads
  (for-each
    (lambda (t)
      (with-exception-catcher void
        (lambda () (##thread-terminate! t))))
    *debug-repl-client-threads*)
  (set! *debug-repl-client-threads* [])
  (delete-repl-port-file!))

(def (debug-repl-port)
  "Return the actual port number the debug REPL is listening on, or #f."
  *debug-repl-actual-port*)
