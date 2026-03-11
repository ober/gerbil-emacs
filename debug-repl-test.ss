;;; -*- Gerbil -*-
;;; Tests for the TCP debug REPL server.

(import :std/test
        :std/sugar
        :std/misc/threads
        :gemacs/debug-repl)

(def (connect-repl port-num)
  "Open a TCP client to the debug REPL on the given port."
  (open-tcp-client
    (list server-address: "127.0.0.1"
          port-number: port-num)))

(def (read-until-prompt client)
  "Read lines until 'gemacs-dbg> ' prompt. Returns lines before the prompt."
  (let loop ((lines []))
    (let ((line (with-exception-catcher
                  (lambda (e) #f)
                  (lambda () (read-line client)))))
      (cond
        ((not line) (reverse lines))
        ((eof-object? line) (reverse lines))
        ((string=? line "gemacs-dbg> ") (reverse lines))
        (else (loop (cons line lines)))))))

(def (send-command client cmd)
  "Send a command and return the response lines."
  (display cmd client)
  (newline client)
  (force-output client)
  (read-until-prompt client))

(def (skip-banner client)
  "Skip the banner and first prompt."
  (read-line client)          ;; banner line
  (read-line client))         ;; first prompt

(def (has-line? substr lines)
  "True if any line in lines contains substr."
  (and (find (lambda (l) (string-contains l substr)) lines) #t))

;;;============================================================================
;;; Test suite
;;;============================================================================

(def debug-repl-test
  (test-suite "debug-repl"

    (test-case "server starts and accepts connections"
      (let ((port (start-debug-repl! 0)))
        (try
          (check (integer? port) => #t)
          (check (> port 0) => #t)
          (check (file-exists? (path-expand ".gemacs-repl-port" (getenv "HOME"))) => #t)
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (check #t => #t)   ;; reached here = connected
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "eval works: (+ 1 2)"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c "(+ 1 2)")))
                (check (and (member "3" resp) #t) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "eval works: string result"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c "(string-append \"hello\" \" world\")")))
                (check (and (member "\"hello world\"" resp) #t) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case ",threads command"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c ",threads")))
                (check (> (length resp) 0) => #t)
                (check (has-line? "debug-repl" resp) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case ",buffers command"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c ",buffers")))
                (check (list? resp) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case ",help command"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c ",help")))
                (check (has-line? ",threads" resp) => #t)
                (check (has-line? ",quit" resp) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "error handling: invalid expression"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c "this-does-not-exist-xyz")))
                (check (has-line? "ERROR" resp) => #t))
              ;; Session should still work after error
              (let ((resp2 (send-command c "(+ 1 1)")))
                (check (and (member "2" resp2) #t) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "token auth: rejected without token"
      (let ((port (start-debug-repl! 0 token: "secret123")))
        (try
          (let ((c (connect-repl port)))
            (try
              (display "wrongtoken\n" c)
              (force-output c)
              ;; Should get "Access denied" or EOF
              (let ((line (with-exception-catcher
                            (lambda (e) "eof")
                            (lambda () (read-line c)))))
                (check (or (and (string? line) (string-contains line "denied") #t)
                           (eof-object? line)
                           (string=? line "eof"))
                       => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "token auth: accepted with correct token"
      (let ((port (start-debug-repl! 0 token: "secret123")))
        (try
          (let ((c (connect-repl port)))
            (try
              (display "secret123\n" c)
              (force-output c)
              (let ((banner (read-line c)))
                (check (and (string? banner) (string-contains banner "debug REPL") #t) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "multiple clients simultaneously"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c1 (connect-repl port))
                (c2 (connect-repl port)))
            (try
              (skip-banner c1)
              (skip-banner c2)
              (let ((r1 (send-command c1 "(+ 10 20)"))
                    (r2 (send-command c2 "(+ 30 40)")))
                (check (and (member "30" r1) #t) => #t)
                (check (and (member "70" r2) #t) => #t))
              (finally
                (close-port c1)
                (close-port c2))))
          (finally (stop-debug-repl!)))))

    (test-case "clean shutdown: stop removes port file"
      (let* ((port (start-debug-repl! 0))
             (pfile (path-expand ".gemacs-repl-port" (getenv "HOME"))))
        (check (file-exists? pfile) => #t)
        (stop-debug-repl!)
        (check (file-exists? pfile) => #f)))

    (test-case ",quit closes connection"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (send-command c ",quit")
              ;; After ,quit connection should close — next read is EOF
              (let ((line (with-exception-catcher
                            (lambda (e) 'eof)
                            (lambda () (read-line c)))))
                (check (or (eof-object? line) (eq? line 'eof)) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case ",state command"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c ",state")))
                (check (has-line? "buffers" resp) => #t)
                (check (has-line? "threads" resp) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case ",gc command"
      (let ((port (start-debug-repl! 0)))
        (try
          (let ((c (connect-repl port)))
            (try
              (skip-banner c)
              (let ((resp (send-command c ",gc")))
                (check (has-line? "GC done" resp) => #t))
              (finally (close-port c))))
          (finally (stop-debug-repl!)))))

    (test-case "debug-repl-port returns actual port"
      (let ((port (start-debug-repl! 0)))
        (try
          (check (equal? port (debug-repl-port)) => #t)
          (check (integer? (debug-repl-port)) => #t)
          (finally (stop-debug-repl!))))
      (check (debug-repl-port) => #f))

    ))

(def (main . _)
  (run-tests! debug-repl-test))
