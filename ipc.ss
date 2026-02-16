;;; -*- Gerbil -*-
;;; IPC server for emacsclient-like remote file opening.
;;; Uses Gambit's built-in TCP server with a mutex-protected queue
;;; to bridge server threads to the UI thread.

(export start-ipc-server! ipc-poll-files! stop-ipc-server! *ipc-server-file*)

(import :std/sugar)

;;;============================================================================
;;; State
;;;============================================================================

(def *ipc-server-file*
  (path-expand ".gerbil-emacs-server" (getenv "HOME")))

;; Mutex-protected queue of file paths received from clients
(def *ipc-queue* [])
(def *ipc-mutex* (make-mutex 'ipc-queue))

;; The server port (for shutdown)
(def *ipc-server-port* #f)

;;;============================================================================
;;; Queue operations (thread-safe)
;;;============================================================================

(def (ipc-queue-push! path)
  "Push a file path onto the IPC queue (called from server threads)."
  (mutex-lock! *ipc-mutex*)
  (unwind-protect
    (set! *ipc-queue* (append *ipc-queue* [path]))
    (mutex-unlock! *ipc-mutex*)))

(def (ipc-poll-files!)
  "Drain the IPC queue and return a list of file paths.
   Called from the UI thread."
  (mutex-lock! *ipc-mutex*)
  (unwind-protect
    (let ((files *ipc-queue*))
      (set! *ipc-queue* [])
      files)
    (mutex-unlock! *ipc-mutex*)))

;;;============================================================================
;;; Server
;;;============================================================================

(def (ipc-handle-client! client)
  "Handle one client connection: read newline-terminated file paths,
   push them to the queue, respond with OK for each."
  (with-catch
    (lambda (e) (void))  ;; ignore errors from disconnected clients
    (lambda ()
      (let loop ()
        (let ((line (read-line client)))
          (unless (eof-object? line)
            (let ((path (string-trim line)))
              (when (> (string-length path) 0)
                (ipc-queue-push! path)
                (display "OK\n" client)
                (force-output client)))
            (loop))))
      (close-port client))))

(def (string-trim s)
  "Remove leading/trailing whitespace and carriage returns."
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len)
                           (let ((c (string-ref s i)))
                             (or (char=? c #\space)
                                 (char=? c #\return)
                                 (char=? c #\tab))))
                    (loop (+ i 1))
                    i)))
         (end (let loop ((i len))
                (if (and (> i start)
                         (let ((c (string-ref s (- i 1))))
                           (or (char=? c #\space)
                               (char=? c #\return)
                               (char=? c #\tab))))
                  (loop (- i 1))
                  i))))
    (substring s start end)))

(def (start-ipc-server!)
  "Start the IPC server on 127.0.0.1 with an OS-assigned port.
   Writes the host:port to *ipc-server-file*."
  (let* ((port-num (let ((env (getenv "GERBIL_EMACS_PORT" #f)))
                     (if env (string->number env) 0)))
         (srv (open-tcp-server
                (list server-address: "127.0.0.1"
                      port-number: port-num
                      reuse-address: #t
                      backlog: 8)))
         (actual-port (socket-info-port-number
                        (tcp-server-socket-info srv))))
    (set! *ipc-server-port* srv)
    ;; Write server file
    (call-with-output-file *ipc-server-file*
      (lambda (p)
        (display "127.0.0.1:" p)
        (display actual-port p)
        (newline p)))
    ;; Accept loop in background thread
    (thread-start!
      (make-thread
        (lambda ()
          (let loop ()
            (let ((client (with-catch
                            (lambda (e) #f)
                            (lambda () (read srv)))))
              (when (and client (not (eof-object? client)))
                (thread-start!
                  (make-thread
                    (lambda () (ipc-handle-client! client))))
                (loop)))))
        'ipc-accept-loop))
    (void)))

(def (stop-ipc-server!)
  "Stop the IPC server and remove the server file."
  (when *ipc-server-port*
    (with-catch void (lambda () (close-port *ipc-server-port*)))
    (set! *ipc-server-port* #f))
  (when (file-exists? *ipc-server-file*)
    (delete-file *ipc-server-file*)))
