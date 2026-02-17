;;; -*- Gerbil -*-
;;; gemacs-client: Open files in a running gemacs session.
;;; Analogous to Emacs's emacsclient.

(export main)

(import :gemacs/ipc)

(include "manifest.ss")

(def (main . args)
  (cond
    ((member "--version" args)
     (displayln "gemacs-client " (cdar version-manifest)))
    ((or (member "--help" args) (member "-h" args))
     (displayln "Usage: gemacs-client [OPTIONS] FILE...")
     (displayln "Open files in a running gemacs session.")
     (displayln)
     (displayln "Options:")
     (displayln "  --version   Show version information")
     (displayln "  --help, -h  Show this help message"))
    ((null? args)
     (displayln "gemacs-client: no files specified")
     (displayln "Usage: gemacs-client FILE...")
     (exit 1))
    (else
     (let ((files (filter (lambda (a) (not (string-prefix? "-" a))) args)))
       (when (null? files)
         (displayln "gemacs-client: no files specified")
         (exit 1))
       (send-files! files)))))

(def (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(def (read-server-file)
  "Read the server file and return (host . port) or #f."
  (if (file-exists? *ipc-server-file*)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((content (call-with-input-file *ipc-server-file* read-line)))
          (let ((colon (let loop ((i (- (string-length content) 1)))
                         (if (< i 0) #f
                           (if (char=? (string-ref content i) #\:)
                             i
                             (loop (- i 1)))))))
            (if colon
              (cons (substring content 0 colon)
                    (string->number (substring content (+ colon 1)
                                               (string-length content))))
              #f)))))
    #f))

(def (send-files! files)
  "Connect to the running server and send file paths."
  (let ((server-info (read-server-file)))
    (unless server-info
      (displayln "gemacs-client: no server running (missing "
                 *ipc-server-file* ")")
      (exit 1))
    (let ((host (car server-info))
          (port-num (cdr server-info)))
      (let ((sock (with-catch
                    (lambda (e)
                      (displayln "gemacs-client: cannot connect to "
                                 host ":" port-num)
                      (exit 1))
                    (lambda ()
                      (open-tcp-client
                        (list server-address: host
                              port-number: port-num))))))
        (unwind-protect
          (for-each
            (lambda (file)
              (let ((abs-path (path-expand file)))
                (display abs-path sock)
                (newline sock)
                (force-output sock)
                (let ((response (read-line sock)))
                  (when (or (eof-object? response)
                            (not (string=? (string-trim-right response) "OK")))
                    (displayln "gemacs-client: unexpected response for "
                               abs-path)))))
            files)
          (close-port sock))))))

(def (string-trim-right s)
  (let loop ((i (string-length s)))
    (if (and (> i 0)
             (let ((c (string-ref s (- i 1))))
               (or (char=? c #\space) (char=? c #\return) (char=? c #\tab))))
      (loop (- i 1))
      (substring s 0 i))))
