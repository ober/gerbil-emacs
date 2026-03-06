;;; -*- Gerbil -*-
;;; Async infrastructure for gemacs SMP
;;;
;;; Provides a unified UI action queue, async process runners,
;;; async file I/O, and a periodic task scheduler.
;;; Background threads push thunks via ui-queue-push!; a master timer
;;; drains them on the UI thread.

(export
  ;; UI action queue
  ui-queue-push!
  ui-queue-drain!

  ;; Async command runner
  async-process!
  async-process-stream!

  ;; Async file I/O
  async-read-file!
  async-write-file!

  ;; Async eval (background thunk → UI callback)
  async-eval!

  ;; Periodic task scheduler
  schedule-periodic!
  master-timer-tick!
  current-time-ms)

(import :std/misc/channel
        :std/sugar
        :gemacs/core)

;;;============================================================================
;;; UI Action Queue
;;;============================================================================

;; Buffered channel for UI actions. Background threads push thunks here;
;; the master timer drains them on the UI thread.
(def *ui-queue* (make-channel 4096))

(def (ui-queue-push! thunk)
  "Push a UI action from any thread. Non-blocking (buffered channel)."
  (channel-try-put *ui-queue* thunk))

(def (ui-queue-drain!)
  "Drain all pending UI actions. Called from the master timer on the UI thread.
   Processes up to 64 actions per tick to avoid starving the event loop."
  (let loop ((n 0))
    (when (< n 64)
      (let ((action (channel-try-get *ui-queue*)))
        (when action
          (with-catch
            (lambda (e) (gemacs-log! "UI queue error: " (##object->string e)))
            action)
          (loop (+ n 1)))))))

;;;============================================================================
;;; Periodic Task Scheduler
;;;============================================================================

;; Each task: (name interval-ms last-run-ms thunk)
(def *scheduled-tasks* [])

(def (current-time-ms)
  "Current wall-clock time in milliseconds."
  (inexact->exact (floor (* (time->seconds (current-time)) 1000))))

(def (schedule-periodic! name interval-ms thunk)
  "Register a periodic task to run at the given interval.
   Tasks are run by master-timer-tick! on the UI thread."
  (set! *scheduled-tasks*
    (cons [name interval-ms 0 thunk] *scheduled-tasks*)))

(def (master-timer-tick!)
  "Master timer callback: drain the UI queue, then run periodic tasks.
   Should be called from a single Qt timer at ~16-50ms interval."
  ;; 1. Drain async UI queue
  (ui-queue-drain!)
  ;; 2. Run periodic tasks whose interval has elapsed
  (let ((now (current-time-ms)))
    (set! *scheduled-tasks*
      (map (lambda (task)
             (let ((name (car task))
                   (interval (cadr task))
                   (last (caddr task))
                   (thunk (cadddr task)))
               (if (>= (- now last) interval)
                 (begin
                   (with-catch
                     (lambda (e)
                       (gemacs-log! "Timer error in " name ": "
                                    (##object->string e)))
                     thunk)
                   [name interval now thunk])
                 task)))
           *scheduled-tasks*))))

;;;============================================================================
;;; Async Process Runner
;;;============================================================================

(def (async-process! cmd
                     callback: callback
                     on-error: (on-error #f)
                     stdin-text: (stdin-text #f))
  "Run shell command in background thread, deliver result string to callback on UI thread."
  (spawn/name 'async-process
    (lambda ()
      (with-catch
        (lambda (e)
          (ui-queue-push!
            (lambda ()
              (if on-error (on-error e)
                (gemacs-log! "async-process error: " (##object->string e))))))
        (lambda ()
          (let ((proc (open-process
                        [path: "/bin/sh"
                         arguments: ["-c" cmd]
                         stdin-redirection: (if stdin-text #t #f)
                         stdout-redirection: #t
                         stderr-redirection: #t])))
            (when stdin-text
              (display stdin-text proc)
              (force-output proc)
              (close-output-port proc))
            ;; Read all output
            (let ((out (read-line proc #f)))
              (close-port proc)
              (let ((result (or out "")))
                (ui-queue-push! (lambda () (callback result)))))))))))

(def (async-process-stream! cmd
                            on-line: on-line
                            on-done: (on-done #f)
                            on-error: (on-error #f))
  "Run shell command in background, deliver each line to on-line callback on UI thread.
   Calls on-done (no args) when the process finishes."
  (spawn/name 'async-process-stream
    (lambda ()
      (with-catch
        (lambda (e)
          (ui-queue-push!
            (lambda ()
              (if on-error (on-error e)
                (gemacs-log! "async-process-stream error: " (##object->string e))))))
        (lambda ()
          (let ((proc (open-process
                        [path: "/bin/sh"
                         arguments: ["-c" cmd]
                         stdout-redirection: #t
                         stderr-redirection: #t])))
            (let loop ()
              (let ((line (read-line proc)))
                (if (eof-object? line)
                  (begin
                    (close-port proc)
                    (when on-done
                      (ui-queue-push! on-done)))
                  (begin
                    (ui-queue-push! (lambda () (on-line line)))
                    (loop)))))))))))

;;;============================================================================
;;; Async File I/O
;;;============================================================================

(def (async-read-file! path callback)
  "Read file in background thread, deliver string (or #f on error) to callback on UI thread."
  (spawn/name 'async-read-file
    (lambda ()
      (let ((content (with-catch (lambda (e) #f)
                       (lambda ()
                         (call-with-input-file path
                           (lambda (port) (read-line port #f)))))))
        (ui-queue-push! (lambda () (callback content)))))))

(def (async-write-file! path content callback)
  "Write string to file in background thread, call callback with #t (success) or #f (error) on UI thread."
  (spawn/name 'async-write-file
    (lambda ()
      (let ((ok (with-catch (lambda (e) #f)
                  (lambda ()
                    (call-with-output-file path
                      (lambda (port) (display content port)))
                    #t))))
        (ui-queue-push! (lambda () (callback ok)))))))

;;;============================================================================
;;; Async Eval
;;;============================================================================

(def (async-eval! thunk callback)
  "Evaluate thunk in background thread, deliver result to callback on UI thread."
  (spawn/name 'async-eval
    (lambda ()
      (let ((result (with-catch
                      (lambda (e) (values 'error e))
                      thunk)))
        (ui-queue-push! (lambda () (callback result)))))))
