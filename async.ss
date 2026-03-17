;;; -*- Gerbil -*-
;;; Async infrastructure for gemacs SMP
;;;
;;; Provides a unified UI action queue, async process runners,
;;; async file I/O, and a periodic task scheduler.
;;; Background threads push thunks via ui-queue-push!; a master timer
;;; drains them on the UI thread.
;;;
;;; SMP Thread Pinning:
;;; Gambit SMP can migrate green threads between OS-level Virtual Processors
;;; via work-stealing.  For Qt apps, the UI thread must stay on processor 0
;;; (the main OS thread) so Qt widget operations happen on the correct pthread.
;;; Use pin-thread-to-processor0! to pin critical threads, and
;;; spawn/name/pinned to spawn a green thread pre-pinned to processor 0.

(export
  ;; UI action queue
  ui-queue-push!
  ui-queue-drain!

  ;; SMP thread pinning
  pin-thread-to-processor0!
  spawn/name/pinned

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
  current-time-ms

  ;; Background services
  *file-index*
  start-file-indexer!
  stop-file-indexer!
  file-index-lookup
  *git-status-cache*
  start-git-watcher!
  stop-git-watcher!
  *flycheck-trigger*
  flycheck-trigger!
  start-flycheck-watcher!
  stop-flycheck-watcher!)

(import :std/misc/channel
        :std/misc/atom
        :std/sugar
        :std/srfi/13
        :gemacs/core)

;;;============================================================================
;;; SMP Thread Pinning
;;;============================================================================

;; Dynamically resolve ##thread-pin! to avoid a hard reference in the compiled
;; code object that would segfault at module init time on older Gambit builds
;; (e.g. the Docker deps image uses v4.9.7-6 which predates ##thread-pin!).
;; With a dynamic lookup the reference is only resolved at call time, so the
;; module loads cleanly and pinning is simply a no-op on old runtimes.
(def *gambit-thread-pin!*
  (let ((v (##global-var-ref (##make-global-var '##thread-pin!))))
    (if (##unbound? v) #f v)))

(def (pin-thread-to-processor0! thread)
  "Pin a green thread to processor 0 (the main OS thread).
   Prevents Gambit's SMP work-stealing scheduler from migrating this thread
   to a different OS thread.  Essential for threads that make Qt calls or
   drain the Qt callback queue.
   Uses ##thread-pin! (internal Gambit API, lib/_thread.scm:1460).
   No-op on Gambit builds that predate ##thread-pin! (e.g. Docker deps image)."
  (when *gambit-thread-pin!*
    (*gambit-thread-pin!* thread (##processor 0))))

(def (spawn/name/pinned name thunk)
  "Spawn a named green thread pinned to processor 0.
   The thread is pinned before starting so it never runs on any other processor.
   Use for threads that must stay on the main OS thread (Qt UI operations)."
  (let ((t (make-thread thunk name)))
    (pin-thread-to-processor0! t)
    (thread-start! t)
    t))

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

;;;============================================================================
;;; Background Services
;;;============================================================================

;;; 8.1 File Indexer — builds file index for fast find-file completion

(def *file-index* (atom (make-hash-table)))
(def *file-indexer-thread* #f)

(def (build-file-index root-dir)
  "Walk directory tree and build a hash of basename -> full-path list."
  (let ((index (make-hash-table)))
    (with-catch
      (lambda (e) index)
      (lambda ()
        (let walk ((dir root-dir))
          (for-each
            (lambda (entry)
              (let ((path (path-expand entry dir)))
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (let ((info (file-info path)))
                      (if (eq? 'directory (file-info-type info))
                        ;; Skip hidden directories
                        (unless (string-prefix? "." entry)
                          (walk path))
                        ;; Index the file
                        (let* ((name (path-strip-directory path))
                               (existing (or (hash-get index name) [])))
                          (hash-put! index name (cons path existing)))))))))
            (directory-files dir)))
        index))))

(def (start-file-indexer! root-dir)
  "Start background file indexer that re-indexes every 30 seconds."
  (stop-file-indexer!)
  (set! *file-indexer-thread*
    (spawn/name 'file-indexer
      (lambda ()
        (let loop ()
          (let ((index (build-file-index root-dir)))
            (atom-reset! *file-index* index))
          (thread-sleep! 30)
          (loop))))))

(def (stop-file-indexer!)
  "Stop the file indexer background thread."
  (when *file-indexer-thread*
    (with-catch (lambda (e) #f)
      (lambda () (thread-interrupt! *file-indexer-thread*
                   (lambda () (raise 'stop)))))
    (set! *file-indexer-thread* #f)))

(def (file-index-lookup name)
  "Look up a filename in the index. Returns list of full paths."
  (or (hash-get (atom-deref *file-index*) name) []))

;;; 8.2 Git Status Watcher — polls git status for modeline

(def *git-status-cache* (atom (make-hash-table)))
(def *git-watcher-thread* #f)

(def (parse-git-status-line line)
  "Parse one line of git status --porcelain output into (status . file)."
  (when (>= (string-length line) 4)
    (let ((status (substring line 0 2))
          (file (substring line 3 (string-length line))))
      (cons (string-trim-both status) file))))

(def (start-git-watcher! dir (on-update #f))
  "Poll git status in background every 5 seconds.
   Optional on-update callback is called on UI thread with the status hash."
  (stop-git-watcher!)
  (set! *git-watcher-thread*
    (spawn/name 'git-watcher
      (lambda ()
        (let loop ()
          (with-catch
            (lambda (e) #f)
            (lambda ()
              (let* ((proc (open-process
                             [path: "/usr/bin/git"
                              arguments: ["status" "--porcelain" "-b"]
                              directory: dir
                              stdout-redirection: #t
                              stderr-redirection: #t]))
                     (lines (let rd ((acc []))
                              (let ((line (read-line proc)))
                                (if (eof-object? line)
                                  (reverse acc)
                                  (rd (cons line acc)))))))
                (close-port proc)
                (let ((status (make-hash-table))
                      (modified 0) (staged 0) (untracked 0))
                  (for-each
                    (lambda (line)
                      (when (>= (string-length line) 3)
                        (let ((xy (substring line 0 2)))
                          (cond
                            ((string-prefix? "##" xy)
                             (hash-put! status 'branch
                               (substring line 3 (string-length line))))
                            ((string-contains xy "?")
                             (set! untracked (+ untracked 1)))
                            ((or (string-contains xy "M")
                                 (string-contains xy "D"))
                             (set! modified (+ modified 1)))
                            ((or (string-contains xy "A")
                                 (string-contains xy "R"))
                             (set! staged (+ staged 1)))))))
                    lines)
                  (hash-put! status 'modified modified)
                  (hash-put! status 'staged staged)
                  (hash-put! status 'untracked untracked)
                  (atom-reset! *git-status-cache* status)
                  (when on-update
                    (ui-queue-push! (lambda () (on-update status))))))))
          (thread-sleep! 5)
          (loop))))))

(def (stop-git-watcher!)
  "Stop the git status watcher."
  (when *git-watcher-thread*
    (with-catch (lambda (e) #f)
      (lambda () (thread-interrupt! *git-watcher-thread*
                   (lambda () (raise 'stop)))))
    (set! *git-watcher-thread* #f)))

;;; 8.3 Flycheck Watcher — runs linter on save via channel trigger

(def *flycheck-trigger* (make-channel 64))
(def *flycheck-watcher-thread* #f)

(def (flycheck-trigger! path)
  "Trigger a flycheck run for the given file path."
  (channel-try-put *flycheck-trigger* path))

(def (start-flycheck-watcher! lint-fn on-result)
  "Start flycheck watcher. lint-fn: (path) -> error-list.
   on-result: (path errors) called on UI thread."
  (stop-flycheck-watcher!)
  (set! *flycheck-watcher-thread*
    (spawn/name 'flycheck-watcher
      (lambda ()
        (let loop ()
          (let ((path (channel-get *flycheck-trigger*)))
            (when (string? path)
              (with-catch
                (lambda (e)
                  (gemacs-log! "flycheck error: " (##object->string e)))
                (lambda ()
                  (let ((errors (lint-fn path)))
                    (ui-queue-push!
                      (lambda () (on-result path errors))))))))
          (loop))))))

(def (stop-flycheck-watcher!)
  "Stop the flycheck watcher."
  (when *flycheck-watcher-thread*
    (with-catch (lambda (e) #f)
      (lambda () (thread-interrupt! *flycheck-watcher-thread*
                   (lambda () (raise 'stop)))))
    (set! *flycheck-watcher-thread* #f)))
