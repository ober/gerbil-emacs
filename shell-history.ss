;;; -*- Gerbil -*-
;;; Shell command history with timestamps for gemacs
;;;
;;; Persistent history stored in ~/.gsh_history with format:
;;;   UNIX_EPOCH<TAB>CWD<TAB>COMMAND
;;;
;;; Optimized for fast append, recency search, and fzf-style parsing.

(export
  *gsh-history*
  *gsh-history-file*
  *gsh-history-max*
  gsh-history-add!
  gsh-history-save!
  gsh-history-load!
  gsh-history-search
  gsh-history-recent
  gsh-history-for-cwd
  gsh-history-all)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core)

;;;============================================================================
;;; Configuration
;;;============================================================================

(def *gsh-history-file* ".gsh_history")
(def *gsh-history-max* 10000)

;;;============================================================================
;;; In-memory history (simple list, newest first)
;;;============================================================================

;; List of (timestamp cwd command) tuples, newest first
(def *gsh-history* [])

;; Append-mode port kept open for incremental writes
(def *gsh-history-port* #f)

(def (gsh-history-path)
  (let ((home (user-info-home (user-info (user-name)))))
    (path-expand *gsh-history-file* home)))

;;;============================================================================
;;; Core operations
;;;============================================================================

(def (gsh-history-add! command (cwd #f))
  "Record a shell command with timestamp and working directory.
   Appends to disk immediately and prepends to in-memory list."
  (let ((trimmed (string-trim-both command)))
    (when (and (string? trimmed) (> (string-length trimmed) 0))
      (let* ((ts (inexact->exact (floor (time->seconds (current-time)))))
             (dir (or cwd (current-directory)))
             (entry (list ts dir trimmed)))
        ;; Prepend to in-memory list
        (set! *gsh-history* (cons entry *gsh-history*))
        ;; Trim if over max
        (when (> (length *gsh-history*) *gsh-history-max*)
          (set! *gsh-history*
            (let loop ((lst *gsh-history*) (n 0) (acc []))
              (if (or (null? lst) (>= n *gsh-history-max*))
                (reverse acc)
                (loop (cdr lst) (+ n 1) (cons (car lst) acc))))))
        ;; Append to disk
        (gsh-history-append-line! ts dir trimmed)))))

(def (gsh-history-append-line! ts cwd command)
  "Append a single history line to the file."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((port (or *gsh-history-port*
                      (let ((p (open-output-file
                                 [path: (gsh-history-path) append: #t])))
                        (set! *gsh-history-port* p)
                        p))))
        (display (number->string ts) port)
        (display "\t" port)
        (display cwd port)
        (display "\t" port)
        (display command port)
        (newline port)
        (force-output port)))))

;;;============================================================================
;;; Load / Save
;;;============================================================================

(def (gsh-history-load!)
  "Load history from ~/.gsh_history into memory.
   Deduplicates consecutive identical commands, keeping the most recent."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((path (gsh-history-path)))
        (when (file-exists? path)
          (let ((entries
                  (call-with-input-file path
                    (lambda (port)
                      (let loop ((acc []))
                        (let ((line (read-line port)))
                          (if (eof-object? line)
                            acc  ;; reversed later
                            (let ((parsed (parse-history-line line)))
                              (if parsed
                                (loop (cons parsed acc))
                                (loop acc))))))))))
            ;; entries is newest-first (last line read = first in list)
            ;; Deduplicate: keep only the most recent occurrence of each command
            (let ((seen (make-hash-table))
                  (deduped []))
              (for-each
                (lambda (entry)
                  (let ((cmd (caddr entry)))
                    (unless (hash-get seen cmd)
                      (hash-put! seen cmd #t)
                      (set! deduped (cons entry deduped)))))
                entries)
              ;; deduped is now oldest-first, reverse to newest-first
              (set! *gsh-history* (reverse deduped)))
            ;; Trim
            (when (> (length *gsh-history*) *gsh-history-max*)
              (set! *gsh-history*
                (let loop ((lst *gsh-history*) (n 0) (acc []))
                  (if (or (null? lst) (>= n *gsh-history-max*))
                    (reverse acc)
                    (loop (cdr lst) (+ n 1) (cons (car lst) acc))))))))))))

(def (parse-history-line line)
  "Parse a history line: EPOCH<TAB>CWD<TAB>COMMAND
   Returns (list timestamp cwd command) or #f."
  (let ((tab1 (string-index line #\tab)))
    (and tab1
         (let ((tab2 (string-index line #\tab (+ tab1 1))))
           (and tab2
                (let* ((ts-str (substring line 0 tab1))
                       (ts (string->number ts-str))
                       (cwd (substring line (+ tab1 1) tab2))
                       (cmd (substring line (+ tab2 1) (string-length line))))
                  (and ts
                       (> (string-length cmd) 0)
                       (list ts cwd cmd))))))))

(def (gsh-history-save!)
  "Rewrite the history file with the current in-memory contents.
   Called periodically or on shutdown to compact the file."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      ;; Close the append port first
      (when *gsh-history-port*
        (with-catch void (lambda () (close-output-port *gsh-history-port*)))
        (set! *gsh-history-port* #f))
      ;; Rewrite the file (oldest first on disk, so newest is at end)
      (let ((entries (reverse *gsh-history*)))
        (call-with-output-file (gsh-history-path)
          (lambda (port)
            (for-each
              (lambda (entry)
                (display (number->string (car entry)) port)
                (display "\t" port)
                (display (cadr entry) port)
                (display "\t" port)
                (display (caddr entry) port)
                (newline port))
              entries)))))))

;;;============================================================================
;;; Query
;;;============================================================================

(def (gsh-history-recent (n 50))
  "Return the N most recent history entries as (timestamp cwd command) tuples."
  (let loop ((lst *gsh-history*) (i 0) (acc []))
    (if (or (null? lst) (>= i n))
      (reverse acc)
      (loop (cdr lst) (+ i 1) (cons (car lst) acc)))))

(def (gsh-history-for-cwd cwd (n 50))
  "Return the N most recent history entries from a specific directory."
  (let loop ((lst *gsh-history*) (i 0) (acc []))
    (if (or (null? lst) (>= i n))
      (reverse acc)
      (let ((entry (car lst)))
        (if (string=? (cadr entry) cwd)
          (loop (cdr lst) (+ i 1) (cons entry acc))
          (loop (cdr lst) i acc))))))

(def (gsh-history-search pattern (max-results 50))
  "Fuzzy search history commands. Returns matching entries sorted by recency."
  (let ((query (string-downcase (string-trim-both pattern))))
    (if (string=? query "")
      (gsh-history-recent max-results)
      (let loop ((lst *gsh-history*) (acc []) (n 0))
        (if (or (null? lst) (>= n max-results))
          (reverse acc)
          (let* ((entry (car lst))
                 (cmd (caddr entry)))
            (if (fuzzy-match? query cmd)
              (loop (cdr lst) (cons entry acc) (+ n 1))
              (loop (cdr lst) acc n))))))))

(def (gsh-history-all)
  "Return all history entries (newest first)."
  *gsh-history*)
