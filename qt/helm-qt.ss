;;; -*- Gerbil -*-
;;; Helm Qt renderer for gemacs
;;;
;;; Uses the existing QListWidget/minibuffer infrastructure from qt/echo.ss
;;; with multi-source header support and helm session management.

(export helm-qt-run!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core
        :gemacs/qt/sci-shim
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/helm)

;;;============================================================================
;;; Qt helm runner
;;;
;;; This leverages the existing narrowing framework in qt/echo.ss.
;;; Rather than duplicating the QListWidget/event loop infrastructure,
;;; we convert helm sources into a flat candidate list and use
;;; qt-echo-read-with-narrowing, then map the result back through
;;; helm's action system.
;;;============================================================================

(def (helm-qt-run! session app)
  "Run a helm session in Qt mode.
   Uses the existing narrowing UI with helm's multi-match and sources.
   Returns the selected candidate's real value, or #f if cancelled."
  (let* ((sources (helm-session-sources session))
         (pattern (helm-session-pattern session))
         ;; Build flat candidate list from all sources
         ;; Format: "source-name: candidate-display" for multi-source
         ;; or just "candidate-display" for single-source
         (multi-source? (> (length sources) 1))
         (all-display-strings [])
         (display-to-candidate (make-hash-table))  ;; display-string → helm-candidate
         )

    ;; Collect all candidates
    (for-each
      (lambda (src)
        (let* ((raw (let ((c (helm-source-candidates src)))
                      (if (procedure? c) (c) c)))
               (display-fn (helm-source-display-fn src))
               (real-fn (helm-source-real-fn src))
               (src-name (helm-source-name src)))
          (for-each
            (lambda (raw-item)
              (let* ((display-str (if display-fn (display-fn raw-item) raw-item))
                     (real-val (if real-fn (real-fn raw-item) raw-item))
                     ;; For multi-source, prefix with source name for clarity
                     (shown (if multi-source?
                              (string-append "[" src-name "] " display-str)
                              display-str))
                     (cand (make-helm-candidate display-str real-val src)))
                (set! all-display-strings (cons shown all-display-strings))
                (hash-put! display-to-candidate shown cand)))
            raw)))
      sources)

    ;; Reverse to preserve original order
    (set! all-display-strings (reverse all-display-strings))

    ;; Use the existing narrowing UI
    (let* ((prompt (if multi-source?
                     "Helm"
                     (helm-source-name (car sources))))
           (result (qt-echo-read-with-narrowing app
                     (string-append prompt ": ")
                     all-display-strings)))
      (if result
        ;; Look up the helm-candidate for this display string
        (let ((cand (hash-get display-to-candidate result)))
          (if cand
            (begin
              (helm-session-store! session)
              ;; Run default action and return real value
              (let* ((src (helm-candidate-source cand))
                     (action (helm-default-action src))
                     (real (helm-candidate-real cand)))
                (when action
                  (action real))
                real))
            ;; No matching candidate — return the raw input
            ;; (useful for "create buffer" type sources)
            (begin
              (helm-session-store! session)
              result)))
        ;; Cancelled
        #f))))
