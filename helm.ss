;;; -*- Gerbil -*-
;;; Helm core framework for gemacs
;;;
;;; Backend-agnostic: data model, multi-match engine, filtering,
;;; session management, action dispatch. No TUI or Qt imports.

(export
  ;; Data structures
  (struct-out helm-source)
  (struct-out helm-session)
  (struct-out helm-candidate)

  ;; Matching
  helm-multi-match
  helm-multi-match?

  ;; Filtering
  helm-filter-source
  helm-filter-all

  ;; Session management
  *helm-sessions*
  *helm-last-session*
  helm-session-store!
  helm-session-resume

  ;; Action dispatch
  helm-default-action
  helm-run-action

  ;; Candidate construction
  make-helm-candidates

  ;; Session construction
  make-new-session
  make-simple-source

  ;; Match highlighting
  helm-match-positions

  ;; Pattern access (for volatile sources like grep)
  *helm-current-pattern*

  ;; Configuration
  *helm-candidate-limit*
  *helm-follow-delay*)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core)

;;;============================================================================
;;; Configuration
;;;============================================================================

;; *helm-mode* is defined in core.ss
(def *helm-candidate-limit* 100)
(def *helm-follow-delay* 0.1)  ;; seconds

;; Dynamic parameter: set to current pattern during helm-filter-source.
;; Volatile sources (e.g. grep) can read this to use the pattern as a search query.
(def *helm-current-pattern* (make-parameter ""))

;;;============================================================================
;;; Data structures
;;;============================================================================

(defstruct helm-source
  (name              ; string: display header (e.g. "Buffers")
   candidates        ; thunk -> list of strings, or list directly
   actions           ; alist: ((label . procedure) ...) — first is default
   persistent-action ; (or procedure #f): C-j preview, session stays open
   display-fn        ; (or (-> candidate string) #f): custom display
   real-fn           ; (or (-> candidate any) #f): display->real for actions
   fuzzy?            ; bool: use fuzzy matching instead of substring
   volatile?         ; bool: rebuild candidates on every pattern change
   candidate-limit   ; integer: max candidates for this source
   keymap            ; (or keymap #f): source-local keymap
   follow?)          ; bool: auto-preview on navigate
  transparent: #t)

(defstruct helm-session
  (sources           ; list of helm-source
   pattern           ; string: current input
   candidates        ; vector of helm-candidate (filtered, sorted)
   selected          ; integer: cursor index
   marked            ; list of integers: marked candidate indices
   buffer-name       ; string: session name for resume
   scroll-offset     ; integer: visible window start
   follow?           ; bool: auto-preview on navigate
   alive?)           ; bool: session running
  transparent: #t)

(defstruct helm-candidate
  (display           ; string: what the user sees
   real              ; any: what actions receive
   source)           ; helm-source: which source produced this
  transparent: #t)

;;;============================================================================
;;; Multi-match engine
;;;
;;; Space-separated tokens, all must match (AND logic).
;;; Prefix modifiers:
;;;   !token  — NOT: exclude candidates matching "token"
;;;   ^token  — PREFIX: candidate must start with "token"
;;;   token   — CONTAINS: substring match (or fuzzy if source.fuzzy?)
;;;============================================================================

(defstruct match-token
  (text    ; string: the search text (without modifier)
   negate? ; bool: ! prefix
   prefix?) ; bool: ^ prefix
  transparent: #t)

(def (parse-match-tokens pattern)
  "Parse a space-separated pattern into a list of match-token structs."
  (let ((words (filter (lambda (w) (> (string-length w) 0))
                       (string-split pattern #\space))))
    (map (lambda (word)
           (cond
             ((and (> (string-length word) 1)
                   (char=? (string-ref word 0) #\!))
              (make-match-token (substring word 1 (string-length word)) #t #f))
             ((and (> (string-length word) 1)
                   (char=? (string-ref word 0) #\^))
              (make-match-token (substring word 1 (string-length word)) #f #t))
             (else
              (make-match-token word #f #f))))
         words)))

(def (token-matches? token candidate-str use-fuzzy?)
  "Check if a single match-token matches a candidate string."
  (let* ((text (match-token-text token))
         (target-lower (string-downcase candidate-str))
         (text-lower (string-downcase text))
         (matches
           (cond
             ((match-token-prefix? token)
              (string-prefix? text-lower target-lower))
             (use-fuzzy?
              (fuzzy-match? text candidate-str))
             (else
              (string-contains target-lower text-lower)))))
    (if (match-token-negate? token)
      (not matches)
      matches)))

(def (helm-multi-match? pattern candidate-str (use-fuzzy? #f))
  "Check if ALL tokens in pattern match the candidate string.
   Returns #t if pattern is empty."
  (let ((tokens (parse-match-tokens pattern)))
    (if (null? tokens)
      #t
      (let loop ((toks tokens))
        (cond
          ((null? toks) #t)
          ((not (token-matches? (car toks) candidate-str use-fuzzy?)) #f)
          (else (loop (cdr toks))))))))

(def (helm-multi-match pattern candidate-str (use-fuzzy? #f))
  "Score a candidate against a multi-match pattern.
   Returns score >= 0 if matches, -1 if no match.
   Higher score = better match."
  (let ((tokens (parse-match-tokens pattern)))
    (if (null? tokens)
      0  ;; empty pattern matches everything with score 0
      (let loop ((toks tokens) (total-score 0))
        (cond
          ((null? toks) total-score)
          (else
           (let ((tok (car toks)))
             (if (match-token-negate? tok)
               ;; Negation: if it matches, fail; if it doesn't match, continue
               (let* ((text (match-token-text tok))
                      (target-lower (string-downcase candidate-str))
                      (text-lower (string-downcase text))
                      (matches (if use-fuzzy?
                                 (fuzzy-match? text candidate-str)
                                 (string-contains target-lower text-lower))))
                 (if matches -1  ;; negated token matched → exclude
                   (loop (cdr toks) total-score)))
               ;; Positive token: score it
               (let ((score (if (match-token-prefix? tok)
                              (let* ((text-lower (string-downcase (match-token-text tok)))
                                     (target-lower (string-downcase candidate-str)))
                                (if (string-prefix? text-lower target-lower)
                                  (+ 5 (string-length (match-token-text tok))) ;; prefix bonus
                                  -1))
                              ;; Regular or fuzzy
                              (if use-fuzzy?
                                (fuzzy-score (match-token-text tok) candidate-str)
                                (let* ((text-lower (string-downcase (match-token-text tok)))
                                       (target-lower (string-downcase candidate-str)))
                                  (if (string-contains target-lower text-lower)
                                    (+ 3 (string-length (match-token-text tok))
                                       (if (string-prefix? text-lower target-lower) 5 0))
                                    -1))))))
                 (if (< score 0) -1
                   (loop (cdr toks) (+ total-score score))))))))))))

;;;============================================================================
;;; Filtering
;;;============================================================================

(def (helm-filter-source source pattern)
  "Filter a single source's candidates against a pattern.
   Returns a list of helm-candidate structs, sorted by score."
  (let* ((raw-candidates (parameterize ((*helm-current-pattern* pattern))
                           (let ((c (helm-source-candidates source)))
                             (if (procedure? c) (c) c))))
         (display-fn (helm-source-display-fn source))
         (real-fn (helm-source-real-fn source))
         (use-fuzzy? (helm-source-fuzzy? source))
         (limit (or (helm-source-candidate-limit source) *helm-candidate-limit*)))
    (let* ((scored
             (filter-map
               (lambda (raw)
                 (let* ((display-str (if display-fn (display-fn raw) raw))
                        (real-val (if real-fn (real-fn raw) raw))
                        (score (if (string=? pattern "")
                                 0
                                 (helm-multi-match pattern display-str use-fuzzy?))))
                   (and (>= score 0)
                        (cons score (make-helm-candidate display-str real-val source)))))
               raw-candidates))
           (sorted (if (string=? pattern "")
                     scored  ;; preserve original order when no pattern
                     (sort scored (lambda (a b) (> (car a) (car b))))))
           (limited (if (> (length sorted) limit)
                      (take sorted limit)
                      sorted)))
      (map cdr limited))))

(def (take lst n)
  "Take first n elements from list."
  (let loop ((l lst) (i 0) (acc []))
    (if (or (null? l) (>= i n))
      (reverse acc)
      (loop (cdr l) (+ i 1) (cons (car l) acc)))))

(def (helm-filter-all session)
  "Filter all sources in a session against the current pattern.
   Returns a flat vector of helm-candidate structs with source headers."
  (let ((pattern (helm-session-pattern session))
        (sources (helm-session-sources session)))
    (let ((all-candidates
            (apply append
              (map (lambda (src)
                     (let ((filtered (helm-filter-source src pattern)))
                       (if (null? filtered)
                         []
                         filtered)))
                   sources))))
      (list->vector all-candidates))))

;;;============================================================================
;;; Candidate construction helpers
;;;============================================================================

(def (make-helm-candidates strings source)
  "Create helm-candidate structs from a list of strings."
  (map (lambda (s)
         (make-helm-candidate s s source))
       strings))

;;;============================================================================
;;; Session management
;;;============================================================================

(def *helm-sessions* [])     ;; alist: buffer-name → session snapshot
(def *helm-last-session* #f) ;; most recent session
(def *helm-max-sessions* 10)

(def (helm-session-store! session)
  "Store a session for later resume."
  (let ((name (helm-session-buffer-name session)))
    (set! *helm-last-session* session)
    ;; Replace existing or add to front
    (set! *helm-sessions*
      (cons (cons name session)
            (filter (lambda (pair) (not (string=? (car pair) name)))
                    *helm-sessions*)))
    ;; Trim to max
    (when (> (length *helm-sessions*) *helm-max-sessions*)
      (set! *helm-sessions* (take *helm-sessions* *helm-max-sessions*)))))

(def (helm-session-resume (name #f))
  "Get a stored session by name, or the last session if no name given."
  (if name
    (let ((found (assoc name *helm-sessions*)))
      (and found (cdr found)))
    *helm-last-session*))

;;;============================================================================
;;; Action dispatch
;;;============================================================================

(def (helm-default-action source)
  "Get the default action procedure for a source (first in actions alist)."
  (let ((actions (helm-source-actions source)))
    (and (pair? actions) (cdar actions))))

(def (helm-run-action action candidates)
  "Run an action on a list of helm-candidate structs.
   The action receives the 'real' value of each candidate."
  (for-each
    (lambda (cand)
      (action (helm-candidate-real cand)))
    candidates))

;;;============================================================================
;;; Source constructors (convenience)
;;;============================================================================

(def (make-simple-source name candidates-thunk action
                         (fuzzy? #t)
                         (persistent-action #f)
                         (display-fn #f)
                         (real-fn #f)
                         (volatile? #f)
                         (follow? #f))
  "Create a helm-source with sensible defaults."
  (make-helm-source
    name
    candidates-thunk
    (list (cons "Default" action))  ;; actions alist
    persistent-action
    display-fn
    real-fn
    fuzzy?
    volatile?
    *helm-candidate-limit*
    #f    ;; keymap
    follow?))

;;;============================================================================
;;; New session creation
;;;============================================================================

(def (make-new-session sources (buffer-name "*helm*") (initial-input ""))
  "Create a fresh helm session with the given sources."
  (let ((session (make-helm-session
                   sources
                   initial-input      ;; pattern
                   (vector)           ;; candidates (empty, will be filtered)
                   0                  ;; selected
                   []                 ;; marked
                   buffer-name
                   0                  ;; scroll-offset
                   #f                 ;; follow?
                   #t)))             ;; alive?
    ;; Initial filter
    (set! (helm-session-candidates session) (helm-filter-all session))
    session))

;;;============================================================================
;;; Match highlighting
;;;============================================================================

(def (fuzzy-match-positions pattern target)
  "Find character positions in target where pattern characters match (fuzzy).
   Returns list of indices, or [] if no full match."
  (let ((plen (string-length pattern))
        (tlen (string-length target)))
    (let loop ((pi 0) (ti 0) (acc []))
      (cond
        ((>= pi plen) (reverse acc))
        ((>= ti tlen) [])
        ((char-ci=? (string-ref pattern pi) (string-ref target ti))
         (loop (+ pi 1) (+ ti 1) (cons ti acc)))
        (else
         (loop pi (+ ti 1) acc))))))

(def (helm-match-positions pattern candidate-str (use-fuzzy? #f))
  "Return a sorted list of character indices in candidate-str that match the pattern.
   Used for visual highlighting of matched characters."
  (let ((tokens (parse-match-tokens pattern)))
    (if (null? tokens)
      []
      (let loop ((toks tokens) (positions []))
        (if (null? toks)
          ;; Deduplicate and sort
          (sort (let dedup ((ps (sort positions <)) (acc []))
                  (cond
                    ((null? ps) (reverse acc))
                    ((and (pair? acc) (= (car ps) (car acc)))
                     (dedup (cdr ps) acc))
                    (else (dedup (cdr ps) (cons (car ps) acc)))))
                <)
          (let ((tok (car toks)))
            (if (match-token-negate? tok)
              (loop (cdr toks) positions)  ;; skip negated tokens
              (let* ((text (match-token-text tok))
                     (text-lower (string-downcase text))
                     (target-lower (string-downcase candidate-str))
                     (new-positions
                       (cond
                         ((match-token-prefix? tok)
                          (if (string-prefix? text-lower target-lower)
                            (let gen ((i 0) (acc []))
                              (if (>= i (string-length text))
                                acc
                                (gen (+ i 1) (cons i acc))))
                            []))
                         (use-fuzzy?
                          (fuzzy-match-positions text candidate-str))
                         (else
                          ;; Substring match — find position
                          (let ((pos (string-contains target-lower text-lower)))
                            (if pos
                              (let gen ((i 0) (acc []))
                                (if (>= i (string-length text))
                                  acc
                                  (gen (+ i 1) (cons (+ pos i) acc))))
                              []))))))
                (loop (cdr toks) (append positions new-positions))))))))))

