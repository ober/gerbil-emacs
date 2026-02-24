;;; -*- Gerbil -*-
;;; Pregexp compatibility layer backed by gerbil-pcre2 (PCRE2 with JIT).
;;;
;;; Exports the same API as :std/pregexp so callers don't need to change:
;;; pregexp, pregexp-match, pregexp-match-positions,
;;; pregexp-replace, pregexp-replace*, pregexp-quote, pregexp-split.
;;;
;;; The JIT match bug (PCRE2_ZERO_TERMINATED passed to pcre2_jit_match_8,
;;; which requires an actual byte length) was fixed in gerbil-pcre by
;;; replacing it with strlen(subject).
;;;
;;; Backreference translation: :std/pregexp uses \N in replacement strings
;;; while PCRE2 uses $N.  In pregexp, \\ is a literal backslash escape.
;;; The replace wrappers translate automatically.

(export pregexp pregexp-match pregexp-match-positions
        pregexp-replace pregexp-replace* pregexp-quote
        pregexp-split)

(import :gerbil-pcre/pcre2/pcre2)

;; Translate pregexp-style replacement strings to PCRE2-style.
;; pregexp: \N = backreference, \\ = literal backslash
;; PCRE2:  $N = backreference, \ has no special meaning
(def (translate-backrefs replacement)
  (let loop ((i 0) (acc []))
    (if (>= i (string-length replacement))
      (list->string (reverse acc))
      (let ((ch (string-ref replacement i)))
        (if (and (char=? ch #\\)
                 (< (+ i 1) (string-length replacement)))
          (let ((next (string-ref replacement (+ i 1))))
            (cond
             ;; \\ → literal backslash
             ((char=? next #\\)
              (loop (+ i 2) (cons #\\ acc)))
             ;; \N → $N
             ((char-numeric? next)
              (loop (+ i 2) (cons next (cons #\$ acc))))
             ;; other \x → pass through both characters
             (else
              (loop (+ i 2) (cons next (cons #\\ acc))))))
          (loop (+ i 1) (cons ch acc)))))))

;; pregexp: compile a pattern string → compiled regex object
(def (pregexp pattern)
  (pcre2-compile pattern))

;; pregexp-match: return list of match + groups, or #f
(def pregexp-match pcre2-pregexp-match)

;; pregexp-match-positions: return list of (start . end) pairs, or #f
(def pregexp-match-positions pcre2-pregexp-match-positions)

;; pregexp-replace: replace first match, translating \N → $N in replacement
(def (pregexp-replace pattern str replacement)
  (pcre2-pregexp-replace pattern str (translate-backrefs replacement)))

;; pregexp-replace*: replace all matches, translating \N → $N in replacement
(def (pregexp-replace* pattern str replacement)
  (pcre2-pregexp-replace* pattern str (translate-backrefs replacement)))

;; pregexp-quote: escape special regex characters
(def pregexp-quote pcre2-pregexp-quote)

;; pregexp-split: split string by regex
(def (pregexp-split pattern str)
  (pcre2-split pattern str))
