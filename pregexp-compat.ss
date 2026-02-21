;;; -*- Gerbil -*-
;;; Pregexp compatibility layer.
;;;
;;; Re-exports :std/pregexp which provides all the functions we need:
;;; pregexp, pregexp-match, pregexp-match-positions,
;;; pregexp-replace, pregexp-replace*, pregexp-quote, pregexp-split.
;;;
;;; Previously delegated to gerbil-pcre2 for performance, but PCRE2's
;;; JIT match path was returning incorrect results (all matches failing).
;;; Switched back to :std/pregexp for correctness.  Can re-enable PCRE2
;;; once the upstream JIT issue is resolved.

(export pregexp pregexp-match pregexp-match-positions
        pregexp-replace pregexp-replace* pregexp-quote
        pregexp-split)

(import :std/pregexp)
