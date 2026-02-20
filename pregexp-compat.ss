;;; -*- Gerbil -*-
;;; PCRE2 compatibility wrapper for :std/pregexp
;;;
;;; Drop-in replacement that re-exports gerbil-pcre2 functions with
;;; pregexp-compatible names. Provides massive performance improvements:
;;; - 86x faster on extract-all operations
;;; - 12x faster on replace-all operations
;;; - 20x faster on split operations
;;; - Built-in LRU cache (max 64 patterns) prevents recompilation overhead
;;;
;;; Based on optimization patterns from gerbil-shell (see ~/mine/gerbil-shell/optimize.md)

(export pregexp pregexp? pregexp-match pregexp-match-positions
        pregexp-replace pregexp-replace* pregexp-quote
        pregexp-split)  ; bonus: split is available in pcre2 but not :std/pregexp

(import :gerbil-pcre/pcre2/pcre2)

;; Re-export PCRE2 functions with pregexp-compatible names
(def pregexp pcre2-compile)
(def pregexp? pcre-regex?)  ; Note: pcre-regex? not pcre2-regex?
(def pregexp-match pcre2-pregexp-match)
(def pregexp-match-positions pcre2-pregexp-match-positions)
(def pregexp-replace pcre2-pregexp-replace)
(def pregexp-replace* pcre2-pregexp-replace*)
(def pregexp-quote pcre2-pregexp-quote)

;; Bonus: pcre2-split is available but :std/pregexp doesn't have pregexp-split
;; Export it for convenience
(def pregexp-split pcre2-split)
