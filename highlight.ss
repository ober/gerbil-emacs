;;; -*- Gerbil -*-
;;; Syntax highlighting for gerbil-emacs
;;;
;;; Shared keyword lists and Scintilla lexer setup for Gerbil Scheme.
;;; Colors based on gerbil-mode.el face definitions.

(export setup-gerbil-highlighting!
        gerbil-file-extension?)

(import :std/sugar
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/lexer
        :gerbil-scintilla/style)

;;;============================================================================
;;; Scintilla Lisp lexer style IDs (from SciLexer.h)
;;;============================================================================

(def SCE_LISP_DEFAULT       0)
(def SCE_LISP_COMMENT       1)
(def SCE_LISP_NUMBER        2)
(def SCE_LISP_KEYWORD       3)
(def SCE_LISP_KEYWORD_KW    4)
(def SCE_LISP_SYMBOL        5)
(def SCE_LISP_STRING        6)
(def SCE_LISP_STRINGEOL     8)
(def SCE_LISP_IDENTIFIER    9)
(def SCE_LISP_OPERATOR     10)
(def SCE_LISP_SPECIAL      11)
(def SCE_LISP_MULTI_COMMENT 12)

;;;============================================================================
;;; Gerbil keyword lists (from gerbil-mode.el)
;;;============================================================================

;; Keyword set 0: Definition forms, control flow, special forms
(def *gerbil-keywords*
  (string-join
    '(;; Definition forms
      "def" "defvalues" "defalias" "defsyntax" "defrule" "defrules"
      "defstruct" "defclass" "defmethod" "defgeneric" "deftype"
      "defmessage" "definline" "defconst" "defcall-actor" "defproto"
      "deferror-class" "defapi" "deftyped"
      ;; Control flow
      "if" "when" "unless" "cond" "case" "case-lambda"
      "match" "match*" "with" "with*"
      "begin" "begin0" "begin-syntax" "begin-annotation"
      "begin-foreign" "begin-ffi"
      ;; Binding forms
      "let" "let*" "letrec" "letrec*" "let-values" "letrec-values"
      "let-syntax" "letrec-syntax" "let-hash" "let/cc" "let/esc"
      "rec" "alet" "alet*" "awhen"
      ;; Lambda
      "lambda" "lambda%"
      ;; Module
      "import" "export" "declare" "include" "module" "extern"
      "require" "provide" "cond-expand"
      ;; Assignment
      "set!" "apply" "eval"
      ;; Logic
      "and" "or" "not"
      ;; Error handling
      "try" "catch" "finally" "error" "raise"
      "unwind-protect" "with-destroy" "guard"
      ;; Syntax
      "syntax-case" "ast-case" "ast-rules" "core-syntax-case"
      "core-ast-case" "core-match" "identifier-rules"
      "with-syntax" "with-syntax*" "with-ast" "with-ast*"
      "syntax-parameterize"
      ;; Iteration
      "for" "for*" "for/collect" "for/fold" "while" "until"
      "for-each" "map" "foldl" "foldr"
      ;; Concurrency
      "spawn" "spawn*" "spawn/name" "spawn/group"
      "sync" "wait"
      ;; Quoting
      "quote" "quasiquote" "unquote" "unquote-splicing"
      "quote-syntax" "syntax" "quasisyntax"
      "unsyntax" "unsyntax-splicing" "syntax/loc"
      ;; Misc
      "parameterize" "parameterize*" "using" "chain" "is"
      "call/cc" "call/values" "values" "cut"
      ;; Interface
      "interface" "with-interface" "with-struct" "with-class"
      "with-methods" "with-class-methods"
      ;; Testing
      "test-suite" "test-case" "check" "run-tests!"
      "check-eq?" "check-equal?" "check-not-equal?"
      "check-output" "check-predicate" "check-exception")
    " "))

;; Keyword set 1: Built-in functions and types (highlighted differently)
(def *gerbil-builtins*
  (string-join
    '(;; Common functions
      "cons" "car" "cdr" "caar" "cadr" "cdar" "cddr"
      "list" "list?" "null?" "pair?" "append" "reverse" "length"
      "assoc" "assq" "assv" "member" "memq" "memv"
      "vector" "vector-ref" "vector-set!" "vector-length"
      "make-vector" "vector->list" "list->vector"
      "string" "string-ref" "string-length" "string-append"
      "substring" "string->list" "list->string"
      "string=?" "string<?" "string>?"
      "number?" "string?" "symbol?" "boolean?" "char?"
      "integer?" "real?" "zero?" "positive?" "negative?"
      "eq?" "eqv?" "equal?"
      "+" "-" "*" "/" "=" "<" ">" "<=" ">="
      "min" "max" "abs" "modulo" "remainder" "quotient"
      "display" "write" "newline" "read" "read-line"
      "open-input-file" "open-output-file" "close-port"
      "call-with-input-file" "call-with-output-file"
      "with-input-from-file" "with-output-to-file"
      "with-input-from-string" "with-output-to-string"
      "current-input-port" "current-output-port"
      "port?" "input-port?" "output-port?"
      "eof-object?" "char-ready?"
      ;; Hash tables
      "make-hash-table" "hash-table?" "hash-get" "hash-put!"
      "hash-remove!" "hash-ref" "hash-key?" "hash-keys"
      "hash-values" "hash-for-each" "hash-map" "hash-copy"
      ;; Type predicates
      "void?" "procedure?" "hash-table?"
      "fixnum?" "flonum?" "exact?" "inexact?"
      ;; Conversion
      "number->string" "string->number"
      "symbol->string" "string->symbol"
      "char->integer" "integer->char"
      "exact->inexact" "inexact->exact"
      ;; Boolean
      "not" "boolean?"
      ;; I/O
      "file-exists?" "delete-file" "rename-file"
      "directory-files" "create-directory"
      "current-directory" "path-expand" "path-directory"
      "path-strip-directory" "path-extension"
      ;; Gerbil specifics
      "make-hash-table-eq" "hash-eq" "hash-eqv"
      "string-empty?" "string-contains"
      "filter" "sort" "iota" "range"
      "void" "raise-type-error")
    " "))

;;;============================================================================
;;; Color theme (dark, based on gerbil-mode.el / base16)
;;;============================================================================

(def (setup-gerbil-highlighting! ed)
  "Configure Scintilla's Lisp lexer for Gerbil with dark theme colors."
  ;; Set the Lisp lexer (same as Scheme)
  (editor-set-lexer-language ed "lisp")

  ;; Set keyword lists
  (editor-set-keywords ed 0 *gerbil-keywords*)
  (editor-set-keywords ed 1 *gerbil-builtins*)

  ;; Default style: light gray on dark
  (editor-style-set-foreground ed SCE_LISP_DEFAULT (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_LISP_DEFAULT (rgb->scintilla #x18 #x18 #x18))

  ;; Comments: gray, italic
  (editor-style-set-foreground ed SCE_LISP_COMMENT (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_LISP_COMMENT (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_LISP_COMMENT #t)

  ;; Multi-line comments: gray, italic
  (editor-style-set-foreground ed SCE_LISP_MULTI_COMMENT (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_LISP_MULTI_COMMENT (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_LISP_MULTI_COMMENT #t)

  ;; Numbers: orange
  (editor-style-set-foreground ed SCE_LISP_NUMBER (rgb->scintilla #xf9 #x91 #x57))
  (editor-style-set-background ed SCE_LISP_NUMBER (rgb->scintilla #x18 #x18 #x18))

  ;; Keywords (set 0): purple, bold
  (editor-style-set-foreground ed SCE_LISP_KEYWORD (rgb->scintilla #xcc #x99 #xcc))
  (editor-style-set-background ed SCE_LISP_KEYWORD (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-bold ed SCE_LISP_KEYWORD #t)

  ;; Keywords KW (set 1): cyan
  (editor-style-set-foreground ed SCE_LISP_KEYWORD_KW (rgb->scintilla #x66 #xcc #xcc))
  (editor-style-set-background ed SCE_LISP_KEYWORD_KW (rgb->scintilla #x18 #x18 #x18))

  ;; Symbols (quoted): green
  (editor-style-set-foreground ed SCE_LISP_SYMBOL (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_LISP_SYMBOL (rgb->scintilla #x18 #x18 #x18))

  ;; Strings: green
  (editor-style-set-foreground ed SCE_LISP_STRING (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_LISP_STRING (rgb->scintilla #x18 #x18 #x18))

  ;; String EOL (unterminated string): red background
  (editor-style-set-foreground ed SCE_LISP_STRINGEOL (rgb->scintilla #xf2 #x77 #x7a))
  (editor-style-set-background ed SCE_LISP_STRINGEOL (rgb->scintilla #x28 #x18 #x18))
  (editor-style-set-eol-filled ed SCE_LISP_STRINGEOL #t)

  ;; Identifiers: light gray (default)
  (editor-style-set-foreground ed SCE_LISP_IDENTIFIER (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_LISP_IDENTIFIER (rgb->scintilla #x18 #x18 #x18))

  ;; Operators (parens, brackets): slightly brighter
  (editor-style-set-foreground ed SCE_LISP_OPERATOR (rgb->scintilla #xb8 #xb8 #xb8))
  (editor-style-set-background ed SCE_LISP_OPERATOR (rgb->scintilla #x18 #x18 #x18))

  ;; Special (#t, #f, #\char, etc.): blue
  (editor-style-set-foreground ed SCE_LISP_SPECIAL (rgb->scintilla #x66 #x99 #xcc))
  (editor-style-set-background ed SCE_LISP_SPECIAL (rgb->scintilla #x18 #x18 #x18))

  ;; Trigger initial colorization
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; File extension detection
;;;============================================================================

(def (gerbil-file-extension? path)
  "Check if a file path has a Gerbil/Scheme extension."
  (and path
       (let ((ext (path-extension path)))
         (and ext
              (or (string=? ext ".ss")
                  (string=? ext ".scm")
                  (string=? ext ".sld")
                  (string=? ext ".sls"))))))
