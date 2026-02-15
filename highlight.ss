;;; -*- Gerbil -*-
;;; Syntax highlighting for gerbil-emacs
;;;
;;; Shared keyword lists and Scintilla lexer setup for Gerbil Scheme.
;;; Colors based on gerbil-mode.el face definitions.

(export setup-gerbil-highlighting!
        setup-highlighting-for-file!
        detect-file-language
        detect-language-from-shebang
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
;;; C/C++ lexer style IDs (from SciLexer.h — SCLEX_CPP)
;;;============================================================================

(def SCE_C_DEFAULT     0)
(def SCE_C_COMMENT     1)
(def SCE_C_COMMENTLINE 2)
(def SCE_C_COMMENTDOC  3)
(def SCE_C_NUMBER      4)
(def SCE_C_WORD        5)   ;; keyword set 0
(def SCE_C_STRING      6)
(def SCE_C_CHARACTER   7)
(def SCE_C_PREPROCESSOR 9)
(def SCE_C_OPERATOR   10)
(def SCE_C_IDENTIFIER 11)
(def SCE_C_STRINGEOL  12)
(def SCE_C_WORD2      16)  ;; keyword set 1 (types)
(def SCE_C_COMMENTDOCKEYWORD  17)
(def SCE_C_COMMENTDOCKEYWORDERROR 18)

(def *c-keywords*
  (string-join
    '("if" "else" "for" "while" "do" "switch" "case" "default"
      "break" "continue" "return" "goto" "struct" "union" "enum"
      "typedef" "sizeof" "static" "const" "volatile" "extern"
      "inline" "register" "auto" "signed" "unsigned"
      "class" "public" "private" "protected" "virtual" "override"
      "template" "typename" "namespace" "using" "new" "delete"
      "throw" "try" "catch" "noexcept" "constexpr" "nullptr"
      "true" "false" "this" "operator" "explicit" "friend"
      "mutable" "final" "abstract" "static_cast" "dynamic_cast"
      "const_cast" "reinterpret_cast" "decltype" "concept"
      "requires" "co_await" "co_yield" "co_return")
    " "))

(def *c-types*
  (string-join
    '("int" "char" "float" "double" "void" "long" "short"
      "bool" "size_t" "ssize_t" "ptrdiff_t" "wchar_t"
      "int8_t" "int16_t" "int32_t" "int64_t"
      "uint8_t" "uint16_t" "uint32_t" "uint64_t"
      "intptr_t" "uintptr_t"
      "FILE" "NULL" "EOF" "stdin" "stdout" "stderr")
    " "))

(def (setup-c-highlighting! ed)
  "Configure Scintilla's CPP lexer for C/C++ with dark theme colors."
  (editor-set-lexer-language ed "cpp")

  ;; Keywords
  (editor-set-keywords ed 0 *c-keywords*)
  (editor-set-keywords ed 1 *c-types*)

  ;; Default: light gray on dark
  (editor-style-set-foreground ed SCE_C_DEFAULT (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_C_DEFAULT (rgb->scintilla #x18 #x18 #x18))

  ;; Comments: gray, italic
  (editor-style-set-foreground ed SCE_C_COMMENT (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_C_COMMENT (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_C_COMMENT #t)
  (editor-style-set-foreground ed SCE_C_COMMENTLINE (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_C_COMMENTLINE (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_C_COMMENTLINE #t)
  (editor-style-set-foreground ed SCE_C_COMMENTDOC (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_C_COMMENTDOC (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_C_COMMENTDOC #t)

  ;; Numbers: orange
  (editor-style-set-foreground ed SCE_C_NUMBER (rgb->scintilla #xf9 #x91 #x57))
  (editor-style-set-background ed SCE_C_NUMBER (rgb->scintilla #x18 #x18 #x18))

  ;; Keywords: purple, bold
  (editor-style-set-foreground ed SCE_C_WORD (rgb->scintilla #xcc #x99 #xcc))
  (editor-style-set-background ed SCE_C_WORD (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-bold ed SCE_C_WORD #t)

  ;; Types (keyword set 1): yellow
  (editor-style-set-foreground ed SCE_C_WORD2 (rgb->scintilla #xff #xcc #x66))
  (editor-style-set-background ed SCE_C_WORD2 (rgb->scintilla #x18 #x18 #x18))

  ;; Strings: green
  (editor-style-set-foreground ed SCE_C_STRING (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_C_STRING (rgb->scintilla #x18 #x18 #x18))

  ;; Character literals: green
  (editor-style-set-foreground ed SCE_C_CHARACTER (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_C_CHARACTER (rgb->scintilla #x18 #x18 #x18))

  ;; Preprocessor: orange
  (editor-style-set-foreground ed SCE_C_PREPROCESSOR (rgb->scintilla #xf9 #x91 #x57))
  (editor-style-set-background ed SCE_C_PREPROCESSOR (rgb->scintilla #x18 #x18 #x18))

  ;; Operators: slightly brighter
  (editor-style-set-foreground ed SCE_C_OPERATOR (rgb->scintilla #xb8 #xb8 #xb8))
  (editor-style-set-background ed SCE_C_OPERATOR (rgb->scintilla #x18 #x18 #x18))

  ;; Identifiers: light gray
  (editor-style-set-foreground ed SCE_C_IDENTIFIER (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_C_IDENTIFIER (rgb->scintilla #x18 #x18 #x18))

  ;; Unterminated strings: red
  (editor-style-set-foreground ed SCE_C_STRINGEOL (rgb->scintilla #xf2 #x77 #x7a))
  (editor-style-set-background ed SCE_C_STRINGEOL (rgb->scintilla #x28 #x18 #x18))
  (editor-style-set-eol-filled ed SCE_C_STRINGEOL #t)

  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Python lexer style IDs (from SciLexer.h — SCLEX_PYTHON)
;;;============================================================================

(def SCE_P_DEFAULT      0)
(def SCE_P_COMMENTLINE  1)
(def SCE_P_NUMBER       2)
(def SCE_P_STRING       3)
(def SCE_P_CHARACTER    4)
(def SCE_P_WORD         5)   ;; keyword set 0
(def SCE_P_TRIPLE       6)   ;; triple-quoted string
(def SCE_P_TRIPLEDOUBLE 7)   ;; triple double-quoted string
(def SCE_P_CLASSNAME    8)
(def SCE_P_DEFNAME      9)
(def SCE_P_OPERATOR    10)
(def SCE_P_IDENTIFIER  11)
(def SCE_P_COMMENTBLOCK 12)
(def SCE_P_STRINGEOL   13)
(def SCE_P_WORD2       14)  ;; keyword set 1 (builtins)
(def SCE_P_DECORATOR   15)
(def SCE_P_FSTRING     16)
(def SCE_P_FTRIPLE     17)
(def SCE_P_FTRIPLEDOUBLE 18)

(def *python-keywords*
  (string-join
    '("False" "None" "True" "and" "as" "assert" "async" "await"
      "break" "class" "continue" "def" "del" "elif" "else" "except"
      "finally" "for" "from" "global" "if" "import" "in" "is"
      "lambda" "nonlocal" "not" "or" "pass" "raise" "return"
      "try" "while" "with" "yield" "match" "case" "type")
    " "))

(def *python-builtins*
  (string-join
    '("print" "len" "range" "int" "str" "float" "list" "dict"
      "tuple" "set" "bool" "type" "isinstance" "issubclass"
      "open" "input" "map" "filter" "zip" "enumerate"
      "sorted" "reversed" "sum" "min" "max" "abs" "any" "all"
      "super" "property" "staticmethod" "classmethod"
      "hasattr" "getattr" "setattr" "delattr" "repr" "hash"
      "id" "iter" "next" "callable" "vars" "dir" "help"
      "ValueError" "TypeError" "KeyError" "IndexError"
      "Exception" "RuntimeError" "StopIteration"
      "AttributeError" "ImportError" "OSError" "IOError"
      "FileNotFoundError" "PermissionError" "NotImplementedError"
      "object" "bytes" "bytearray" "memoryview" "frozenset"
      "complex" "slice" "format" "globals" "locals" "exec" "eval"
      "compile" "breakpoint" "exit" "quit")
    " "))

(def (setup-python-highlighting! ed)
  "Configure Scintilla's Python lexer with dark theme colors."
  (editor-set-lexer-language ed "python")

  ;; Keywords
  (editor-set-keywords ed 0 *python-keywords*)
  (editor-set-keywords ed 1 *python-builtins*)

  ;; Default: light gray on dark
  (editor-style-set-foreground ed SCE_P_DEFAULT (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_P_DEFAULT (rgb->scintilla #x18 #x18 #x18))

  ;; Comments: gray, italic
  (editor-style-set-foreground ed SCE_P_COMMENTLINE (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_P_COMMENTLINE (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_P_COMMENTLINE #t)
  (editor-style-set-foreground ed SCE_P_COMMENTBLOCK (rgb->scintilla #x99 #x99 #x99))
  (editor-style-set-background ed SCE_P_COMMENTBLOCK (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-italic ed SCE_P_COMMENTBLOCK #t)

  ;; Numbers: orange
  (editor-style-set-foreground ed SCE_P_NUMBER (rgb->scintilla #xf9 #x91 #x57))
  (editor-style-set-background ed SCE_P_NUMBER (rgb->scintilla #x18 #x18 #x18))

  ;; Keywords: purple, bold
  (editor-style-set-foreground ed SCE_P_WORD (rgb->scintilla #xcc #x99 #xcc))
  (editor-style-set-background ed SCE_P_WORD (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-bold ed SCE_P_WORD #t)

  ;; Builtins (keyword set 1): cyan
  (editor-style-set-foreground ed SCE_P_WORD2 (rgb->scintilla #x66 #xcc #xcc))
  (editor-style-set-background ed SCE_P_WORD2 (rgb->scintilla #x18 #x18 #x18))

  ;; Strings: green
  (editor-style-set-foreground ed SCE_P_STRING (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_STRING (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-foreground ed SCE_P_CHARACTER (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_CHARACTER (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-foreground ed SCE_P_TRIPLE (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_TRIPLE (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-foreground ed SCE_P_TRIPLEDOUBLE (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_TRIPLEDOUBLE (rgb->scintilla #x18 #x18 #x18))

  ;; F-strings: green
  (editor-style-set-foreground ed SCE_P_FSTRING (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_FSTRING (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-foreground ed SCE_P_FTRIPLE (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_FTRIPLE (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-foreground ed SCE_P_FTRIPLEDOUBLE (rgb->scintilla #x99 #xcc #x99))
  (editor-style-set-background ed SCE_P_FTRIPLEDOUBLE (rgb->scintilla #x18 #x18 #x18))

  ;; Class/def names: cyan
  (editor-style-set-foreground ed SCE_P_CLASSNAME (rgb->scintilla #x66 #xcc #xcc))
  (editor-style-set-background ed SCE_P_CLASSNAME (rgb->scintilla #x18 #x18 #x18))
  (editor-style-set-bold ed SCE_P_CLASSNAME #t)
  (editor-style-set-foreground ed SCE_P_DEFNAME (rgb->scintilla #x66 #xcc #xcc))
  (editor-style-set-background ed SCE_P_DEFNAME (rgb->scintilla #x18 #x18 #x18))

  ;; Decorators: orange
  (editor-style-set-foreground ed SCE_P_DECORATOR (rgb->scintilla #xf9 #x91 #x57))
  (editor-style-set-background ed SCE_P_DECORATOR (rgb->scintilla #x18 #x18 #x18))

  ;; Operators: slightly brighter
  (editor-style-set-foreground ed SCE_P_OPERATOR (rgb->scintilla #xb8 #xb8 #xb8))
  (editor-style-set-background ed SCE_P_OPERATOR (rgb->scintilla #x18 #x18 #x18))

  ;; Identifiers: light gray
  (editor-style-set-foreground ed SCE_P_IDENTIFIER (rgb->scintilla #xd8 #xd8 #xd8))
  (editor-style-set-background ed SCE_P_IDENTIFIER (rgb->scintilla #x18 #x18 #x18))

  ;; Unterminated strings: red
  (editor-style-set-foreground ed SCE_P_STRINGEOL (rgb->scintilla #xf2 #x77 #x7a))
  (editor-style-set-background ed SCE_P_STRINGEOL (rgb->scintilla #x28 #x18 #x18))
  (editor-style-set-eol-filled ed SCE_P_STRINGEOL #t)

  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Dark theme color palette (shared across all languages)
;;;============================================================================

(def *theme-bg*        (rgb->scintilla #x18 #x18 #x18))
(def *theme-fg*        (rgb->scintilla #xd8 #xd8 #xd8))
(def *theme-comment*   (rgb->scintilla #x99 #x99 #x99))
(def *theme-keyword*   (rgb->scintilla #xcc #x99 #xcc))
(def *theme-string*    (rgb->scintilla #x99 #xcc #x99))
(def *theme-number*    (rgb->scintilla #xf9 #x91 #x57))
(def *theme-type*      (rgb->scintilla #x66 #xcc #xcc))
(def *theme-function*  (rgb->scintilla #x66 #x99 #xcc))
(def *theme-operator*  (rgb->scintilla #xb8 #xb8 #xb8))
(def *theme-error*     (rgb->scintilla #xf2 #x77 #x7a))
(def *theme-tag*       (rgb->scintilla #xf2 #x77 #x7a))
(def *theme-attribute* (rgb->scintilla #xf9 #x91 #x57))
(def *theme-added*     (rgb->scintilla #x99 #xcc #x99))
(def *theme-removed*   (rgb->scintilla #xf2 #x77 #x7a))
(def *theme-heading*   (rgb->scintilla #xcc #x99 #xcc))

(def (style-set! ed id fg (bold? #f) (italic? #f))
  "Set foreground and background for a style ID."
  (editor-style-set-foreground ed id fg)
  (editor-style-set-background ed id *theme-bg*)
  (when bold? (editor-style-set-bold ed id #t))
  (when italic? (editor-style-set-italic ed id #t)))

;;;============================================================================
;;; JavaScript / TypeScript (SCLEX_CPP with JS/TS keywords)
;;;============================================================================

(def *js-keywords*
  "abstract arguments async await boolean break byte case catch char class
   const continue debugger default delete do double else enum export extends
   false final finally float for from function get goto if implements import
   in instanceof int interface let long native new null of package private
   protected public return set short static super switch synchronized this
   throw throws transient true try typeof undefined var void volatile while
   with yield")

(def *ts-extra-keywords*
  "any bigint constructor declare infer is keyof module namespace never
   readonly symbol type unique unknown")

(def *js-builtins*
  "Array Boolean Date Error Function JSON Map Math Number Object Promise
   Proxy Reflect RegExp Set String Symbol WeakMap WeakSet console document
   window parseInt parseFloat isNaN isFinite encodeURI decodeURI
   encodeURIComponent decodeURIComponent setTimeout setInterval clearTimeout
   clearInterval fetch require module exports process")

(def (setup-js-highlighting! ed lang)
  "Configure C++ lexer for JavaScript/TypeScript."
  (editor-set-lexer-language ed "cpp")
  (editor-set-keywords ed 0
    (if (eq? lang 'typescript)
      (string-append *js-keywords* " " *ts-extra-keywords*)
      *js-keywords*))
  (editor-set-keywords ed 1 *js-builtins*)
  ;; Reuse C++ style IDs (SCE_C_*)
  (style-set! ed 0  *theme-fg*)                   ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-comment* #f #t)         ; comment line
  (style-set! ed 3  *theme-comment* #f #t)         ; comment doc
  (style-set! ed 4  *theme-number*)                ; number
  (style-set! ed 5  *theme-keyword* #t)            ; keyword
  (style-set! ed 6  *theme-string*)                ; string
  (style-set! ed 7  *theme-string*)                ; character
  (style-set! ed 10 *theme-operator*)              ; operator
  (style-set! ed 11 *theme-fg*)                    ; identifier
  (style-set! ed 12 *theme-string*)                ; string eol
  (style-set! ed 16 *theme-type*)                  ; keyword2 (builtins)
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Java / Go (SCLEX_CPP with language-specific keywords)
;;;============================================================================

(def *java-keywords*
  "abstract assert boolean break byte case catch char class const continue
   default do double else enum extends final finally float for goto if
   implements import instanceof int interface long native new package private
   protected public return short static strictfp super switch synchronized
   this throw throws transient try void volatile while")

(def *java-builtins*
  "String System Integer Boolean Character Double Float Long Short Byte
   Object Class Thread Runnable Exception RuntimeException Error ArrayList
   HashMap HashSet LinkedList Iterator Collections Arrays Math Override
   Deprecated SuppressWarnings")

(def *go-keywords*
  "break case chan const continue default defer else fallthrough for func go
   goto if import interface map package range return select struct switch
   type var")

(def *go-builtins*
  "append cap close complex copy delete imag len make new panic print
   println real recover bool byte complex64 complex128 error float32 float64
   int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64
   uintptr true false nil iota")

(def (setup-c-family-highlighting! ed lang)
  "Configure C++ lexer for Java or Go."
  (editor-set-lexer-language ed "cpp")
  (case lang
    ((java)
     (editor-set-keywords ed 0 *java-keywords*)
     (editor-set-keywords ed 1 *java-builtins*))
    ((go)
     (editor-set-keywords ed 0 *go-keywords*)
     (editor-set-keywords ed 1 *go-builtins*)))
  (style-set! ed 0  *theme-fg*)
  (style-set! ed 1  *theme-comment* #f #t)
  (style-set! ed 2  *theme-comment* #f #t)
  (style-set! ed 3  *theme-comment* #f #t)
  (style-set! ed 4  *theme-number*)
  (style-set! ed 5  *theme-keyword* #t)
  (style-set! ed 6  *theme-string*)
  (style-set! ed 7  *theme-string*)
  (style-set! ed 10 *theme-operator*)
  (style-set! ed 11 *theme-fg*)
  (style-set! ed 16 *theme-type*)
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; HTML (SCLEX_HTML)
;;;============================================================================

(def (setup-html-highlighting! ed)
  (editor-set-lexer-language ed "hypertext")
  ;; HTML tag styles
  (style-set! ed 1  *theme-tag*)                   ; tag
  (style-set! ed 2  *theme-tag*)                   ; tag unknown
  (style-set! ed 3  *theme-attribute*)              ; attribute
  (style-set! ed 4  *theme-attribute*)              ; attribute unknown
  (style-set! ed 5  *theme-number*)                ; number
  (style-set! ed 6  *theme-string*)                ; double string
  (style-set! ed 7  *theme-string*)                ; single string
  (style-set! ed 8  *theme-keyword* #t)            ; other
  (style-set! ed 9  *theme-comment* #f #t)         ; comment
  (style-set! ed 17 *theme-fg*)                    ; CDATA
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; CSS (SCLEX_CSS)
;;;============================================================================

(def *css-keywords*
  "color background background-color font-size font-family font-weight
   margin padding border display position width height top left right bottom
   float clear text-align text-decoration line-height overflow visibility
   z-index opacity flex grid align-items justify-content cursor transition
   transform animation box-shadow border-radius content")

(def (setup-css-highlighting! ed)
  (editor-set-lexer-language ed "css")
  (editor-set-keywords ed 0 *css-keywords*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-tag*)                   ; tag
  (style-set! ed 2  *theme-type*)                  ; class
  (style-set! ed 3  *theme-type* #t)               ; pseudo class
  (style-set! ed 5  *theme-operator*)              ; operator
  (style-set! ed 6  *theme-keyword* #t)            ; property
  (style-set! ed 7  *theme-attribute*)              ; unknown property
  (style-set! ed 9  *theme-comment* #f #t)         ; comment
  (style-set! ed 13 *theme-string*)                ; double string
  (style-set! ed 14 *theme-string*)                ; single string
  (style-set! ed 16 *theme-number*)                ; number
  (style-set! ed 17 *theme-function*)              ; function
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; JSON (SCLEX_JSON)
;;;============================================================================

(def (setup-json-highlighting! ed)
  (editor-set-lexer-language ed "json")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-number*)                ; number
  (style-set! ed 2  *theme-string*)                ; string
  (style-set! ed 3  *theme-string*)                ; string eol
  (style-set! ed 4  *theme-type*)                  ; property name
  (style-set! ed 5  *theme-fg*)                    ; escape sequence
  (style-set! ed 6  *theme-comment* #f #t)         ; line comment
  (style-set! ed 7  *theme-comment* #f #t)         ; block comment
  (style-set! ed 8  *theme-operator*)              ; operator
  (style-set! ed 9  *theme-fg*)                    ; URI
  (style-set! ed 10 *theme-fg*)                    ; compact IRI
  (style-set! ed 11 *theme-keyword* #t)            ; keyword
  (style-set! ed 12 *theme-keyword* #t)            ; LD keyword
  (style-set! ed 13 *theme-error*)                 ; error
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; YAML (SCLEX_YAML)
;;;============================================================================

(def (setup-yaml-highlighting! ed)
  (editor-set-lexer-language ed "yaml")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-type* #t)               ; identifier / key
  (style-set! ed 3  *theme-keyword* #t)            ; keyword
  (style-set! ed 4  *theme-number*)                ; number
  (style-set! ed 5  *theme-type*)                  ; reference
  (style-set! ed 6  *theme-fg*)                    ; document
  (style-set! ed 7  *theme-string*)                ; text
  (style-set! ed 8  *theme-error*)                 ; error
  (style-set! ed 9  *theme-operator*)              ; operator
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; TOML (SCLEX_TOML)
;;;============================================================================

(def (setup-toml-highlighting! ed)
  (editor-set-lexer-language ed "toml")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-type* #t)               ; key
  (style-set! ed 3  *theme-type* #t)               ; section / table
  (style-set! ed 4  *theme-keyword* #t)            ; assignment
  (style-set! ed 5  *theme-string*)                ; string
  (style-set! ed 6  *theme-number*)                ; number
  (style-set! ed 7  *theme-keyword* #t)            ; boolean
  (style-set! ed 8  *theme-number*)                ; datetime
  (style-set! ed 9  *theme-string*)                ; triple string
  (style-set! ed 10 *theme-error*)                 ; error
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Markdown (SCLEX_MARKDOWN)
;;;============================================================================

(def (setup-markdown-highlighting! ed)
  (editor-set-lexer-language ed "markdown")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-heading* #t)            ; line begin
  (style-set! ed 2  *theme-keyword* #t)            ; strong1
  (style-set! ed 3  *theme-keyword* #t)            ; strong2
  (style-set! ed 4  *theme-fg* #f #t)              ; em1
  (style-set! ed 5  *theme-fg* #f #t)              ; em2
  (style-set! ed 6  *theme-heading* #t)            ; header1
  (style-set! ed 7  *theme-heading* #t)            ; header2
  (style-set! ed 8  *theme-heading*)               ; header3
  (style-set! ed 9  *theme-heading*)               ; header4
  (style-set! ed 10 *theme-heading*)               ; header5
  (style-set! ed 11 *theme-heading*)               ; header6
  (style-set! ed 12 *theme-type*)                  ; prechar
  (style-set! ed 13 *theme-fg*)                    ; ulist_item
  (style-set! ed 14 *theme-fg*)                    ; olist_item
  (style-set! ed 15 *theme-number*)                ; blockquote
  (style-set! ed 16 *theme-error*)                 ; strikeout
  (style-set! ed 17 *theme-number*)                ; hrule
  (style-set! ed 18 *theme-function*)              ; link
  (style-set! ed 19 *theme-string*)                ; code
  (style-set! ed 20 *theme-string*)                ; code2
  (style-set! ed 21 *theme-string*)                ; codeblock
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Bash / Shell (SCLEX_BASH)
;;;============================================================================

(def *bash-keywords*
  "if then else elif fi case esac for while until do done in function select
   time coproc return exit break continue declare typeset local export
   readonly unset shift trap eval exec source")

(def *bash-builtins*
  "echo printf read cd pwd pushd popd dirs let alias unalias type hash
   ulimit umask set shopt enable help history fc jobs fg bg wait kill
   disown suspend logout test true false command builtin caller getopts
   mapfile readarray compgen complete compopt")

(def (setup-bash-highlighting! ed)
  (editor-set-lexer-language ed "bash")
  (editor-set-keywords ed 0 *bash-keywords*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-error*)                 ; error
  (style-set! ed 2  *theme-comment* #f #t)         ; comment
  (style-set! ed 3  *theme-number*)                ; number
  (style-set! ed 4  *theme-keyword* #t)            ; keyword
  (style-set! ed 5  *theme-string*)                ; double string
  (style-set! ed 6  *theme-string*)                ; single string
  (style-set! ed 7  *theme-operator*)              ; operator
  (style-set! ed 8  *theme-type*)                  ; identifier
  (style-set! ed 9  *theme-function*)              ; scalar var $x
  (style-set! ed 10 *theme-function*)              ; param expansion
  (style-set! ed 11 *theme-string*)                ; backtick
  (style-set! ed 12 *theme-string*)                ; here delim
  (style-set! ed 13 *theme-string*)                ; here q
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Ruby (SCLEX_RUBY)
;;;============================================================================

(def *ruby-keywords*
  "BEGIN END __ENCODING__ __END__ __FILE__ __LINE__ __method__ alias and
   begin break case class def defined? do else elsif end ensure false for
   if in module next nil not or redo rescue retry return self super then
   true undef unless until when while yield")

(def (setup-ruby-highlighting! ed)
  (editor-set-lexer-language ed "ruby")
  (editor-set-keywords ed 0 *ruby-keywords*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-error*)                 ; error
  (style-set! ed 2  *theme-comment* #f #t)         ; comment
  (style-set! ed 3  *theme-comment* #f #t)         ; POD
  (style-set! ed 4  *theme-number*)                ; number
  (style-set! ed 5  *theme-keyword* #t)            ; keyword
  (style-set! ed 6  *theme-string*)                ; string
  (style-set! ed 7  *theme-string*)                ; character
  (style-set! ed 8  *theme-type*)                  ; classname
  (style-set! ed 9  *theme-function*)              ; defname
  (style-set! ed 10 *theme-operator*)              ; operator
  (style-set! ed 11 *theme-type*)                  ; identifier
  (style-set! ed 12 *theme-string*)                ; regex
  (style-set! ed 13 *theme-function*)              ; global
  (style-set! ed 14 *theme-type*)                  ; symbol
  (style-set! ed 15 *theme-attribute*)              ; module name
  (style-set! ed 16 *theme-function*)              ; instance var
  (style-set! ed 17 *theme-function*)              ; class var
  (style-set! ed 18 *theme-string*)                ; backticks
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Rust (SCLEX_RUST)
;;;============================================================================

(def *rust-keywords*
  "as async await break const continue crate dyn else enum extern false fn
   for if impl in let loop match mod move mut pub ref return self Self
   static struct super trait true type unsafe use where while")

(def *rust-builtins*
  "bool char f32 f64 i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize
   str String Vec Option Result Box Some None Ok Err println eprintln
   format vec panic assert assert_eq assert_ne todo unimplemented unreachable")

(def (setup-rust-highlighting! ed)
  (editor-set-lexer-language ed "rust")
  (editor-set-keywords ed 0 *rust-keywords*)
  (editor-set-keywords ed 1 *rust-builtins*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment block
  (style-set! ed 2  *theme-comment* #f #t)         ; comment line
  (style-set! ed 3  *theme-comment* #f #t)         ; comment block doc
  (style-set! ed 4  *theme-comment* #f #t)         ; comment line doc
  (style-set! ed 5  *theme-keyword* #t)            ; word
  (style-set! ed 6  *theme-type*)                  ; word2
  (style-set! ed 7  *theme-number*)                ; number
  (style-set! ed 8  *theme-string*)                ; string
  (style-set! ed 9  *theme-string*)                ; string raw
  (style-set! ed 10 *theme-string*)                ; character
  (style-set! ed 11 *theme-operator*)              ; operator
  (style-set! ed 12 *theme-type*)                  ; identifier
  (style-set! ed 13 *theme-attribute*)              ; lifetime
  (style-set! ed 14 *theme-string*)                ; macro
  (style-set! ed 15 *theme-string*)                ; byte string
  (style-set! ed 16 *theme-string*)                ; byte string raw
  (style-set! ed 17 *theme-string*)                ; byte char
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Lua (SCLEX_LUA)
;;;============================================================================

(def *lua-keywords*
  "and break do else elseif end false for function goto if in local nil not
   or repeat return then true until while")

(def *lua-builtins*
  "assert collectgarbage dofile error getmetatable ipairs load loadfile next
   pairs pcall print rawequal rawget rawlen rawset require select setmetatable
   tonumber tostring type xpcall string table math io os coroutine debug utf8")

(def (setup-lua-highlighting! ed)
  (editor-set-lexer-language ed "lua")
  (editor-set-keywords ed 0 *lua-keywords*)
  (editor-set-keywords ed 1 *lua-builtins*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-comment* #f #t)         ; comment line
  (style-set! ed 3  *theme-comment* #f #t)         ; comment doc
  (style-set! ed 4  *theme-number*)                ; number
  (style-set! ed 5  *theme-keyword* #t)            ; keyword
  (style-set! ed 6  *theme-string*)                ; string
  (style-set! ed 7  *theme-string*)                ; character
  (style-set! ed 8  *theme-string*)                ; literal string
  (style-set! ed 9  *theme-function*)              ; preprocessor
  (style-set! ed 10 *theme-operator*)              ; operator
  (style-set! ed 11 *theme-fg*)                    ; identifier
  (style-set! ed 12 *theme-string*)                ; string eol
  (style-set! ed 13 *theme-type*)                  ; keyword2 (builtins)
  (style-set! ed 14 *theme-type*)                  ; keyword3
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; SQL (SCLEX_SQL)
;;;============================================================================

(def *sql-keywords*
  "select from where insert into values update set delete create table alter
   drop index view join inner outer left right cross on as and or not null
   is in between like exists having group by order asc desc limit offset
   union all distinct case when then else end primary key foreign references
   default constraint unique check grant revoke begin commit rollback
   transaction declare cursor fetch close trigger procedure function returns
   return if while for each row execute call")

(def (setup-sql-highlighting! ed)
  (editor-set-lexer-language ed "sql")
  (editor-set-keywords ed 0 *sql-keywords*)
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-comment* #f #t)         ; comment line
  (style-set! ed 3  *theme-comment* #f #t)         ; comment doc
  (style-set! ed 4  *theme-number*)                ; number
  (style-set! ed 5  *theme-keyword* #t)            ; keyword
  (style-set! ed 6  *theme-string*)                ; string
  (style-set! ed 7  *theme-string*)                ; character
  (style-set! ed 10 *theme-operator*)              ; operator
  (style-set! ed 11 *theme-fg*)                    ; identifier
  (style-set! ed 12 *theme-string*)                ; string eol
  (style-set! ed 16 *theme-type*)                  ; keyword2
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Makefile (SCLEX_MAKEFILE)
;;;============================================================================

(def (setup-makefile-highlighting! ed)
  (editor-set-lexer-language ed "makefile")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-function*)              ; preprocessor
  (style-set! ed 3  *theme-type* #t)               ; identifier / variable
  (style-set! ed 4  *theme-operator*)              ; operator
  (style-set! ed 5  *theme-keyword* #t)            ; target
  (style-set! ed 9  *theme-error*)                 ; error
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; Diff (SCLEX_DIFF)
;;;============================================================================

(def (setup-diff-highlighting! ed)
  (editor-set-lexer-language ed "diff")
  (style-set! ed 0  *theme-fg*)                    ; default
  (style-set! ed 1  *theme-comment* #f #t)         ; comment
  (style-set! ed 2  *theme-keyword* #t)            ; command
  (style-set! ed 3  *theme-heading* #t)            ; header
  (style-set! ed 4  *theme-type* #t)               ; position
  (style-set! ed 5  *theme-removed*)               ; deleted
  (style-set! ed 6  *theme-added*)                 ; added
  (style-set! ed 7  *theme-fg*)                    ; changed
  (editor-colourise ed 0 -1))

;;;============================================================================
;;; File extension detection and multi-language dispatcher
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

(def (detect-language-from-shebang text)
  "Detect language from a shebang line (first line starting with #!).
   Returns a symbol or #f."
  (and text
       (> (string-length text) 2)
       (char=? (string-ref text 0) #\#)
       (char=? (string-ref text 1) #\!)
       (let* ((nl (string-index text #\newline))
              (line (if nl (substring text 0 nl) text)))
         (cond
           ((or (string-contains line "/bash")
                (string-contains line "/sh")
                (string-contains line "/zsh")
                (string-contains line "/ksh")
                (string-contains line "/ash")
                (string-contains line "/dash"))
            'bash)
           ((string-contains line "python") 'python)
           ((string-contains line "ruby") 'ruby)
           ((string-contains line "perl") 'perl)
           ((string-contains line "node") 'javascript)
           ((string-contains line "lua") 'lua)
           (else 'bash)))))  ; default shebang = shell script

(def (detect-file-language path)
  "Detect language from file extension or filename. Returns a symbol or #f."
  (and path
       (let ((ext (path-extension path))
             (base (path-strip-directory path)))
         (cond
           ((or (not ext) (string=? ext ""))
            ;; Check full filename for extensionless files
            (cond
              ((member base '("Makefile" "makefile" "GNUmakefile")) 'makefile)
              ((member base '("Dockerfile" "Containerfile")) 'bash)
              ((member base '("Rakefile" "Gemfile")) 'ruby)
              ((member base '("CMakeLists.txt")) 'makefile)
              ((member base '("Vagrantfile")) 'ruby)
              (else #f)))
           ;; Scheme / Gerbil
           ((member ext '(".ss" ".scm" ".sld" ".sls" ".rkt")) 'scheme)
           ;; C / C++
           ((member ext '(".c" ".h" ".cpp" ".hpp" ".cc" ".cxx" ".hh" ".hxx" ".ino")) 'c)
           ;; Python
           ((member ext '(".py" ".pyw" ".pyi")) 'python)
           ;; JavaScript
           ((member ext '(".js" ".jsx" ".mjs" ".cjs")) 'javascript)
           ;; TypeScript
           ((member ext '(".ts" ".tsx" ".mts" ".cts")) 'typescript)
           ;; HTML
           ((member ext '(".html" ".htm" ".xhtml" ".svelte" ".vue")) 'html)
           ;; CSS
           ((member ext '(".css" ".scss" ".sass" ".less")) 'css)
           ;; JSON
           ((member ext '(".json" ".jsonl" ".jsonc")) 'json)
           ;; YAML
           ((member ext '(".yaml" ".yml")) 'yaml)
           ;; TOML
           ((string=? ext ".toml") 'toml)
           ;; Markdown
           ((member ext '(".md" ".markdown" ".mkd" ".rst")) 'markdown)
           ;; Shell / Bash
           ((member ext '(".sh" ".bash" ".zsh" ".ksh" ".fish")) 'bash)
           ;; Ruby
           ((member ext '(".rb" ".rake" ".gemspec" ".erb")) 'ruby)
           ;; Rust
           ((string=? ext ".rs") 'rust)
           ;; Go
           ((string=? ext ".go") 'go)
           ;; Java
           ((member ext '(".java" ".kt" ".kts" ".scala")) 'java)
           ;; Lua
           ((string=? ext ".lua") 'lua)
           ;; SQL
           ((string=? ext ".sql") 'sql)
           ;; Perl
           ((member ext '(".pl" ".pm" ".t")) 'perl)
           ;; Haskell
           ((member ext '(".hs" ".lhs")) 'haskell)
           ;; Elixir / Erlang
           ((member ext '(".ex" ".exs" ".erl" ".hrl")) 'elixir)
           ;; Swift
           ((string=? ext ".swift") 'swift)
           ;; Zig
           ((string=? ext ".zig") 'c)
           ;; Nim
           ((string=? ext ".nim") 'python)  ; similar indentation style
           ;; R
           ((string=? ext ".r") 'python)  ; similar commenting style
           ;; Makefile
           ((string=? ext ".mk") 'makefile)
           ;; XML
           ((member ext '(".xml" ".xsl" ".xsd" ".plist" ".svg")) 'html)
           ;; Conf / INI
           ((member ext '(".ini" ".conf" ".cfg" ".properties")) 'toml)
           ;; Dockerfile
           ((member ext '(".dockerfile")) 'bash)
           ;; Diff / Patch
           ((member ext '(".diff" ".patch")) 'diff)
           (else #f)))))

(def (enable-code-file-features! ed)
  "Enable folding, indent guides, and column indicator for code files."
  ;; Fold properties
  (editor-set-property ed "fold" "1")
  (editor-set-property ed "fold.compact" "0")
  (editor-set-property ed "fold.comment" "1")
  ;; Set up fold margin (margin 2, symbol type)
  (send-message ed SCI_SETMARGINTYPEN 2 SC_MARGIN_SYMBOL)
  (send-message ed SCI_SETMARGINWIDTHN 2 14)
  (send-message ed SCI_SETMARGINMASKN 2 SC_MASK_FOLDERS)
  (send-message ed SCI_SETMARGINSENSITIVEN 2 1)
  ;; Fold markers: box tree style
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPEN SC_MARK_BOXMINUS)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDER SC_MARK_BOXPLUS)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERSUB SC_MARK_VLINE)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERTAIL SC_MARK_LCORNER)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEREND SC_MARK_BOXPLUSCONNECTED)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPENMID SC_MARK_BOXMINUSCONNECTED)
  (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERMIDTAIL SC_MARK_TCORNER)
  ;; Fold marker colors (dark theme)
  (send-message ed SCI_MARKERSETFORE SC_MARKNUM_FOLDER #x808080)
  (send-message ed SCI_MARKERSETBACK SC_MARKNUM_FOLDER #x282828)
  (send-message ed SCI_MARKERSETFORE SC_MARKNUM_FOLDEROPEN #x808080)
  (send-message ed SCI_MARKERSETBACK SC_MARKNUM_FOLDEROPEN #x282828)
  ;; Auto-fold on margin clicks
  (send-message ed SCI_SETAUTOMATICFOLD 7 0)  ;; SC_AUTOMATICFOLD_SHOW|CLICK|CHANGE
  ;; Indent guides (SC_IV_LOOKBOTH = 3)
  (send-message ed SCI_SETINDENTATIONGUIDES 3 0)
  ;; Column indicator at column 80 (SCI_SETEDGEMODE=2363 EDGE_LINE=1, SCI_SETEDGECOLUMN=2361)
  (send-message ed 2363 1 0)
  (send-message ed 2361 80 0)
  ;; Edge color: subtle dark line
  (send-message ed 2364 #x333333 0))  ;; SCI_SETEDGECOLOUR

(def (setup-highlighting-for-file! ed filename)
  "Set up syntax highlighting based on file extension.
   Dispatches to the appropriate language-specific setup."
  (let ((lang (detect-file-language filename)))
    (case lang
      ((scheme) (setup-gerbil-highlighting! ed))
      ((c)      (setup-c-highlighting! ed))
      ((python) (setup-python-highlighting! ed))
      ((javascript typescript) (setup-js-highlighting! ed lang))
      ((java go swift elixir haskell perl) (setup-c-family-highlighting! ed lang))
      ((html)     (setup-html-highlighting! ed))
      ((css)      (setup-css-highlighting! ed))
      ((json)     (setup-json-highlighting! ed))
      ((yaml)     (setup-yaml-highlighting! ed))
      ((toml)     (setup-toml-highlighting! ed))
      ((markdown) (setup-markdown-highlighting! ed))
      ((bash)     (setup-bash-highlighting! ed))
      ((ruby)     (setup-ruby-highlighting! ed))
      ((rust)     (setup-rust-highlighting! ed))
      ((lua)      (setup-lua-highlighting! ed))
      ((sql)      (setup-sql-highlighting! ed))
      ((makefile)  (setup-makefile-highlighting! ed))
      ((diff)     (setup-diff-highlighting! ed))
      (else (void)))
    ;; Enable folding, indent guides, and column indicator for code files
    (when lang
      (enable-code-file-features! ed))))
