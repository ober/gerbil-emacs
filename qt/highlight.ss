;;; -*- Gerbil -*-
;;; Qt syntax highlighting for gerbil-emacs
;;;
;;; Configurable QSyntaxHighlighter-based highlighting for multiple languages.
;;; Detects language from file extension and applies regex-based rules.

(export qt-setup-highlighting!
        qt-remove-highlighting!
        qt-update-visual-decorations!
        qt-highlight-search-matches!
        qt-clear-search-highlights!
        *search-highlight-active*)

(import :std/sugar
        :std/srfi/13
        :gerbil-qt/qt
        :gerbil-emacs/core)

;;;============================================================================
;;; Highlighter lifecycle tracking
;;;============================================================================

(def *qt-highlighters* (make-hash-table))

;;;============================================================================
;;; Color constants (matching TUI dark theme from highlight.ss)
;;;============================================================================

;; Keywords: purple, bold
(def kw-r #xcc) (def kw-g #x99) (def kw-b #xcc)
;; Builtins: cyan
(def bi-r #x66) (def bi-g #xcc) (def bi-b #xcc)
;; Strings: green
(def st-r #x99) (def st-g #xcc) (def st-b #x99)
;; Comments: gray, italic
(def cm-r #x99) (def cm-g #x99) (def cm-b #x99)
;; Numbers: orange
(def nm-r #xf9) (def nm-g #x91) (def nm-b #x57)
;; Operators/special: light gray
(def op-r #xb8) (def op-g #xb8) (def op-b #xb8)
;; Headings: blue, bold
(def hd-r #x66) (def hd-g #x99) (def hd-b #xcc)
;; Preprocessor: orange
(def pp-r #xf9) (def pp-g #x91) (def pp-b #x57)
;; Types: yellow
(def ty-r #xff) (def ty-g #xcc) (def ty-b #x66)

;;;============================================================================
;;; File extension → language detection
;;;============================================================================

(def (detect-language path)
  (and path
    (let ((ext (path-extension path)))
      (cond
        ((member ext '(".ss" ".scm" ".sld" ".sls" ".rkt")) 'scheme)
        ((member ext '(".c" ".h" ".cpp" ".hpp" ".cc" ".cxx")) 'c)
        ((member ext '(".py" ".pyw")) 'python)
        ((member ext '(".js" ".jsx" ".ts" ".tsx" ".mjs")) 'javascript)
        ((member ext '(".org")) 'org)
        ((member ext '(".md" ".markdown")) 'markdown)
        ((member ext '(".sh" ".bash" ".zsh" ".fish")) 'shell)
        ((member ext '(".rb" ".rake")) 'ruby)
        ((member ext '(".rs")) 'rust)
        ((member ext '(".go")) 'go)
        ((member ext '(".java")) 'java)
        ((member ext '(".json")) 'json)
        ((member ext '(".yaml" ".yml")) 'yaml)
        ((member ext '(".toml")) 'toml)
        ((member ext '(".xml" ".html" ".htm" ".svg")) 'xml)
        ((member ext '(".css" ".scss")) 'css)
        ((member ext '(".sql")) 'sql)
        ((member ext '(".el" ".lisp" ".cl")) 'lisp)
        ((member ext '(".lua")) 'lua)
        ((member ext '(".zig")) 'zig)
        ((member ext '(".nix")) 'nix)
        (else #f)))))

;;;============================================================================
;;; Gerbil/Scheme keyword lists (reused from highlight.ss)
;;;============================================================================

(def *gerbil-keywords*
  (string-join
    '("def" "defvalues" "defalias" "defsyntax" "defrule" "defrules"
      "defstruct" "defclass" "defmethod" "defgeneric" "deftype"
      "defmessage" "definline" "defconst" "defcall-actor" "defproto"
      "deferror-class" "defapi" "deftyped"
      "if" "when" "unless" "cond" "case" "case-lambda"
      "match" "match*" "with" "with*"
      "begin" "begin0" "begin-syntax" "begin-annotation"
      "begin-foreign" "begin-ffi"
      "let" "let*" "letrec" "letrec*" "let-values" "letrec-values"
      "let-syntax" "letrec-syntax" "let-hash" "let/cc" "let/esc"
      "rec" "alet" "alet*" "awhen"
      "lambda" "lambda%"
      "import" "export" "declare" "include" "module" "extern"
      "require" "provide" "cond-expand"
      "set!" "apply" "eval"
      "and" "or" "not"
      "try" "catch" "finally" "error" "raise"
      "unwind-protect" "with-destroy" "guard"
      "syntax-case" "ast-case" "ast-rules"
      "with-syntax" "with-syntax*"
      "for" "for*" "for/collect" "for/fold" "while" "until"
      "for-each" "map" "foldl" "foldr"
      "spawn" "spawn*" "spawn/name" "spawn/group"
      "sync" "wait"
      "quote" "quasiquote" "unquote" "unquote-splicing"
      "parameterize" "parameterize*" "using" "chain" "is"
      "interface" "with-interface"
      "test-suite" "test-case" "check" "run-tests!")
    " "))

(def *gerbil-builtins*
  (string-join
    '("cons" "car" "cdr" "caar" "cadr" "cdar" "cddr"
      "list" "list?" "null?" "pair?" "append" "reverse" "length"
      "assoc" "assq" "assv" "member" "memq" "memv"
      "vector" "vector-ref" "vector-set!" "vector-length"
      "make-vector" "vector->list" "list->vector"
      "string" "string-ref" "string-length" "string-append"
      "substring" "string->list" "list->string"
      "number?" "string?" "symbol?" "boolean?" "char?"
      "integer?" "real?" "zero?" "positive?" "negative?"
      "eq?" "eqv?" "equal?"
      "display" "write" "newline" "read" "read-line"
      "open-input-file" "open-output-file" "close-port"
      "current-input-port" "current-output-port"
      "make-hash-table" "hash-table?" "hash-get" "hash-put!"
      "hash-remove!" "hash-ref" "hash-key?" "hash-keys"
      "hash-values" "hash-for-each" "hash-map" "hash-copy"
      "void?" "procedure?" "fixnum?" "flonum?"
      "number->string" "string->number"
      "symbol->string" "string->symbol"
      "file-exists?" "delete-file" "rename-file"
      "directory-files" "create-directory"
      "current-directory" "path-expand" "path-directory"
      "path-strip-directory" "path-extension"
      "filter" "sort" "iota" "void" "raise-type-error")
    " "))

;;;============================================================================
;;; Language rule setup functions
;;;============================================================================

;;; --- Scheme / Lisp ---

(def (setup-scheme-rules! h)
  ;; Keywords (purple, bold)
  (qt-syntax-highlighter-add-keywords! h *gerbil-keywords*
    kw-r kw-g kw-b #t #f)
  ;; Builtins (cyan)
  (qt-syntax-highlighter-add-keywords! h *gerbil-builtins*
    bi-r bi-g bi-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h ";.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Hex numbers
  (qt-syntax-highlighter-add-rule! h "#x[0-9a-fA-F]+"
    nm-r nm-g nm-b #f #f)
  ;; Character literals
  (qt-syntax-highlighter-add-rule! h "#\\\\\\S+"
    st-r st-g st-b #f #f)
  ;; Booleans and special
  (qt-syntax-highlighter-add-rule! h "#[tf]\\b"
    hd-r hd-g hd-b #f #f)
  ;; Multi-line comments #| ... |#
  (qt-syntax-highlighter-add-multiline-rule! h "#\\|" "\\|#"
    cm-r cm-g cm-b #f #t))

;;; --- C / C++ ---

(def (setup-c-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("if" "else" "for" "while" "do" "switch" "case" "default"
        "break" "continue" "return" "goto" "struct" "union" "enum"
        "typedef" "sizeof" "static" "const" "volatile" "extern"
        "inline" "register" "auto" "signed" "unsigned"
        "class" "public" "private" "protected" "virtual" "override"
        "template" "typename" "namespace" "using" "new" "delete"
        "throw" "try" "catch" "noexcept" "constexpr" "nullptr"
        "true" "false" "this" "operator")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Types
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("int" "char" "float" "double" "void" "long" "short"
        "bool" "size_t" "ssize_t" "int8_t" "int16_t" "int32_t" "int64_t"
        "uint8_t" "uint16_t" "uint32_t" "uint64_t"
        "FILE" "NULL" "EOF")
      " ")
    ty-r ty-g ty-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Character literals
  (qt-syntax-highlighter-add-rule! h "'[^'\\\\]*(\\\\.[^'\\\\]*)*'"
    st-r st-g st-b #f #f)
  ;; Preprocessor
  (qt-syntax-highlighter-add-rule! h "^\\s*#\\s*\\w+"
    pp-r pp-g pp-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?[fFlLuU]*\\b"
    nm-r nm-g nm-b #f #f)
  ;; Hex numbers
  (qt-syntax-highlighter-add-rule! h "\\b0[xX][0-9a-fA-F]+[uUlL]*\\b"
    nm-r nm-g nm-b #f #f)
  ;; Block comments /* ... */
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Python ---

(def (setup-python-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("False" "None" "True" "and" "as" "assert" "async" "await"
        "break" "class" "continue" "def" "del" "elif" "else" "except"
        "finally" "for" "from" "global" "if" "import" "in" "is"
        "lambda" "nonlocal" "not" "or" "pass" "raise" "return"
        "try" "while" "with" "yield")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Builtins
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("print" "len" "range" "int" "str" "float" "list" "dict"
        "tuple" "set" "bool" "type" "isinstance" "issubclass"
        "open" "input" "map" "filter" "zip" "enumerate"
        "sorted" "reversed" "sum" "min" "max" "abs" "any" "all"
        "super" "property" "staticmethod" "classmethod"
        "hasattr" "getattr" "setattr" "delattr"
        "ValueError" "TypeError" "KeyError" "IndexError"
        "Exception" "RuntimeError" "StopIteration")
      " ")
    bi-r bi-g bi-b #f #f)
  ;; Comments
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings (double-quoted)
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Strings (single-quoted)
  (qt-syntax-highlighter-add-rule! h "'[^'\\\\]*(\\\\.[^'\\\\]*)*'"
    st-r st-g st-b #f #f)
  ;; Decorators
  (qt-syntax-highlighter-add-rule! h "@\\w+"
    pp-r pp-g pp-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; f-strings prefix
  (qt-syntax-highlighter-add-rule! h "\\bf\"" st-r st-g st-b #f #f)
  ;; Triple-quoted strings
  (qt-syntax-highlighter-add-multiline-rule! h "\"\"\"" "\"\"\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-multiline-rule! h "'''" "'''"
    st-r st-g st-b #f #f))

;;; --- JavaScript / TypeScript ---

(def (setup-javascript-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("function" "const" "let" "var" "if" "else" "for" "while"
        "do" "switch" "case" "default" "break" "continue" "return"
        "try" "catch" "finally" "throw" "new" "delete" "typeof"
        "instanceof" "in" "of" "class" "extends" "super" "this"
        "import" "export" "from" "as" "async" "await" "yield"
        "true" "false" "null" "undefined" "void"
        "interface" "type" "enum" "implements" "abstract"
        "public" "private" "protected" "readonly" "static")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Builtins
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("console" "Math" "JSON" "Object" "Array" "String" "Number"
        "Boolean" "Date" "RegExp" "Error" "Promise" "Map" "Set"
        "parseInt" "parseFloat" "isNaN" "isFinite"
        "setTimeout" "setInterval" "fetch" "require")
      " ")
    bi-r bi-g bi-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^'\\\\]*(\\\\.[^'\\\\]*)*'"
    st-r st-g st-b #f #f)
  ;; Template literals
  (qt-syntax-highlighter-add-rule! h "`[^`]*`"
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Block comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Org-mode ---

(def (setup-org-rules! h)
  ;; Headings (blue, bold)
  (qt-syntax-highlighter-add-rule! h "^\\*+ .*$"
    hd-r hd-g hd-b #t #f)
  ;; Directives (#+...)
  (qt-syntax-highlighter-add-rule! h "^#\\+.*$"
    cm-r cm-g cm-b #f #f)
  ;; Inline code ~code~ and =code=
  (qt-syntax-highlighter-add-rule! h "~[^~]+~"
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "=[^=]+="
    st-r st-g st-b #f #f)
  ;; Bold *text*
  (qt-syntax-highlighter-add-rule! h "\\*[^*]+\\*"
    op-r op-g op-b #t #f)
  ;; Italic /text/
  (qt-syntax-highlighter-add-rule! h "/[^/]+/"
    op-r op-g op-b #f #t)
  ;; Links [[...]]
  (qt-syntax-highlighter-add-rule! h "\\[\\[.*?\\]\\]"
    bi-r bi-g bi-b #f #f)
  ;; Comments (lines starting with #, not #+)
  (qt-syntax-highlighter-add-rule! h "^# [^+].*$"
    cm-r cm-g cm-b #f #t)
  ;; Source blocks
  (qt-syntax-highlighter-add-multiline-rule! h
    "^#\\+begin_src" "^#\\+end_src"
    st-r st-g st-b #f #f)
  ;; Example blocks
  (qt-syntax-highlighter-add-multiline-rule! h
    "^#\\+begin_example" "^#\\+end_example"
    st-r st-g st-b #f #f))

;;; --- Markdown ---

(def (setup-markdown-rules! h)
  ;; Headings (blue, bold)
  (qt-syntax-highlighter-add-rule! h "^#{1,6} .*$"
    hd-r hd-g hd-b #t #f)
  ;; Inline code
  (qt-syntax-highlighter-add-rule! h "`[^`]+`"
    st-r st-g st-b #f #f)
  ;; Bold **text**
  (qt-syntax-highlighter-add-rule! h "\\*\\*[^*]+\\*\\*"
    op-r op-g op-b #t #f)
  ;; Italic *text*
  (qt-syntax-highlighter-add-rule! h "\\*[^*]+\\*"
    op-r op-g op-b #f #t)
  ;; Links [text](url)
  (qt-syntax-highlighter-add-rule! h "\\[.*?\\]\\(.*?\\)"
    bi-r bi-g bi-b #f #f)
  ;; Blockquote
  (qt-syntax-highlighter-add-rule! h "^>.*$"
    cm-r cm-g cm-b #f #t)
  ;; List markers
  (qt-syntax-highlighter-add-rule! h "^\\s*[-*+] "
    op-r op-g op-b #t #f)
  ;; Fenced code blocks
  (qt-syntax-highlighter-add-multiline-rule! h "^```" "^```$"
    st-r st-g st-b #f #f))

;;; --- Shell ---

(def (setup-shell-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("if" "then" "else" "elif" "fi" "for" "do" "done"
        "while" "until" "case" "esac" "in" "function"
        "return" "exit" "break" "continue"
        "local" "export" "readonly" "declare" "typeset"
        "source" "eval" "exec" "trap" "shift" "set" "unset")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Comments
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^']*'"
    st-r st-g st-b #f #f)
  ;; Variables
  (qt-syntax-highlighter-add-rule! h "\\$\\{?\\w+"
    bi-r bi-g bi-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+\\b"
    nm-r nm-g nm-b #f #f))

;;; --- Rust ---

(def (setup-rust-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("fn" "let" "mut" "const" "static" "if" "else" "match"
        "for" "while" "loop" "break" "continue" "return"
        "struct" "enum" "impl" "trait" "type" "where"
        "pub" "mod" "use" "crate" "self" "super"
        "as" "in" "ref" "move" "unsafe" "async" "await"
        "true" "false" "Some" "None" "Ok" "Err")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Types
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("i8" "i16" "i32" "i64" "i128" "isize"
        "u8" "u16" "u32" "u64" "u128" "usize"
        "f32" "f64" "bool" "char" "str"
        "String" "Vec" "Box" "Rc" "Arc" "Option" "Result"
        "HashMap" "HashSet" "BTreeMap" "BTreeSet")
      " ")
    ty-r ty-g ty-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Macros (name!)
  (qt-syntax-highlighter-add-rule! h "\\b\\w+!"
    pp-r pp-g pp-b #f #f)
  ;; Attributes
  (qt-syntax-highlighter-add-rule! h "#\\[.*?\\]"
    pp-r pp-g pp-b #f #f)
  ;; Block comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Go ---

(def (setup-go-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("break" "case" "chan" "const" "continue" "default" "defer"
        "else" "fallthrough" "for" "func" "go" "goto" "if"
        "import" "interface" "map" "package" "range" "return"
        "select" "struct" "switch" "type" "var"
        "true" "false" "nil" "iota")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Types
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("int" "int8" "int16" "int32" "int64"
        "uint" "uint8" "uint16" "uint32" "uint64" "uintptr"
        "float32" "float64" "complex64" "complex128"
        "bool" "byte" "rune" "string" "error"
        "any" "comparable")
      " ")
    ty-r ty-g ty-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Raw strings
  (qt-syntax-highlighter-add-rule! h "`[^`]*`"
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Block comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Java ---

(def (setup-java-rules! h)
  ;; Keywords
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("abstract" "assert" "break" "case" "catch" "class" "const"
        "continue" "default" "do" "else" "enum" "extends" "final"
        "finally" "for" "goto" "if" "implements" "import"
        "instanceof" "interface" "native" "new" "package" "private"
        "protected" "public" "return" "static" "strictfp" "super"
        "switch" "synchronized" "this" "throw" "throws" "transient"
        "try" "void" "volatile" "while"
        "true" "false" "null")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Types
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("boolean" "byte" "char" "double" "float" "int" "long" "short"
        "String" "Object" "Integer" "Double" "Float" "Long"
        "List" "Map" "Set" "ArrayList" "HashMap" "HashSet")
      " ")
    ty-r ty-g ty-b #f #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?[fFdDlL]?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Annotations
  (qt-syntax-highlighter-add-rule! h "@\\w+"
    pp-r pp-g pp-b #f #f)
  ;; Block comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Ruby ---

(def (setup-ruby-rules! h)
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("def" "class" "module" "end" "if" "elsif" "else" "unless"
        "while" "until" "for" "do" "begin" "rescue" "ensure" "raise"
        "return" "yield" "break" "next" "redo" "retry"
        "and" "or" "not" "in" "then" "when" "case"
        "self" "super" "nil" "true" "false"
        "require" "require_relative" "include" "extend"
        "attr_reader" "attr_writer" "attr_accessor"
        "puts" "print" "p" "lambda" "proc" "block_given?")
      " ")
    kw-r kw-g kw-b #t #f)
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^'\\\\]*(\\\\.[^'\\\\]*)*'"
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h ":[a-zA-Z_]\\w*"
    bi-r bi-g bi-b #f #f)
  (qt-syntax-highlighter-add-rule! h "@\\w+"
    bi-r bi-g bi-b #f #f)
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f))

;;; --- JSON ---

(def (setup-json-rules! h)
  ;; Keys
  (qt-syntax-highlighter-add-rule! h "\"[^\"]*\"\\s*:"
    bi-r bi-g bi-b #f #f)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Booleans and null
  (qt-syntax-highlighter-add-keywords! h "true false null"
    kw-r kw-g kw-b #t #f))

;;; --- YAML ---

(def (setup-yaml-rules! h)
  ;; Keys
  (qt-syntax-highlighter-add-rule! h "^\\s*[\\w.-]+:"
    bi-r bi-g bi-b #f #f)
  ;; Comments
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^']*'"
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Booleans
  (qt-syntax-highlighter-add-keywords! h "true false yes no on off null"
    kw-r kw-g kw-b #t #f)
  ;; Anchors and aliases
  (qt-syntax-highlighter-add-rule! h "[&*]\\w+"
    pp-r pp-g pp-b #f #f))

;;; --- TOML ---

(def (setup-toml-rules! h)
  ;; Section headers
  (qt-syntax-highlighter-add-rule! h "^\\[.*\\]"
    hd-r hd-g hd-b #t #f)
  ;; Keys
  (qt-syntax-highlighter-add-rule! h "^\\s*[\\w.-]+\\s*="
    bi-r bi-g bi-b #f #f)
  ;; Comments
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Booleans
  (qt-syntax-highlighter-add-keywords! h "true false"
    kw-r kw-g kw-b #t #f))

;;; --- XML / HTML ---

(def (setup-xml-rules! h)
  ;; Tags
  (qt-syntax-highlighter-add-rule! h "</?\\w[^>]*>"
    bi-r bi-g bi-b #f #f)
  ;; Tag names
  (qt-syntax-highlighter-add-rule! h "</?\\w+"
    kw-r kw-g kw-b #t #f)
  ;; Attributes
  (qt-syntax-highlighter-add-rule! h "\\b\\w+="
    bi-r bi-g bi-b #f #f)
  ;; Strings in attributes
  (qt-syntax-highlighter-add-rule! h "\"[^\"]*\""
    st-r st-g st-b #f #f)
  ;; Comments
  (qt-syntax-highlighter-add-multiline-rule! h "<!--" "-->"
    cm-r cm-g cm-b #f #t))

;;; --- CSS ---

(def (setup-css-rules! h)
  ;; Selectors (simplified)
  (qt-syntax-highlighter-add-rule! h "[.#]?\\w[\\w-]*\\s*\\{"
    kw-r kw-g kw-b #t #f)
  ;; Properties
  (qt-syntax-highlighter-add-rule! h "\\b[a-z-]+:"
    bi-r bi-g bi-b #f #f)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"]*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^']*'"
    st-r st-g st-b #f #f)
  ;; Numbers and units
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?(px|em|rem|%|vh|vw|pt|cm|mm)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Colors
  (qt-syntax-highlighter-add-rule! h "#[0-9a-fA-F]{3,8}\\b"
    nm-r nm-g nm-b #f #f)
  ;; Comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- SQL ---

(def (setup-sql-rules! h)
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("SELECT" "FROM" "WHERE" "INSERT" "INTO" "VALUES" "UPDATE" "SET"
        "DELETE" "CREATE" "DROP" "ALTER" "TABLE" "INDEX" "VIEW"
        "JOIN" "INNER" "LEFT" "RIGHT" "OUTER" "ON" "AS"
        "AND" "OR" "NOT" "IN" "IS" "NULL" "LIKE" "BETWEEN"
        "ORDER" "BY" "GROUP" "HAVING" "LIMIT" "OFFSET"
        "UNION" "ALL" "DISTINCT" "EXISTS" "CASE" "WHEN" "THEN" "ELSE" "END"
        "BEGIN" "COMMIT" "ROLLBACK" "TRANSACTION"
        "PRIMARY" "KEY" "FOREIGN" "REFERENCES" "UNIQUE" "CHECK" "DEFAULT"
        "INTEGER" "TEXT" "REAL" "BLOB" "VARCHAR" "CHAR" "BOOLEAN" "DATE"
        "COUNT" "SUM" "AVG" "MIN" "MAX"
        ;; lowercase too
        "select" "from" "where" "insert" "into" "values" "update" "set"
        "delete" "create" "drop" "alter" "table" "index" "view"
        "join" "inner" "left" "right" "outer" "on" "as"
        "and" "or" "not" "in" "is" "null" "like" "between"
        "order" "by" "group" "having" "limit" "offset")
      " ")
    kw-r kw-g kw-b #t #f)
  ;; Line comments
  (qt-syntax-highlighter-add-rule! h "--.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "'[^']*'"
    st-r st-g st-b #f #f)
  ;; Numbers
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  ;; Block comments
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Lua ---

(def (setup-lua-rules! h)
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("and" "break" "do" "else" "elseif" "end" "false" "for"
        "function" "goto" "if" "in" "local" "nil" "not" "or"
        "repeat" "return" "then" "true" "until" "while")
      " ")
    kw-r kw-g kw-b #t #f)
  (qt-syntax-highlighter-add-rule! h "--.*$"
    cm-r cm-g cm-b #f #t)
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^'\\\\]*(\\\\.[^'\\\\]*)*'"
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  (qt-syntax-highlighter-add-multiline-rule! h "--\\[\\[" "\\]\\]"
    cm-r cm-g cm-b #f #t))

;;; --- Zig ---

(def (setup-zig-rules! h)
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("fn" "pub" "const" "var" "if" "else" "while" "for"
        "break" "continue" "return" "switch" "unreachable"
        "struct" "enum" "union" "error" "try" "catch"
        "defer" "errdefer" "comptime" "inline" "export"
        "test" "undefined" "null" "true" "false"
        "and" "or" "orelse" "async" "await" "suspend" "resume")
      " ")
    kw-r kw-g kw-b #t #f)
  (qt-syntax-highlighter-add-rule! h "//.*$"
    cm-r cm-g cm-b #f #t)
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+(\\.[0-9]+)?\\b"
    nm-r nm-g nm-b #f #f)
  (qt-syntax-highlighter-add-rule! h "@\\w+"
    pp-r pp-g pp-b #f #f))

;;; --- Nix ---

(def (setup-nix-rules! h)
  (qt-syntax-highlighter-add-keywords! h
    (string-join
      '("let" "in" "with" "rec" "if" "then" "else" "assert"
        "inherit" "import" "true" "false" "null"
        "or" "and" "builtins")
      " ")
    kw-r kw-g kw-b #t #f)
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  (qt-syntax-highlighter-add-rule! h "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "''.+?''"
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "\\b[0-9]+\\b"
    nm-r nm-g nm-b #f #f)
  (qt-syntax-highlighter-add-multiline-rule! h "/\\*" "\\*/"
    cm-r cm-g cm-b #f #t))

;;; --- Makefile ---

(def (setup-makefile-rules! h)
  ;; Targets
  (qt-syntax-highlighter-add-rule! h "^[\\w.-]+:"
    hd-r hd-g hd-b #t #f)
  ;; Variables
  (qt-syntax-highlighter-add-rule! h "\\$[({]\\w+[)}]"
    bi-r bi-g bi-b #f #f)
  (qt-syntax-highlighter-add-rule! h "\\$\\w"
    bi-r bi-g bi-b #f #f)
  ;; Variable assignments
  (qt-syntax-highlighter-add-rule! h "^\\w+\\s*[:+?]?="
    kw-r kw-g kw-b #t #f)
  ;; Comments
  (qt-syntax-highlighter-add-rule! h "#.*$"
    cm-r cm-g cm-b #f #t)
  ;; Strings
  (qt-syntax-highlighter-add-rule! h "\"[^\"]*\""
    st-r st-g st-b #f #f)
  (qt-syntax-highlighter-add-rule! h "'[^']*'"
    st-r st-g st-b #f #f))

;;;============================================================================
;;; Main setup and teardown
;;;============================================================================

(def (qt-setup-highlighting! app buf)
  (let* ((lang (or (detect-language (buffer-file-path buf))
                   (let ((l (buffer-lexer-lang buf)))
                     ;; Don't highlight special buffers
                     (and (not (memq l '(dired repl eshell shell))) l))))
         (doc (buffer-doc-pointer buf)))
    (when (and lang doc)
      ;; Remove old highlighter if any
      (qt-remove-highlighting! buf)
      ;; Create new highlighter on the document
      (let ((h (qt-syntax-highlighter-create doc)))
        ;; Apply language rules
        (case lang
          ((scheme)     (setup-scheme-rules! h))
          ((lisp)       (setup-scheme-rules! h))
          ((c)          (setup-c-rules! h))
          ((python)     (setup-python-rules! h))
          ((javascript) (setup-javascript-rules! h))
          ((org)        (setup-org-rules! h))
          ((markdown)   (setup-markdown-rules! h))
          ((shell)      (setup-shell-rules! h))
          ((rust)       (setup-rust-rules! h))
          ((go)         (setup-go-rules! h))
          ((java)       (setup-java-rules! h))
          ((ruby)       (setup-ruby-rules! h))
          ((json)       (setup-json-rules! h))
          ((yaml)       (setup-yaml-rules! h))
          ((toml)       (setup-toml-rules! h))
          ((xml)        (setup-xml-rules! h))
          ((css)        (setup-css-rules! h))
          ((sql)        (setup-sql-rules! h))
          ((lua)        (setup-lua-rules! h))
          ((zig)        (setup-zig-rules! h))
          ((nix)        (setup-nix-rules! h))
          ((makefile)   (setup-makefile-rules! h))
          (else (void)))
        (hash-put! *qt-highlighters* buf h)))))

(def (qt-remove-highlighting! buf)
  (let ((h (hash-get *qt-highlighters* buf)))
    (when h
      (qt-syntax-highlighter-destroy! h)
      (hash-remove! *qt-highlighters* buf))))

;;;============================================================================
;;; Visual decorations (current line + brace matching)
;;;============================================================================

;; Current line highlight color (dark gray)
(def cline-bg-r #x22) (def cline-bg-g #x22) (def cline-bg-b #x28)

;; Brace match colors
(def brace-fg-r #xff) (def brace-fg-g #xff) (def brace-fg-b #x00)
(def brace-bg-r #x40) (def brace-bg-g #x40) (def brace-bg-b #x60)

;; Brace mismatch colors
(def brace-err-fg-r #xff) (def brace-err-fg-g #x40) (def brace-err-fg-b #x40)
(def brace-err-bg-r #x60) (def brace-err-bg-g #x20) (def brace-err-bg-b #x20)

(def (brace-open? ch)
  (or (char=? ch #\() (char=? ch #\[) (char=? ch #\{)))

(def (brace-close? ch)
  (or (char=? ch #\)) (char=? ch #\]) (char=? ch #\})))

(def (brace-match? open close)
  (or (and (char=? open #\() (char=? close #\)))
      (and (char=? open #\[) (char=? close #\]))
      (and (char=? open #\{) (char=? close #\}))))

(def (find-matching-brace text pos)
  "Find matching brace position. Returns (values match-pos matched?) or (values #f #f)."
  (let ((len (string-length text)))
    (if (< pos len)
      (let ((ch (string-ref text pos)))
        (cond
          ((brace-open? ch)
           ;; Scan forward
           (let loop ((i (+ pos 1)) (depth 1))
             (cond
               ((>= i len) (values #f #f))
               ((brace-open? (string-ref text i))
                (loop (+ i 1) (+ depth 1)))
               ((brace-close? (string-ref text i))
                (if (= depth 1)
                  (values i (brace-match? ch (string-ref text i)))
                  (loop (+ i 1) (- depth 1))))
               (else (loop (+ i 1) depth)))))
          ((brace-close? ch)
           ;; Scan backward
           (let loop ((i (- pos 1)) (depth 1))
             (cond
               ((< i 0) (values #f #f))
               ((brace-close? (string-ref text i))
                (loop (- i 1) (+ depth 1)))
               ((brace-open? (string-ref text i))
                (if (= depth 1)
                  (values i (brace-match? (string-ref text i) ch))
                  (loop (- i 1) (- depth 1))))
               (else (loop (- i 1) depth)))))
          (else (values #f #f))))
      (values #f #f))))

(def (qt-update-visual-decorations! ed)
  "Update current-line highlight and brace matching on the given editor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (text (qt-plain-text-edit-text ed)))
    ;; 1. Clear all extra selections
    (qt-extra-selections-clear! ed)
    ;; 2. Current line highlight
    (qt-extra-selection-add-line! ed line cline-bg-r cline-bg-g cline-bg-b)
    ;; 3. Brace matching — check char at cursor and char before cursor
    (let check ((check-pos pos))
      (let-values (((match-pos matched?) (find-matching-brace text check-pos)))
        (cond
          (match-pos
           (if matched?
             ;; Good match — highlight both braces
             (begin
               (qt-extra-selection-add-range! ed check-pos 1
                 brace-fg-r brace-fg-g brace-fg-b
                 brace-bg-r brace-bg-g brace-bg-b bold: #t)
               (qt-extra-selection-add-range! ed match-pos 1
                 brace-fg-r brace-fg-g brace-fg-b
                 brace-bg-r brace-bg-g brace-bg-b bold: #t))
             ;; Mismatch — highlight in error color
             (begin
               (qt-extra-selection-add-range! ed check-pos 1
                 brace-err-fg-r brace-err-fg-g brace-err-fg-b
                 brace-err-bg-r brace-err-bg-g brace-err-bg-b bold: #t)
               (qt-extra-selection-add-range! ed match-pos 1
                 brace-err-fg-r brace-err-fg-g brace-err-fg-b
                 brace-err-bg-r brace-err-bg-g brace-err-bg-b bold: #t))))
          ;; If not found at cursor, try one position back
          ((and (> check-pos 0) (= check-pos pos))
           (check (- pos 1)))
          (else (void)))))
    ;; 4. Apply all accumulated selections
    (qt-extra-selections-apply! ed)))

;;;============================================================================
;;; Search result highlighting
;;;============================================================================

;; Track whether search highlights are currently active
(def *search-highlight-active* #f)

;; Search match colors (orange background)
(def search-fg-r #x00) (def search-fg-g #x00) (def search-fg-b #x00)
(def search-bg-r #xff) (def search-bg-g #xcc) (def search-bg-b #x00)

(def (qt-highlight-search-matches! ed pattern)
  "Highlight all occurrences of pattern in the editor."
  (when (and pattern (> (string-length pattern) 0))
    (let* ((text (qt-plain-text-edit-text ed))
           (len (string-length text))
           (pat-len (string-length pattern)))
      ;; Find all occurrences
      (let loop ((i 0))
        (when (< (+ i pat-len) len)
          (let ((found (string-contains text pattern i)))
            (when found
              (qt-extra-selection-add-range! ed found pat-len
                search-fg-r search-fg-g search-fg-b
                search-bg-r search-bg-g search-bg-b bold: #f)
              (loop (+ found 1))))))
      (qt-extra-selections-apply! ed)
      (set! *search-highlight-active* #t))))

(def (qt-clear-search-highlights! ed)
  "Clear search highlights from the editor."
  (when *search-highlight-active*
    (qt-extra-selections-clear! ed)
    (qt-extra-selections-apply! ed)
    (set! *search-highlight-active* #f)))
