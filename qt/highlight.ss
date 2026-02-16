;;; -*- Gerbil -*-
;;; Qt syntax highlighting for gerbil-emacs
;;;
;;; Uses QScintilla's built-in Lexilla lexers for syntax highlighting.
;;; Detects language from file extension and configures lexer + style colors.

(export qt-setup-highlighting!
        qt-remove-highlighting!
        qt-update-visual-decorations!
        qt-highlight-search-matches!
        qt-clear-search-highlights!
        *search-highlight-active*)

(import :std/sugar
        :std/srfi/13
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core)

;;;============================================================================
;;; Color constants (dark theme)
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
;;; File extension -> language detection
;;;============================================================================

(def (detect-language path)
  (and path
    (let ((ext (path-extension path))
          (base (path-strip-directory path)))
      (cond
        ((or (not ext) (string=? ext ""))
         (cond
           ((member base '("Makefile" "makefile" "GNUmakefile" "CMakeLists.txt")) 'makefile)
           ((member base '("Dockerfile" "Containerfile")) 'shell)
           ((member base '("Rakefile" "Gemfile" "Vagrantfile")) 'ruby)
           (else #f)))
        ((member ext '(".ss" ".scm" ".sld" ".sls" ".rkt")) 'scheme)
        ((member ext '(".c" ".h" ".cpp" ".hpp" ".cc" ".cxx" ".hh" ".hxx" ".ino")) 'c)
        ((member ext '(".py" ".pyw" ".pyi")) 'python)
        ((member ext '(".js" ".jsx" ".ts" ".tsx" ".mjs" ".cjs" ".mts" ".cts")) 'javascript)
        ((member ext '(".org")) 'org)
        ((member ext '(".md" ".markdown" ".mkd" ".rst")) 'markdown)
        ((member ext '(".sh" ".bash" ".zsh" ".fish" ".ksh")) 'shell)
        ((member ext '(".rb" ".rake" ".gemspec" ".erb")) 'ruby)
        ((member ext '(".rs")) 'rust)
        ((member ext '(".go")) 'go)
        ((member ext '(".java" ".kt" ".kts" ".scala")) 'java)
        ((member ext '(".json" ".jsonl" ".jsonc")) 'json)
        ((member ext '(".yaml" ".yml")) 'yaml)
        ((member ext '(".toml")) 'toml)
        ((member ext '(".xml" ".html" ".htm" ".svg" ".xhtml" ".xsl" ".xsd" ".plist")) 'xml)
        ((member ext '(".css" ".scss" ".sass" ".less")) 'css)
        ((member ext '(".sql")) 'sql)
        ((member ext '(".el" ".lisp" ".cl")) 'lisp)
        ((member ext '(".lua")) 'lua)
        ((member ext '(".zig")) 'zig)
        ((member ext '(".nix")) 'nix)
        ((member ext '(".pl" ".pm" ".t")) 'perl)
        ((member ext '(".hs" ".lhs")) 'haskell)
        ((member ext '(".ex" ".exs" ".erl" ".hrl")) 'elixir)
        ((member ext '(".swift")) 'swift)
        ((member ext '(".nim")) 'python)
        ((member ext '(".svelte" ".vue")) 'xml)
        ((member ext '(".dockerfile")) 'shell)
        ((member ext '(".diff" ".patch")) 'diff)
        ((member ext '(".ini" ".conf" ".cfg" ".properties")) 'toml)
        ((string=? ext ".mk") 'makefile)
        (else #f)))))

(def (detect-language-from-shebang-qt text)
  "Detect language from shebang line."
  (and text
       (> (string-length text) 2)
       (char=? (string-ref text 0) #\#)
       (char=? (string-ref text 1) #\!)
       (let* ((nl (string-index text #\newline))
              (line (if nl (substring text 0 nl) text)))
         (cond
           ((or (string-contains line "/bash")
                (string-contains line "/sh")
                (string-contains line "/zsh"))
            'shell)
           ((string-contains line "python") 'python)
           ((string-contains line "ruby") 'ruby)
           ((string-contains line "node") 'javascript)
           ((string-contains line "lua") 'lua)
           (else 'shell)))))

;;;============================================================================
;;; Language -> QScintilla lexer name mapping
;;;============================================================================

(def (language->lexer-name lang)
  (case lang
    ((scheme lisp)      "lisp")
    ((c)                "cpp")
    ((python)           "python")
    ((javascript)       "cpp")
    ((shell)            "bash")
    ((rust)             "rust")
    ((go)               "cpp")
    ((java)             "cpp")
    ((ruby)             "ruby")
    ((json)             "json")
    ((yaml)             "yaml")
    ((xml)              "xml")
    ((css)              "css")
    ((sql)              "sql")
    ((lua)              "lua")
    ((makefile)         "makefile")
    ((diff)             "diff")
    ((markdown)         "markdown")
    ((perl)             "perl")
    ((haskell)          "haskell")
    ((zig nix swift elixir) "cpp")
    ((toml)             "props")
    ((org)              #f)
    (else               #f)))

;;;============================================================================
;;; Keyword strings per language
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

(def *c-keywords*
  "if else for while do switch case default break continue return goto struct union enum typedef sizeof static const volatile extern inline register auto signed unsigned class public private protected virtual override template typename namespace using new delete throw try catch noexcept constexpr nullptr true false this operator")

(def *c-types*
  "int char float double void long short bool size_t ssize_t int8_t int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t FILE NULL EOF")

(def *python-keywords*
  "False None True and as assert async await break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield")

(def *python-builtins*
  "print len range int str float list dict tuple set bool type isinstance issubclass open input map filter zip enumerate sorted reversed sum min max abs any all super property staticmethod classmethod hasattr getattr setattr delattr ValueError TypeError KeyError IndexError Exception RuntimeError StopIteration")

(def *js-keywords*
  "function const let var if else for while do switch case default break continue return try catch finally throw new delete typeof instanceof in of class extends super this import export from as async await yield true false null undefined void interface type enum implements abstract public private protected readonly static")

(def *js-builtins*
  "console Math JSON Object Array String Number Boolean Date RegExp Error Promise Map Set parseInt parseFloat isNaN isFinite setTimeout setInterval fetch require")

(def *shell-keywords*
  "if then else elif fi for do done while until case esac in function return exit break continue local export readonly declare typeset source eval exec trap shift set unset")

(def *go-keywords*
  "break case chan const continue default defer else fallthrough for func go goto if import interface map package range return select struct switch type var true false nil iota")

(def *go-types*
  "int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 uintptr float32 float64 complex64 complex128 bool byte rune string error any comparable")

(def *rust-keywords*
  "fn let mut const static if else match for while loop break continue return struct enum impl trait type where pub mod use crate self super as in ref move unsafe async await true false Some None Ok Err")

(def *rust-types*
  "i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 bool char str String Vec Box Rc Arc Option Result HashMap HashSet BTreeMap BTreeSet")

(def *java-keywords*
  "abstract assert break case catch class const continue default do else enum extends final finally for goto if implements import instanceof interface native new package private protected public return static strictfp super switch synchronized this throw throws transient try void volatile while true false null")

(def *java-types*
  "boolean byte char double float int long short String Object Integer Double Float Long List Map Set ArrayList HashMap HashSet")

(def *ruby-keywords*
  "def class module end if elsif else unless while until for do begin rescue ensure raise return yield break next redo retry and or not in then when case self super nil true false require require_relative include extend attr_reader attr_writer attr_accessor puts print p lambda proc")

(def *lua-keywords*
  "and break do else elseif end false for function goto if in local nil not or repeat return then true until while")

(def *sql-keywords*
  "SELECT FROM WHERE INSERT INTO VALUES UPDATE SET DELETE CREATE DROP ALTER TABLE INDEX VIEW JOIN INNER LEFT RIGHT OUTER ON AS AND OR NOT IN IS NULL LIKE BETWEEN ORDER BY GROUP HAVING LIMIT OFFSET UNION ALL DISTINCT EXISTS CASE WHEN THEN ELSE END BEGIN COMMIT ROLLBACK TRANSACTION PRIMARY KEY FOREIGN REFERENCES UNIQUE CHECK DEFAULT INTEGER TEXT REAL BLOB VARCHAR CHAR BOOLEAN DATE COUNT SUM AVG MIN MAX select from where insert into values update set delete create drop alter table index view join inner left right outer on as and or not in is null like between order by group having limit offset")

(def *perl-keywords*
  "if elsif else unless while until for foreach do sub my local our use require package return last next redo goto die warn print say open close chomp chop push pop shift unshift sort reverse map grep join split")

;;;============================================================================
;;; Apply base dark theme and reset styles
;;;============================================================================

(def (apply-base-dark-theme! ed)
  "Reset all styles to dark theme defaults."
  (sci-send ed SCI_STYLESETBACK STYLE_DEFAULT (rgb->sci #x1e #x1e #x2e))
  (sci-send ed SCI_STYLESETFORE STYLE_DEFAULT (rgb->sci #xd4 #xd4 #xd4))
  (sci-send ed SCI_STYLECLEARALL)
  ;; Restore line number margin style (STYLECLEARALL resets it)
  (sci-send ed SCI_STYLESETBACK STYLE_LINENUMBER (rgb->sci #x20 #x20 #x20))
  (sci-send ed SCI_STYLESETFORE STYLE_LINENUMBER (rgb->sci #x8c #x8c #x8c)))

;;;============================================================================
;;; Lexer-specific style setup
;;;============================================================================

;;; --- C/C++/Java/JS/Go/Rust/Zig lexer ("cpp") ---
;;; Style IDs: 1=comment, 2=commentline, 3=commentdoc, 4=number,
;;; 5=keyword, 6=string, 7=character, 9=preprocessor, 10=operator, 16=keyword2

(def (setup-cpp-styles! ed keywords (types #f))
  ;; Comments: gray, italic
  (for-each (lambda (s)
              (sci-send ed SCI_STYLESETFORE s (rgb->sci cm-r cm-g cm-b))
              (sci-send ed SCI_STYLESETITALIC s 1))
            '(1 2 3 15))
  ;; Numbers: orange
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b))
  ;; Keywords: purple, bold
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  ;; Strings: green
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
  ;; Preprocessor: orange
  (sci-send ed SCI_STYLESETFORE 9 (rgb->sci pp-r pp-g pp-b))
  ;; Operator: light gray
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci op-r op-g op-b))
  ;; Types/keyword2: yellow
  (sci-send ed SCI_STYLESETFORE 16 (rgb->sci ty-r ty-g ty-b))
  ;; Set keyword lists
  (sci-send/string ed SCI_SETKEYWORDS keywords 0)
  (when types
    (sci-send/string ed SCI_SETKEYWORDS types 1)))

;;; --- Python lexer ("python") ---
;;; Style IDs: 1=comment, 2=number, 3=string, 4=character, 5=keyword,
;;; 6=triple, 7=tripledouble, 8=classname, 9=defname, 10=operator,
;;; 12=commentblock, 14=keyword2, 15=decorator

(def (setup-python-styles! ed)
  ;; Comments: gray, italic
  (sci-send ed SCI_STYLESETFORE 1 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 1 1)
  (sci-send ed SCI_STYLESETFORE 12 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 12 1)
  ;; Numbers: orange
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci nm-r nm-g nm-b))
  ;; Strings: green (single, double, triple)
  (for-each (lambda (s) (sci-send ed SCI_STYLESETFORE s (rgb->sci st-r st-g st-b)))
            '(3 4 6 7))
  ;; Keywords: purple, bold
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  ;; Class/def names: yellow
  (sci-send ed SCI_STYLESETFORE 8 (rgb->sci ty-r ty-g ty-b))
  (sci-send ed SCI_STYLESETFORE 9 (rgb->sci ty-r ty-g ty-b))
  ;; Operator: light gray
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci op-r op-g op-b))
  ;; Builtins/keyword2: cyan
  (sci-send ed SCI_STYLESETFORE 14 (rgb->sci bi-r bi-g bi-b))
  ;; Decorators: orange
  (sci-send ed SCI_STYLESETFORE 15 (rgb->sci pp-r pp-g pp-b))
  ;; Set keyword lists
  (sci-send/string ed SCI_SETKEYWORDS *python-keywords* 0)
  (sci-send/string ed SCI_SETKEYWORDS *python-builtins* 1))

;;; --- Lisp/Scheme lexer ("lisp") ---
;;; Style IDs: 1=comment, 2=number, 3=keyword, 4=keyword_kw,
;;; 5=symbol, 6=string, 9=operator, 11=multi-comment

(def (setup-lisp-styles! ed)
  ;; Comments: gray, italic
  (sci-send ed SCI_STYLESETFORE 1 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 1 1)
  (sci-send ed SCI_STYLESETFORE 11 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 11 1)
  ;; Numbers: orange
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci nm-r nm-g nm-b))
  ;; Keywords: purple, bold
  (sci-send ed SCI_STYLESETFORE 3 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 3 1)
  ;; Keyword-kw (builtins): cyan
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci bi-r bi-g bi-b))
  ;; Strings: green
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  ;; Operator: light gray
  (sci-send ed SCI_STYLESETFORE 9 (rgb->sci op-r op-g op-b))
  ;; Set keyword lists
  (sci-send/string ed SCI_SETKEYWORDS *gerbil-keywords* 0)
  (sci-send/string ed SCI_SETKEYWORDS *gerbil-builtins* 1))

;;; --- Bash lexer ("bash") ---
;;; Style IDs: 2=comment, 3=number, 4=keyword, 5=string(dq),
;;; 6=character(sq), 7=operator, 9=scalar($var), 10=param(${var}), 11=backticks

(def (setup-bash-styles! ed)
  ;; Comment: gray, italic
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 2 1)
  ;; Number: orange
  (sci-send ed SCI_STYLESETFORE 3 (rgb->sci nm-r nm-g nm-b))
  ;; Keyword: purple, bold
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 4 1)
  ;; Strings: green
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  ;; Operator: light gray
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci op-r op-g op-b))
  ;; Variables: cyan
  (sci-send ed SCI_STYLESETFORE 9 (rgb->sci bi-r bi-g bi-b))
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci bi-r bi-g bi-b))
  ;; Backticks: green
  (sci-send ed SCI_STYLESETFORE 11 (rgb->sci st-r st-g st-b))
  ;; Set keyword list
  (sci-send/string ed SCI_SETKEYWORDS *shell-keywords* 0))

;;; --- Ruby lexer ("ruby") ---
;;; Style IDs: 2=comment, 4=number, 5=keyword, 6=string(dq),
;;; 7=character(sq), 10=symbol, 11=classvar, 12=instancevar

(def (setup-ruby-styles! ed)
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 2 1)
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b))
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci bi-r bi-g bi-b))
  (sci-send ed SCI_STYLESETFORE 11 (rgb->sci bi-r bi-g bi-b))
  (sci-send ed SCI_STYLESETFORE 12 (rgb->sci bi-r bi-g bi-b))
  (sci-send/string ed SCI_SETKEYWORDS *ruby-keywords* 0))

;;; --- Lua lexer ("lua") ---
;;; Style IDs: 1=comment, 2=commentline, 3=commentdoc, 4=number,
;;; 5=keyword, 6=string, 7=character, 10=operator

(def (setup-lua-styles! ed)
  (for-each (lambda (s)
              (sci-send ed SCI_STYLESETFORE s (rgb->sci cm-r cm-g cm-b))
              (sci-send ed SCI_STYLESETITALIC s 1))
            '(1 2 3))
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b))
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci op-r op-g op-b))
  (sci-send/string ed SCI_SETKEYWORDS *lua-keywords* 0))

;;; --- SQL lexer ("sql") ---
;;; Style IDs: 1=comment, 2=commentline, 4=number, 5=keyword,
;;; 6=string(dq), 7=string(sq), 10=operator, 11=identifier

(def (setup-sql-styles! ed)
  (sci-send ed SCI_STYLESETFORE 1 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 1 1)
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 2 1)
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b))
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci op-r op-g op-b))
  (sci-send/string ed SCI_SETKEYWORDS *sql-keywords* 0))

;;; --- Perl lexer ("perl") ---
;;; Style IDs: 1=error, 2=comment, 3=POD, 4=number, 5=keyword,
;;; 6=string(dq), 7=string(sq), 10=operator, 11=identifier

(def (setup-perl-styles! ed)
  (sci-send ed SCI_STYLESETFORE 2 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 2 1)
  (sci-send ed SCI_STYLESETFORE 3 (rgb->sci cm-r cm-g cm-b))
  (sci-send ed SCI_STYLESETITALIC 3 1)
  (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b))
  (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
  (sci-send ed SCI_STYLESETBOLD 5 1)
  (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
  (sci-send ed SCI_STYLESETFORE 10 (rgb->sci op-r op-g op-b))
  (sci-send/string ed SCI_SETKEYWORDS *perl-keywords* 0))

;;;============================================================================
;;; Main setup and teardown
;;;============================================================================

(def (qt-setup-highlighting! app buf)
  (let* ((lang (or (detect-language (buffer-file-path buf))
                   (let ((path (buffer-file-path buf)))
                     (and path (file-exists? path)
                          (with-catch (lambda (e) #f)
                            (lambda ()
                              (let ((line (call-with-input-file path read-line)))
                                (and (string? line)
                                     (> (string-length line) 2)
                                     (detect-language-from-shebang-qt
                                       (string-append line "\n"))))))))
                   (let ((l (buffer-lexer-lang buf)))
                     (and (not (memq l '(dired repl eshell shell))) l))))
         (doc (buffer-doc-pointer buf))
         (ed (and doc (hash-get *doc-editor-map* doc)))
         (lexer-name (and lang (language->lexer-name lang))))
    (when (and ed lexer-name)
      ;; Store language in buffer
      (set! (buffer-lexer-lang buf) lang)
      ;; Reset to dark theme base
      (apply-base-dark-theme! ed)
      ;; Set lexer language — QScintilla has wrapper classes for common languages,
      ;; but not for lisp, rust, diff, perl, haskell, props.
      ;; For those, use SCI_SETLEXERLANGUAGE to invoke Lexilla directly.
      (if (member lexer-name '("lisp" "rust" "diff" "perl" "haskell" "props"))
        (begin
          ;; No QsciLexer wrapper — use raw Scintilla message and force colorize
          (sci-send/string ed SCI_SETLEXERLANGUAGE lexer-name)
          (sci-send ed SCI_COLOURISE 0 -1))
        (qt-scintilla-set-lexer-language! ed lexer-name))
      ;; Apply lexer-specific styles and keywords
      (case lang
        ((scheme lisp)
         (setup-lisp-styles! ed))
        ((c)
         (setup-cpp-styles! ed *c-keywords* *c-types*))
        ((javascript)
         (setup-cpp-styles! ed *js-keywords* *js-builtins*))
        ((go)
         (setup-cpp-styles! ed *go-keywords* *go-types*))
        ((rust)
         (setup-cpp-styles! ed *rust-keywords* *rust-types*))
        ((java haskell swift elixir)
         (setup-cpp-styles! ed *java-keywords* *java-types*))
        ((zig nix)
         (setup-cpp-styles! ed *c-keywords* *c-types*))
        ((python)
         (setup-python-styles! ed))
        ((shell)
         (setup-bash-styles! ed))
        ((ruby)
         (setup-ruby-styles! ed))
        ((lua)
         (setup-lua-styles! ed))
        ((sql)
         (setup-sql-styles! ed))
        ((perl)
         (setup-perl-styles! ed))
        ;; For json, yaml, xml, css, markdown, makefile, diff, toml:
        ;; The lexer handles tokenization; we just set basic comment/string colors
        ((json yaml xml css markdown makefile diff toml)
         ;; Comments: styles 1-3 for most lexers
         (for-each (lambda (s)
                     (sci-send ed SCI_STYLESETFORE s (rgb->sci cm-r cm-g cm-b))
                     (sci-send ed SCI_STYLESETITALIC s 1))
                   '(1 2 3))
         ;; Keywords: style 5
         (sci-send ed SCI_STYLESETFORE 5 (rgb->sci kw-r kw-g kw-b))
         (sci-send ed SCI_STYLESETBOLD 5 1)
         ;; Strings: styles 6-7
         (sci-send ed SCI_STYLESETFORE 6 (rgb->sci st-r st-g st-b))
         (sci-send ed SCI_STYLESETFORE 7 (rgb->sci st-r st-g st-b))
         ;; Numbers: style 4
         (sci-send ed SCI_STYLESETFORE 4 (rgb->sci nm-r nm-g nm-b)))
        (else (void))))))

(def (qt-remove-highlighting! buf)
  (let* ((doc (buffer-doc-pointer buf))
         (ed (and doc (hash-get *doc-editor-map* doc))))
    (when ed
      (sci-send ed SCI_SETLEXER 0)  ;; SCLEX_NULL
      (apply-base-dark-theme! ed))))

;;;============================================================================
;;; Visual decorations (current line + brace matching)
;;;============================================================================

(def cline-bg-r #x22) (def cline-bg-g #x22) (def cline-bg-b #x28)

(def brace-fg-r #xff) (def brace-fg-g #xff) (def brace-fg-b #x00)
(def brace-bg-r #x40) (def brace-bg-g #x40) (def brace-bg-b #x60)

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
    ;; 3. Brace matching
    (let check ((check-pos pos))
      (let-values (((match-pos matched?) (find-matching-brace text check-pos)))
        (cond
          (match-pos
           (if matched?
             (begin
               (qt-extra-selection-add-range! ed check-pos 1
                 brace-fg-r brace-fg-g brace-fg-b
                 brace-bg-r brace-bg-g brace-bg-b bold: #t)
               (qt-extra-selection-add-range! ed match-pos 1
                 brace-fg-r brace-fg-g brace-fg-b
                 brace-bg-r brace-bg-g brace-bg-b bold: #t))
             (begin
               (qt-extra-selection-add-range! ed check-pos 1
                 brace-err-fg-r brace-err-fg-g brace-err-fg-b
                 brace-err-bg-r brace-err-bg-g brace-err-bg-b bold: #t)
               (qt-extra-selection-add-range! ed match-pos 1
                 brace-err-fg-r brace-err-fg-g brace-err-fg-b
                 brace-err-bg-r brace-err-bg-g brace-err-bg-b bold: #t))))
          ((and (> check-pos 0) (= check-pos pos))
           (check (- pos 1)))
          (else (void)))))
    ;; 4. Apply all accumulated selections
    (qt-extra-selections-apply! ed)))

;;;============================================================================
;;; Search result highlighting
;;;============================================================================

(def *search-highlight-active* #f)

(def search-fg-r #x00) (def search-fg-g #x00) (def search-fg-b #x00)
(def search-bg-r #xff) (def search-bg-g #xcc) (def search-bg-b #x00)

(def (qt-highlight-search-matches! ed pattern)
  "Highlight all occurrences of pattern in the editor."
  (when (and pattern (> (string-length pattern) 0))
    (let* ((text (qt-plain-text-edit-text ed))
           (len (string-length text))
           (pat-len (string-length pattern)))
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
