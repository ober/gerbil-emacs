;;; -*- Gerbil -*-
;;; Qt syntax highlighting for gemacs
;;;
;;; Uses QScintilla's built-in Lexilla lexers for syntax highlighting.
;;; Detects language from file extension and configures lexer + style colors.

(export qt-setup-highlighting!
        qt-remove-highlighting!
        qt-setup-org-styles!
        qt-org-highlight-buffer!
        qt-update-visual-decorations!
        qt-highlight-search-matches!
        qt-clear-search-highlights!
        *search-highlight-active*)

(import :std/sugar
        :std/srfi/13
        :std/pregexp
        :std/misc/string
        :gemacs/qt/sci-shim
        :gemacs/core
        (only-in :gemacs/org-parse
                 org-heading-line? org-heading-stars-of-line
                 org-comment-line? org-keyword-line?
                 org-table-line? org-block-begin? org-block-end?))

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
    ;; Org mode: no QScintilla lexer — use manual styling
    (when (and ed (eq? lang 'org))
      (set! (buffer-lexer-lang buf) 'org)
      (apply-base-dark-theme! ed)
      ;; Disable any built-in lexer for manual styling (SCI_SETILEXER = 4033)
      (sci-send ed 4033 0)
      (qt-setup-org-styles! ed)
      ;; Apply full-buffer org highlighting (headings, tables, blocks, etc.)
      (let ((text (qt-plain-text-edit-text ed)))
        (qt-org-highlight-buffer! ed text)))
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

;;;============================================================================
;;; Org-mode manual styles (no QScintilla lexer — uses SCI style messages)
;;;============================================================================

;; Org style IDs (32-58, same as TUI org-highlight.ss)
(def (qt-setup-org-styles! ed)
  "Configure org-mode style colors for QScintilla editor."
  ;; Heading colors (style 33-40)
  (let ((colors (list (rgb->sci #x50 #x78 #xDC)    ; heading 1 blue
                      (rgb->sci #xD2 #x8C #x32)    ; heading 2 orange
                      (rgb->sci #x3C #xA0 #x3C)    ; heading 3 green
                      (rgb->sci #x32 #xB4 #xB4)    ; heading 4 cyan
                      (rgb->sci #x96 #x50 #xB4)    ; heading 5 purple
                      (rgb->sci #xB4 #xA0 #x28)    ; heading 6 yellow
                      (rgb->sci #x78 #x78 #x78)    ; heading 7 gray
                      (rgb->sci #x64 #x64 #x64)))) ; heading 8 dark gray
    (let loop ((i 0) (cs colors))
      (when (and (< i 8) (pair? cs))
        (let ((style (+ 33 i)))
          (sci-send ed SCI_STYLESETFORE style (car cs))
          (sci-send ed SCI_STYLESETBOLD style 1))
        (loop (+ i 1) (cdr cs)))))
  ;; TODO (41) / DONE (42)
  (sci-send ed SCI_STYLESETFORE 41 (rgb->sci #xDC #x32 #x32))
  (sci-send ed SCI_STYLESETBOLD 41 1)
  (sci-send ed SCI_STYLESETFORE 42 (rgb->sci #x32 #xB4 #x32))
  (sci-send ed SCI_STYLESETBOLD 42 1)
  ;; Tags (43)
  (sci-send ed SCI_STYLESETFORE 43 (rgb->sci #x96 #x50 #xB4))
  ;; Comment (44)
  (sci-send ed SCI_STYLESETFORE 44 (rgb->sci #x82 #x82 #x82))
  (sci-send ed SCI_STYLESETITALIC 44 1)
  ;; Keyword #+TITLE: (45)
  (sci-send ed SCI_STYLESETFORE 45 (rgb->sci #xD2 #x8C #x32))
  ;; Bold (46) / Italic (47) / Underline (48)
  (sci-send ed SCI_STYLESETBOLD 46 1)
  (sci-send ed SCI_STYLESETITALIC 47 1)
  (sci-send ed SCI_STYLESETUNDERLINE 48 1)
  ;; Verbatim (49) / Code (50)
  (sci-send ed SCI_STYLESETFORE 49 (rgb->sci #x32 #xB4 #xB4))
  (sci-send ed SCI_STYLESETFORE 50 (rgb->sci #x3C #xA0 #x3C))
  ;; Link (51)
  (sci-send ed SCI_STYLESETFORE 51 (rgb->sci #x50 #x78 #xDC))
  (sci-send ed SCI_STYLESETUNDERLINE 51 1)
  ;; Date (52)
  (sci-send ed SCI_STYLESETFORE 52 (rgb->sci #xB4 #x50 #xB4))
  ;; Property (53)
  (sci-send ed SCI_STYLESETFORE 53 (rgb->sci #x64 #x64 #x64))
  ;; Block delimiters (54) / Block body (55)
  (sci-send ed SCI_STYLESETFORE 54 (rgb->sci #xD2 #x8C #x32))
  (sci-send ed SCI_STYLESETFORE 55 (rgb->sci #x64 #x64 #x64))
  ;; Checkbox on (56) / off (57)
  (sci-send ed SCI_STYLESETFORE 56 (rgb->sci #x32 #xB4 #x32))
  (sci-send ed SCI_STYLESETFORE 57 (rgb->sci #xDC #x32 #x32))
  ;; Table (58)
  (sci-send ed SCI_STYLESETFORE 58 (rgb->sci #x32 #xB4 #xB4))
  ;; Source block inline highlighting (59-62)
  (sci-send ed SCI_STYLESETFORE 59 (rgb->sci kw-r kw-g kw-b))   ; keyword — purple bold
  (sci-send ed SCI_STYLESETBOLD 59 1)
  (sci-send ed SCI_STYLESETFORE 60 (rgb->sci st-r st-g st-b))   ; string — green
  (sci-send ed SCI_STYLESETFORE 61 (rgb->sci cm-r cm-g cm-b))   ; comment — gray italic
  (sci-send ed SCI_STYLESETITALIC 61 1)
  (sci-send ed SCI_STYLESETFORE 62 (rgb->sci nm-r nm-g nm-b)))  ; number — orange

;;;============================================================================
;;; Qt Org full-buffer highlighting (mirrors org-highlight.ss for Qt editors)
;;;============================================================================

;; Scintilla style message constants
(def SCI_STARTSTYLING 2032)
(def SCI_SETSTYLING   2033)

;; Org style IDs (must match qt-setup-org-styles! above and TUI org-highlight.ss)
(def QT_ORG_DEFAULT       32)
(def QT_ORG_HEADING_1     33)
(def QT_ORG_TODO          41)
(def QT_ORG_DONE          42)
(def QT_ORG_TAGS          43)
(def QT_ORG_COMMENT       44)
(def QT_ORG_KEYWORD       45)
(def QT_ORG_BOLD          46)
(def QT_ORG_ITALIC        47)
(def QT_ORG_UNDERLINE     48)
(def QT_ORG_VERBATIM      49)
(def QT_ORG_CODE          50)
(def QT_ORG_LINK          51)
(def QT_ORG_DATE          52)
(def QT_ORG_PROPERTY      53)
(def QT_ORG_BLOCK_DELIM   54)
(def QT_ORG_BLOCK_BODY    55)
(def QT_ORG_CHECKBOX_ON   56)
(def QT_ORG_CHECKBOX_OFF  57)
(def QT_ORG_TABLE         58)
;; Source block inline highlighting styles
(def QT_ORG_SRC_KW        59)   ; keyword (purple bold)
(def QT_ORG_SRC_STR       60)   ; string literal (green)
(def QT_ORG_SRC_COMMENT   61)   ; comment (gray italic)
(def QT_ORG_SRC_NUMBER    62)   ; numeric literal (orange)

(def (qt-org-style-line! ed pos len style)
  (when (> len 0)
    (sci-send ed SCI_STARTSTYLING pos 0)
    (sci-send ed SCI_SETSTYLING len style)))

(def (qt-org-style-range! ed pos len style)
  (when (> len 0)
    (sci-send ed SCI_STARTSTYLING pos 0)
    (sci-send ed SCI_SETSTYLING len style)))

(def (qt-org-heading-style level)
  (cond ((<= level 0) QT_ORG_DEFAULT)
        ((<= level 8) (+ QT_ORG_HEADING_1 (- level 1)))
        (else (+ QT_ORG_HEADING_1 7))))

(def (qt-org-highlight-heading! ed pos line line-len)
  (let* ((level (org-heading-stars-of-line line))
         (style (qt-org-heading-style level)))
    (qt-org-style-line! ed pos line-len style)
    ;; Highlight TODO/DONE keyword
    (let ((m (pregexp-match "^(\\*+)\\s+(TODO|NEXT|DOING|WAITING|HOLD)\\s" line)))
      (when m
        (let* ((stars-len (string-length (list-ref m 1)))
               (kw-start (+ stars-len 1))
               (kw-len (string-length (list-ref m 2))))
          (qt-org-style-range! ed (+ pos kw-start) kw-len QT_ORG_TODO))))
    (let ((m (pregexp-match "^(\\*+)\\s+(DONE|CANCELLED)\\s" line)))
      (when m
        (let* ((stars-len (string-length (list-ref m 1)))
               (kw-start (+ stars-len 1))
               (kw-len (string-length (list-ref m 2))))
          (qt-org-style-range! ed (+ pos kw-start) kw-len QT_ORG_DONE))))
    ;; Highlight tags at end of line
    (let ((m (pregexp-match "(:\\S+:)\\s*$" line)))
      (when m
        (let* ((tag-str (list-ref m 1))
               (tag-pos (string-contains line tag-str)))
          (when tag-pos
            (qt-org-style-range! ed (+ pos tag-pos) (string-length tag-str)
                                 QT_ORG_TAGS)))))))

(def (qt-org-highlight-markup-pairs! ed pos line marker style)
  (let* ((marker-char (string-ref marker 0))
         (len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 2))
        (if (and (char=? (string-ref line i) marker-char)
                 (or (= i 0) (char-whitespace? (string-ref line (- i 1))))
                 (not (char-whitespace? (string-ref line (+ i 1)))))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((and (char=? (string-ref line j) marker-char)
                    (not (char-whitespace? (string-ref line (- j 1))))
                    (or (= j (- len 1))
                        (let ((c (string-ref line (+ j 1))))
                          (or (char-whitespace? c)
                              (memv c '(#\. #\, #\; #\: #\! #\? #\) #\]))))))
               (qt-org-style-range! ed (+ pos i) (+ (- j i) 1) style)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (qt-org-highlight-links! ed pos line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 3))
        (if (and (char=? (string-ref line i) #\[)
                 (< (+ i 1) len)
                 (char=? (string-ref line (+ i 1)) #\[))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j (- len 1)) (loop (+ i 1)))
              ((and (char=? (string-ref line j) #\])
                    (char=? (string-ref line (+ j 1)) #\]))
               (qt-org-style-range! ed (+ pos i) (+ (- j i) 2) QT_ORG_LINK)
               (loop (+ j 2)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (qt-org-highlight-dates! ed pos line)
  (let ((len (string-length line)))
    ;; Active timestamps <...>
    (let loop ((i 0))
      (when (< i (- len 5))
        (if (and (char=? (string-ref line i) #\<)
                 (< (+ i 5) len)
                 (char-numeric? (string-ref line (+ i 1))))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((char=? (string-ref line j) #\>)
               (qt-org-style-range! ed (+ pos i) (+ (- j i) 1) QT_ORG_DATE)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))
    ;; Inactive timestamps [...]
    (let loop ((i 0))
      (when (< i (- len 5))
        (if (and (char=? (string-ref line i) #\[)
                 (< (+ i 5) len)
                 (char-numeric? (string-ref line (+ i 1))))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((char=? (string-ref line j) #\])
               (qt-org-style-range! ed (+ pos i) (+ (- j i) 1) QT_ORG_DATE)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (qt-org-highlight-checkboxes! ed pos line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 2))
        (if (char=? (string-ref line i) #\[)
          (cond
            ((and (< (+ i 2) len)
                  (char=? (string-ref line (+ i 2)) #\]))
             (let ((inner (string-ref line (+ i 1))))
               (cond
                 ((or (char=? inner #\X) (char=? inner #\x))
                  (qt-org-style-range! ed (+ pos i) 3 QT_ORG_CHECKBOX_ON))
                 ((char=? inner #\space)
                  (qt-org-style-range! ed (+ pos i) 3 QT_ORG_CHECKBOX_OFF)))
               (loop (+ i 3))))
            (else (loop (+ i 1))))
          (loop (+ i 1)))))))

(def (qt-org-highlight-inline! ed pos line line-len)
  (qt-org-highlight-markup-pairs! ed pos line "*" QT_ORG_BOLD)
  (qt-org-highlight-markup-pairs! ed pos line "/" QT_ORG_ITALIC)
  (qt-org-highlight-markup-pairs! ed pos line "_" QT_ORG_UNDERLINE)
  (qt-org-highlight-markup-pairs! ed pos line "=" QT_ORG_VERBATIM)
  (qt-org-highlight-markup-pairs! ed pos line "~" QT_ORG_CODE)
  (qt-org-highlight-links! ed pos line)
  (qt-org-highlight-dates! ed pos line)
  (qt-org-highlight-checkboxes! ed pos line))

(def (qt-org-table-separator? line)
  "True if the line is an org table separator row (only |, -, + chars)."
  (let ((trimmed (string-trim-both line)))
    (and (> (string-length trimmed) 1)
         (char=? (string-ref trimmed 0) #\|)
         ;; No char other than |, -, + present
         (not (pregexp-match "[^|+\\-]" trimmed)))))

(def (qt-org-highlight-table-line! ed pos line line-len)
  "Style org table line: separators get full table color;
   content rows get default body color with only '|' chars colored."
  (if (qt-org-table-separator? line)
    ;; Horizontal rule — color everything
    (qt-org-style-line! ed pos line-len QT_ORG_TABLE)
    ;; Content row — default body, then paint each '|' in table color
    (begin
      (qt-org-style-line! ed pos line-len QT_ORG_DEFAULT)
      (let loop ((i 0))
        (when (< i line-len)
          (when (char=? (string-ref line i) #\|)
            (qt-org-style-range! ed (+ pos i) 1 QT_ORG_TABLE))
          (loop (+ i 1)))))))

(def (qt-org-highlight-normal-line! ed pos line line-len)
  (cond
    ((org-heading-line? line)
     (qt-org-highlight-heading! ed pos line line-len))
    ((org-comment-line? line)
     (qt-org-style-line! ed pos line-len QT_ORG_COMMENT))
    ((org-keyword-line? line)
     (qt-org-style-line! ed pos line-len QT_ORG_KEYWORD))
    ((org-table-line? line)
     (qt-org-highlight-table-line! ed pos line line-len))
    (else
     (qt-org-style-line! ed pos line-len QT_ORG_DEFAULT)
     (qt-org-highlight-inline! ed pos line line-len))))

;;;============================================================================
;;; Org source block syntax highlighting
;;;============================================================================

(def (qt-org-block-src-language line)
  "Extract language symbol from #+BEGIN_SRC lang line. Returns symbol or #f."
  (let ((m (pregexp-match "(?i)^[[:space:]]*#\\+begin_src[[:space:]]+(\\S+)" line)))
    (and m
         (let ((lang-str (string-downcase (list-ref m 1))))
           (cond
             ((member lang-str '("sh" "shell" "bash" "zsh" "fish")) 'shell)
             ((member lang-str '("python" "python3" "py")) 'python)
             ((member lang-str '("scheme" "gerbil" "lisp" "elisp" "emacs-lisp" "cl" "common-lisp")) 'scheme)
             ((member lang-str '("javascript" "js" "typescript" "ts" "node")) 'javascript)
             ((member lang-str '("c" "cpp" "c++" "cxx" "objc")) 'c)
             ((member lang-str '("rust")) 'rust)
             ((member lang-str '("go" "golang")) 'go)
             ((member lang-str '("ruby" "rb")) 'ruby)
             ((member lang-str '("sql")) 'sql)
             ((member lang-str '("json")) 'json)
             ((member lang-str '("yaml" "yml")) 'yaml)
             (else 'text))))))

(def (qt-org-src-comment? trimmed lang)
  "True if trimmed line looks like a comment in language lang."
  (and (> (string-length trimmed) 0)
       (case lang
         ((shell python ruby yaml)
          (string-prefix? "#" trimmed))
         ((scheme lisp)
          (string-prefix? ";" trimmed))
         ((c javascript go rust java)
          (or (string-prefix? "//" trimmed)
              (string-prefix? "/*" trimmed)))
         ((sql)
          (string-prefix? "--" trimmed))
         (else #f))))

(def *dquote* (integer->char 34))   ; " — avoids confusing the Gerbil reader

(def (qt-org-highlight-src-strings! ed pos line line-len)
  "Highlight double-quoted string literals in a src block body line."
  (let loop ((i 0) (in-str #f) (str-start 0))
    (when (< i line-len)
      (let ((ch (string-ref line i)))
        (cond
          ;; Enter string
          ((and (not in-str) (char=? ch *dquote*))
           (loop (+ i 1) #t i))
          ;; Backslash escape inside string — skip next char
          ((and in-str (char=? ch #\\) (< (+ i 1) line-len))
           (loop (+ i 2) in-str str-start))
          ;; Closing double-quote — style the whole string token
          ((and in-str (char=? ch *dquote*))
           (qt-org-style-range! ed (+ pos str-start) (+ (- i str-start) 1) QT_ORG_SRC_STR)
           (loop (+ i 1) #f 0))
          (else
           (loop (+ i 1) in-str str-start)))))))

(def (qt-org-highlight-src-line! ed pos line line-len lang)
  "Apply language-appropriate highlighting to a src block body line."
  ;; Default: dim block body color for the whole line
  (qt-org-style-line! ed pos line-len QT_ORG_BLOCK_BODY)
  (let ((trimmed (string-trim line)))
    (cond
      ;; Empty line — nothing more to do
      ((= (string-length trimmed) 0) (void))
      ;; Comment line — override with comment style
      ((qt-org-src-comment? trimmed lang)
       (qt-org-style-line! ed pos line-len QT_ORG_SRC_COMMENT))
      ;; Non-comment: highlight string literals
      (else
       (qt-org-highlight-src-strings! ed pos line line-len)))))

(def (qt-org-highlight-buffer! ed text)
  "Full-buffer org-mode highlighting for Qt editors using sci-send.
  State is 'normal, (cons 'block lang), or 'drawer.
  lang = #f for non-src blocks; a symbol for #+BEGIN_SRC lang blocks."
  (let* ((lines (string-split text #\newline))
         (total (length lines)))
    (let loop ((i 0) (pos 0) (state 'normal))
      (when (< i total)
        (let* ((line (list-ref lines i))
               (line-len (string-length line))
               (next-pos (+ pos line-len 1)))
          (cond
            ;; Block begin
            ((and (eq? state 'normal) (org-block-begin? line))
             (qt-org-style-line! ed pos line-len QT_ORG_BLOCK_DELIM)
             ;; Extract language for src blocks; #f for all other block types
             (let ((lang (qt-org-block-src-language line)))
               (loop (+ i 1) next-pos (cons 'block lang))))
            ;; Block end
            ((and (pair? state) (eq? (car state) 'block) (org-block-end? line))
             (qt-org-style-line! ed pos line-len QT_ORG_BLOCK_DELIM)
             (loop (+ i 1) next-pos 'normal))
            ;; Inside block
            ((and (pair? state) (eq? (car state) 'block))
             (let ((lang (cdr state)))
               (if lang
                 (qt-org-highlight-src-line! ed pos line line-len lang)
                 (qt-org-style-line! ed pos line-len QT_ORG_BLOCK_BODY)))
             (loop (+ i 1) next-pos state))
            ;; Drawer begin
            ((and (eq? state 'normal)
                  (or (pregexp-match "^\\s*:PROPERTIES:" line)
                      (pregexp-match "^\\s*:LOGBOOK:" line)))
             (qt-org-style-line! ed pos line-len QT_ORG_PROPERTY)
             (loop (+ i 1) next-pos 'drawer))
            ;; Drawer end
            ((and (eq? state 'drawer)
                  (pregexp-match "^\\s*:END:" line))
             (qt-org-style-line! ed pos line-len QT_ORG_PROPERTY)
             (loop (+ i 1) next-pos 'normal))
            ;; Inside drawer
            ((eq? state 'drawer)
             (qt-org-style-line! ed pos line-len QT_ORG_PROPERTY)
             (loop (+ i 1) next-pos 'drawer))
            ;; Normal state
            (else
             (qt-org-highlight-normal-line! ed pos line line-len)
             (loop (+ i 1) next-pos 'normal))))))))

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
