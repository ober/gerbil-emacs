;;; -*- Gerbil -*-
;;; Shared snippet infrastructure for gemacs
;;; Used by both TUI and Qt layers.

(export #t)

(import :std/sugar
        :std/srfi/13
        :std/sort)

;;; ============================================================================
;;; Core data structures
;;; ============================================================================

(def *snippet-table* (make-hash-table)) ;; lang -> (hash trigger -> template)
(def *snippet-active* #f)              ;; #f or #t when navigating snippet fields
(def *snippet-field-positions* [])     ;; list of cursor positions for fields
(def *snippet-mirror-groups* (make-hash-table)) ;; field-num -> list of (start . end) positions
(def *snippet-current-field* #f)       ;; current field number being edited
(def *snippet-base-offset* 0)          ;; base offset of snippet in buffer

;;; ============================================================================
;;; Core functions
;;; ============================================================================

(def (snippet-define! lang trigger template)
  "Define a snippet: LANG is language symbol or 'global, TRIGGER is prefix string,
   TEMPLATE is string with $1, $2 etc. for fields, ${1:default} for defaults, $0 for final pos."
  (let ((lang-table (or (hash-get *snippet-table* lang) (make-hash-table))))
    (hash-put! lang-table trigger template)
    (hash-put! *snippet-table* lang lang-table)))

(def (snippet-lookup trigger lang)
  "Look up a snippet by trigger, checking lang-specific then global."
  (or (let ((lt (hash-get *snippet-table* lang)))
        (and lt (hash-get lt trigger)))
      (let ((gt (hash-get *snippet-table* 'global)))
        (and gt (hash-get gt trigger)))))

(def (snippet-expand-template template)
  "Expand template: replace $N and ${N:default} with placeholders.
   Returns (text . field-offsets) where offsets are (n start . end) triples.
   Supports mirror fields: same $N appearing multiple times.
   Sets *snippet-mirror-groups* as side effect."
  (let ((out (open-output-string))
        (fields (make-hash-table))  ;; n -> list of (start . end) in reverse order
        (len (string-length template)))
    (let loop ((i 0))
      (cond
        ((>= i len)
         (let* ((text (get-output-string out))
                ;; Build mirror groups and flat offset list (all positions)
                (mirrors (make-hash-table))
                (all-offsets
                  (let collect ((n 1) (acc []))
                    (if (> n 9)
                      (let ((zero-positions (hash-get fields 0)))
                        (if (and zero-positions (not (null? zero-positions)))
                          (let ((p (car (reverse zero-positions))))
                            (reverse (cons (cons 0 (car p)) acc)))
                          (reverse acc)))
                      (let ((positions (hash-get fields n)))
                        (if (and positions (not (null? positions)))
                          (let ((sorted (reverse positions)))
                            ;; Store all positions in mirror groups
                            (hash-put! mirrors n sorted)
                            ;; Add ALL occurrences to navigation list
                            (let add-all ((ps sorted) (a acc))
                              (if (null? ps)
                                (collect (+ n 1) a)
                                (add-all (cdr ps)
                                         (cons (cons n (car (car ps))) a)))))
                          (collect (+ n 1) acc))))))
                ;; Sort by position so TAB visits in document order
                (sorted-offsets (sort all-offsets (lambda (a b) (< (cdr a) (cdr b))))))
           (set! *snippet-mirror-groups* mirrors)
           (cons text sorted-offsets)))
        ;; ${N:default} syntax
        ((and (char=? (string-ref template i) #\$)
              (< (+ i 2) len)
              (char=? (string-ref template (+ i 1)) #\{)
              (char-numeric? (string-ref template (+ i 2))))
         (let* ((n (- (char->integer (string-ref template (+ i 2)))
                      (char->integer #\0)))
                ;; Find the colon and closing brace
                (colon-pos (let scan ((j (+ i 3)))
                             (cond ((>= j len) #f)
                                   ((char=? (string-ref template j) #\:) j)
                                   ((char=? (string-ref template j) #\}) j)
                                   (else (scan (+ j 1))))))
                (close-pos (and colon-pos
                                (let scan ((j (if (char=? (string-ref template colon-pos) #\:)
                                               (+ colon-pos 1)
                                               colon-pos)))
                                  (cond ((>= j len) #f)
                                        ((char=? (string-ref template j) #\}) j)
                                        (else (scan (+ j 1))))))))
           (if (and colon-pos close-pos
                    (char=? (string-ref template colon-pos) #\:))
             ;; Has default text: ${N:default}
             (let* ((default-text (substring template (+ colon-pos 1) close-pos))
                    (pos (string-length (get-output-string out)))
                    (end-pos (+ pos (string-length default-text)))
                    (old (or (hash-get fields n) [])))
               (hash-put! fields n (cons (cons pos end-pos) old))
               (display default-text out)
               (loop (+ close-pos 1)))
             ;; No default: ${N} — same as $N
             (let* ((pos (string-length (get-output-string out)))
                    (skip-to (if close-pos (+ close-pos 1) (+ i 2)))
                    (old (or (hash-get fields n) [])))
               (hash-put! fields n (cons (cons pos pos) old))
               (loop skip-to)))))
        ;; Bare $N syntax
        ((and (char=? (string-ref template i) #\$)
              (< (+ i 1) len)
              (char-numeric? (string-ref template (+ i 1))))
         (let* ((n (- (char->integer (string-ref template (+ i 1)))
                      (char->integer #\0)))
                (pos (string-length (get-output-string out)))
                (old (or (hash-get fields n) [])))
           (hash-put! fields n (cons (cons pos pos) old))
           (loop (+ i 2))))
        ;; Regular character
        (else
         (display (string (string-ref template i)) out)
         (loop (+ i 1)))))))

(def (snippet-deactivate!)
  "Deactivate snippet field navigation."
  (set! *snippet-active* #f)
  (set! *snippet-field-positions* [])
  (set! *snippet-mirror-groups* (make-hash-table))
  (set! *snippet-current-field* #f)
  (set! *snippet-base-offset* 0))

(def (snippet-update-mirrors! get-text-fn set-text-fn field-num new-text base-offset)
  "Update all mirror positions for FIELD-NUM with NEW-TEXT.
   GET-TEXT-FN: () -> string, SET-TEXT-FN: string -> void.
   BASE-OFFSET is the snippet insertion point in the buffer.
   Returns the delta in text length caused by mirror updates."
  (let ((positions (hash-get *snippet-mirror-groups* field-num)))
    (if (or (not positions) (<= (length positions) 1))
      0  ;; No mirrors to update
      (let* ((full-text (get-text-fn))
             (sorted-positions (sort (cdr positions) ;; skip first (primary) position
                                     (lambda (a b) (> (car a) (car b))))) ;; reverse order
             (total-delta 0))
        ;; Replace each mirror position from end to start to preserve offsets
        (for-each
          (lambda (pos-pair)
            (let* ((start (+ base-offset (car pos-pair)))
                   (end (+ base-offset (cdr pos-pair)))
                   (old-len (- end start))
                   (new-len (string-length new-text))
                   (delta (- new-len old-len)))
              (when (and (<= 0 start) (<= end (string-length full-text)))
                (set! full-text
                  (string-append
                    (substring full-text 0 start)
                    new-text
                    (substring full-text end (string-length full-text))))
                (set! total-delta (+ total-delta delta)))))
          sorted-positions)
        (set-text-fn full-text)
        total-delta))))

(def (snippet-all-triggers lang)
  "Get all triggers available for a language (lang-specific + global).
   Returns list of (trigger . template) pairs."
  (let ((result []))
    (let ((gt (hash-get *snippet-table* 'global)))
      (when gt
        (hash-for-each (lambda (k v) (set! result (cons (cons k v) result))) gt)))
    (let ((lt (hash-get *snippet-table* lang)))
      (when lt
        (hash-for-each (lambda (k v) (set! result (cons (cons k v) result))) lt)))
    result))

;;; ============================================================================
;;; File-based snippet loading
;;; ============================================================================

(def (load-snippet-file! lang trigger path)
  "Load a snippet from a file. File contents = template."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((template (call-with-input-file path (lambda (p) (read-line p #f)))))
        (when (and template (> (string-length template) 0))
          (snippet-define! lang trigger template))))))

(def (load-snippet-directory! dir)
  "Load snippets from ~/.gemacs-snippets/ directory.
   Structure: dir/<lang>/<trigger> where file contents = template.
   Also supports dir/<lang>.snippets with # trigger: ... / # -- sections."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? dir)
        (for-each
          (lambda (name)
            (unless (member name '("." ".."))
              (let ((path (string-append dir "/" name)))
                (cond
                  ;; Directory = language, files inside = triggers
                  ((eq? (file-info-type (file-info path)) 'directory)
                   (let ((lang (string->symbol name)))
                     (for-each
                       (lambda (f)
                         (unless (member f '("." ".."))
                           (load-snippet-file! lang f (string-append path "/" f))))
                       (directory-files path))))
                  ;; <lang>.snippets file
                  ((string-suffix? ".snippets" name)
                   (let ((lang (string->symbol
                                 (substring name 0 (- (string-length name) 9)))))
                     (load-snippets-file! lang path)))))))
          (directory-files dir))))))

(def (load-snippets-file! lang path)
  "Load snippets from a .snippets file with # trigger: ... / # -- sections."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (let loop ((trigger #f) (lines []))
            (let ((line (read-line port)))
              (cond
                ((eof-object? line)
                 ;; Save last snippet if any
                 (when (and trigger (not (null? lines)))
                   (snippet-define! lang trigger
                     (string-join (reverse lines) "\n"))))
                ;; New snippet trigger
                ((and (>= (string-length line) 11)
                      (string-prefix? "# trigger: " line))
                 ;; Save previous snippet
                 (when (and trigger (not (null? lines)))
                   (snippet-define! lang trigger
                     (string-join (reverse lines) "\n")))
                 (loop (string-trim (substring line 11 (string-length line))) []))
                ;; Section separator
                ((string=? (string-trim line) "# --")
                 (when (and trigger (not (null? lines)))
                   (snippet-define! lang trigger
                     (string-join (reverse lines) "\n")))
                 (loop #f []))
                ;; Content line
                (else
                 (loop trigger (cons line lines)))))))))))

;;; ============================================================================
;;; Built-in snippets — Scheme / Gerbil
;;; ============================================================================

(snippet-define! 'scheme "def" "(def ($1)\n  $2)\n$0")
(snippet-define! 'scheme "defn" "(def ($1 $2)\n  $3)\n$0")
(snippet-define! 'scheme "let" "(let (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "let*" "(let* (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "when" "(when $1\n  $2)\n$0")
(snippet-define! 'scheme "unless" "(unless $1\n  $2)\n$0")
(snippet-define! 'scheme "cond" "(cond\n  (($1) $2)\n  (else $3))\n$0")
(snippet-define! 'scheme "if" "(if $1\n  $2\n  $3)\n$0")
(snippet-define! 'scheme "lambda" "(lambda ($1)\n  $2)\n$0")
(snippet-define! 'scheme "match" "(match $1\n  (($2) $3))\n$0")
(snippet-define! 'scheme "defstruct" "(defstruct $1 ($2))\n$0")
(snippet-define! 'scheme "defclass" "(defclass $1 ($2)\n  $3)\n$0")
(snippet-define! 'scheme "import" "(import $1)\n$0")
(snippet-define! 'scheme "export" "(export $1)\n$0")
(snippet-define! 'scheme "with-catch" "(with-catch\n  (lambda (e) $1)\n  (lambda ()\n    $2))\n$0")
(snippet-define! 'scheme "for-each" "(for-each\n  (lambda ($1)\n    $2)\n  $3)\n$0")

;;; ============================================================================
;;; Built-in snippets — Python
;;; ============================================================================

(snippet-define! 'python "def" "def ${1:name}(${2:args}):\n    ${3:pass}\n$0")
(snippet-define! 'python "class" "class ${1:Name}:\n    def __init__(self${2:, args}):\n        ${3:pass}\n$0")
(snippet-define! 'python "ifmain" "if __name__ == \"__main__\":\n    ${1:main()}\n$0")
(snippet-define! 'python "for" "for ${1:item} in ${2:iterable}:\n    $3\n$0")
(snippet-define! 'python "while" "while ${1:condition}:\n    $2\n$0")
(snippet-define! 'python "with" "with ${1:expr} as ${2:var}:\n    $3\n$0")
(snippet-define! 'python "try" "try:\n    $1\nexcept ${2:Exception} as ${3:e}:\n    $4\n$0")
(snippet-define! 'python "lam" "lambda ${1:x}: $2")
(snippet-define! 'python "init" "def __init__(self${1:, args}):\n    $2\n$0")
(snippet-define! 'python "prop" "@property\ndef ${1:name}(self):\n    return self._${1:name}\n$0")
(snippet-define! 'python "imp" "import ${1:module}\n$0")
(snippet-define! 'python "from" "from ${1:module} import ${2:name}\n$0")

;;; ============================================================================
;;; Built-in snippets — JavaScript / TypeScript
;;; ============================================================================

(snippet-define! 'javascript "fn" "function ${1:name}(${2:args}) {\n  $3\n}\n$0")
(snippet-define! 'javascript "afn" "async function ${1:name}(${2:args}) {\n  $3\n}\n$0")
(snippet-define! 'javascript "arr" "(${1:args}) => {\n  $2\n}\n$0")
(snippet-define! 'javascript "class" "class ${1:Name} {\n  constructor(${2:args}) {\n    $3\n  }\n}\n$0")
(snippet-define! 'javascript "for" "for (let ${1:i} = 0; ${1:i} < ${2:arr}.length; ${1:i}++) {\n  $3\n}\n$0")
(snippet-define! 'javascript "fore" "${1:arr}.forEach((${2:item}) => {\n  $3\n});\n$0")
(snippet-define! 'javascript "if" "if (${1:condition}) {\n  $2\n}\n$0")
(snippet-define! 'javascript "try" "try {\n  $1\n} catch (${2:err}) {\n  $3\n}\n$0")
(snippet-define! 'javascript "imp" "import ${1:name} from '${2:module}';\n$0")
(snippet-define! 'javascript "exp" "export ${1:default }$2\n$0")
(snippet-define! 'javascript "cl" "console.log(${1:value});\n$0")
(snippet-define! 'javascript "const" "const ${1:name} = $2;\n$0")
(snippet-define! 'javascript "let" "let ${1:name} = $2;\n$0")

;;; ============================================================================
;;; Built-in snippets — C / C++
;;; ============================================================================

(snippet-define! 'cpp "inc" "#include <${1:stdio.h}>\n$0")
(snippet-define! 'cpp "incl" "#include \"${1:header.h}\"\n$0")
(snippet-define! 'cpp "main" "int main(int argc, char *argv[]) {\n    $1\n    return 0;\n}\n$0")
(snippet-define! 'cpp "for" "for (int ${1:i} = 0; ${1:i} < ${2:n}; ${1:i}++) {\n    $3\n}\n$0")
(snippet-define! 'cpp "while" "while (${1:condition}) {\n    $2\n}\n$0")
(snippet-define! 'cpp "if" "if (${1:condition}) {\n    $2\n}\n$0")
(snippet-define! 'cpp "switch" "switch (${1:expr}) {\ncase ${2:val}:\n    $3\n    break;\ndefault:\n    $4\n}\n$0")
(snippet-define! 'cpp "struct" "struct ${1:Name} {\n    $2\n};\n$0")
(snippet-define! 'cpp "typedef" "typedef ${1:type} ${2:Name};\n$0")
(snippet-define! 'cpp "printf" "printf(\"${1:%s}\\n\", ${2:arg});\n$0")

;;; ============================================================================
;;; Built-in snippets — Go
;;; ============================================================================

(snippet-define! 'go "fn" "func ${1:name}(${2:args}) ${3:error} {\n\t$4\n}\n$0")
(snippet-define! 'go "main" "func main() {\n\t$1\n}\n$0")
(snippet-define! 'go "if" "if ${1:condition} {\n\t$2\n}\n$0")
(snippet-define! 'go "ife" "if err != nil {\n\t${1:return err}\n}\n$0")
(snippet-define! 'go "for" "for ${1:i} := 0; ${1:i} < ${2:n}; ${1:i}++ {\n\t$3\n}\n$0")
(snippet-define! 'go "forr" "for ${1:k}, ${2:v} := range ${3:collection} {\n\t$4\n}\n$0")
(snippet-define! 'go "switch" "switch ${1:expr} {\ncase ${2:val}:\n\t$3\ndefault:\n\t$4\n}\n$0")
(snippet-define! 'go "struct" "type ${1:Name} struct {\n\t$2\n}\n$0")
(snippet-define! 'go "iface" "type ${1:Name} interface {\n\t$2\n}\n$0")
(snippet-define! 'go "goroutine" "go func() {\n\t$1\n}()\n$0")
(snippet-define! 'go "defer" "defer ${1:func()}\n$0")

;;; ============================================================================
;;; Built-in snippets — Rust
;;; ============================================================================

(snippet-define! 'rust "fn" "fn ${1:name}(${2:args}) -> ${3:()}{{\n    $4\n}}\n$0")
(snippet-define! 'rust "main" "fn main() {{\n    $1\n}}\n$0")
(snippet-define! 'rust "struct" "struct ${1:Name} {{\n    $2\n}}\n$0")
(snippet-define! 'rust "impl" "impl ${1:Type} {{\n    $2\n}}\n$0")
(snippet-define! 'rust "enum" "enum ${1:Name} {{\n    $2\n}}\n$0")
(snippet-define! 'rust "match" "match ${1:expr} {{\n    ${2:pattern} => $3,\n}}\n$0")
(snippet-define! 'rust "if" "if ${1:condition} {{\n    $2\n}}\n$0")
(snippet-define! 'rust "for" "for ${1:item} in ${2:iter} {{\n    $3\n}}\n$0")
(snippet-define! 'rust "let" "let ${1:name} = $2;\n$0")
(snippet-define! 'rust "letm" "let mut ${1:name} = $2;\n$0")
(snippet-define! 'rust "println" "println!(\"${1:{}}\", ${2:val});\n$0")

;;; ============================================================================
;;; Built-in snippets — HTML
;;; ============================================================================

(snippet-define! 'hypertext "html" "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n    <meta charset=\"UTF-8\">\n    <title>${1:Title}</title>\n</head>\n<body>\n    $2\n</body>\n</html>\n$0")
(snippet-define! 'hypertext "div" "<div${1: class=\"$2\"}>$3</div>\n$0")
(snippet-define! 'hypertext "span" "<span${1: class=\"$2\"}>$3</span>\n$0")
(snippet-define! 'hypertext "a" "<a href=\"${1:url}\">${2:text}</a>\n$0")
(snippet-define! 'hypertext "img" "<img src=\"${1:url}\" alt=\"${2:desc}\">\n$0")
(snippet-define! 'hypertext "form" "<form action=\"${1:url}\" method=\"${2:post}\">\n    $3\n</form>\n$0")
(snippet-define! 'hypertext "input" "<input type=\"${1:text}\" name=\"${2:name}\"${3: value=\"$4\"}>\n$0")
(snippet-define! 'hypertext "ul" "<ul>\n    <li>$1</li>\n</ul>\n$0")
(snippet-define! 'hypertext "table" "<table>\n    <tr>\n        <th>$1</th>\n    </tr>\n    <tr>\n        <td>$2</td>\n    </tr>\n</table>\n$0")
(snippet-define! 'hypertext "script" "<script>\n    $1\n</script>\n$0")
(snippet-define! 'hypertext "style" "<style>\n    $1\n</style>\n$0")
(snippet-define! 'hypertext "link" "<link rel=\"stylesheet\" href=\"${1:style.css}\">\n$0")

;;; ============================================================================
;;; Built-in snippets — Shell / Bash
;;; ============================================================================

(snippet-define! 'bash "if" "if [ ${1:condition} ]; then\n    $2\nfi\n$0")
(snippet-define! 'bash "ife" "if [ ${1:condition} ]; then\n    $2\nelse\n    $3\nfi\n$0")
(snippet-define! 'bash "for" "for ${1:var} in ${2:list}; do\n    $3\ndone\n$0")
(snippet-define! 'bash "while" "while ${1:condition}; do\n    $2\ndone\n$0")
(snippet-define! 'bash "case" "case ${1:var} in\n    ${2:pattern})\n        $3\n        ;;\n    *)\n        $4\n        ;;\nesac\n$0")
(snippet-define! 'bash "func" "${1:name}() {\n    $2\n}\n$0")
(snippet-define! 'bash "shebang" "#!/usr/bin/env ${1:bash}\n$0")

;;; ============================================================================
;;; Built-in snippets — Markdown
;;; ============================================================================

(snippet-define! 'markdown "link" "[${1:text}](${2:url})\n$0")
(snippet-define! 'markdown "img" "![${1:alt}](${2:url})\n$0")
(snippet-define! 'markdown "code" "```${1:lang}\n$2\n```\n$0")
(snippet-define! 'markdown "table" "| ${1:Header} | ${2:Header} |\n|----------|----------|\n| $3 | $4 |\n$0")
(snippet-define! 'markdown "h1" "# ${1:Title}\n$0")
(snippet-define! 'markdown "h2" "## ${1:Title}\n$0")
(snippet-define! 'markdown "h3" "### ${1:Title}\n$0")

;;; ============================================================================
;;; Built-in snippets — Global (all languages)
;;; ============================================================================

(snippet-define! 'global "todo" ";; TODO: $1\n$0")
(snippet-define! 'global "fixme" ";; FIXME: $1\n$0")
(snippet-define! 'global "note" ";; NOTE: $1\n$0")
(snippet-define! 'global "date" "$1\n$0")  ;; User fills in date
(snippet-define! 'global "hr" "---\n$0")

;;; ============================================================================
;;; Initialize: load user snippets on module load
;;; ============================================================================

(def (snippet-init!)
  "Load user snippets from ~/.gemacs-snippets/ if it exists."
  (let ((home (getenv "HOME" #f)))
    (when home
      (load-snippet-directory! (string-append home "/.gemacs-snippets")))))

;; Auto-load on import
(snippet-init!)
