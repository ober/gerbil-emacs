;;; -*- Gerbil -*-
;;; Eshell: Gerbil-powered shell with built-in commands
;;;
;;; Features:
;;; - Built-in commands implemented in Gerbil (cd, ls, pwd, cat, echo, etc.)
;;; - Pipeline support (cmd1 | cmd2 | cmd3)
;;; - Gerbil expression evaluation (lines starting with '(')
;;; - External command execution for anything not built-in

(export eshell-buffer?
        *eshell-state*
        eshell-prompt
        eshell-process-input
        eshell-format-ls)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/process
        :std/pregexp
        :gerbil-emacs/core)

;;;============================================================================
;;; Eshell state
;;;============================================================================

(def (eshell-buffer? buf)
  "Check if this buffer is an eshell buffer."
  (eq? (buffer-lexer-lang buf) 'eshell))

;; Maps eshell buffers to their state (current directory, history, etc.)
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *eshell-state* (make-hash-table-eq))

(def eshell-prompt "eshell> ")

;;;============================================================================
;;; Built-in commands
;;;============================================================================

(def *eshell-builtins* (make-hash-table))

(def (register-builtin! name proc)
  (hash-put! *eshell-builtins* name proc))

(def (builtin? name)
  (hash-get *eshell-builtins* name))

;; pwd
(register-builtin! "pwd"
  (lambda (args cwd)
    (values (string-append cwd "\n") cwd)))

;; cd
(register-builtin! "cd"
  (lambda (args cwd)
    (let* ((target (if (null? args)
                     (getenv "HOME" "/")
                     (car args)))
           (path (if (string-prefix? "/" target)
                   target
                   (path-expand target cwd))))
      (if (and (file-exists? path)
               (eq? 'directory (file-info-type (file-info path))))
        (values "" path)
        (values (string-append "cd: no such directory: " target "\n") cwd)))))

;; echo
(register-builtin! "echo"
  (lambda (args cwd)
    (values (string-append (string-join args " ") "\n") cwd)))

;; cat
(register-builtin! "cat"
  (lambda (args cwd)
    (if (null? args)
      (values "" cwd)
      (let ((output
             (string-join
               (map (lambda (file)
                      (let ((path (if (string-prefix? "/" file) file
                                    (string-append cwd "/" file))))
                        (if (file-exists? path)
                          (call-with-input-file path
                            (lambda (port) (read-line port #f)))
                          (string-append "cat: " file ": No such file\n"))))
                    args)
               "")))
        (values output cwd)))))

;; ls
(register-builtin! "ls"
  (lambda (args cwd)
    (let* ((show-long? (member "-l" args))
           (show-all? (member "-a" args))
           (dirs (filter (lambda (a) (not (string-prefix? "-" a))) args))
           (target (if (null? dirs) cwd
                     (let ((d (car dirs)))
                       (if (string-prefix? "/" d) d
                         (string-append cwd "/" d))))))
      (if (and (file-exists? target)
               (eq? 'directory (file-info-type (file-info target))))
        (let ((text (eshell-format-ls target show-long? show-all?)))
          (values text cwd))
        (values (string-append "ls: cannot access '" target "': No such file or directory\n") cwd)))))

;; mkdir
(register-builtin! "mkdir"
  (lambda (args cwd)
    (if (null? args)
      (values "mkdir: missing operand\n" cwd)
      (begin
        (for-each
          (lambda (dir)
            (let ((path (if (string-prefix? "/" dir) dir
                          (string-append cwd "/" dir))))
              (create-directory path)))
          args)
        (values "" cwd)))))

;; rm
(register-builtin! "rm"
  (lambda (args cwd)
    (if (null? args)
      (values "rm: missing operand\n" cwd)
      (let ((output ""))
        (for-each
          (lambda (file)
            (let ((path (if (string-prefix? "/" file) file
                          (string-append cwd "/" file))))
              (if (file-exists? path)
                (with-catch
                  (lambda (e)
                    (set! output (string-append output "rm: " file ": "
                                   (with-output-to-string
                                     (lambda () (display-exception e))) "\n")))
                  (lambda () (delete-file path)))
                (set! output (string-append output "rm: " file ": No such file\n")))))
          args)
        (values output cwd)))))

;; cp
(register-builtin! "cp"
  (lambda (args cwd)
    (if (< (length args) 2)
      (values "cp: missing operand\n" cwd)
      (let* ((src-name (car args))
             (dst-name (cadr args))
             (src (if (string-prefix? "/" src-name) src-name
                    (string-append cwd "/" src-name)))
             (dst (if (string-prefix? "/" dst-name) dst-name
                    (string-append cwd "/" dst-name))))
        (if (file-exists? src)
          (begin
            (copy-file src dst)
            (values "" cwd))
          (values (string-append "cp: " src-name ": No such file\n") cwd))))))

;; mv
(register-builtin! "mv"
  (lambda (args cwd)
    (if (< (length args) 2)
      (values "mv: missing operand\n" cwd)
      (let* ((src-name (car args))
             (dst-name (cadr args))
             (src (if (string-prefix? "/" src-name) src-name
                    (string-append cwd "/" src-name)))
             (dst (if (string-prefix? "/" dst-name) dst-name
                    (string-append cwd "/" dst-name))))
        (if (file-exists? src)
          (begin
            (rename-file src dst)
            (values "" cwd))
          (values (string-append "mv: " src-name ": No such file\n") cwd))))))

;; wc (word count)
(register-builtin! "wc"
  (lambda (args cwd)
    (if (null? args)
      (values "wc: missing operand\n" cwd)
      (let ((output
             (string-join
               (map (lambda (file)
                      (let ((path (if (string-prefix? "/" file) file
                                    (string-append cwd "/" file))))
                        (if (file-exists? path)
                          (let* ((text (call-with-input-file path
                                         (lambda (port) (read-line port #f))))
                                 (lines (length (pregexp-split "\n" text)))
                                 (words (length (pregexp-split "\\s+" text)))
                                 (chars (string-length text)))
                            (string-append
                              (number->string lines) "\t"
                              (number->string words) "\t"
                              (number->string chars) "\t"
                              file))
                          (string-append "wc: " file ": No such file"))))
                    args)
               "\n")))
        (values (string-append output "\n") cwd)))))

;; head
(register-builtin! "head"
  (lambda (args cwd)
    (let* ((n-flag (member "-n" args))
           (n (if (and n-flag (pair? (cdr n-flag)))
                (or (string->number (cadr n-flag)) 10)
                10))
           (files (filter (lambda (a) (and (not (string=? a "-n"))
                                            (not (and n-flag (equal? a (cadr n-flag))))
                                            (not (string-prefix? "-" a))))
                          args)))
      (if (null? files)
        (values "head: missing operand\n" cwd)
        (let ((path (let ((f (car files)))
                      (if (string-prefix? "/" f) f
                        (string-append cwd "/" f)))))
          (if (file-exists? path)
            (let* ((text (call-with-input-file path
                           (lambda (port) (read-line port #f))))
                   (all-lines (pregexp-split "\n" text))
                   (taken (let loop ((ls all-lines) (i 0) (acc []))
                            (if (or (null? ls) (>= i n))
                              (reverse acc)
                              (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
              (values (string-append (string-join taken "\n") "\n") cwd))
            (values (string-append "head: " (car files) ": No such file\n") cwd)))))))

;; tail
(register-builtin! "tail"
  (lambda (args cwd)
    (let* ((n-flag (member "-n" args))
           (n (if (and n-flag (pair? (cdr n-flag)))
                (or (string->number (cadr n-flag)) 10)
                10))
           (files (filter (lambda (a) (and (not (string=? a "-n"))
                                            (not (and n-flag (equal? a (cadr n-flag))))
                                            (not (string-prefix? "-" a))))
                          args)))
      (if (null? files)
        (values "tail: missing operand\n" cwd)
        (let ((path (let ((f (car files)))
                      (if (string-prefix? "/" f) f
                        (string-append cwd "/" f)))))
          (if (file-exists? path)
            (let* ((text (call-with-input-file path
                           (lambda (port) (read-line port #f))))
                   (all-lines (pregexp-split "\n" text))
                   (total (length all-lines))
                   (start (max 0 (- total n)))
                   (taken (let loop ((ls all-lines) (i 0) (acc []))
                            (cond
                              ((null? ls) (reverse acc))
                              ((>= i start) (loop (cdr ls) (+ i 1) (cons (car ls) acc)))
                              (else (loop (cdr ls) (+ i 1) acc))))))
              (values (string-append (string-join taken "\n") "\n") cwd))
            (values (string-append "tail: " (car files) ": No such file\n") cwd)))))))

;; which
(register-builtin! "which"
  (lambda (args cwd)
    (if (null? args)
      (values "which: missing argument\n" cwd)
      (let* ((name (car args))
             (path-dirs (pregexp-split ":" (getenv "PATH" "")))
             (found (let loop ((dirs path-dirs))
                      (if (null? dirs) #f
                        (let ((full (string-append (car dirs) "/" name)))
                          (if (file-exists? full) full
                            (loop (cdr dirs))))))))
        (if found
          (values (string-append found "\n") cwd)
          (values (string-append name " not found\n") cwd))))))

;; env
(register-builtin! "env"
  (lambda (args cwd)
    (if (null? args)
      ;; Show some common env vars
      (let ((vars '("HOME" "PATH" "SHELL" "USER" "LANG" "TERM"
                     "GERBIL_HOME" "GERBIL_PATH")))
        (values
          (string-join
            (filter-map (lambda (v)
                          (let ((val (getenv v #f)))
                            (and val (string-append v "=" val))))
                        vars)
            "\n"
            'suffix)
          cwd))
      ;; Show specific var
      (let ((val (getenv (car args) #f)))
        (if val
          (values (string-append val "\n") cwd)
          (values (string-append (car args) ": not set\n") cwd))))))

;; export (set env var)
(register-builtin! "export"
  (lambda (args cwd)
    (if (null? args)
      (values "export: usage: export NAME=VALUE\n" cwd)
      (begin
        (for-each
          (lambda (arg)
            (let ((eq-pos (string-contains arg "=")))
              (when eq-pos
                (let ((name (substring arg 0 eq-pos))
                      (val (substring arg (+ eq-pos 1) (string-length arg))))
                  (setenv name val)))))
          args)
        (values "" cwd)))))

;; clear
(register-builtin! "clear"
  (lambda (args cwd)
    (values 'clear cwd)))

;; exit
(register-builtin! "exit"
  (lambda (args cwd)
    (values 'exit cwd)))

;; grep
(register-builtin! "grep"
  (lambda (args cwd)
    (if (< (length args) 2)
      (values "grep: usage: grep PATTERN FILE...\n" cwd)
      (let* ((pattern (car args))
             (files (cdr args))
             (rx (with-catch (lambda (e) #f)
                   (lambda () (pregexp pattern)))))
        (if (not rx)
          (values (string-append "grep: invalid pattern: " pattern "\n") cwd)
          (let ((output
                 (string-join
                   (filter-map
                     (lambda (file)
                       (let ((path (if (string-prefix? "/" file) file
                                     (string-append cwd "/" file))))
                         (if (file-exists? path)
                           (let* ((text (call-with-input-file path
                                          (lambda (port) (read-line port #f))))
                                  (lines (pregexp-split "\n" text))
                                  (matches (filter (lambda (line)
                                                     (pregexp-match rx line))
                                                   lines)))
                             (if (null? matches) #f
                               (string-join
                                 (map (lambda (line)
                                        (if (> (length files) 1)
                                          (string-append file ":" line)
                                          line))
                                      matches)
                                 "\n")))
                           (string-append "grep: " file ": No such file"))))
                     files)
                   "\n")))
            (values (if (string=? output "") ""
                      (string-append output "\n"))
                    cwd)))))))

;; find
(register-builtin! "find"
  (lambda (args cwd)
    (let* ((dir (if (null? args) cwd
                  (let ((d (car args)))
                    (if (string-prefix? "/" d) d
                      (string-append cwd "/" d)))))
           (name-flag (member "-name" args))
           (name-pattern (and name-flag (pair? (cdr name-flag))
                              (cadr name-flag)))
           (results []))
      (if (not (and (file-exists? dir)
                    (eq? 'directory (file-info-type (file-info dir)))))
        (values (string-append "find: '" dir "': No such directory\n") cwd)
        (begin
          (walk-dir! dir
            (lambda (path)
              (if name-pattern
                ;; Convert glob pattern to regex
                (let* ((rx-str (glob->regex name-pattern))
                       (rx (pregexp rx-str))
                       (basename (path-strip-directory path)))
                  (when (pregexp-match rx basename)
                    (set! results (cons path results))))
                (set! results (cons path results)))))
          (values (if (null? results) ""
                    (string-append
                      (string-join (sort (reverse results) string<?) "\n")
                      "\n"))
                  cwd))))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (eshell-format-ls dir show-long? show-all?)
  "Format ls output."
  (let* ((entries (directory-files
                    (list path: dir
                          ignore-hidden: (if show-all? #f 'dot-and-dot-dot))))
         (sorted (sort entries string<?)))
    (if show-long?
      ;; Long format
      (string-append
        (string-join
          (map (lambda (name)
                 (let ((full (string-append dir "/" name)))
                   (with-catch
                     (lambda (e) (string-append "?????????? " name))
                     (lambda ()
                       (let* ((info (file-info full))
                              (type (file-info-type info))
                              (mode (file-info-mode info))
                              (size (file-info-size info))
                              (type-char (case type
                                           ((directory) #\d)
                                           ((symbolic-link) #\l)
                                           (else #\-))))
                         (string-append
                           (string type-char)
                           (mode->permission-string mode)
                           " " (number->string size)
                           "\t" name
                           (if (eq? type 'directory) "/" "")))))))
               sorted)
          "\n")
        "\n")
      ;; Short format: names separated by spaces
      (string-append (string-join sorted "  ") "\n"))))

(def (mode->permission-string mode)
  (let ((p (bitwise-and mode #o777)))
    (string
      (if (not (zero? (bitwise-and p #o400))) #\r #\-)
      (if (not (zero? (bitwise-and p #o200))) #\w #\-)
      (if (not (zero? (bitwise-and p #o100))) #\x #\-)
      (if (not (zero? (bitwise-and p #o040))) #\r #\-)
      (if (not (zero? (bitwise-and p #o020))) #\w #\-)
      (if (not (zero? (bitwise-and p #o010))) #\x #\-)
      (if (not (zero? (bitwise-and p #o004))) #\r #\-)
      (if (not (zero? (bitwise-and p #o002))) #\w #\-)
      (if (not (zero? (bitwise-and p #o001))) #\x #\-))))

(def (walk-dir! dir proc)
  "Recursively walk dir, calling (proc full-path) for each entry."
  (for-each
    (lambda (name)
      (unless (member name '("." ".."))
        (let ((full (string-append dir "/" name)))
          (with-catch
            (lambda (e) (void))  ; skip unreadable entries
            (lambda ()
              (proc full)
              (when (eq? (file-info-type (file-info full)) 'directory)
                (walk-dir! full proc)))))))
    (directory-files dir)))

(def (glob->regex pattern)
  "Convert a simple glob pattern (with * and ?) to a regex."
  (let loop ((chars (string->list pattern)) (acc "^"))
    (if (null? chars)
      (string-append acc "$")
      (let ((ch (car chars)))
        (cond
          ((char=? ch #\*) (loop (cdr chars) (string-append acc ".*")))
          ((char=? ch #\?) (loop (cdr chars) (string-append acc ".")))
          ((char=? ch #\.) (loop (cdr chars) (string-append acc "\\.")))
          (else (loop (cdr chars) (string-append acc (string ch)))))))))

;;;============================================================================
;;; Input processing
;;;============================================================================

(def (eshell-process-input input cwd)
  "Process an eshell input line.
   Returns (values output new-cwd).
   output can be:
     - a string to display
     - 'clear to clear the buffer
     - 'exit to close the eshell"
  (let ((trimmed (string-trim-both input)))
    (cond
      ;; Empty input
      ((string=? trimmed "")
       (values "" cwd))

      ;; Gerbil expression (starts with open paren)
      ((char=? (string-ref trimmed 0) #\()
       (eshell-eval-expression trimmed cwd))

      ;; Pipeline (contains |)
      ((string-contains trimmed "|")
       (eshell-process-pipeline trimmed cwd))

      ;; Regular command
      (else
       (eshell-execute-command trimmed cwd)))))

(def (eshell-eval-expression expr cwd)
  "Evaluate a Gerbil expression and return the result."
  (with-catch
    (lambda (e)
      (values (string-append "Error: "
                (with-output-to-string (lambda () (display-exception e)))
                "\n")
              cwd))
    (lambda ()
      (let* ((sexp (with-input-from-string expr read))
             (result (eval sexp))
             (output (with-output-to-string (lambda () (write result)))))
        (values (string-append output "\n") cwd)))))

(def (parse-command-line line)
  "Parse a command line into a list of tokens.
   Simple splitting on whitespace, respecting double quotes."
  (let loop ((chars (string->list line))
             (current "")
             (tokens [])
             (in-quote? #f))
    (cond
      ((null? chars)
       (reverse (if (string=? current "") tokens
                  (cons current tokens))))
      ((and (char=? (car chars) #\") (not in-quote?))
       (loop (cdr chars) current tokens #t))
      ((and (char=? (car chars) #\") in-quote?)
       (loop (cdr chars) current tokens #f))
      ((and (char-whitespace? (car chars)) (not in-quote?))
       (if (string=? current "")
         (loop (cdr chars) "" tokens #f)
         (loop (cdr chars) "" (cons current tokens) #f)))
      (else
       (loop (cdr chars) (string-append current (string (car chars)))
             tokens in-quote?)))))

(def (eshell-execute-command line cwd)
  "Execute a single command (builtin or external)."
  (let* ((tokens (parse-command-line line))
         (cmd (if (null? tokens) "" (car tokens)))
         (args (if (null? tokens) [] (cdr tokens)))
         (builtin (builtin? cmd)))
    (if builtin
      (with-catch
        (lambda (e)
          (values (string-append cmd ": "
                    (with-output-to-string (lambda () (display-exception e)))
                    "\n")
                  cwd))
        (lambda () (builtin args cwd)))
      ;; External command
      (eshell-run-external cmd args cwd))))

(def (eshell-run-external cmd args cwd)
  "Run an external command."
  (with-catch
    (lambda (e)
      (values (string-append cmd ": "
                (with-output-to-string (lambda () (display-exception e)))
                "\n")
              cwd))
    (lambda ()
      (let* ((full-args (cons cmd args))
             (output (parameterize ((current-directory cwd))
                       (run-process full-args))))
        (values output cwd)))))

(def (eshell-process-pipeline line cwd)
  "Process a pipeline: cmd1 | cmd2 | cmd3."
  (let* ((segments (map string-trim-both (pregexp-split "\\|" line)))
         ;; Execute first command
         (first-output
           (let-values (((out new-cwd) (eshell-execute-command (car segments) cwd)))
             (if (string? out) out ""))))
    ;; Pipe through remaining commands
    (let loop ((remaining (cdr segments)) (input first-output))
      (if (null? remaining)
        (values input cwd)
        (let* ((seg (car remaining))
               (tokens (parse-command-line seg))
               (cmd (if (null? tokens) "" (car tokens))))
          (if (builtin? cmd)
            ;; Can't easily pipe into builtins; just run them normally
            (let-values (((out new-cwd) (eshell-execute-command seg cwd)))
              (loop (cdr remaining) (if (string? out) out "")))
            ;; External: use filter-with-process with writer/reader
            (let ((output
                    (with-catch
                      (lambda (e) input)
                      (lambda ()
                        (filter-with-process
                          tokens
                          (lambda (port) (display input port))
                          (lambda (port) (read-line port #f))
                          directory: cwd)))))
              (loop (cdr remaining) (if output output "")))))))))
