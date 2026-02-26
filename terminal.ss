;;; -*- Gerbil -*-
;;; Terminal mode: gsh-backed shell with ANSI color rendering
;;;
;;; Uses gerbil-shell (gsh) for in-process POSIX shell execution.
;;; Parses ANSI SGR escape sequences for colors and renders them
;;; via Scintilla styles.

(export terminal-buffer?
        *terminal-state*
        (struct-out terminal-state)
        terminal-start!
        terminal-execute!
        terminal-execute-async!
        terminal-poll-output
        terminal-interrupt!
        terminal-resize!
        terminal-send-input!
        terminal-pty-busy?
        terminal-cleanup-pty!
        terminal-prompt
        terminal-prompt-raw
        terminal-stop!
        setup-terminal-styles!
        parse-ansi-segments
        ;; Scintilla styling constants
        SCI_STARTSTYLING
        SCI_SETSTYLING
        terminal-insert-styled!
        color-to-style
        (struct-out text-segment)
        ;; Terminal style base
        *term-style-base*)

(import :std/sugar
        :std/srfi/13
        :std/misc/channel
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gsh/lib
        :gsh/environment
        :gsh/startup
        :gsh/registry
        (only-in :gsh/prompt expand-prompt)
        :gemacs/core
        :gemacs/pty)

;;;============================================================================
;;; Scintilla styling message IDs (not in constants.ss)
;;;============================================================================

(def SCI_STARTSTYLING 2032)
(def SCI_SETSTYLING   2033)

;;;============================================================================
;;; Terminal ANSI color palette (standard 16 colors)
;;;============================================================================

;; Styles 64-79 reserved for terminal colors
(def *term-style-base* 64)

;; Standard terminal colors (dark theme)
(def *term-colors*
  (vector
   ;; Normal colors (0-7)
   #x000000  ; 0 black
   #xcc6666  ; 1 red
   #xb5bd68  ; 2 green
   #xf0c674  ; 3 yellow
   #x81a2be  ; 4 blue
   #xb294bb  ; 5 magenta
   #x8abeb7  ; 6 cyan
   #xc5c8c6  ; 7 white (light gray)
   ;; Bright colors (8-15)
   #x969896  ; 8 bright black (dark gray)
   #xde935f  ; 9 bright red
   #xa3be8c  ; 10 bright green
   #xebcb8b  ; 11 bright yellow
   #x5f819d  ; 12 bright blue
   #x85678f  ; 13 bright magenta
   #x5e8d87  ; 14 bright cyan
   #xffffff  ; 15 bright white
   ))

;;;============================================================================
;;; Terminal state
;;;============================================================================

(def (terminal-buffer? buf)
  "Check if this buffer is a terminal buffer."
  (eq? (buffer-lexer-lang buf) 'terminal))

;; Maps terminal buffers to their terminal-state structs
;; Use eq? table: buffer structs are mutable (transparent: #t)
(def *terminal-state* (make-hash-table-eq))

(defstruct terminal-state
  (env         ; gsh shell-environment
   prompt-pos  ; character position where current input starts (after prompt)
   fg-color    ; current ANSI foreground color index (0-15, or -1 for default)
   bold?       ; ANSI bold/bright flag (shifts color index +8)
   ;; Async PTY fields
   pty-master  ; master fd (int) or #f when no PTY child running
   pty-pid     ; child PID or #f
   pty-channel ; channel for reader thread -> UI communication, or #f
   pty-thread) ; reader thread or #f
  transparent: #t)

;;;============================================================================
;;; Terminal style setup
;;;============================================================================

(def (setup-terminal-styles! ed)
  "Configure Scintilla styles 64-79 for terminal ANSI colors.
   Must be called AFTER setup-editor-theme! (which calls STYLECLEARALL)."
  ;; Style 64 = default terminal text (inherits from STYLE_DEFAULT)
  (let loop ((i 0))
    (when (< i 16)
      (let ((style (+ *term-style-base* i))
            (color (vector-ref *term-colors* i)))
        (send-message ed SCI_STYLESETFORE style color)
        (send-message ed SCI_STYLESETBACK style #x181818))
      (loop (+ i 1)))))

;;;============================================================================
;;; ANSI escape sequence parsing
;;;============================================================================

;; A text segment with associated ANSI color
(defstruct text-segment
  (text      ; string
   fg-color  ; 0-15 or -1 (default)
   bold?)    ; boolean
  transparent: #t)

(def (parse-ansi-segments str)
  "Parse ANSI SGR escape sequences from a string.
   Returns a list of text-segment structs.
   Handles ESC[...m (SGR), strips other ESC sequences.
   Also strips carriage returns."
  (let ((len (string-length str))
        (esc (integer->char 27))
        (bel (integer->char 7)))
    (let loop ((i 0) (fg -1) (bold? #f) (text-acc []) (segments []))
      (if (>= i len)
        ;; Flush remaining text
        (let ((final-text (list->string (reverse text-acc))))
          (reverse
            (if (string=? final-text "")
              segments
              (cons (make-text-segment final-text fg bold?) segments))))
        (let ((ch (string-ref str i)))
          (cond
            ;; Skip carriage returns
            ((char=? ch #\return)
             (loop (+ i 1) fg bold? text-acc segments))

            ;; ESC sequence
            ((char=? ch esc)
             ;; Flush current text
             (let* ((current-text (list->string (reverse text-acc)))
                    (new-segments
                      (if (string=? current-text "")
                        segments
                        (cons (make-text-segment current-text fg bold?) segments))))
               (if (< (+ i 1) len)
                 (let ((next (string-ref str (+ i 1))))
                   (cond
                     ;; CSI: ESC[
                     ((char=? next #\[)
                      (let-values (((new-i new-fg new-bold?)
                                    (parse-csi str (+ i 2) len fg bold?)))
                        (loop new-i new-fg new-bold? [] new-segments)))
                     ;; OSC: ESC] ... BEL
                     ((char=? next #\])
                      (let skip ((j (+ i 2)))
                        (if (>= j len) (loop j fg bold? [] new-segments)
                          (if (char=? (string-ref str j) bel)
                            (loop (+ j 1) fg bold? [] new-segments)
                            (skip (+ j 1))))))
                     ;; Character set designation: ESC ( X, ESC ) X, etc. (3 bytes)
                     ((and (memv next '(#\( #\) #\* #\+))
                           (< (+ i 2) len))
                      (loop (+ i 3) fg bold? [] new-segments))
                     ;; Other: ESC + single char (skip)
                     (else (loop (+ i 2) fg bold? [] new-segments))))
                 (loop (+ i 1) fg bold? [] new-segments))))

            ;; Regular character
            (else
             (loop (+ i 1) fg bold? (cons ch text-acc) segments))))))))

(def (parse-csi str i len fg bold?)
  "Parse a CSI sequence (ESC[ already consumed, i points after [).
   For SGR (m suffix), update fg/bold.
   Returns (values new-i new-fg new-bold?)."
  (let collect-params ((j i) (params []) (current ""))
    (if (>= j len)
      ;; Unterminated sequence
      (values j fg bold?)
      (let ((ch (string-ref str j)))
        (cond
          ;; Parameter digit
          ((and (char>=? ch #\0) (char<=? ch #\9))
           (collect-params (+ j 1) params (string-append current (string ch))))
          ;; Semicolon separator
          ((char=? ch #\;)
           (collect-params (+ j 1)
                           (cons (if (string=? current "") 0
                                   (or (string->number current) 0))
                                 params)
                           ""))
          ;; Final byte (determines command)
          ((and (char>=? ch #\@) (char<=? ch #\~))
           (let ((final-params
                   (reverse
                     (cons (if (string=? current "") 0
                             (or (string->number current) 0))
                           params))))
             (if (char=? ch #\m)
               ;; SGR - Select Graphic Rendition
               (let-values (((new-fg new-bold?) (apply-sgr-params final-params fg bold?)))
                 (values (+ j 1) new-fg new-bold?))
               ;; Non-SGR CSI: skip
               (values (+ j 1) fg bold?))))
          ;; Intermediate byte or unknown
          (else
           (collect-params (+ j 1) params current)))))))

(def (apply-sgr-params params fg bold?)
  "Apply SGR parameters to current state.
   Returns (values new-fg new-bold?)."
  (let loop ((ps params) (fg fg) (bold? bold?))
    (if (null? ps)
      (values fg bold?)
      (let ((p (car ps)))
        (cond
          ;; Reset
          ((= p 0)   (loop (cdr ps) -1 #f))
          ;; Bold/bright
          ((= p 1)   (loop (cdr ps) fg #t))
          ;; Normal intensity
          ((= p 22)  (loop (cdr ps) fg #f))
          ;; Foreground colors 30-37
          ((and (>= p 30) (<= p 37))
           (loop (cdr ps) (- p 30) bold?))
          ;; Default foreground
          ((= p 39)  (loop (cdr ps) -1 bold?))
          ;; Bright foreground colors 90-97
          ((and (>= p 90) (<= p 97))
           (loop (cdr ps) (+ (- p 90) 8) bold?))
          ;; Everything else (background, underline, etc.): skip
          (else       (loop (cdr ps) fg bold?)))))))

;;;============================================================================
;;; Compute style index from color state
;;;============================================================================

(def (color-to-style fg bold?)
  "Map ANSI color state to a Scintilla style index.
   Returns style index (64-79), or 0 for default."
  (cond
    ((= fg -1)
     ;; Default color
     (if bold?
       (+ *term-style-base* 15)  ; bright white for bold default
       0))                       ; STYLE_DEFAULT for normal default
    (else
     ;; Apply bold offset (shift to bright colors)
     (let ((idx (if (and bold? (< fg 8)) (+ fg 8) fg)))
       (+ *term-style-base* idx)))))

;;;============================================================================
;;; Terminal lifecycle (gsh-backed)
;;;============================================================================

(def (terminal-start!)
  "Create a gsh-backed terminal and return a terminal-state.
   Sources ~/.gshrc for PS1, aliases, etc."
  (let ((env (gsh-init! #t)))  ; interactive? = #t for alias expansion
    (env-set! env "SHELL" "gsh")
    (unless (env-get env "PS1")
      (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
    (with-catch
      (lambda (e) (void))
      (lambda () (load-startup-files! env #f #t)))
    (make-terminal-state env 0 -1 #f #f #f #f #f)))

(def (make-cmd-exec-fn env)
  "Create a command-execution function for PS1 $(...) expansion."
  (lambda (cmd)
    (with-catch
      (lambda (e) "")
      (lambda ()
        (let-values (((output status) (gsh-capture cmd env)))
          (let ((s (or output "")))
            ;; Strip trailing newline (command substitution convention)
            (if (and (> (string-length s) 0)
                     (char=? (string-ref s (- (string-length s) 1)) #\newline))
              (substring s 0 (- (string-length s) 1))
              s)))))))

(def (terminal-prompt-raw ts)
  "Return the expanded PS1 prompt string with ANSI codes intact."
  (let* ((env (terminal-state-env ts))
         (ps1 (or (env-get env "PS1") "$ "))
         (env-getter (lambda (name) (env-get env name))))
    (expand-prompt ps1 env-getter
                   0  ; job-count
                   (shell-environment-cmd-number env)
                   0  ; history-number
                   (make-cmd-exec-fn env))))

(def (terminal-prompt ts)
  "Return the expanded PS1 prompt string (ANSI stripped)."
  (strip-ansi-codes (terminal-prompt-raw ts)))

(def (strip-ansi-codes str)
  "Remove ANSI escape sequences from a string."
  (let* ((len (string-length str))
         (esc (integer->char 27))
         (bel (integer->char 7)))
    (let loop ((i 0) (acc []))
      (if (>= i len)
        (list->string (reverse acc))
        (let ((ch (string-ref str i)))
          (if (char=? ch esc)
            (if (< (+ i 1) len)
              (let ((next (string-ref str (+ i 1))))
                (cond
                  ((char=? next #\[)
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (let ((c (string-ref str j)))
                         (if (and (char>=? c #\@) (char<=? c #\~))
                           (loop (+ j 1) acc)
                           (skip (+ j 1)))))))
                  ((char=? next #\])
                   (let skip ((j (+ i 2)))
                     (if (>= j len) (loop j acc)
                       (if (char=? (string-ref str j) bel)
                         (loop (+ j 1) acc)
                         (skip (+ j 1))))))
                  ;; Character set designation: ESC ( X, ESC ) X, etc. (3 bytes)
                  ((and (memv next '(#\( #\) #\* #\+))
                        (< (+ i 2) len))
                   (loop (+ i 3) acc))
                  (else (loop (+ i 2) acc))))
              (loop (+ i 1) acc))
            (if (or (char=? ch #\return)
                    (char=? ch (integer->char 1))   ; RL_PROMPT_START_IGNORE
                    (char=? ch (integer->char 2)))   ; RL_PROMPT_END_IGNORE
              (loop (+ i 1) acc)
              (loop (+ i 1) (cons ch acc)))))))))

(def (terminal-execute! input ts)
  "Execute a command via gsh, return (values output-string new-cwd).
   Output may be a string, 'clear, or 'exit.
   Captures both stdout and stderr."
  (let ((env (terminal-state-env ts))
        (trimmed (string-trim-both input)))
    (env-inc-cmd-number! env)
    (cond
      ((string=? trimmed "")
       (values "" (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "clear")
       (values 'clear (or (env-get env "PWD") (current-directory))))
      ((string=? trimmed "exit")
       (values 'exit (or (env-get env "PWD") (current-directory))))
      (else
       (with-catch
         (lambda (e)
           (values (string-append "gsh: "
                     (with-output-to-string (lambda () (display-exception e)))
                     "\n")
                   (or (env-get env "PWD") (current-directory))))
         (lambda ()
           (let* ((err-port (open-output-string))
                  (result (parameterize ((current-error-port err-port))
                            (gsh-capture trimmed env))))
             (let-values (((stdout status) result))
               (let ((stderr (get-output-string err-port))
                     (cwd (or (env-get env "PWD") (current-directory))))
                 (values (string-append (or stdout "")
                                        (if (> (string-length stderr) 0) stderr ""))
                         cwd))))))))))

(def (terminal-stop! ts)
  "Clean up the terminal state. Kills PTY child if running."
  (terminal-cleanup-pty! ts))

;;;============================================================================
;;; Terminal text insertion with styling
;;;============================================================================

(def (terminal-insert-styled! ed segments start-pos)
  "Insert text segments into editor at end, applying ANSI styles.
   Returns the total number of bytes inserted."
  (let loop ((segs segments) (pos start-pos) (total 0))
    (if (null? segs)
      total
      (let* ((seg (car segs))
             (text (text-segment-text seg))
             (fg (text-segment-fg-color seg))
             (bold? (text-segment-bold? seg))
             (style (color-to-style fg bold?))
             (text-len (string-length text)))
        ;; Insert the text
        (editor-append-text ed text)
        ;; Apply style if not default
        (when (> style 0)
          (send-message ed SCI_STARTSTYLING pos 0)
          (send-message ed SCI_SETSTYLING text-len style))
        (loop (cdr segs) (+ pos text-len) (+ total text-len))))))

;;;============================================================================
;;; Async PTY execution
;;;============================================================================

(def (pure-simple-command? input)
  "Check if input is a simple command (no pipes, &&, ||, ;, &, backticks).
   Returns #t if safe to run as an in-process builtin."
  (let ((len (string-length input)))
    (let loop ((i 0) (in-sq? #f) (in-dq? #f))
      (if (>= i len)
        #t
        (let ((ch (string-ref input i)))
          (cond
            ;; Toggle single-quote state
            ((and (char=? ch #\') (not in-dq?))
             (loop (+ i 1) (not in-sq?) in-dq?))
            ;; Toggle double-quote state
            ((and (char=? ch #\") (not in-sq?))
             (loop (+ i 1) in-sq? (not in-dq?)))
            ;; Inside quotes: skip
            ((or in-sq? in-dq?)
             (loop (+ i 1) in-sq? in-dq?))
            ;; Metacharacters outside quotes
            ((memv ch '(#\| #\; #\& #\` #\( #\)))
             #f)
            (else
             (loop (+ i 1) in-sq? in-dq?))))))))

(def (extract-first-word input)
  "Extract the first command word from input, skipping leading var assignments.
   Returns the word or #f."
  (let* ((trimmed (string-trim input))
         (len (string-length trimmed)))
    (let loop ((i 0))
      (if (>= i len)
        #f
        (let ((ch (string-ref trimmed i)))
          (cond
            ;; Skip leading variable assignments (FOO=bar)
            ((and (or (char-alphabetic? ch) (char=? ch #\_))
                  (let scan ((j (+ i 1)))
                    (and (< j len)
                         (let ((c (string-ref trimmed j)))
                           (cond
                             ((char=? c #\=) #t)
                             ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))
                              (scan (+ j 1)))
                             (else #f))))))
             ;; Skip past the value
             (let skip-val ((j i))
               (if (>= j len) #f
                 (if (char=? (string-ref trimmed j) #\space)
                   (loop (+ j 1))
                   (skip-val (+ j 1))))))
            ;; Found start of command word
            ((not (char-whitespace? ch))
             (let end ((j i))
               (if (or (>= j len) (char-whitespace? (string-ref trimmed j)))
                 (substring trimmed i j)
                 (end (+ j 1)))))
            (else (loop (+ i 1)))))))))

(def (pty-waitpid-status pid nohang?)
  "Wait for child and return just the exit status integer."
  (let-values (((status exited?) (pty-waitpid pid nohang?)))
    status))

(def (pty-reader-loop mfd pid ch)
  "Reader thread body: poll PTY output and post to channel.
   Posts (cons 'data string) for output chunks,
   (cons 'done exit-status) when child exits."
  (with-catch
    (lambda (e)
      (gemacs-log! "PTY reader error: "
        (with-output-to-string
          (lambda () (display-exception e))))
      (channel-put ch (cons 'done -1)))
    (lambda ()
      (let loop ()
        (thread-sleep! 0.05)
        (let ((data (pty-read mfd)))
          (cond
            ((string? data)
             (channel-put ch (cons 'data data))
             (loop))
            ((eq? data 'eof)
             ;; Child exited, reap and get status
             (let ((status (pty-waitpid-status pid #f)))
               (channel-put ch (cons 'done status))))
            (else
             ;; EAGAIN: no data yet
             (if (pty-child-alive? pid)
               (loop)
               ;; Child died but no EOF yet, drain remaining output
               (let drain ()
                 (let ((d (pty-read mfd)))
                   (cond
                     ((string? d)
                      (channel-put ch (cons 'data d))
                      (drain))
                     (else
                      (let ((status (pty-waitpid-status pid #t)))
                        (channel-put ch (cons 'done status)))))))))))))))

(def (terminal-execute-async! input ts)
  "Execute command: builtins go through gsh-capture (sync),
   external commands go through PTY subprocess (async).
   Returns:
   - (values 'sync output cwd) for sync builtins
   - (values 'async #f #f) when command dispatched to PTY
   - (values 'special 'clear|'exit #f) for clear/exit"
  (let ((trimmed (string-trim-both input)))
    (cond
      ((string=? trimmed "")
       (values 'sync "" (or (env-get (terminal-state-env ts) "PWD")
                            (current-directory))))
      ((string=? trimmed "clear")
       (values 'special 'clear #f))
      ((string=? trimmed "exit")
       (values 'special 'exit #f))
      (else
       (let ((first-word (extract-first-word trimmed)))
         (if (and first-word
                  (builtin? first-word)
                  (pure-simple-command? trimmed))
           ;; Builtin: run in-process via gsh-capture (synchronous)
           (let-values (((output cwd) (terminal-execute! trimmed ts)))
             (values 'sync output cwd))
           ;; External/compound: spawn PTY subprocess (async)
           (let* ((env (terminal-state-env ts))
                  (env-alist (env-exported-alist env))
                  (rows 24) (cols 80))
             (let-values (((mfd pid) (pty-spawn trimmed env-alist rows cols)))
               (if (and mfd pid)
                 (let ((ch (make-channel)))
                   ;; Store PTY state
                   (set! (terminal-state-pty-master ts) mfd)
                   (set! (terminal-state-pty-pid ts) pid)
                   (set! (terminal-state-pty-channel ts) ch)
                   ;; Spawn reader thread
                   (let ((thread (spawn (lambda () (pty-reader-loop mfd pid ch)))))
                     (set! (terminal-state-pty-thread ts) thread)
                     (values 'async #f #f)))
                 ;; forkpty failed, fall back to sync
                 (let-values (((output cwd) (terminal-execute! trimmed ts)))
                   (values 'sync output cwd)))))))))))

(def (terminal-poll-output ts)
  "Non-blocking check for PTY output. Returns:
   - (cons 'data string) — output chunk
   - (cons 'done exit-status) — child exited
   - #f — nothing available"
  (let ((ch (terminal-state-pty-channel ts)))
    (and ch (channel-try-get ch))))

(def (terminal-pty-busy? ts)
  "Check if a PTY command is currently running."
  (and (terminal-state-pty-pid ts) #t))

(def (terminal-interrupt! ts)
  "Send SIGINT to the PTY child process group."
  (let ((pid (terminal-state-pty-pid ts)))
    (when pid
      (pty-kill! pid 2))))  ;; SIGINT = 2

(def (terminal-resize! ts rows cols)
  "Notify PTY child of window size change."
  (let ((master (terminal-state-pty-master ts)))
    (when master
      (pty-resize! master rows cols))))

(def (terminal-send-input! ts str)
  "Send keystrokes to PTY child's stdin."
  (let ((master (terminal-state-pty-master ts)))
    (when master
      (pty-write master str))))

(def (terminal-cleanup-pty! ts)
  "Clean up PTY resources: kill child, close fd, terminate reader thread."
  (let ((master (terminal-state-pty-master ts))
        (pid (terminal-state-pty-pid ts))
        (thread (terminal-state-pty-thread ts))
        (ch (terminal-state-pty-channel ts)))
    (when thread
      (with-catch void (lambda () (thread-terminate! thread))))
    (when (and master pid)
      (pty-close! master pid))
    (when ch
      (with-catch void (lambda () (channel-close ch))))
    (set! (terminal-state-pty-master ts) #f)
    (set! (terminal-state-pty-pid ts) #f)
    (set! (terminal-state-pty-channel ts) #f)
    (set! (terminal-state-pty-thread ts) #f)))
