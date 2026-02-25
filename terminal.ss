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
        terminal-prompt
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
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gsh/lib
        :gsh/environment
        :gemacs/core)

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
   bold?)      ; ANSI bold/bright flag (shifts color index +8)
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
  "Create a gsh-backed terminal and return a terminal-state."
  (let ((env (gsh-init!)))
    (make-terminal-state env 0 -1 #f)))

(def (terminal-prompt ts)
  "Return the terminal prompt string showing cwd."
  (let* ((env (terminal-state-env ts))
         (cwd (or (env-get env "PWD") (current-directory)))
         (home (getenv "HOME" ""))
         (display-cwd (if (and (> (string-length home) 0)
                               (string-prefix? home cwd))
                        (string-append "~" (substring cwd (string-length home)
                                                          (string-length cwd)))
                        cwd)))
    (string-append display-cwd " $ ")))

(def (terminal-execute! input ts)
  "Execute a command via gsh, return (values output-string new-cwd).
   Output may be a string, 'clear, or 'exit."
  (let ((env (terminal-state-env ts))
        (trimmed (string-trim-both input)))
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
           (let-values (((output status) (gsh-capture trimmed env)))
             (values (or output "")
                     (or (env-get env "PWD") (current-directory))))))))))

(def (terminal-stop! ts)
  "Clean up the terminal state (no-op for gsh-backed terminals)."
  (void))

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
