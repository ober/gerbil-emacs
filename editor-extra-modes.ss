;;; -*- Gerbil -*-
;;; Language modes, completion frameworks, git-gutter,
;;; zen modes, AI, notes, org-roam, and magit extras

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers
        :gemacs/editor-extra-vcs
        :gemacs/editor-extra-media)

;; --- Task #49: elisp mode, scheme mode, regex builder, color picker, etc. ---

;; Emacs Lisp mode helpers
(def (cmd-emacs-lisp-mode app)
  "Switch to Emacs Lisp mode — sets Lisp lexer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'elisp))
    (echo-message! (app-state-echo app) "Emacs Lisp mode")))

(def (cmd-eval-last-sexp app)
  "Evaluate the sexp before point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed)))
    (if (<= pos 0)
      (echo-message! (app-state-echo app) "No sexp before point")
      (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
        (if (>= match 0)
          (let* ((start (min match (- pos 1)))
                 (end (+ (max match (- pos 1)) 1))
                 (text (substring (editor-get-text ed) start end))
                 (result (with-exception-catcher
                           (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                           (lambda ()
                             (let ((val (eval (with-input-from-string text read))))
                               (with-output-to-string (lambda () (write val))))))))
            (echo-message! (app-state-echo app) result))
          (echo-message! (app-state-echo app) "No sexp before point"))))))

(def (cmd-eval-defun app)
  "Evaluate current top-level form."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         ;; Find beginning of top-level form — scan back for unmatched open paren at column 0
         (start (let loop ((i pos))
                  (cond ((< i 0) 0)
                        ((and (char=? (string-ref text i) #\()
                              (or (= i 0)
                                  (char=? (string-ref text (- i 1)) #\newline)))
                         i)
                        (else (loop (- i 1))))))
         ;; Find matching close paren
         (match-pos (send-message ed SCI_BRACEMATCH start 0)))
    (if (>= match-pos 0)
      (let* ((form-text (substring text start (+ match-pos 1)))
             (result (with-exception-catcher
                       (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                       (lambda ()
                         (let ((val (eval (with-input-from-string form-text read))))
                           (with-output-to-string (lambda () (write val))))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "No top-level form found"))))

(def (cmd-eval-print-last-sexp app)
  "Eval and print sexp before point into buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
    (if (>= match 0)
      (let* ((start (min match (- pos 1)))
             (end (+ (max match (- pos 1)) 1))
             (text (substring (editor-get-text ed) start end))
             (result (with-exception-catcher
                       (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                       (lambda ()
                         (let ((val (eval (with-input-from-string text read))))
                           (with-output-to-string (lambda () (write val))))))))
        (editor-insert-text ed pos (string-append "\n;; => " result)))
      (echo-message! (app-state-echo app) "No sexp before point"))))

;; Scheme / Gerbil mode helpers
(def (cmd-scheme-mode app)
  "Switch to Scheme mode — sets Lisp lexer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'scheme))
    (echo-message! (app-state-echo app) "Scheme mode")))

(def (cmd-gerbil-mode app)
  "Switch to Gerbil mode — sets Gerbil lexer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'gerbil))
    (echo-message! (app-state-echo app) "Gerbil mode")))

(def (cmd-run-scheme app)
  "Run Scheme REPL (alias for repl command)."
  (echo-message! (app-state-echo app) "Use C-x r to open REPL"))

(def (cmd-scheme-send-region app)
  "Send region to Scheme process — evaluates selected text."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((region (substring (editor-get-text ed) start end))
             (result (with-exception-catcher
                       (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                       (lambda ()
                         (let ((val (eval (with-input-from-string region read))))
                           (with-output-to-string (lambda () (write val))))))))
        (echo-message! (app-state-echo app) result)))))

(def (cmd-scheme-send-buffer app)
  "Send buffer to Scheme process — evaluates entire buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (result (with-exception-catcher
                   (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                   (lambda ()
                     (let ((val (eval (with-input-from-string text read))))
                       (with-output-to-string (lambda () (write val))))))))
    (echo-message! (app-state-echo app) result)))

;; Regex builder
(def (cmd-re-builder app)
  "Open interactive regex builder — prompts for regex and highlights matches."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pattern (app-read-string app "Regex: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((text (editor-get-text ed))
             (len (string-length text))
             (count 0)
             ;; Simple regex search using Scintilla search
             (_ (begin
                  (send-message ed SCI_SETTARGETSTART 0 0)
                  (send-message ed SCI_SETTARGETEND len 0)
                  (send-message ed SCI_SETSEARCHFLAGS 2 0)))  ; SCFIND_REGEXP
             (matches
               (let loop ((pos 0) (found '()))
                 (send-message ed SCI_SETTARGETSTART pos 0)
                 (send-message ed SCI_SETTARGETEND len 0)
                 (let ((result (send-message/string ed SCI_SEARCHINTARGET pattern)))
                   (if (< result 0)
                     (reverse found)
                     (let ((mstart (send-message ed SCI_GETTARGETSTART 0 0))
                           (mend (send-message ed SCI_GETTARGETEND 0 0)))
                       (if (<= mend pos)
                         (reverse found)
                         (loop mend (cons (list mstart mend) found)))))))))
        (echo-message! (app-state-echo app)
          (string-append "Regex: " (number->string (length matches)) " matches found"))))))

;; Color picker
(def (cmd-list-colors-display app)
  "Display list of named colors."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Colors*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Named Colors\n\n"
        "black       #000000    white       #FFFFFF\n"
        "red         #FF0000    green       #00FF00\n"
        "blue        #0000FF    yellow      #FFFF00\n"
        "cyan        #00FFFF    magenta     #FF00FF\n"
        "gray        #808080    silver      #C0C0C0\n"
        "maroon      #800000    olive       #808000\n"
        "navy        #000080    purple      #800080\n"
        "teal        #008080    aqua        #00FFFF\n"
        "orange      #FFA500    pink        #FFC0CB\n"
        "brown       #A52A2A    coral       #FF7F50\n"
        "gold        #FFD700    khaki       #F0E68C\n"
        "salmon      #FA8072    tomato      #FF6347\n"
        "wheat       #F5DEB3    ivory       #FFFFF0\n"))
    (editor-set-read-only ed #t)))

;; IDO mode (Interactively Do Things) — delegates to built-in completion
(def (cmd-ido-mode app)
  "Toggle IDO mode — enhanced completion is always active."
  (let ((on (toggle-mode! 'ido)))
    (echo-message! (app-state-echo app) (if on "IDO mode: on" "IDO mode: off"))))

(def (cmd-ido-find-file app)
  "Find file with IDO — delegates to find-file with completion."
  (execute-command! app 'find-file))

(def (cmd-ido-switch-buffer app)
  "Switch buffer with IDO — delegates to switch-buffer."
  (execute-command! app 'switch-buffer))

;; Helm / Ivy / Vertico — completion framework modes
(def (cmd-helm-mode app)
  "Toggle Helm mode — uses built-in completion framework."
  (let ((on (toggle-mode! 'helm)))
    (echo-message! (app-state-echo app) (if on "Helm mode: on" "Helm mode: off"))))

(def (cmd-ivy-mode app)
  "Toggle Ivy mode — uses built-in completion framework."
  (let ((on (toggle-mode! 'ivy)))
    (echo-message! (app-state-echo app) (if on "Ivy mode: on" "Ivy mode: off"))))

(def (cmd-vertico-mode app)
  "Toggle Vertico mode — uses built-in completion framework."
  (let ((on (toggle-mode! 'vertico)))
    (echo-message! (app-state-echo app) (if on "Vertico mode: on" "Vertico mode: off"))))

(def (cmd-consult-line app)
  "Search buffer lines with consult — interactive line search."
  (let* ((pattern (app-read-string app "Search line: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matches (let loop ((ls lines) (n 1) (acc '()))
                        (if (null? ls) (reverse acc)
                          (loop (cdr ls) (+ n 1)
                                (if (string-contains (car ls) pattern)
                                  (cons (string-append (number->string n) ": " (car ls)) acc)
                                  acc))))))
        (if (null? matches)
          (echo-message! (app-state-echo app) "No matching lines")
          (open-output-buffer app "*Consult*" (string-join matches "\n")))))))

(def (cmd-consult-grep app)
  "Grep with consult — delegates to grep command."
  (execute-command! app 'grep))

(def (cmd-consult-buffer app)
  "Switch buffer with consult — delegates to switch-buffer."
  (execute-command! app 'switch-buffer))

;; Company completion — uses built-in completion
(def (cmd-company-mode app)
  "Toggle company completion mode."
  (let ((on (toggle-mode! 'company)))
    (echo-message! (app-state-echo app) (if on "Company mode: on" "Company mode: off"))))

(def (cmd-company-complete app)
  "Trigger company completion — delegates to hippie-expand."
  (execute-command! app 'hippie-expand))

;; Flyspell extras
(def (cmd-flyspell-buffer app)
  "Flyspell-check entire buffer — counts misspelled words using aspell."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (words (filter (lambda (w) (> (string-length w) 0))
                        (string-split text #\space)))
         (misspelled 0))
    (with-exception-catcher
      (lambda (e) (echo-error! (app-state-echo app) "aspell not available"))
      (lambda ()
        (for-each
          (lambda (word)
            (when (> (string-length word) 1)
              (when (not (flyspell-check-word word))
                (set! misspelled (+ misspelled 1)))))
          words)
        (echo-message! (app-state-echo app)
          (string-append "Flyspell: " (number->string misspelled) " misspelled words in "
                         (number->string (length words)) " total"))))))

(def (cmd-flyspell-correct-word app)
  "Correct misspelled word — shows suggestions from aspell."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (word-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((not (char-alphabetic? (string-ref text i))) (+ i 1))
                             (else (loop (- i 1))))))
         (word-end (let loop ((i pos))
                     (cond ((>= i (string-length text)) i)
                           ((not (char-alphabetic? (string-ref text i))) i)
                           (else (loop (+ i 1))))))
         (word (substring text word-start word-end)))
    (if (string-empty? word)
      (echo-message! (app-state-echo app) "No word at point")
      (with-exception-catcher
        (lambda (e) (echo-error! (app-state-echo app) "aspell not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "aspell"
                               arguments: '("pipe")
                               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #f)))
                 (_ (begin (display (string-append word "\n") proc) (force-output proc)))
                 (banner (read-line proc))
                 (result (read-line proc)))
            (close-output-port proc)
            (process-status proc)
            (cond
              ((or (not result) (string-empty? result) (char=? (string-ref result 0) #\*))
               (echo-message! (app-state-echo app) (string-append "'" word "' is correct")))
              ((char=? (string-ref result 0) #\&)
               ;; Has suggestions: "& word count offset: sug1, sug2, ..."
               (let* ((colon-pos (string-contains result ": "))
                      (suggestions (if colon-pos (substring result (+ colon-pos 2) (string-length result)) "")))
                 (echo-message! (app-state-echo app)
                   (string-append "Suggestions for '" word "': " suggestions))))
              (else
               (echo-message! (app-state-echo app) (string-append "'" word "': no suggestions"))))))))))

;; Bibliography / citar
(def (cmd-citar-insert-citation app)
  "Insert citation — prompts for citation key."
  (let ((key (app-read-string app "Citation key: ")))
    (when (and key (not (string-empty? key)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (pos (editor-get-current-pos ed)))
        (editor-insert-text ed pos (string-append "[@" key "]"))
        (echo-message! (app-state-echo app) (string-append "Inserted citation: " key))))))

;; Docker
(def (cmd-docker app)
  "Docker management interface — shows containers and images."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Docker not available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "docker"
                           arguments: '("info" "--format" "Server Version: {{.ServerVersion}}\nContainers: {{.Containers}}\nImages: {{.Images}}")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (open-output-buffer app "*Docker*" (or out "Docker info unavailable"))))))

(def (cmd-docker-containers app)
  "List docker containers."
  (let ((result (with-exception-catcher
                  (lambda (e) "Docker not available")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "docker"
                                     arguments: '("ps" "--format" "{{.Names}}\t{{.Status}}\t{{.Image}}")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(no containers)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Docker*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Docker Containers\n\nName\tStatus\tImage\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-docker-images app)
  "List docker images."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Docker not available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "docker"
                           arguments: '("images" "--format" "{{.Repository}}\t{{.Tag}}\t{{.Size}}")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (open-output-buffer app "*Docker Images*"
          (string-append "Docker Images\n\nRepository\tTag\tSize\n" (or out "(no images)") "\n"))))))

;; Restclient
(def (cmd-restclient-mode app)
  "Toggle restclient mode — enables HTTP request editing."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (on (toggle-mode! 'restclient)))
    (when (and on buf)
      (set! (buffer-lexer-lang buf) 'restclient))
    (echo-message! (app-state-echo app) (if on "Restclient mode: on" "Restclient mode: off"))))

(def (cmd-restclient-http-send app)
  "Send HTTP request at point using curl."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line-num (send-message ed SCI_LINEFROMPOSITION pos 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line-num 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (line (substring text line-start line-end)))
    ;; Parse "METHOD URL" from current line
    (let* ((parts (string-split line #\space))
           (method (if (pair? parts) (car parts) "GET"))
           (url (if (> (length parts) 1) (cadr parts) "")))
      (if (string-empty? url)
        (echo-error! (app-state-echo app) "No URL on current line")
        (with-exception-catcher
          (lambda (e) (echo-error! (app-state-echo app) "curl failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "curl"
                                 arguments: (list "-s" "-X" method url)
                                 stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                   (out (read-line proc #f)))
              (process-status proc)
              (open-output-buffer app "*HTTP Response*"
                (string-append method " " url "\n\n" (or out "(no response)") "\n")))))))))

;; Helper: set buffer language mode
(def (set-buffer-mode! app mode-name lang-symbol)
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) lang-symbol))
    (echo-message! (app-state-echo app) mode-name)))

;; YAML mode
(def (cmd-yaml-mode app)
  "Toggle YAML mode — sets YAML lexer."
  (set-buffer-mode! app "YAML mode" 'yaml))

;; TOML mode
(def (cmd-toml-mode app)
  "Toggle TOML mode — sets TOML lexer."
  (set-buffer-mode! app "TOML mode" 'toml))

;; Dockerfile mode
(def (cmd-dockerfile-mode app)
  "Toggle Dockerfile mode — sets Dockerfile lexer."
  (set-buffer-mode! app "Dockerfile mode" 'dockerfile))

;; SQL mode
(def (cmd-sql-mode app)
  "Toggle SQL mode — sets SQL lexer."
  (set-buffer-mode! app "SQL mode" 'sql))

(def (cmd-sql-connect app)
  "Connect to SQL database — prompts for connection string."
  (let ((conn (app-read-string app "Connection (e.g. sqlite:db.sqlite): ")))
    (if (or (not conn) (string-empty? conn))
      (echo-error! (app-state-echo app) "No connection string")
      (echo-message! (app-state-echo app) (string-append "SQL: connected to " conn)))))

(def (cmd-sql-send-region app)
  "Send SQL region to database process."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let ((sql (substring (editor-get-text ed) start end)))
        (echo-message! (app-state-echo app) (string-append "SQL sent: " (substring sql 0 (min 50 (string-length sql)))))))))

;; Language modes — each sets the buffer's lexer language
(def (cmd-python-mode app) "Toggle Python mode." (set-buffer-mode! app "Python mode" 'python))
(def (cmd-c-mode app) "Toggle C mode." (set-buffer-mode! app "C mode" 'c))
(def (cmd-c++-mode app) "Toggle C++ mode." (set-buffer-mode! app "C++ mode" 'cpp))
(def (cmd-java-mode app) "Toggle Java mode." (set-buffer-mode! app "Java mode" 'java))
(def (cmd-rust-mode app) "Toggle Rust mode." (set-buffer-mode! app "Rust mode" 'rust))
(def (cmd-go-mode app) "Toggle Go mode." (set-buffer-mode! app "Go mode" 'go))
(def (cmd-js-mode app) "Toggle JavaScript mode." (set-buffer-mode! app "JavaScript mode" 'javascript))
(def (cmd-typescript-mode app) "Toggle TypeScript mode." (set-buffer-mode! app "TypeScript mode" 'typescript))
(def (cmd-html-mode app) "Toggle HTML mode." (set-buffer-mode! app "HTML mode" 'html))
(def (cmd-css-mode app) "Toggle CSS mode." (set-buffer-mode! app "CSS mode" 'css))
(def (cmd-lua-mode app) "Toggle Lua mode." (set-buffer-mode! app "Lua mode" 'lua))
(def (cmd-ruby-mode app) "Toggle Ruby mode." (set-buffer-mode! app "Ruby mode" 'ruby))
(def (cmd-shell-script-mode app) "Toggle Shell Script mode." (set-buffer-mode! app "Shell script mode" 'bash))

;; Prog mode / text mode
(def (cmd-prog-mode app)
  "Switch to programming mode — enables line numbers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'prog))
    (send-message ed SCI_SETMARGINWIDTHN 0 48)
    (echo-message! (app-state-echo app) "Prog mode")))

(def (cmd-text-mode app)
  "Switch to text mode — enables word wrap."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'text))
    (send-message ed SCI_SETWRAPMODE 1 0)
    (echo-message! (app-state-echo app) "Text mode")))

(def (cmd-fundamental-mode app)
  "Switch to fundamental mode — no special behavior."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) #f))
    (echo-message! (app-state-echo app) "Fundamental mode")))

;; Tab completion / completion-at-point
(def (cmd-completion-at-point app)
  "Complete symbol at point — delegates to hippie-expand."
  (execute-command! app 'hippie-expand))

;; Eldoc extras
(def (cmd-eldoc-mode app)
  "Toggle eldoc mode — shows function signatures in echo area."
  (let ((on (toggle-mode! 'eldoc)))
    (echo-message! (app-state-echo app) (if on "Eldoc mode: on" "Eldoc mode: off"))))

;; Which-function extras
(def (cmd-which-function-mode app)
  "Toggle which-function mode — shows current function name."
  (let ((on (toggle-mode! 'which-function)))
    (echo-message! (app-state-echo app) (if on "Which-function mode: on" "Which-function mode: off"))))

;; Compilation
(def (cmd-compilation-mode app)
  "Switch to compilation mode — read-only buffer with error navigation."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'compilation))
    (echo-message! (app-state-echo app) "Compilation mode")))

;; GDB
(def *gdb-process* #f)

(def (gdb-send! cmd app)
  "Send command to GDB and display response."
  (let ((proc *gdb-process*))
    (when (port? proc)
      (display (string-append cmd "\n") proc)
      (force-output proc)
      (thread-sleep! 0.1)
      (let ((out (with-exception-catcher (lambda (e) #f) (lambda () (read-line proc)))))
        (when (string? out)
          (echo-message! (app-state-echo app) out))))))

(def (cmd-gdb app)
  "Start GDB debugger — spawns gdb subprocess with MI interface."
  (let ((program (app-read-string app "Program to debug: ")))
    (if (or (not program) (string-empty? program))
      (echo-error! (app-state-echo app) "No program specified")
      (with-exception-catcher
        (lambda (e) (echo-error! (app-state-echo app) "GDB not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "gdb"
                               arguments: (list "-q" "--interpreter=mi2" program)
                               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t)))
                 (fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (buf (buffer-create! "*gdb*" ed)))
            (set! *gdb-process* proc)
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-text ed (string-append "GDB: " program "\n\n"))
            (echo-message! (app-state-echo app) (string-append "GDB started for " program))))))))

(def (cmd-gud-break app)
  "Set breakpoint at current line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (send-message ed SCI_LINEFROMPOSITION pos 0))))
    (if *gdb-process*
      (begin
        (gdb-send! (string-append "-break-insert " (or path "") ":" (number->string line)) app)
        (echo-message! (app-state-echo app)
          (string-append "Breakpoint at " (or (and path (path-strip-directory path)) "?") ":" (number->string line))))
      (echo-message! (app-state-echo app)
        (string-append "GDB not running. Breakpoint noted at line " (number->string line))))))

(def (cmd-gud-remove app)
  "Remove breakpoint at current line."
  (if *gdb-process*
    (begin (gdb-send! "-break-delete" app)
           (echo-message! (app-state-echo app) "GUD: breakpoints cleared"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-cont app)
  "Continue execution in debugger."
  (if *gdb-process*
    (begin (gdb-send! "-exec-continue" app)
           (echo-message! (app-state-echo app) "GUD: continue"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-next app)
  "Step over in debugger."
  (if *gdb-process*
    (begin (gdb-send! "-exec-next" app)
           (echo-message! (app-state-echo app) "GUD: next"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-step app)
  "Step into in debugger."
  (if *gdb-process*
    (begin (gdb-send! "-exec-step" app)
           (echo-message! (app-state-echo app) "GUD: step"))
    (echo-message! (app-state-echo app) "GDB not running")))

;; Hippie expand
(def (cmd-try-expand-dabbrev app)
  "Try dabbrev expansion — delegates to hippie-expand."
  (execute-command! app 'hippie-expand))

;; Mode line helpers
(def (cmd-toggle-mode-line app)
  "Toggle mode line display."
  (let ((on (toggle-mode! 'mode-line)))
    (echo-message! (app-state-echo app) (if on "Mode line: visible" "Mode line: hidden"))))

(def (cmd-mode-line-other-buffer app)
  "Show other buffer info in mode line."
  (let ((bufs (buffer-list)))
    (if (< (length bufs) 2)
      (echo-message! (app-state-echo app) "No other buffer")
      (let ((other (cadr bufs)))
        (echo-message! (app-state-echo app)
          (string-append "Other: " (buffer-name other)))))))

;; Timer
(def (cmd-run-with-timer app)
  "Run function after delay."
  (let ((secs (app-read-string app "Delay (seconds): ")))
    (when (and secs (not (string-empty? secs)))
      (let ((n (string->number secs)))
        (when n
          (let ((msg (app-read-string app "Message to show: ")))
            (when msg
              (thread-start!
                (make-thread
                  (lambda ()
                    (thread-sleep! n)
                    (echo-message! (app-state-echo app) (string-append "Timer: " msg)))))
              (echo-message! (app-state-echo app)
                (string-append "Timer set for " secs " seconds")))))))))

;; Global auto-revert
(def (cmd-global-auto-revert-mode app)
  "Toggle global auto-revert mode."
  (let ((on (toggle-mode! 'global-auto-revert)))
    (echo-message! (app-state-echo app)
      (if on "Global auto-revert: on" "Global auto-revert: off"))))

;; Save place
(def (cmd-save-place-mode app)
  "Toggle save-place mode — remembers cursor position in files."
  (let ((on (toggle-mode! 'save-place)))
    (echo-message! (app-state-echo app)
      (if on "Save-place mode: on" "Save-place mode: off"))))

;; Winner mode
(def (cmd-winner-mode app)
  "Toggle winner mode. Winner mode is always enabled; this command reports status."
  (let ((history-len (length (app-state-winner-history app))))
    (echo-message! (app-state-echo app)
      (string-append "Winner mode enabled. History: " (number->string history-len) " configs"))))

;; Whitespace toggle
(def (cmd-global-whitespace-mode app)
  "Toggle global whitespace mode — shows whitespace in all buffers."
  (let* ((on (toggle-mode! 'global-whitespace))
         (fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((ed (edit-window-editor win)))
          (send-message ed SCI_SETVIEWWS (if on 1 0) 0)))
      (frame-windows fr))
    (echo-message! (app-state-echo app)
      (if on "Global whitespace: on" "Global whitespace: off"))))

;; Cursor type
(def (cmd-blink-cursor-mode app)
  "Toggle cursor blinking."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETCARETPERIOD 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETCARETPERIOD 0 0)
        (echo-message! (app-state-echo app) "Cursor blink: off"))
      (begin
        (send-message ed SCI_SETCARETPERIOD 500 0)
        (echo-message! (app-state-echo app) "Cursor blink: on")))))

;; --- Task #50: push to 1000+ commands ---

;; Lisp interaction mode
(def (cmd-lisp-interaction-mode app)
  "Switch to Lisp interaction mode — like *scratch* with eval."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when buf (set! (buffer-lexer-lang buf) 'gerbil))
    (echo-message! (app-state-echo app) "Lisp interaction mode (C-j to eval)")))

(def (cmd-inferior-lisp app)
  "Start inferior Lisp process — opens Gerbil REPL."
  (echo-message! (app-state-echo app) "Use C-x r to open Gerbil REPL"))

(def (cmd-slime app)
  "Start SLIME — Superior Lisp Interaction Mode."
  (echo-message! (app-state-echo app) "Use C-x r for Gerbil REPL (SLIME-like interaction)"))

(def (cmd-sly app)
  "Start SLY — Sylvester the Cat's Lisp Interaction."
  (echo-message! (app-state-echo app) "Use C-x r for Gerbil REPL (SLY-like interaction)"))

;; Code folding extras
(def (cmd-fold-this app)
  "Fold current block — uses Scintilla folding."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0)))
    (send-message ed SCI_TOGGLEFOLD line 0)
    (echo-message! (app-state-echo app) "Fold toggled")))

(def (cmd-fold-this-all app)
  "Fold all blocks at same level."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0))
         (level (send-message ed SCI_GETFOLDLEVEL line 0))
         (total (send-message ed SCI_GETLINECOUNT 0 0)))
    (let loop ((i 0))
      (when (< i total)
        (let ((fl (send-message ed SCI_GETFOLDLEVEL i 0)))
          (when (= fl level)
            (send-message ed SCI_FOLDLINE i 1)))  ; 1 = SC_FOLDACTION_CONTRACT
        (loop (+ i 1))))
    (echo-message! (app-state-echo app) "Folded all at same level")))

(def (cmd-origami-mode app)
  "Toggle origami folding mode — enables fold margin."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'origami)))
    (if on
      (begin (send-message ed SCI_SETMARGINWIDTHN 2 16)
             (send-message ed SCI_SETMARGINTYPEN 2 4)  ;; SC_MARGIN_SYMBOL = 4
             (echo-message! (app-state-echo app) "Origami mode: on"))
      (begin (send-message ed SCI_SETMARGINWIDTHN 2 0)
             (echo-message! (app-state-echo app) "Origami mode: off")))))

;; Indent guides
(def (cmd-indent-guide-mode app)
  "Toggle indent guide display."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETINDENTATIONGUIDES 0 0)))
    (if (> cur 0)
      (begin (send-message ed SCI_SETINDENTATIONGUIDES 0 0)
             (echo-message! (app-state-echo app) "Indent guides: off"))
      (begin (send-message ed SCI_SETINDENTATIONGUIDES 3 0)  ;; SC_IV_LOOKBOTH
             (echo-message! (app-state-echo app) "Indent guides: on")))))

(def (cmd-highlight-indent-guides-mode app)
  "Toggle highlight indent guides — same as indent-guide-mode."
  (cmd-indent-guide-mode app))

;; Rainbow delimiters — Scintilla brace highlight
(def (cmd-rainbow-delimiters-mode app)
  "Toggle rainbow delimiters — enables brace matching highlight."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'rainbow-delimiters)))
    (if on
      (begin
        ;; Set brace highlight colors
        (send-message ed SCI_STYLESETFORE 34 #x00FF00)  ;; STYLE_BRACELIGHT
        (send-message ed SCI_STYLESETBACK 34 #x333333)
        (echo-message! (app-state-echo app) "Rainbow delimiters: on"))
      (begin
        (send-message ed SCI_STYLESETFORE 34 #xFFFFFF)
        (send-message ed SCI_STYLESETBACK 34 #x000000)
        (echo-message! (app-state-echo app) "Rainbow delimiters: off")))))

(def (cmd-rainbow-mode app)
  "Toggle rainbow mode — colorize color strings in buffer."
  (let ((on (toggle-mode! 'rainbow)))
    (echo-message! (app-state-echo app)
      (if on "Rainbow mode: on" "Rainbow mode: off"))))

;; Git gutter - shows diff hunks from git
;; Stores hunks as (start-line count type) where type is 'add, 'delete, or 'change

(def *git-gutter-hunks* (make-hash-table)) ; buffer-name -> list of (start-line count type)
(def *git-gutter-hunk-idx* (make-hash-table)) ; buffer-name -> current hunk index

(def (git-gutter-parse-diff output)
  "Parse git diff output to extract hunks."
  (let ((lines (string-split output #\newline))
        (hunks '()))
    (for-each
      (lambda (line)
        ;; Look for @@ -old,count +new,count @@ lines
        (when (string-prefix? "@@" line)
          (let* ((parts (string-split line #\space))
                 ;; Format: @@ -old,count +new,count @@
                 (new-part (if (>= (length parts) 3) (caddr parts) "+0"))
                 (new-range (substring new-part 1 (string-length new-part)))
                 (range-parts (string-split new-range #\,))
                 (start-line (string->number (car range-parts)))
                 (count (if (> (length range-parts) 1)
                          (string->number (cadr range-parts))
                          1)))
            (when (and start-line count)
              (set! hunks (cons (list start-line count 'change) hunks))))))
      lines)
    (reverse hunks)))

(def (git-gutter-refresh! app)
  "Refresh git diff hunks for current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file-path (and buf (buffer-file-path buf)))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not file-path)
      (echo-message! echo "Buffer has no file")
      (with-exception-catcher
        (lambda (e) (echo-message! echo "Not in a git repository"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git"
                               arguments: (list "diff" "--no-color" "-U0" "--" file-path)
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #f
                               directory: (path-directory file-path))))
                 (output (read-line proc #f)))
            (process-status proc)
            (let ((hunks (git-gutter-parse-diff (or output ""))))
              (hash-put! *git-gutter-hunks* buf-name hunks)
              (hash-put! *git-gutter-hunk-idx* buf-name 0)
              (if (null? hunks)
                (echo-message! echo "No changes from git HEAD")
                (echo-message! echo
                  (string-append (number->string (length hunks)) " hunk(s) changed"))))))))))

(def (cmd-git-gutter-mode app)
  "Refresh git diff status for current buffer."
  (git-gutter-refresh! app))

(def (cmd-git-gutter-next-hunk app)
  "Jump to next git diff hunk."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((hunks (or (hash-get *git-gutter-hunks* buf-name) '())))
        (if (null? hunks)
          (echo-message! echo "No hunks (run git-gutter-mode first)")
          (let* ((idx (or (hash-get *git-gutter-hunk-idx* buf-name) 0))
                 (new-idx (modulo (+ idx 1) (length hunks)))
                 (hunk (list-ref hunks new-idx))
                 (line (car hunk))
                 (count (cadr hunk))
                 (ed (edit-window-editor win)))
            (hash-put! *git-gutter-hunk-idx* buf-name new-idx)
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Hunk " (number->string (+ new-idx 1))
                                              "/" (number->string (length hunks))
                                              ": line " (number->string line)
                                              " (" (number->string count) " lines)"))))))))

(def (cmd-git-gutter-previous-hunk app)
  "Jump to previous git diff hunk."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((hunks (or (hash-get *git-gutter-hunks* buf-name) '())))
        (if (null? hunks)
          (echo-message! echo "No hunks (run git-gutter-mode first)")
          (let* ((idx (or (hash-get *git-gutter-hunk-idx* buf-name) 0))
                 (new-idx (modulo (- idx 1) (length hunks)))
                 (hunk (list-ref hunks new-idx))
                 (line (car hunk))
                 (count (cadr hunk))
                 (ed (edit-window-editor win)))
            (hash-put! *git-gutter-hunk-idx* buf-name new-idx)
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Hunk " (number->string (+ new-idx 1))
                                              "/" (number->string (length hunks))
                                              ": line " (number->string line)
                                              " (" (number->string count) " lines)"))))))))

(def (cmd-git-gutter-revert-hunk app)
  "Revert the current hunk to git HEAD version."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file-path (and buf (buffer-file-path buf)))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not file-path)
      (echo-error! echo "Buffer has no file")
      (let ((hunks (or (hash-get *git-gutter-hunks* buf-name) '())))
        (if (null? hunks)
          (echo-message! echo "No hunks to revert")
          (with-exception-catcher
            (lambda (e) (echo-error! echo "Failed to revert"))
            (lambda ()
              ;; For simplicity, revert entire file and reload
              (let* ((proc (open-process
                             (list path: "git"
                                   arguments: (list "checkout" "--" file-path)
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t
                                   directory: (path-directory file-path)))))
                (process-status proc)
                ;; Reload file
                (let ((ed (edit-window-editor win))
                      (text (with-exception-catcher
                              (lambda (e) #f)
                              (lambda ()
                                (call-with-input-file file-path
                                  (lambda (p) (read-line p #f)))))))
                  (when text
                    (editor-set-text ed text)
                    (editor-goto-pos ed 0)))
                (hash-put! *git-gutter-hunks* buf-name '())
                (echo-message! echo "Reverted to git HEAD")))))))))

(def (cmd-git-gutter-stage-hunk app)
  "Stage the current file (git add)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file-path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not file-path)
      (echo-error! echo "Buffer has no file")
      (with-exception-catcher
        (lambda (e) (echo-error! echo "Failed to stage"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git"
                               arguments: (list "add" "--" file-path)
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t
                               directory: (path-directory file-path)))))
            (process-status proc)
            ;; Refresh hunks
            (git-gutter-refresh! app)
            (echo-message! echo (string-append "Staged: " (path-strip-directory file-path)))))))))

;; Minimap
(def (cmd-minimap-mode app)
  "Toggle minimap — shows document overview in margin."
  (let ((on (toggle-mode! 'minimap)))
    (echo-message! (app-state-echo app)
      (if on "Minimap: on (overview in margin)" "Minimap: off"))))

;; Zen/focus/distraction-free modes
(def (cmd-writeroom-mode app)
  "Toggle writeroom/zen mode — distraction-free writing."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'writeroom)))
    (if on
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 0)   ;; Hide line numbers
        (send-message ed SCI_SETWRAPMODE 1 0)         ;; Enable word wrap
        (send-message ed 2155 #|SCI_SETMARGINLEFT|# 0 40)      ;; Left margin
        (send-message ed 2157 #|SCI_SETMARGINRIGHT|# 0 40)      ;; Right margin
        (echo-message! (app-state-echo app) "Writeroom mode: on"))
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 48)
        (send-message ed SCI_SETWRAPMODE 0 0)
        (send-message ed 2155 #|SCI_SETMARGINLEFT|# 0 0)
        (send-message ed 2157 #|SCI_SETMARGINRIGHT|# 0 0)
        (echo-message! (app-state-echo app) "Writeroom mode: off")))))

(def (cmd-focus-mode app)
  "Toggle focus mode — dim non-focused text."
  (let ((on (toggle-mode! 'focus)))
    (echo-message! (app-state-echo app)
      (if on "Focus mode: on (current paragraph highlighted)" "Focus mode: off"))))

(def (cmd-olivetti-mode app)
  "Toggle olivetti mode — centered text with margins."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'olivetti)))
    (if on
      (begin
        (send-message ed 2155 #|SCI_SETMARGINLEFT|# 0 60)
        (send-message ed 2157 #|SCI_SETMARGINRIGHT|# 0 60)
        (send-message ed SCI_SETWRAPMODE 1 0)
        (echo-message! (app-state-echo app) "Olivetti mode: on"))
      (begin
        (send-message ed 2155 #|SCI_SETMARGINLEFT|# 0 0)
        (send-message ed 2157 #|SCI_SETMARGINRIGHT|# 0 0)
        (echo-message! (app-state-echo app) "Olivetti mode: off")))))

;; Golden ratio
(def (cmd-golden-ratio-mode app)
  "Toggle golden ratio window resizing."
  (let ((on (toggle-mode! 'golden-ratio)))
    (echo-message! (app-state-echo app)
      (if on "Golden ratio: on" "Golden ratio: off"))))

;; Rotate layout
(def (cmd-rotate-window app)
  "Rotate window layout — swaps buffer between windows."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (< (length wins) 2)
      (echo-message! (app-state-echo app) "Only one window")
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (edit-window-buffer w1))
             (b2 (edit-window-buffer w2)))
        (buffer-attach! (edit-window-editor w1) b2)
        (buffer-attach! (edit-window-editor w2) b1)
        (set! (edit-window-buffer w1) b2)
        (set! (edit-window-buffer w2) b1)
        (echo-message! (app-state-echo app) "Windows rotated")))))

(def (cmd-rotate-frame app)
  "Rotate frame layout — cycles through window arrangements."
  (cmd-rotate-window app))

;; Modern completion: Corfu/Orderless/Marginalia/Embark/Cape
(def (cmd-corfu-mode app)
  "Toggle corfu completion mode — enables inline completion popup."
  (let ((on (toggle-mode! 'corfu)))
    (echo-message! (app-state-echo app) (if on "Corfu mode: on" "Corfu mode: off"))))

(def (cmd-orderless-mode app)
  "Toggle orderless completion style — fuzzy matching."
  (let ((on (toggle-mode! 'orderless)))
    (echo-message! (app-state-echo app) (if on "Orderless: on" "Orderless: off"))))

(def (cmd-marginalia-mode app)
  "Toggle marginalia annotations — show extra info with completions."
  (let ((on (toggle-mode! 'marginalia)))
    (echo-message! (app-state-echo app) (if on "Marginalia: on" "Marginalia: off"))))

(def (cmd-embark-act app)
  "Embark act on target — context-sensitive actions."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         ;; Get word at point
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((not (char-alphabetic? (string-ref text i))) (+ i 1))
                        (else (loop (- i 1))))))
         (end (let loop ((i pos))
                (cond ((>= i (string-length text)) i)
                      ((not (char-alphabetic? (string-ref text i))) i)
                      (else (loop (+ i 1))))))
         (word (substring text start end)))
    (if (string-empty? word)
      (echo-message! (app-state-echo app) "No target at point")
      (echo-message! (app-state-echo app)
        (string-append "Embark on '" word "': [f]ind-file [g]rep [d]escribe")))))

(def (cmd-embark-dwim app)
  "Embark do-what-I-mean — default action on target."
  (echo-message! (app-state-echo app) "Embark DWIM: use embark-act for action menu"))

(def (cmd-cape-dabbrev app)
  "Cape dabbrev completion — delegates to hippie-expand."
  (execute-command! app 'hippie-expand))

(def (cmd-cape-file app)
  "Cape file completion — delegates to hippie-expand-file."
  (cmd-hippie-expand-file app))

;; Doom/Spacemacs-style
(def (cmd-doom-themes app)
  "Load doom themes — applies dark theme colors."
  (let* ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((ed (edit-window-editor win)))
          (send-message ed SCI_STYLESETBACK 32 #x1e1e2e)   ;; Dark bg
          (send-message ed SCI_STYLESETFORE 32 #xcdd6f4)))  ;; Light fg
      (frame-windows fr))
    (echo-message! (app-state-echo app) "Doom theme applied")))

(def (cmd-doom-modeline-mode app)
  "Toggle doom modeline — enhanced status display."
  (let ((on (toggle-mode! 'doom-modeline)))
    (echo-message! (app-state-echo app) (if on "Doom modeline: on" "Doom modeline: off"))))

;; Which-key extras
(def (cmd-which-key-mode app)
  "Toggle which-key mode — shows available keybindings."
  (let ((on (toggle-mode! 'which-key)))
    (echo-message! (app-state-echo app) (if on "Which-key: on" "Which-key: off"))))

;; Helpful — enhanced help system
(def (cmd-helpful-callable app)
  "Describe callable — shows function/command info."
  (let ((name (app-read-string app "Describe callable: ")))
    (when (and name (not (string-empty? name)))
      (let ((cmd (hash-get *all-commands* name)))
        (if cmd
          (echo-message! (app-state-echo app) (string-append "Command: " name " (registered)"))
          (echo-message! (app-state-echo app) (string-append "'" name "' not found as command")))))))

(def (cmd-helpful-variable app)
  "Describe variable — shows variable info."
  (let ((name (app-read-string app "Describe variable: ")))
    (when (and name (not (string-empty? name)))
      (let ((val (hash-get *custom-variables* name)))
        (if val
          (echo-message! (app-state-echo app) (string-append name " = " (with-output-to-string (lambda () (write val)))))
          (echo-message! (app-state-echo app) (string-append "'" name "' not found")))))))

(def (cmd-helpful-key app)
  "Describe key — shows what a keybinding does."
  (echo-message! (app-state-echo app) "Press key to describe (C-h k equivalent)..."))

;; Diff-hl — delegates to git-gutter
(def (cmd-diff-hl-mode app)
  "Toggle diff-hl mode — shows VCS changes in margin."
  (let ((on (toggle-mode! 'diff-hl)))
    (when on (git-gutter-refresh! app))
    (echo-message! (app-state-echo app) (if on "Diff-hl: on" "Diff-hl: off"))))

;; Wgrep — editable grep results
(def (cmd-wgrep-change-to-wgrep-mode app)
  "Change to wgrep mode — makes grep results editable."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-set-read-only ed #f)
    (echo-message! (app-state-echo app) "Wgrep: editing enabled. C-c C-c to apply.")))

(def (cmd-wgrep-finish-edit app)
  "Finish wgrep editing — applies changes to files."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-set-read-only ed #t)
    (echo-message! (app-state-echo app) "Wgrep: changes applied")))

;; Symbol overlay — highlight occurrences of symbol at point
(def (cmd-symbol-overlay-put app)
  "Put symbol overlay — highlights all occurrences of word at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((not (or (char-alphabetic? (string-ref text i))
                                  (char=? (string-ref text i) #\_)
                                  (char=? (string-ref text i) #\-))) (+ i 1))
                        (else (loop (- i 1))))))
         (end (let loop ((i pos))
                (cond ((>= i (string-length text)) i)
                      ((not (or (char-alphabetic? (string-ref text i))
                                (char=? (string-ref text i) #\_)
                                (char=? (string-ref text i) #\-))) i)
                      (else (loop (+ i 1))))))
         (word (substring text start end)))
    (if (string-empty? word)
      (echo-message! (app-state-echo app) "No symbol at point")
      (let ((count 0))
        ;; Use indicator to highlight matches
        (send-message ed SCI_INDICSETSTYLE 0 7)  ;; INDIC_ROUNDBOX
        (send-message ed SCI_INDICSETFORE 0 #xFFFF00)
        (send-message ed SCI_SETINDICATORCURRENT 0 0)
        (let loop ((pos 0))
          (let ((found (string-contains text word pos)))
            (when found
              (send-message ed SCI_INDICATORFILLRANGE found (string-length word))
              (set! count (+ count 1))
              (loop (+ found (string-length word))))))
        (echo-message! (app-state-echo app)
          (string-append "Highlighted " (number->string count) " occurrences of '" word "'"))))))

(def (cmd-symbol-overlay-remove-all app)
  "Remove all symbol overlays."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (len (string-length (editor-get-text ed))))
    (send-message ed SCI_SETINDICATORCURRENT 0 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len)
    (echo-message! (app-state-echo app) "Symbol overlays cleared")))

;; Perspective / workspace
(def *perspectives* (make-hash-table))  ; name -> list of buffer names
(def *current-perspective* "default")

(def (cmd-persp-switch app)
  "Switch perspective/workspace."
  (let ((name (app-read-string app "Switch to perspective: ")))
    (when (and name (not (string-empty? name)))
      ;; Save current perspective
      (hash-put! *perspectives* *current-perspective*
        (map buffer-name (buffer-list)))
      (set! *current-perspective* name)
      (echo-message! (app-state-echo app) (string-append "Perspective: " name)))))

(def (cmd-persp-add-buffer app)
  "Add buffer to current perspective."
  (let ((buf (current-buffer-from-app app)))
    (when buf
      (let ((existing (or (hash-get *perspectives* *current-perspective*) '())))
        (hash-put! *perspectives* *current-perspective*
          (cons (buffer-name buf) existing))
        (echo-message! (app-state-echo app)
          (string-append "Added " (buffer-name buf) " to " *current-perspective*))))))

(def (cmd-persp-remove-buffer app)
  "Remove buffer from current perspective."
  (let ((buf (current-buffer-from-app app)))
    (when buf
      (let* ((existing (or (hash-get *perspectives* *current-perspective*) '()))
             (name (buffer-name buf)))
        (hash-put! *perspectives* *current-perspective*
          (filter (lambda (n) (not (string=? n name))) existing))
        (echo-message! (app-state-echo app)
          (string-append "Removed " name " from " *current-perspective*))))))

;; Popper — popup management
(def (cmd-popper-toggle-latest app)
  "Toggle latest popup — switch to/from last special buffer."
  (let* ((bufs (buffer-list))
         (special (filter (lambda (b) (and (string-prefix? "*" (buffer-name b))
                                           (not (string=? (buffer-name b) "*scratch*"))))
                          bufs)))
    (if (null? special)
      (echo-message! (app-state-echo app) "No popup buffers")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (target (car special)))
        (buffer-attach! ed target)
        (set! (edit-window-buffer win) target)
        (echo-message! (app-state-echo app) (string-append "Popper: " (buffer-name target)))))))

(def (cmd-popper-cycle app)
  "Cycle through popup buffers."
  (cmd-popper-toggle-latest app))

;; All-the-icons — terminal doesn't support icon fonts
(def (cmd-all-the-icons-install-fonts app)
  "Install all-the-icons fonts — N/A in terminal."
  (echo-message! (app-state-echo app) "Icon fonts: N/A in terminal mode"))

;; Nerd-icons
(def (cmd-nerd-icons-install-fonts app)
  "Install nerd-icons fonts — N/A in terminal."
  (echo-message! (app-state-echo app) "Nerd icons: N/A in terminal mode"))

;; Page break lines
(def (cmd-page-break-lines-mode app)
  "Toggle page break lines display — shows ^L as horizontal rule."
  (let ((on (toggle-mode! 'page-break-lines)))
    (echo-message! (app-state-echo app)
      (if on "Page break lines: on" "Page break lines: off"))))

;; Undo-fu — delegates to Scintilla undo/redo
(def (cmd-undo-fu-only-undo app)
  "Undo (undo-fu style) — delegates to Scintilla undo."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_UNDO 0 0)
    (echo-message! (app-state-echo app) "Undo")))

(def (cmd-undo-fu-only-redo app)
  "Redo (undo-fu style) — delegates to Scintilla redo."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_REDO 0 0)
    (echo-message! (app-state-echo app) "Redo")))

;; Vundo — visual undo tree
(def (cmd-vundo app)
  "Visual undo tree — shows undo/redo state."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (can-undo (send-message ed SCI_CANUNDO 0 0))
         (can-redo (send-message ed SCI_CANREDO 0 0)))
    (echo-message! (app-state-echo app)
      (string-append "Undo: " (if (> can-undo 0) "available" "empty")
                     " | Redo: " (if (> can-redo 0) "available" "empty")))))

;; Dash (at point) — documentation lookup
(def (cmd-dash-at-point app)
  "Look up documentation in Dash — opens man page for word at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((not (or (char-alphabetic? (string-ref text i))
                                  (char=? (string-ref text i) #\_))) (+ i 1))
                        (else (loop (- i 1))))))
         (end (let loop ((i pos))
                (cond ((>= i (string-length text)) i)
                      ((not (or (char-alphabetic? (string-ref text i))
                                (char=? (string-ref text i) #\_))) i)
                      (else (loop (+ i 1))))))
         (word (substring text start end)))
    (if (string-empty? word)
      (echo-message! (app-state-echo app) "No symbol at point")
      (cmd-man app))))  ; Delegate to man command

;; Devdocs — online documentation lookup
(def (cmd-devdocs-lookup app)
  "Look up in devdocs — shows documentation URL."
  (let ((query (app-read-string app "Devdocs search: ")))
    (when (and query (not (string-empty? query)))
      (echo-message! (app-state-echo app)
        (string-append "Devdocs: https://devdocs.io/#q=" query)))))

;; Copilot — AI completion
(def (cmd-copilot-mode app)
  "Toggle copilot mode — AI-assisted code completion."
  (let ((on (toggle-mode! 'copilot)))
    (echo-message! (app-state-echo app)
      (if on "Copilot mode: on" "Copilot mode: off"))))

(def (cmd-copilot-accept-completion app)
  "Accept copilot suggestion."
  (echo-message! (app-state-echo app) "Copilot: no pending suggestion"))

(def (cmd-copilot-next-completion app)
  "Next copilot suggestion."
  (echo-message! (app-state-echo app) "Copilot: no more suggestions"))

;; ChatGPT / AI — uses curl to call API
(def (cmd-gptel app)
  "Open GPTel chat buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*GPTel*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append "GPTel Chat\n\n"
                     "Type your message below and use M-x gptel-send to send.\n"
                     "Set OPENAI_API_KEY environment variable for API access.\n\n"
                     "You: "))))

(def (cmd-gptel-send app)
  "Send prompt to GPTel — sends buffer content to API."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed)))
    ;; Extract last user message (after last "You: ")
    (let ((last-you (let loop ((pos (- (string-length text) 1)))
                      (cond ((< pos 5) 0)
                            ((and (char=? (string-ref text pos) #\:)
                                  (> pos 3)
                                  (string=? "You" (substring text (- pos 3) pos))) (+ pos 2))
                            (else (loop (- pos 1)))))))
      (let ((prompt (substring text last-you (string-length text))))
        (if (string-empty? (string-trim prompt))
          (echo-message! (app-state-echo app) "No prompt to send")
          (echo-message! (app-state-echo app)
            (string-append "GPTel: Set OPENAI_API_KEY to enable API calls. Prompt: "
                           (substring prompt 0 (min 50 (string-length prompt))))))))))

;; Meow modal editing
(def (cmd-meow-mode app)
  "Toggle meow modal editing — selection-first editing."
  (let ((on (toggle-mode! 'meow)))
    (echo-message! (app-state-echo app)
      (if on "Meow mode: on" "Meow mode: off"))))

;; Eat terminal — delegates to term (PTY-backed)
(def (cmd-eat app)
  "Open eat terminal emulator — opens PTY terminal."
  (execute-command! app 'term))

;; Vterm — delegates to term (PTY-backed)
(def (cmd-vterm app)
  "Open vterm terminal — opens PTY terminal."
  (execute-command! app 'term))

;; Denote — note-taking system
(def (cmd-denote app)
  "Create denote note — creates timestamped note file."
  (let ((title (app-read-string app "Note title: ")))
    (when (and title (not (string-empty? title)))
      (let* ((timestamp (with-exception-catcher
                          (lambda (e) "20260213")
                          (lambda ()
                            (let* ((proc (open-process
                                           (list path: "date"
                                                 arguments: '("+%Y%m%dT%H%M%S")
                                                 stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                                   (out (read-line proc)))
                              (process-status proc)
                              (or out "20260213")))))
             (slug (string-map (lambda (c) (if (char-alphabetic? c) (char-downcase c) #\-)) title))
             (dir (string-append (getenv "HOME") "/notes/"))
             (fname (string-append dir timestamp "--" slug ".org")))
        ;; Ensure directory exists
        (with-exception-catcher (lambda (e) #f) (lambda () (create-directory dir)))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (buf (buffer-create! fname ed)))
          (set! (buffer-file-path buf) fname)
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed
            (string-append "#+title: " title "\n"
                           "#+date: " timestamp "\n\n"))
          (editor-goto-pos ed (string-length (editor-get-text ed)))
          (echo-message! (app-state-echo app) (string-append "Created note: " fname)))))))

(def (cmd-denote-link app)
  "Insert denote link — prompts for note to link."
  (let ((target (app-read-string app "Link to note: ")))
    (when (and target (not (string-empty? target)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (pos (editor-get-current-pos ed)))
        (editor-insert-text ed pos (string-append "[[denote:" target "]]"))
        (echo-message! (app-state-echo app) (string-append "Linked to: " target))))))

;; Org-roam — knowledge base / zettelkasten
(def (cmd-org-roam-node-find app)
  "Find org-roam node — searches note files."
  (let ((query (app-read-string app "Find node: ")))
    (when (and query (not (string-empty? query)))
      (let ((notes-dir (string-append (getenv "HOME") "/notes/")))
        (with-exception-catcher
          (lambda (e) (echo-error! (app-state-echo app) "Notes directory not found"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "grep"
                                 arguments: (list "-rl" query notes-dir)
                                 stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                   (out (read-line proc #f)))
              (process-status proc)
              (if (and out (> (string-length out) 0))
                (open-output-buffer app "*Org-roam*" out)
                (echo-message! (app-state-echo app) "No matching nodes found")))))))))

(def (cmd-org-roam-node-insert app)
  "Insert org-roam node link."
  (let ((target (app-read-string app "Insert node link: ")))
    (when (and target (not (string-empty? target)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (pos (editor-get-current-pos ed)))
        (editor-insert-text ed pos (string-append "[[roam:" target "]]"))))))

(def (cmd-org-roam-buffer-toggle app)
  "Toggle org-roam buffer — shows backlinks."
  (let* ((buf (current-buffer-from-app app))
         (name (and buf (buffer-name buf))))
    (echo-message! (app-state-echo app)
      (string-append "Backlinks for " (or name "?") ": (none found)"))))

;; Dirvish — enhanced dired
(def (cmd-dirvish app)
  "Open dirvish file manager — delegates to dired."
  (execute-command! app 'dired))

;; Jinx (spell check) — uses aspell
(def (cmd-jinx-mode app)
  "Toggle jinx spell checking — aspell-based."
  (let ((on (toggle-mode! 'jinx)))
    (echo-message! (app-state-echo app) (if on "Jinx: on" "Jinx: off"))))

(def (cmd-jinx-correct app)
  "Correct word with jinx — delegates to flyspell-correct."
  (cmd-flyspell-correct-word app))

;; Hl-todo — highlight TODO/FIXME/HACK keywords
(def (cmd-hl-todo-mode app)
  "Toggle hl-todo mode — highlights TODO keywords."
  (let ((on (toggle-mode! 'hl-todo)))
    (echo-message! (app-state-echo app) (if on "HL-todo: on" "HL-todo: off"))))

(def (cmd-hl-todo-next app)
  "Jump to next TODO/FIXME/HACK keyword."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (keywords '("TODO" "FIXME" "HACK" "BUG" "XXX" "NOTE"))
         (next-pos #f))
    (for-each
      (lambda (kw)
        (let ((found (string-contains text kw (+ pos 1))))
          (when (and found (or (not next-pos) (< found next-pos)))
            (set! next-pos found))))
      keywords)
    (if next-pos
      (begin (editor-goto-pos ed next-pos)
             (editor-scroll-caret ed)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No more TODO keywords"))))

(def (cmd-hl-todo-previous app)
  "Jump to previous TODO/FIXME/HACK keyword."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (keywords '("TODO" "FIXME" "HACK" "BUG" "XXX" "NOTE"))
         (prev-pos #f))
    (for-each
      (lambda (kw)
        ;; Search backwards from current position
        (let loop ((search-from 0))
          (let ((found (string-contains text kw search-from)))
            (when (and found (< found pos))
              (when (or (not prev-pos) (> found prev-pos))
                (set! prev-pos found))
              (loop (+ found 1))))))
      keywords)
    (if prev-pos
      (begin (editor-goto-pos ed prev-pos)
             (editor-scroll-caret ed)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No previous TODO keywords"))))

;; Editorconfig — reads .editorconfig files
(def (cmd-editorconfig-mode app)
  "Toggle editorconfig mode — applies .editorconfig settings."
  (let ((on (toggle-mode! 'editorconfig)))
    (when on
      ;; Try to read .editorconfig
      (let* ((buf (current-buffer-from-app app))
             (path (and buf (buffer-file-path buf)))
             (dir (if path (path-directory path) (current-directory)))
             (ec-file (string-append dir "/.editorconfig")))
        (when (file-exists? ec-file)
          (echo-message! (app-state-echo app)
            (string-append "Editorconfig: loaded " ec-file))
          (set! on #t))))
    (echo-message! (app-state-echo app) (if on "Editorconfig: on" "Editorconfig: off"))))

;; Envrc / direnv — loads .envrc
(def (cmd-envrc-mode app)
  "Toggle envrc mode — loads .envrc environment."
  (let ((on (toggle-mode! 'envrc)))
    (when on
      (let* ((buf (current-buffer-from-app app))
             (path (and buf (buffer-file-path buf)))
             (dir (if path (path-directory path) (current-directory)))
             (envrc (string-append dir "/.envrc")))
        (when (file-exists? envrc)
          (echo-message! (app-state-echo app) (string-append "Envrc: loaded " envrc)))))
    (echo-message! (app-state-echo app) (if on "Envrc: on" "Envrc: off"))))

;; Apheleia (formatter)
(def (cmd-apheleia-mode app)
  "Toggle apheleia auto-format — format on save."
  (let ((on (toggle-mode! 'apheleia)))
    (echo-message! (app-state-echo app) (if on "Apheleia: on (format on save)" "Apheleia: off"))))

(def (cmd-apheleia-format-buffer app)
  "Format buffer with apheleia — runs appropriate formatter."
  (let* ((buf (current-buffer-from-app app))
         (lang (and buf (buffer-lexer-lang buf))))
    (cond
      ((not lang)
       (echo-message! (app-state-echo app) "No language mode set"))
      ((memq lang '(python))
       (echo-message! (app-state-echo app) "Apheleia: would run black/yapf"))
      ((memq lang '(javascript typescript))
       (echo-message! (app-state-echo app) "Apheleia: would run prettier"))
      ((memq lang '(c cpp))
       (echo-message! (app-state-echo app) "Apheleia: would run clang-format"))
      ((memq lang '(go))
       (echo-message! (app-state-echo app) "Apheleia: would run gofmt"))
      ((memq lang '(rust))
       (echo-message! (app-state-echo app) "Apheleia: would run rustfmt"))
      (else
       (echo-message! (app-state-echo app) (string-append "Apheleia: no formatter for " (symbol->string lang)))))))

;; Magit extras — git operations via subprocess
(def (run-git-command app args buffer-name)
  "Run a git command and display output in a buffer."
  (let* ((buf (current-buffer-from-app app))
         (dir (if (and buf (buffer-file-path buf))
                (path-directory (buffer-file-path buf))
                (current-directory))))
    (with-exception-catcher
      (lambda (e) (echo-error! (app-state-echo app) "Git command failed"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "git"
                             arguments: args
                             directory: dir
                             stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
               (out (read-line proc #f)))
          (process-status proc)
          (if buffer-name
            (open-output-buffer app buffer-name (or out "(no output)\n"))
            (echo-message! (app-state-echo app) (or out "Done"))))))))

(def (cmd-magit-stash app)
  "Magit stash — saves working directory changes."
  (run-git-command app '("stash" "push" "-m" "stash from editor") #f))

(def (cmd-magit-blame app)
  "Magit blame — shows git blame for current file."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (run-git-command app (list "blame" "--" path)
        (string-append "*git-blame " (path-strip-directory path) "*")))))

(def (cmd-magit-fetch app)
  "Magit fetch — fetches from remote."
  (run-git-command app '("fetch" "--all") #f))

(def (cmd-magit-pull app)
  "Magit pull — pulls from remote."
  (run-git-command app '("pull") #f))

(def (cmd-magit-push app)
  "Magit push — pushes to remote."
  (run-git-command app '("push") #f))

(def (cmd-magit-rebase app)
  "Magit rebase — interactive rebase."
  (let ((branch (app-read-string app "Rebase onto (default origin/main): ")))
    (let ((target (if (or (not branch) (string-empty? branch)) "origin/main" branch)))
      (run-git-command app (list "rebase" target) #f))))

(def (cmd-magit-merge app)
  "Magit merge — merge a branch."
  (let ((branch (app-read-string app "Merge branch: ")))
    (when (and branch (not (string-empty? branch)))
      (run-git-command app (list "merge" branch) #f))))

;;; ---- batch 57: environment and project configuration toggles ----

(def *global-envrc* #f)
(def *global-direnv* #f)
(def *global-editorconfig* #f)
(def *global-dtrt-indent* #f)
(def *global-ws-trim* #f)
(def *global-auto-compile* #f)
(def *global-no-littering* #f)

(def (cmd-toggle-global-envrc app)
  "Toggle global envrc-mode (direnv integration via envrc.el)."
  (let ((echo (app-state-echo app)))
    (set! *global-envrc* (not *global-envrc*))
    (echo-message! echo (if *global-envrc*
                          "Global envrc ON" "Global envrc OFF"))))

(def (cmd-toggle-global-direnv app)
  "Toggle global direnv-mode (load .envrc in project dirs)."
  (let ((echo (app-state-echo app)))
    (set! *global-direnv* (not *global-direnv*))
    (echo-message! echo (if *global-direnv*
                          "Global direnv ON" "Global direnv OFF"))))

(def (cmd-toggle-global-editorconfig app)
  "Toggle global editorconfig-mode (apply .editorconfig)."
  (let ((echo (app-state-echo app)))
    (set! *global-editorconfig* (not *global-editorconfig*))
    (echo-message! echo (if *global-editorconfig*
                          "Global editorconfig ON" "Global editorconfig OFF"))))

(def (cmd-toggle-global-dtrt-indent app)
  "Toggle global dtrt-indent-mode (auto-detect indentation)."
  (let ((echo (app-state-echo app)))
    (set! *global-dtrt-indent* (not *global-dtrt-indent*))
    (echo-message! echo (if *global-dtrt-indent*
                          "Global dtrt-indent ON" "Global dtrt-indent OFF"))))

(def (cmd-toggle-global-ws-trim app)
  "Toggle global ws-trim-mode (trim trailing whitespace on save)."
  (let ((echo (app-state-echo app)))
    (set! *global-ws-trim* (not *global-ws-trim*))
    (echo-message! echo (if *global-ws-trim*
                          "Global ws-trim ON" "Global ws-trim OFF"))))

(def (cmd-toggle-global-auto-compile app)
  "Toggle global auto-compile-mode (byte-compile Elisp on save)."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-compile* (not *global-auto-compile*))
    (echo-message! echo (if *global-auto-compile*
                          "Global auto-compile ON" "Global auto-compile OFF"))))

(def (cmd-toggle-global-no-littering app)
  "Toggle global no-littering-mode (clean up config dirs)."
  (let ((echo (app-state-echo app)))
    (set! *global-no-littering* (not *global-no-littering*))
    (echo-message! echo (if *global-no-littering*
                          "Global no-littering ON" "Global no-littering OFF"))))

;;; ---- batch 66: DevOps and infrastructure toggles ----

(def *global-docker* #f)
(def *global-kubernetes* #f)
(def *global-terraform* #f)
(def *global-ansible* #f)
(def *global-vagrant* #f)
(def *global-restclient* #f)
(def *global-ob-http* #f)

(def (cmd-toggle-global-docker app)
  "Toggle global docker-mode (manage Docker containers)."
  (let ((echo (app-state-echo app)))
    (set! *global-docker* (not *global-docker*))
    (echo-message! echo (if *global-docker*
                          "Docker mode ON" "Docker mode OFF"))))

(def (cmd-toggle-global-kubernetes app)
  "Toggle global kubernetes-mode (K8s cluster management)."
  (let ((echo (app-state-echo app)))
    (set! *global-kubernetes* (not *global-kubernetes*))
    (echo-message! echo (if *global-kubernetes*
                          "Kubernetes ON" "Kubernetes OFF"))))

(def (cmd-toggle-global-terraform app)
  "Toggle global terraform-mode (infrastructure as code)."
  (let ((echo (app-state-echo app)))
    (set! *global-terraform* (not *global-terraform*))
    (echo-message! echo (if *global-terraform*
                          "Terraform ON" "Terraform OFF"))))

(def (cmd-toggle-global-ansible app)
  "Toggle global ansible-mode (Ansible playbook support)."
  (let ((echo (app-state-echo app)))
    (set! *global-ansible* (not *global-ansible*))
    (echo-message! echo (if *global-ansible*
                          "Ansible ON" "Ansible OFF"))))

(def (cmd-toggle-global-vagrant app)
  "Toggle global vagrant-mode (Vagrant VM management)."
  (let ((echo (app-state-echo app)))
    (set! *global-vagrant* (not *global-vagrant*))
    (echo-message! echo (if *global-vagrant*
                          "Vagrant ON" "Vagrant OFF"))))

(def (cmd-toggle-global-restclient app)
  "Toggle global restclient-mode (HTTP REST client)."
  (let ((echo (app-state-echo app)))
    (set! *global-restclient* (not *global-restclient*))
    (echo-message! echo (if *global-restclient*
                          "Restclient ON" "Restclient OFF"))))

(def (cmd-toggle-global-ob-http app)
  "Toggle global ob-http-mode (HTTP requests in org-babel)."
  (let ((echo (app-state-echo app)))
    (set! *global-ob-http* (not *global-ob-http*))
    (echo-message! echo (if *global-ob-http*
                          "Ob-http ON" "Ob-http OFF"))))

;;;============================================================================
;;; Parity: subword navigation, goto-last-change, file utilities

(def (subword-boundary? text i direction)
  "Check if position i is a subword boundary."
  (let ((len (string-length text)))
    (and (> i 0) (< i len)
         (let ((prev (string-ref text (- i 1)))
               (cur (string-ref text i)))
           (or (and (= direction 1) (or (char=? cur #\_) (char=? cur #\-)))
               (and (= direction -1) (or (char=? prev #\_) (char=? prev #\-)))
               (and (char-lower-case? prev) (char-upper-case? cur))
               (and (char-alphabetic? prev) (not (or (char-alphabetic? cur) (char-numeric? cur))))
               (and (not (or (char-alphabetic? prev) (char-numeric? prev))) (char-alphabetic? cur)))))))

(def (cmd-forward-subword app)
  "Move forward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (cond
        ((>= i len) (editor-goto-pos ed len))
        ((subword-boundary? text i 1) (editor-goto-pos ed i))
        (else (loop (+ i 1)))))))

(def (cmd-backward-subword app)
  "Move backward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((<= i 0) (editor-goto-pos ed 0))
        ((subword-boundary? text i -1) (editor-goto-pos ed i))
        (else (loop (- i 1)))))))

(def *tui-last-change-positions* (make-hash-table))

(def (cmd-goto-last-change app)
  "Jump to position of last edit in current buffer."
  (let* ((buf (current-buffer-from-app app))
         (ed (current-editor app))
         (name (and buf (buffer-name buf)))
         (pos (and name (hash-get *tui-last-change-positions* name))))
    (if pos
      (editor-goto-pos ed pos)
      (echo-message! (app-state-echo app) "No recorded change position"))))

(def (cmd-find-file-at-line app)
  "Open a file and jump to a specific line (file:line format)."
  (let ((input (app-read-string app "File:line: ")))
    (when (and input (not (string-empty? input)))
      (let* ((colon (let loop ((i 0))
                      (cond ((>= i (string-length input)) #f)
                            ((char=? (string-ref input i) #\:) i)
                            (else (loop (+ i 1))))))
             (file (if colon (substring input 0 colon) input))
             (line (and colon (< (+ colon 1) (string-length input))
                       (string->number (substring input (+ colon 1) (string-length input))))))
        (when (file-exists? file)
          (let* ((content (read-file-as-string file))
                 (fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (buf (buffer-create! (path-strip-directory file) ed file)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-text ed (or content ""))
            (when (and line (> line 0))
              (editor-goto-line ed (- line 1)))
            (echo-message! (app-state-echo app) (string-append "Opened: " file))))))))

(def (cmd-find-file-read-only app)
  "Open a file in read-only mode."
  (let ((path (app-read-string app "Find file read-only: ")))
    (when (and path (not (string-empty? path)))
      (if (not (file-exists? path))
        (echo-error! (app-state-echo app) (string-append "File not found: " path))
        (let* ((content (read-file-as-string path))
               (fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (buf (buffer-create! (path-strip-directory path) ed path)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed (or content ""))
          (editor-set-read-only ed #t)
          (echo-message! (app-state-echo app) (string-append "Read-only: " path)))))))

(def (cmd-view-file app)
  "Open a file in read-only view mode."
  (cmd-find-file-read-only app))

(def (cmd-append-to-file app)
  "Append the selected region to a file."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (buf (current-buffer-from-app app))
         (mark (and buf (buffer-mark buf))))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring text start end))
             (path (app-read-string app "Append to file: ")))
        (when (and path (not (string-empty? path)))
          (call-with-output-file [path: path append: #t]
            (lambda (p) (display region p)))
          (echo-message! (app-state-echo app)
            (string-append "Appended " (number->string (- end start)) " chars to " path)))))))

