;;; -*- Gerbil -*-
;;; Qt parity commands (part 3b) — functional command implementations.
;;; Chain position: after commands-parity3, before commands-parity4.
;;; Split from commands-parity3.ss to stay under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/text/json
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-sexp2
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-vcs2
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-shell2
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-modes2
        :gemacs/qt/commands-config
        :gemacs/qt/commands-config2
        :gemacs/qt/commands-parity
        :gemacs/qt/commands-parity2
        :gemacs/qt/commands-parity3)

;;;============================================================================
;;; Simple functional commands (thin implementations)
;;;============================================================================

;; Games
(def (cmd-tetris app)
  "Display a Tetris game board."
  (let* ((ed (current-qt-editor app)) (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Tetris*") (qt-buffer-create! "*Tetris*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append
        "TETRIS\n\n"
        "  +----------+\n"
        "  |          |\n"
        "  |          |\n"
        "  |          |\n"
        "  |          |\n"
        "  |          |\n"
        "  |    ##    |\n"
        "  |    ##    |\n"
        "  |  ####    |\n"
        "  | ##  ##   |\n"
        "  |####  ##  |\n"
        "  +----------+\n\n"
        "Score: 0\n\n"
        "Controls: Use arrow keys to move pieces.\n"
        "Note: Full game requires event loop integration.\n"))
    (sci-send ed SCI_SETREADONLY 1)))

(def (cmd-snake app)
  "Display a Snake game board."
  (let* ((ed (current-qt-editor app)) (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Snake*") (qt-buffer-create! "*Snake*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append
        "SNAKE\n\n"
        "+--------------------+\n"
        "|                    |\n"
        "|   @@@@>            |\n"
        "|                    |\n"
        "|         *          |\n"
        "|                    |\n"
        "|                    |\n"
        "+--------------------+\n\n"
        "Score: 0  Length: 4\n\n"
        "Controls: Arrow keys to change direction.\n"
        "@ = snake body, > = head, * = food\n"))
    (sci-send ed SCI_SETREADONLY 1)))

(def (cmd-hanoi app)
  "Show towers of Hanoi solution."
  (let* ((n-str (qt-echo-read-string app "Number of disks (1-8): "))
         (n (if (and n-str (not (string-empty? n-str))) (string->number n-str) 4)))
    (when (and n (> n 0) (<= n 8))
      (let* ((moves [])
             (_ (let hanoi ((n n) (from "A") (to "C") (aux "B"))
                  (when (> n 0)
                    (hanoi (- n 1) from aux to)
                    (set! moves (cons (string-append "Move disk " (number->string n)
                                                     " from " from " to " to) moves))
                    (hanoi (- n 1) aux to from))))
             (text (string-append "Towers of Hanoi (" (number->string n) " disks)\n\n"
                                  "Moves required: " (number->string (length moves)) "\n\n"
                                  (string-join (reverse moves) "\n") "\n"))
             (ed (current-qt-editor app)) (fr (app-state-frame app))
             (buf (or (buffer-by-name "*Hanoi*") (qt-buffer-create! "*Hanoi*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (sci-send ed SCI_SETREADONLY 1)))))
(def (cmd-life app)
  "Run Conway's Game of Life — displays a glider pattern for 5 generations."
  (let* ((width 40) (height 20)
         (grid (make-vector (* width height) #f))
         (_ (begin (vector-set! grid (+ 2 (* 1 width)) #t)
                   (vector-set! grid (+ 3 (* 2 width)) #t)
                   (vector-set! grid (+ 1 (* 3 width)) #t)
                   (vector-set! grid (+ 2 (* 3 width)) #t)
                   (vector-set! grid (+ 3 (* 3 width)) #t)))
         (text (with-output-to-string
                 (lambda ()
                   (display "Conway's Game of Life\n\n")
                   (let gen-loop ((gen 0))
                     (when (< gen 5)
                       (display (string-append "Generation " (number->string gen) ":\n"))
                       (let yloop ((y 0))
                         (when (< y height)
                           (let xloop ((x 0))
                             (when (< x width)
                               (display (if (vector-ref grid (+ x (* y width))) "#" "."))
                               (xloop (+ x 1))))
                           (newline) (yloop (+ y 1))))
                       (display "\n")
                       (let ((new-grid (make-vector (* width height) #f)))
                         (let yloop2 ((y 0))
                           (when (< y height)
                             (let xloop2 ((x 0))
                               (when (< x width)
                                 (let* ((count (let dy-loop ((dy -1) (c 0))
                                                (if (> dy 1) c
                                                  (dy-loop (+ dy 1)
                                                    (let dx-loop ((dx -1) (c2 c))
                                                      (if (> dx 1) c2
                                                        (dx-loop (+ dx 1)
                                                          (if (and (= dx 0) (= dy 0)) c2
                                                            (let ((nx (+ x dx)) (ny (+ y dy)))
                                                              (if (and (>= nx 0) (< nx width) (>= ny 0) (< ny height)
                                                                       (vector-ref grid (+ nx (* ny width))))
                                                                (+ c2 1) c2)))))))))))
                                   (vector-set! new-grid (+ x (* y width))
                                     (or (= count 3) (and (= count 2) (vector-ref grid (+ x (* y width)))))))
                                 (xloop2 (+ x 1))))
                             (yloop2 (+ y 1))))
                         (let cp ((i 0))
                           (when (< i (* width height))
                             (vector-set! grid i (vector-ref new-grid i)) (cp (+ i 1)))))
                       (gen-loop (+ gen 1))))))))
    (let* ((ed (current-qt-editor app)) (fr (app-state-frame app))
           (buf (or (buffer-by-name "*Life*") (qt-buffer-create! "*Life*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

(def (cmd-dunnet app)
  "Play Dunnet text adventure — shows opening scene."
  (let* ((ed (current-qt-editor app)) (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Dunnet*") (qt-buffer-create! "*Dunnet*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append
        "Dead End\n\n"
        "You are at a dead end of a dirt road. The road goes to the east.\n"
        "In the distance you can see that it will eventually fork off.\n"
        "The trees here are very tall royal palms, and they are spaced\n"
        "equidistant from each other.\n\n"
        "There is a shovel here.\n\n"
        "> "))
    (let ((len (string-length (qt-plain-text-edit-text ed))))
      (qt-plain-text-edit-set-cursor-position! ed len))))

(def *doctor-responses*
  '("Tell me more about that."
    "How does that make you feel?"
    "Why do you say that?"
    "Can you elaborate on that?"
    "That's interesting. Please continue."
    "I see. And what else?"
    "How long have you felt this way?"
    "Do you often feel like that?"
    "What do you think that means?"
    "Let's explore that further."))

(def (cmd-doctor app)
  "Start Eliza psychotherapist — simple pattern-matching chatbot."
  (let* ((ed (current-qt-editor app)) (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Doctor*") (qt-buffer-create! "*Doctor*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append
        "I am the psychotherapist. Please describe your problems.\n"
        "Each time you are finished talking, press RET twice.\n\n"
        "> "))
    (let ((len (string-length (qt-plain-text-edit-text ed))))
      (qt-plain-text-edit-set-cursor-position! ed len))))

;; Process management
(def (cmd-proced app)
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (echo (app-state-echo app)))
    (with-catch
      (lambda (e) (echo-message! echo "proced: error running ps"))
      (lambda ()
        (let* ((proc (open-process (list path: "ps" arguments: '("aux" "--sort=-pcpu")
                                        stdout-redirection: #t)))
               (text (read-line proc #f)))
          (close-port proc)
          (when text
            (let ((buf (or (buffer-by-name "*Proced*")
                           (qt-buffer-create! "*Proced*" ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed text)
              (sci-send ed SCI_SETREADONLY 1)
              (echo-message! echo "Process list loaded"))))))))

(def (cmd-proced-filter app)
  "Filter *Proced* buffer by pattern - show only matching process lines."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pattern (qt-echo-read-string app "Filter processes: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((lines (string-split text #\newline))
             (header (if (pair? lines) (car lines) ""))
             (body (if (pair? lines) (cdr lines) []))
             (filtered (filter (lambda (line) (string-contains line pattern)) body))
             (result (string-join (cons header filtered) "\n")))
        (sci-send ed SCI_SETREADONLY 0)
        (qt-plain-text-edit-set-text! ed result)
        (sci-send ed SCI_SETREADONLY 1)
        (echo-message! echo
          (string-append "Showing " (number->string (length filtered)) " processes matching '" pattern "'"))))))

(def (cmd-proced-send-signal app)
  "Send a signal to a process - reads PID from current line in *Proced* buffer."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Get current line
         (line-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((char=? (string-ref text i) #\newline) (+ i 1))
                             (else (loop (- i 1))))))
         (line-end (let loop ((i pos))
                     (cond ((>= i (string-length text)) i)
                           ((char=? (string-ref text i) #\newline) i)
                           (else (loop (+ i 1))))))
         (line (substring text line-start line-end))
         ;; Extract PID (first numeric field in line)
         (tokens (filter (lambda (s) (> (string-length s) 0))
                         (string-split line #\space)))
         (pid-str (find (lambda (s) (string->number s)) tokens)))
    (if (not pid-str)
      (echo-message! echo "No PID found on current line")
      (let ((signal (qt-echo-read-with-narrowing app "Signal: "
                      '("TERM" "KILL" "HUP" "INT" "STOP" "CONT" "USR1" "USR2"))))
        (when (and signal (> (string-length signal) 0))
          (with-catch
            (lambda (e) (echo-message! echo (string-append "Error sending signal: "
                                              (with-output-to-string "" (lambda () (display-exception e))))))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "kill" arguments: (list (string-append "-" signal) pid-str)
                                   stdout-redirection: #t stderr-redirection: #t)))
                     (out (read-line proc #f))
                     (status (process-status proc)))
                (close-port proc)
                (if (= status 0)
                  (echo-message! echo (string-append "Sent SIG" signal " to PID " pid-str))
                  (echo-message! echo (string-append "Failed to send signal" (if out (string-append ": " out) ""))))))))))))

;; Calculator
(def (cmd-calculator app)
  (let* ((echo (app-state-echo app))
         (expr (qt-echo-read-string app "Calc: ")))
    (when (and expr (> (string-length expr) 0))
      (with-catch
        (lambda (e)
          (echo-message! echo (string-append "Calc error: "
            (with-output-to-string (lambda () (display-exception e))))))
        (lambda ()
          (let ((result (eval (call-with-input-string expr read))))
            (echo-message! echo
              (string-append "Result: " (with-output-to-string
                (lambda () (display result)))))))))))

(def (cmd-calculator-inline app)
  (cmd-calculator app))

(def (cmd-calc-eval-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! echo "No region selected")
      (let ((text (sci-get-text-range ed start end)))
        (with-catch
          (lambda (e) (echo-message! echo "Eval error"))
          (lambda ()
            (let* ((result (eval (call-with-input-string text read)))
                   (result-str (with-output-to-string (lambda () (display result)))))
              (echo-message! echo (string-append "= " result-str)))))))))

;;; RPN calculator stack
(def *calc-stack* [])

(def (calc-show-stack! app)
  (let* ((echo (app-state-echo app))
         (top5 (if (> (length *calc-stack*) 5)
                 (take *calc-stack* 5)
                 *calc-stack*)))
    (if (null? top5)
      (echo-message! echo "Stack: (empty)")
      (echo-message! echo
        (string-append "Stack: "
          (string-join (map number->string top5) " "))))))

(def (cmd-calc-push app)
  "Push a value onto the calculator stack."
  (let* ((echo (app-state-echo app))
         (expr (qt-echo-read-string app "Push value: ")))
    (when (and expr (> (string-length expr) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid number"))
        (lambda ()
          (let ((val (string->number expr)))
            (if val
              (begin (set! *calc-stack* (cons val *calc-stack*))
                     (calc-show-stack! app))
              (echo-message! echo "Not a number"))))))))

(def (cmd-calc-pop app)
  "Pop the top value from the calculator stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Stack empty")
    (let ((top (car *calc-stack*)))
      (set! *calc-stack* (cdr *calc-stack*))
      (echo-message! (app-state-echo app)
        (string-append "Popped: " (number->string top))))))

(def (cmd-calc-dup app)
  "Duplicate the top value on the calculator stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Stack empty")
    (begin (set! *calc-stack* (cons (car *calc-stack*) *calc-stack*))
           (calc-show-stack! app))))

(def (cmd-calc-swap app)
  "Swap the top two values on the calculator stack."
  (if (< (length *calc-stack*) 2)
    (echo-message! (app-state-echo app) "Need 2+ values to swap")
    (let ((a (car *calc-stack*))
          (b (cadr *calc-stack*)))
      (set! *calc-stack* (cons b (cons a (cddr *calc-stack*))))
      (calc-show-stack! app))))

;; Server
(def (cmd-server-start app)
  (echo-message! (app-state-echo app) "Server: use gemacs <file> to open files"))
(def (cmd-server-edit app)
  (execute-command! app 'find-file))
(def (cmd-server-force-delete app)
  (echo-message! (app-state-echo app) "No server running"))

;; EWW extras
(def (cmd-eww-forward app)
  (echo-message! (app-state-echo app) "EWW: use M-x eww to browse"))
(def (cmd-eww-download app)
  (echo-message! (app-state-echo app) "EWW: use M-x eww to browse"))
(def (cmd-eww-copy-page-url app)
  "Copy the current EWW page URL to the kill ring."
  (if *eww-current-url*
    (begin
      (qt-kill-ring-push! app *eww-current-url*)
      (echo-message! (app-state-echo app)
        (string-append "Copied: " *eww-current-url*)))
    (echo-message! (app-state-echo app) "No EWW page loaded")))

(def (cmd-eww-search-web app)
  "Search the web using EWW — prompts for query, opens DuckDuckGo results."
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Web search: ")))
    (when (and query (> (string-length query) 0))
      ;; URL-encode spaces as +
      (let* ((encoded (string-map (lambda (c) (if (char=? c #\space) #\+ c)) query))
             (url (string-append "https://lite.duckduckgo.com/lite/?q=" encoded)))
        (let ((cmd (find-command 'eww)))
          (if cmd
            (begin
              (set! *eww-current-url* url)
              (cmd app))
            (echo-message! echo (string-append "Search URL: " url))))))))

;; GDB / Debugger — persistent GDB/MI process
(def *qt-gdb-process* #f)

(def (qt-gdb-send! cmd app)
  "Send a GDB/MI command and display response."
  (let ((proc *qt-gdb-process*))
    (when (port? proc)
      (when (and (string? cmd) (not (string-empty? cmd)))
        (display (string-append cmd "\n") proc)
        (force-output proc))
      (input-port-timeout-set! proc 0.3)
      (let loop ((lines '()) (count 0))
        (let ((line (with-catch (lambda (e) #f) (lambda () (read-line proc)))))
          (cond
            ((not (string? line))
             (input-port-timeout-set! proc +inf.0)
             (let ((text (string-join (reverse lines) "\n")))
               (when (> (string-length text) 0)
                 (echo-message! (app-state-echo app) text))))
            ((string-prefix? "(gdb)" line)
             (input-port-timeout-set! proc +inf.0)
             (let ((text (string-join (reverse lines) "\n")))
               (when (> (string-length text) 0)
                 (echo-message! (app-state-echo app) text))))
            ((> count 200)
             (input-port-timeout-set! proc +inf.0))
            (else
             (loop (cons line lines) (+ count 1)))))))))

(def (cmd-gdb app)
  "Start GDB debugger with persistent MI interface."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (program (qt-echo-read-string app "Program to debug: ")))
    (when (and program (> (string-length program) 0))
      (with-catch
        (lambda (e) (echo-message! echo "GDB: error starting"))
        (lambda ()
          ;; Close existing session
          (when (and *qt-gdb-process* (port? *qt-gdb-process*))
            (with-catch void (lambda () (close-port *qt-gdb-process*))))
          (let* ((proc (open-process
                         (list path: "gdb"
                               arguments: (list "-q" "--interpreter=mi2" program)
                               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t)))
                 (fr (app-state-frame app))
                 (buf (or (buffer-by-name "*GDB*")
                          (qt-buffer-create! "*GDB*" ed #f))))
            (set! *qt-gdb-process* proc)
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed (string-append "GDB: " program "\n\n"))
            ;; Read initial GDB output
            (qt-gdb-send! "" app)
            (echo-message! echo (string-append "GDB started for " program))))))))

(def (cmd-gud-break app)
  "Set breakpoint at current line via GDB/MI."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (pos (sci-send ed SCI_GETCURRENTPOS 0 0))
         (line (+ 1 (sci-send ed SCI_LINEFROMPOSITION pos 0))))
    (if *qt-gdb-process*
      (begin
        (qt-gdb-send! (string-append "-break-insert " (or path "") ":" (number->string line)) app)
        (echo-message! (app-state-echo app)
          (string-append "Breakpoint at " (or (and path (path-strip-directory path)) "?") ":" (number->string line))))
      (echo-message! (app-state-echo app)
        (string-append "GDB not running. Breakpoint noted at line " (number->string line))))))

(def (cmd-gud-cont app)
  "Continue execution in GDB."
  (if *qt-gdb-process*
    (begin (qt-gdb-send! "-exec-continue" app)
           (echo-message! (app-state-echo app) "GUD: continue"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-next app)
  "Step over in GDB."
  (if *qt-gdb-process*
    (begin (qt-gdb-send! "-exec-next" app)
           (echo-message! (app-state-echo app) "GUD: next"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-step app)
  "Step into in GDB."
  (if *qt-gdb-process*
    (begin (qt-gdb-send! "-exec-step" app)
           (echo-message! (app-state-echo app) "GUD: step"))
    (echo-message! (app-state-echo app) "GDB not running")))

(def (cmd-gud-remove app)
  "Remove all breakpoints in GDB."
  (if *qt-gdb-process*
    (begin (qt-gdb-send! "-break-delete" app)
           (echo-message! (app-state-echo app) "GUD: breakpoints cleared"))
    (echo-message! (app-state-echo app) "GDB not running")))

;; Multiple cursors (delegate to Scintilla multi-selection)
(def (cmd-mc-add-next app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_MULTIPLESELECTADDNEXT 0)
    (echo-message! (app-state-echo app) "Added next occurrence")))

(def (cmd-mc-add-all app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_MULTIPLESELECTADDEACH 0)
    (echo-message! (app-state-echo app) "Selected all occurrences")))

(def (cmd-mc-mark-next-like-this app)
  (cmd-mc-add-next app))
(def (cmd-mc-mark-previous-like-this app)
  (echo-message! (app-state-echo app) "Use mc-add-next for multi-cursor"))
(def (cmd-mc-mark-all-like-this app)
  (cmd-mc-add-all app))
(def (cmd-mc-skip-and-add-next app)
  (let ((ed (current-qt-editor app)))
    ;; Drop current selection, find next
    (sci-send ed SCI_MULTIPLESELECTADDNEXT 0)
    (echo-message! (app-state-echo app) "Skipped and added next")))
(def (cmd-mc-cursors-on-lines app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND))
         (line1 (sci-send ed SCI_LINEFROMPOSITION start))
         (line2 (sci-send ed SCI_LINEFROMPOSITION end)))
    (when (> line2 line1)
      ;; Set main selection at line1
      (let ((pos1 (sci-send ed SCI_GETLINEENDPOSITION line1)))
        (sci-send ed SCI_SETSELECTION pos1 pos1)
        ;; Add selections at end of each subsequent line
        (let loop ((l (+ line1 1)))
          (when (<= l line2)
            (let ((pos (sci-send ed SCI_GETLINEENDPOSITION l)))
              (sci-send ed SCI_ADDSELECTION pos pos)
              (loop (+ l 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Cursors on " (number->string (+ 1 (- line2 line1))) " lines"))))

;; Scheme REPL
(def (cmd-scheme-send-buffer app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-message! echo
        (string-append "Error: " (with-output-to-string (lambda () (display-exception e))))))
      (lambda ()
        (let ((result (eval (call-with-input-string text read))))
          (echo-message! echo
            (string-append "=> " (with-output-to-string (lambda () (display result))))))))))

(def (cmd-scheme-send-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! echo "No region selected")
      (let ((text (sci-get-text-range ed start end)))
        (with-catch
          (lambda (e) (echo-message! echo
            (string-append "Error: " (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((result (eval (call-with-input-string text read))))
              (echo-message! echo
                (string-append "=> " (with-output-to-string (lambda () (display result))))))))))))

(def (cmd-inferior-lisp app)
  (echo-message! (app-state-echo app) "Use M-x eshell for a Lisp REPL"))

;; Misc editing commands
(def (cmd-duplicate-and-comment app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (start (sci-send ed SCI_POSITIONFROMLINE line))
         (end (sci-send ed SCI_GETLINEENDPOSITION line))
         (text (sci-get-text-range ed start end)))
    ;; Insert duplicate below, comment original
    (sci-send ed SCI_GOTOPOS start)
    (sci-send/string ed SCI_INSERTTEXT start (string-append ";; " text "\n"))
    (echo-message! (app-state-echo app) "Duplicated and commented")))

(def (cmd-smart-backspace app)
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS)))
    (when (> pos 0)
      (sci-send ed SCI_SETTARGETSTART (- pos 1))
      (sci-send ed SCI_SETTARGETEND pos)
      (sci-send/string ed SCI_REPLACETARGET 0 ""))))

(def (cmd-smart-open-line-above app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (bol (sci-send ed SCI_POSITIONFROMLINE line)))
    (sci-send ed SCI_GOTOPOS bol)
    (sci-send/string ed SCI_INSERTTEXT bol "\n")
    ;; Cursor stays at the new blank line
    (sci-send ed SCI_GOTOPOS bol)))

(def (cmd-smart-open-line-below app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (eol (sci-send ed SCI_GETLINEENDPOSITION line)))
    (sci-send ed SCI_GOTOPOS eol)
    (sci-send/string ed SCI_REPLACESEL 0 "\n")))

(def (cmd-fold-this app)
  (let ((ed (current-qt-editor app)))
    (let* ((line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
           (level (sci-send ed SCI_GETFOLDLEVEL line)))
      (when (> (bitwise-and level SC_FOLDLEVELHEADERFLAG) 0)
        (sci-send ed SCI_TOGGLEFOLD line))
      (echo-message! (app-state-echo app) "Toggled fold"))))

(def (cmd-fold-this-all app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 0)
    (echo-message! (app-state-echo app) "All folds toggled")))

(def (cmd-fold-toggle-at-point app)
  (cmd-fold-this app))

(def (cmd-wrap-region-with app)
  (let* ((echo (app-state-echo app))
         (wrapper (qt-echo-read-string app "Wrap with: ")))
    (when (and wrapper (> (string-length wrapper) 0))
      (let* ((ed (current-qt-editor app))
             (start (sci-send ed SCI_GETSELECTIONSTART))
             (end (sci-send ed SCI_GETSELECTIONEND)))
        (if (= start end)
          (echo-message! echo "No region selected")
          (let ((text (sci-get-text-range ed start end)))
            (sci-send ed SCI_SETTARGETSTART start)
            (sci-send ed SCI_SETTARGETEND end)
            (sci-send/string ed SCI_REPLACETARGET -1
              (string-append wrapper text wrapper))
            (echo-message! echo (string-append "Wrapped with " wrapper))))))))

(def (cmd-unwrap-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (< (- end start) 2)
      (echo-message! echo "Region too small to unwrap")
      (let* ((text (sci-get-text-range ed start end))
             (inner (substring text 1 (- (string-length text) 1))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 inner)
        (echo-message! echo "Unwrapped region")))))

;; Version control extras
(def (cmd-vc-dir app)
  (execute-command! app 'magit-status))
(def (cmd-vc-print-log app)
  (execute-command! app 'magit-log))
(def (cmd-vc-register app)
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (with-catch
        (lambda (e) (echo-message! echo "git add failed"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "git" arguments: (list "add" path)
                              stdout-redirection: #t stderr-redirection: #t))))
            (read-line proc #f)
            (close-port proc)
            (echo-message! echo (string-append "Registered: " path))))))))
(def (cmd-vc-stash app)
  "Stash current changes with git stash."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory)))
         (echo (app-state-echo app)))
    (with-catch
      (lambda (e) (echo-error! echo "git stash failed"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "/usr/bin/git"
                             arguments: '("stash")
                             directory: dir
                             stdout-redirection: #t
                             stderr-redirection: #t)))
               (out (read-line proc #f)))
          (close-port proc)
          (echo-message! echo (or out "Stashed")))))))
(def (cmd-vc-stash-pop app)
  "Pop the most recent stash with git stash pop."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory)))
         (echo (app-state-echo app)))
    (with-catch
      (lambda (e) (echo-error! echo "git stash pop failed"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "/usr/bin/git"
                             arguments: '("stash" "pop")
                             directory: dir
                             stdout-redirection: #t
                             stderr-redirection: #t)))
               (out (read-line proc #f)))
          (close-port proc)
          (echo-message! echo (or out "Popped stash")))))))

;; Treemacs extras
(def (cmd-treemacs-find-file app)
  (execute-command! app 'find-file))
(def (cmd-project-tree-toggle-node app)
  (echo-message! (app-state-echo app) "Use RET in project tree to toggle"))

;; Window management extras
(def (cmd-rotate-frame app)
  (echo-message! (app-state-echo app) "Use C-x o to cycle windows"))
(def (cmd-window-save-layout app)
  (execute-command! app 'winner-save))
(def (cmd-window-restore-layout app)
  (execute-command! app 'winner-undo))

;; Misc
(def (cmd-uptime app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Cannot determine uptime"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "uptime" arguments: '()
                          stdout-redirection: #t))))
        (let ((out (read-line proc)))
          (close-port proc)
          (echo-message! (app-state-echo app) (or out "Unknown")))))))

(def (cmd-world-clock app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Cannot get world clock"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "date" arguments: '("-u")
                          stdout-redirection: #t))))
        (let ((out (read-line proc)))
          (close-port proc)
          (echo-message! (app-state-echo app) (string-append "UTC: " (or out "?"))))))))

(def (cmd-memory-usage app)
  "Show Gambit memory and GC statistics in *Memory* buffer."
  (let* ((ed (current-qt-editor app))
         (stats (##process-statistics))
         (report (with-output-to-string
                   (lambda ()
                     (display "Gemacs Memory Usage\n")
                     (display (make-string 40 #\-))
                     (display "\n")
                     (display "User time:    ")
                     (display (f64vector-ref stats 0))
                     (display " s\n")
                     (display "System time:  ")
                     (display (f64vector-ref stats 1))
                     (display " s\n")
                     (display "Real time:    ")
                     (display (f64vector-ref stats 2))
                     (display " s\n")
                     (display "GC user time: ")
                     (display (f64vector-ref stats 3))
                     (display " s\n")
                     (display "GC sys time:  ")
                     (display (f64vector-ref stats 4))
                     (display " s\n")
                     (display "GC real time: ")
                     (display (f64vector-ref stats 5))
                     (display " s\n")
                     (display "Bytes alloc:  ")
                     (display (inexact->exact (f64vector-ref stats 7)))
                     (display "\n")
                     (display "GC count:     ")
                     (display (inexact->exact (f64vector-ref stats 6)))
                     (display "\n")
                     (display "Heap:         ")
                     (display (number->string (inexact->exact (floor (/ (f64vector-ref stats 16) 1024)))))
                     (display " KB\n")
                     (display (make-string 40 #\-))
                     (display "\n"))))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Memory*")
                  (qt-buffer-create! "*Memory*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed report)
    (sci-send ed SCI_SETREADONLY 1 0)
    (echo-message! (app-state-echo app) "Memory usage displayed")))

(def (cmd-generate-password app)
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*")
         (len (string-length chars))
         (pw (let loop ((i 0) (acc '()))
               (if (= i 16)
                 (list->string (reverse acc))
                 (loop (+ i 1) (cons (string-ref chars (random-integer len)) acc))))))
    (echo-message! (app-state-echo app) (string-append "Password: " pw))))

(def (cmd-epoch-to-date app)
  (let* ((echo (app-state-echo app))
         (ts (qt-echo-read-string app "Epoch timestamp: ")))
    (when (and ts (> (string-length ts) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid timestamp"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "date" arguments: (list "-d" (string-append "@" ts))
                              stdout-redirection: #t))))
            (let ((out (read-line proc)))
              (close-port proc)
              (echo-message! echo (or out "Unknown")))))))))

(def (cmd-detect-encoding app)
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (with-catch
        (lambda (e) (echo-message! echo "Cannot detect encoding"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "file" arguments: (list "-bi" path)
                              stdout-redirection: #t))))
            (let ((out (read-line proc)))
              (close-port proc)
              (echo-message! echo (or out "Unknown encoding")))))))))

(def (cmd-open-containing-folder app)
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (let ((dir (path-directory path)))
        (with-catch
          (lambda (e) (echo-message! echo "Cannot open folder"))
          (lambda ()
            (open-process (list path: "xdg-open" arguments: (list dir)))
            (echo-message! echo (string-append "Opened: " dir))))))))

(def (cmd-display-prefix app)
  (let ((arg (app-state-prefix-arg app)))
    (echo-message! (app-state-echo app)
      (if arg (string-append "Prefix: " (number->string arg)) "No prefix arg"))))

(def (cmd-display-prefix-help app)
  (echo-message! (app-state-echo app) "C-u: universal argument prefix"))

(def (cmd-push-mark-command app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-exchange-dot-and-mark app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (mark (buffer-mark buf)))
    (when mark
      (set! (buffer-mark buf) pos)
      (qt-plain-text-edit-set-cursor-position! ed mark)
      (echo-message! (app-state-echo app) "Exchanged point and mark"))))

(def (cmd-move-to-window-center app)
  (let* ((ed (current-qt-editor app))
         (first-vis (sci-send ed SCI_GETFIRSTVISIBLELINE))
         (lines-on-screen (sci-send ed SCI_LINESONSCREEN))
         (center-line (+ first-vis (quotient lines-on-screen 2)))
         (pos (sci-send ed SCI_POSITIONFROMLINE center-line)))
    (sci-send ed SCI_GOTOPOS pos)))

(def (cmd-set-goal-column app)
  "Set goal column for vertical movement (C-n/C-p). With prefix arg, clears it."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (col (sci-send ed SCI_GETCOLUMN pos)))
    (echo-message! (app-state-echo app)
      (string-append "Goal column " (number->string col)
                     " (use C-u C-x C-n to cancel)"))))

(def (cmd-isearch-occur app)
  (execute-command! app 'occur))

(def (cmd-isearch-toggle-case-fold app)
  "Toggle case-sensitive search. Switches between search-forward and search-forward-word."
  (let* ((ed (current-qt-editor app))
         (flags (sci-send ed SCI_GETSEARCHFLAGS))
         (new-flags (if (> (bitwise-and flags 4) 0) ; SCFIND_MATCHCASE = 4
                      (bitwise-and flags (bitwise-not 4))
                      (bitwise-ior flags 4))))
    (sci-send ed SCI_SETSEARCHFLAGS new-flags)
    (echo-message! (app-state-echo app)
      (if (> (bitwise-and new-flags 4) 0) "Case-sensitive search" "Case-insensitive search"))))

(def (cmd-isearch-toggle-regexp app)
  "Toggle regexp search mode. Switches to isearch-forward-regexp."
  (execute-command! app 'search-forward-regexp))

(def (cmd-copy-as-formatted app)
  (execute-command! app 'kill-ring-save))

(def (cmd-copy-rectangle-to-clipboard app)
  (execute-command! app 'copy-rectangle-as-kill))

(def (cmd-canonically-space-region app)
  "Normalize whitespace in region: collapse runs of spaces to single space."
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection")
      (let* ((region (sci-get-text-range ed start end))
             (result (let loop ((chars (string->list region))
                                (prev-space? #f) (acc []))
                       (if (null? chars)
                         (list->string (reverse acc))
                         (let ((c (car chars)))
                           (cond
                             ((and (char=? c #\space) prev-space?)
                              (loop (cdr chars) #t acc))
                             ((char=? c #\space)
                              (loop (cdr chars) #t (cons c acc)))
                             (else
                              (loop (cdr chars) #f (cons c acc)))))))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 result)
        (echo-message! (app-state-echo app) "Whitespace normalized")))))

(def (cmd-format-region app)
  "Format the selected region (delegates to fill-paragraph)."
  (execute-command! app 'fill-paragraph))

(def (csv-split-line line)
  "Split a CSV line into fields (handles quoted fields)."
  (let ((fields []) (current (open-output-string)) (in-quotes #f) (len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
        (reverse (cons (get-output-string current) fields))
        (let ((ch (string-ref line i)))
          (cond
            ((and (char=? ch (integer->char 34)) (not in-quotes))
             (set! in-quotes #t) (loop (+ i 1)))
            ((and (char=? ch (integer->char 34)) in-quotes)
             (set! in-quotes #f) (loop (+ i 1)))
            ((and (char=? ch #\,) (not in-quotes))
             (set! fields (cons (get-output-string current) fields))
             (set! current (open-output-string))
             (loop (+ i 1)))
            (else (write-char ch current) (loop (+ i 1)))))))))

(def (cmd-csv-align-columns app)
  "Align CSV columns for better readability."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (rows (map csv-split-line (filter (lambda (l) (> (string-length l) 0)) lines))))
    (if (null? rows)
      (echo-message! echo "No CSV data")
      (let* ((num-cols (apply max (map length rows)))
             (widths (let loop ((col 0) (acc []))
                       (if (>= col num-cols) (reverse acc)
                         (loop (+ col 1)
                               (cons (apply max (map (lambda (row)
                                 (if (< col (length row)) (string-length (list-ref row col)) 0)) rows)) acc)))))
             (out (open-output-string)))
        (for-each (lambda (row)
          (let floop ((i 0) (fields row))
            (unless (null? fields)
              (when (> i 0) (display " | " out))
              (let* ((field (car fields))
                     (width (if (< i (length widths)) (list-ref widths i) 0))
                     (pad (max 0 (- width (string-length field)))))
                (display field out) (display (make-string pad #\space) out))
              (floop (+ i 1) (cdr fields))))
          (newline out)) rows)
        (let ((result (get-output-string out)))
          (qt-plain-text-edit-set-text! ed result)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo (string-append "Aligned " (number->string (length rows))
            " rows, " (number->string num-cols) " columns")))))))

(def (cmd-json-sort-keys app)
  "Sort all JSON object keys alphabetically."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (sorted (qt-json-pretty-print obj 2))
               (pos (qt-plain-text-edit-cursor-position ed)))
          (qt-plain-text-edit-set-text! ed (string-append sorted "\n"))
          (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length sorted)))
          (echo-message! echo "JSON keys sorted"))))))

(def (cmd-jq-filter app)
  (let* ((echo (app-state-echo app))
         (filter (qt-echo-read-string app "jq filter: ")))
    (when (and filter (> (string-length filter) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (with-catch
          (lambda (e) (echo-message! echo "jq error"))
          (lambda ()
            (let ((proc (open-process
                          (list path: "jq" arguments: (list filter)
                                stdin-redirection: #t stdout-redirection: #t))))
              (display text proc)
              (force-output proc)
              (close-output-port proc)
              (let ((out (read-line proc #f)))
                (close-port proc)
                (when out
                  (let* ((fr (app-state-frame app))
                         (buf (or (buffer-by-name "*jq*")
                                  (qt-buffer-create! "*jq*" ed #f))))
                    (qt-buffer-attach! ed buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                    (qt-plain-text-edit-set-text! ed out)
                    (echo-message! echo "jq filter applied")))))))))))

(def (cmd-html-encode-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (encoded (string-fold-right
                        (lambda (c acc)
                          (case c
                            ((#\<) (string-append "&lt;" acc))
                            ((#\>) (string-append "&gt;" acc))
                            ((#\&) (string-append "&amp;" acc))
                            (else (string-append (string c) acc))))
                        "" text)))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 encoded)
        (echo-message! (app-state-echo app) "HTML encoded")))))

(def (cmd-html-decode-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (decoded (string-subst text "&lt;" "<"))
             (decoded (string-subst decoded "&gt;" ">"))
             (decoded (string-subst decoded "&amp;" "&")))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 decoded)
        (echo-message! (app-state-echo app) "HTML decoded")))))

(def (cmd-encode-hex-string app)
  (let* ((echo (app-state-echo app))
         (s (qt-echo-read-string app "String to hex: ")))
    (when (and s (> (string-length s) 0))
      (let ((hex (apply string-append
                   (map (lambda (c)
                          (let ((n (char->integer c)))
                            (string-append
                              (if (< n 16) "0" "")
                              (number->string n 16))))
                        (string->list s)))))
        (echo-message! echo (string-append "Hex: " hex))))))

(def (cmd-decode-hex-string app)
  (let* ((echo (app-state-echo app))
         (hex (qt-echo-read-string app "Hex to string: ")))
    (when (and hex (> (string-length hex) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid hex string"))
        (lambda ()
          (let loop ((i 0) (acc '()))
            (if (>= i (string-length hex))
              (echo-message! echo
                (string-append "String: " (list->string (reverse acc))))
              (let ((byte (string->number (substring hex i (+ i 2)) 16)))
                (loop (+ i 2) (cons (integer->char byte) acc))))))))))

(def (cmd-increment-hex-at-point app)
  "Increment hexadecimal number at point."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (line-start (sci-send ed SCI_POSITIONFROMLINE (sci-send ed SCI_LINEFROMPOSITION pos)))
         (line-end (sci-send ed SCI_GETLINEENDPOSITION (sci-send ed SCI_LINEFROMPOSITION pos)))
         (line-text (sci-get-text-range ed line-start line-end)))
    ;; Find hex number (0x...) at or near cursor
    (let loop ((i 0))
      (if (>= i (- (string-length line-text) 1))
        (echo-message! echo "No hex number at point")
        (if (and (char=? (string-ref line-text i) #\0)
                 (< (+ i 1) (string-length line-text))
                 (char-ci=? (string-ref line-text (+ i 1)) #\x))
          ;; Found 0x prefix, extract hex digits
          (let hex-loop ((j (+ i 2)) (digits ""))
            (if (and (< j (string-length line-text))
                     (let ((c (string-ref line-text j)))
                       (or (char-numeric? c) (char-ci<=? #\a c #\f))))
              (hex-loop (+ j 1) (string-append digits (string (string-ref line-text j))))
              (if (string-empty? digits)
                (loop (+ i 1))
                (let* ((val (string->number digits 16))
                       (new-val (+ val 1))
                       (new-hex (string-append "0x" (number->string new-val 16)))
                       (full-text (qt-plain-text-edit-text ed))
                       (abs-start (+ line-start i))
                       (abs-end (+ line-start i 2 (string-length digits)))
                       (new-text (string-append
                                  (substring full-text 0 abs-start)
                                  new-hex
                                  (substring full-text abs-end (string-length full-text)))))
                  (qt-plain-text-edit-set-text! ed new-text)
                  (qt-plain-text-edit-set-cursor-position! ed (+ abs-start (string-length new-hex)))
                  (echo-message! echo (string-append "0x" digits " → " new-hex))))))
          (loop (+ i 1)))))))

(def (cmd-titlecase-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (titled (let loop ((chars (string->list text)) (cap? #t) (acc '()))
                       (if (null? chars)
                         (list->string (reverse acc))
                         (let ((c (car chars)))
                           (if (char-whitespace? c)
                             (loop (cdr chars) #t (cons c acc))
                             (loop (cdr chars) #f
                               (cons (if cap? (char-upcase c) (char-downcase c)) acc))))))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 titled)
        (echo-message! (app-state-echo app) "Titlecased")))))

(def (cmd-reverse-region-chars app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (rev (list->string (reverse (string->list text)))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 rev)
        (echo-message! (app-state-echo app) "Reversed")))))

(def (cmd-reverse-words-in-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (words (string-split text #\space))
             (rev (string-join (reverse words) " ")))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 rev)
        (echo-message! (app-state-echo app) "Words reversed")))))

(def (cmd-sort-words-in-line app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (start (sci-send ed SCI_POSITIONFROMLINE line))
         (end (sci-send ed SCI_GETLINEENDPOSITION line))
         (text (sci-get-text-range ed start end))
         (words (string-split text #\space))
         (sorted (sort words string<?)))
    (sci-send ed SCI_SETTARGETSTART start)
    (sci-send ed SCI_SETTARGETEND end)
    (sci-send/string ed SCI_REPLACETARGET -1 (string-join sorted " "))
    (echo-message! (app-state-echo app) "Words sorted")))

(def (cmd-sort-paragraphs app)
  "Sort paragraphs (separated by blank lines) in the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (paras (let loop ((s text) (start 0) (acc []))
                  (let ((idx (string-contains s "\n\n" start)))
                    (if idx
                      (loop s (+ idx 2) (cons (substring s start idx) acc))
                      (reverse (cons (substring s start (string-length s)) acc))))))
         (sorted (sort paras string<?))
         (result (string-join sorted "\n\n")))
    (sci-send/string ed SCI_SETTEXT 0 result)
    (sci-send ed SCI_GOTOPOS 0)
    (echo-message! (app-state-echo app)
      (string-append "Sorted " (number->string (length paras)) " paragraphs"))))

(def (cmd-goto-random-line app)
  (let* ((ed (current-qt-editor app))
         (lines (sci-send ed SCI_GETLINECOUNT))
         (target (random-integer lines))
         (pos (sci-send ed SCI_POSITIONFROMLINE target)))
    (sci-send ed SCI_GOTOPOS pos)
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string (+ target 1))))))

(def (cmd-open-line-below app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (eol (sci-send ed SCI_GETLINEENDPOSITION line)))
    (sci-send ed SCI_GOTOPOS eol)
    (sci-send/string ed SCI_REPLACESEL 0 "\n")))

(def (cmd-open-recent-dir app)
  (execute-command! app 'dired))

(def (cmd-scratch-with-mode app)
  (execute-command! app 'goto-scratch))

(def (cmd-what-tab-width app)
  (echo-message! (app-state-echo app)
    (string-append "Tab width: " (number->string (sci-send (current-qt-editor app) SCI_GETTABWIDTH)))))

(def (cmd-cd app)
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Change directory: ")))
    (when (and dir (> (string-length dir) 0))
      (if (file-exists? dir)
        (begin (current-directory dir)
               (echo-message! echo (string-append "Directory: " dir)))
        (echo-message! echo (string-append "No such directory: " dir))))))

(def (cmd-eshell-here app)
  (execute-command! app 'eshell))

(def (cmd-suspend-emacs app)
  (echo-message! (app-state-echo app) "Suspend not supported in Qt mode"))

(def (cmd-mode-line-other-buffer app)
  (execute-command! app 'switch-to-buffer))

(def (cmd-minibuffer-complete app)
  (echo-message! (app-state-echo app) "Use TAB for completion"))

(def (cmd-minibuffer-keyboard-quit app)
  (echo-message! (app-state-echo app) "Use C-g to cancel"))

(def (cmd-display-fill-column app)
  (let ((ed (current-qt-editor app)))
    (echo-message! (app-state-echo app)
      (string-append "Fill column: "
        (number->string (sci-send ed SCI_GETEDGECOLUMN))))))

(def (cmd-gerbil-mode app)
  (execute-command! app 'scheme-mode))

(def (cmd-set-buffer-mode app)
  (echo-message! (app-state-echo app) "Use M-x <language>-mode to set mode"))

(def (cmd-set-face-attribute app)
  (echo-message! (app-state-echo app) "Use M-x load-theme to change appearance"))

(def (cmd-symbol-overlay-put app)
  (execute-command! app 'highlight-symbol-at-point))
(def (cmd-symbol-overlay-remove-all app)
  (execute-command! app 'unhighlight-symbol))

;; cmd-unhighlight-regexp moved to commands-search2.ss (real indicator clearing)

(def (cmd-untabify-region app)
  (execute-command! app 'tabify-region))

(def *re-builder-indicator* 30)  ;; Scintilla indicator for regex matches

(def (cmd-re-builder app)
  "Interactive regex builder — highlights matches in current buffer."
  (let ((pattern (qt-echo-read-string app "Regex: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((fr (app-state-frame app))
             (ed (qt-edit-window-editor (qt-current-window fr)))
             (text (qt-plain-text-edit-text ed))
             (tlen (string-length text)))
        ;; Setup indicator for regex matches
        (sci-send ed SCI_INDICSETSTYLE *re-builder-indicator* 7)  ; INDIC_ROUNDBOX
        (sci-send ed SCI_INDICSETFORE *re-builder-indicator* #x00FF00)  ; green
        (sci-send ed 2523 *re-builder-indicator* 80)  ; SCI_INDICSETALPHA
        (sci-send ed SCI_SETINDICATORCURRENT *re-builder-indicator*)
        (sci-send ed SCI_INDICATORCLEARRANGE 0 tlen)
        ;; Search for regex matches using Scintilla
        (sci-send ed SCI_SETSEARCHFLAGS 2)  ; SCFIND_REGEXP
        (let loop ((pos 0) (count 0))
          (sci-send ed SCI_SETTARGETSTART pos)
          (sci-send ed SCI_SETTARGETEND tlen)
          (let ((found (sci-send/string ed SCI_SEARCHINTARGET pattern)))
            (if (< found 0)
              (echo-message! (app-state-echo app)
                (string-append "Regex: " (number->string count) " matches for /" pattern "/"))
              (let ((mstart (sci-send ed SCI_GETTARGETSTART))
                    (mend (sci-send ed SCI_GETTARGETEND)))
                (if (or (<= mend pos) (= mstart mend))
                  (echo-message! (app-state-echo app)
                    (string-append "Regex: " (number->string count) " matches for /" pattern "/"))
                  (begin
                    ;; Highlight this match
                    (sci-send ed SCI_SETINDICATORCURRENT *re-builder-indicator*)
                    (sci-send ed SCI_INDICATORFILLRANGE mstart (- mend mstart))
                    (loop mend (+ count 1))))))))))))

(def (cmd-regex-builder app)
  (cmd-re-builder app))

(def (cmd-find-file-with-warnings app)
  (execute-command! app 'find-file))

(def (cmd-quick-run app)
  (execute-command! app 'compile))

(def (cmd-flyspell-auto-correct-word app)
  (execute-command! app 'ispell-word))
(def (cmd-flyspell-goto-next-error app)
  (execute-command! app 'next-error))

(def (cmd-helpful-callable app)
  (execute-command! app 'describe-function))
(def (cmd-helpful-key app)
  (execute-command! app 'describe-key))
(def (cmd-helpful-variable app)
  (execute-command! app 'describe-variable))

(def (cmd-tags-search app)
  (execute-command! app 'find-tag))
(def (cmd-tags-query-replace app)
  (execute-command! app 'query-replace))

(def (cmd-tramp-cleanup-connections app)
  (echo-message! (app-state-echo app) "No TRAMP connections to clean"))
(def (cmd-tramp-cleanup-all-connections app)
  (echo-message! (app-state-echo app) "No TRAMP connections to clean"))
(def (cmd-tramp-version app)
  (echo-message! (app-state-echo app) "TRAMP: SSH-based remote access"))

(def (cmd-apply-macro-to-region-lines app)
  "Apply last keyboard macro to each line in region."
  (execute-command! app 'apply-macro-to-region))

(def (cmd-edit-kbd-macro app)
  "Show the last keyboard macro definition."
  (execute-command! app 'insert-kbd-macro))
(def (cmd-execute-named-macro app)
  (execute-command! app 'call-last-kbd-macro))

(def (cmd-kmacro-add-counter app)
  "Add 1 to the keyboard macro counter."
  (execute-command! app 'kbd-macro-counter-set))
(def (cmd-kmacro-insert-counter app)
  "Insert the keyboard macro counter and increment."
  (execute-command! app 'kbd-macro-counter-insert))
(def (cmd-kmacro-set-counter app)
  "Set the keyboard macro counter."
  (execute-command! app 'kbd-macro-counter-set))
(def (cmd-kmacro-set-format app)
  "Set keyboard macro counter format (currently integer only)."
  (echo-message! (app-state-echo app) "Macro counter format: integer"))

(def (cmd-insert-mode-line app)
  "Insert an Emacs-style mode line comment at the top of the buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mode (or (buffer-lexer-lang buf) "text"))
         (mode-str (if (symbol? mode) (symbol->string mode) mode))
         (line (string-append ";; -*- mode: " mode-str " -*-\n")))
    (sci-send ed SCI_GOTOPOS 0)
    (sci-send/string ed SCI_REPLACESEL 0 line)
    (echo-message! (app-state-echo app)
      (string-append "Inserted mode line for " mode-str))))
(def (cmd-insert-random-line app)
  (let* ((ed (current-qt-editor app))
         (lines (sci-send ed SCI_GETLINECOUNT))
         (target (random-integer lines))
         (start (sci-send ed SCI_POSITIONFROMLINE target))
         (end (sci-send ed SCI_GETLINEENDPOSITION target))
         (text (sci-get-text-range ed start end)))
    (sci-send/string ed SCI_REPLACESEL 0 text)
    (echo-message! (app-state-echo app)
      (string-append "Inserted line " (number->string (+ target 1))))))
(def (cmd-insert-register-content app)
  (execute-command! app 'insert-register))
(def (cmd-insert-scratch-message app)
  (sci-send/string (current-qt-editor app) SCI_REPLACESEL 0
    ";; This buffer is for text that is not saved.\n;; Use C-x C-f to visit a file.\n")
  (echo-message! (app-state-echo app) "Scratch message inserted"))

(def (cmd-markdown-insert-header app)
  (sci-send/string (current-qt-editor app) SCI_REPLACESEL 0 "# ")
  (echo-message! (app-state-echo app) "Header inserted"))

(def (cmd-selection-info app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection")
      (echo-message! (app-state-echo app)
        (string-append "Selection: " (number->string (- end start)) " chars")))))

(def (cmd-rename-symbol app)
  (execute-command! app 'query-replace))

(def (cmd-reopen-killed-buffer app)
  (echo-message! (app-state-echo app) "Use M-x recentf for recently closed files"))

(def (cmd-save-persistent-scratch app)
  (execute-command! app 'scratch-save))
(def (cmd-load-persistent-scratch app)
  (execute-command! app 'scratch-restore))

(def (cmd-complete-word-from-buffer app)
  (execute-command! app 'hippie-expand))

(def (cmd-add-dir-local-variable app)
  (echo-message! (app-state-echo app) "Edit .gemacs-config for dir-locals"))
(def (cmd-add-file-local-variable app)
  (echo-message! (app-state-echo app) "Add file-local variables in file header"))

;; org-set-tags already in commands-parity.ss

(def (cmd-org-roam-node-find app)
  "Find org-roam node — searches note files in ~/notes/."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (echo (app-state-echo app))
         (query (qt-echo-read-string app "Find node: ")))
    (when (and query (not (string-empty? query)))
      (let ((notes-dir (string-append (getenv "HOME") "/notes/")))
        (with-exception-catcher
          (lambda (e) (echo-error! echo "Notes directory not found — create ~/notes/"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "grep"
                                 arguments: (list "-rl" query notes-dir)
                                 stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                   (out (read-line proc #f)))
              (process-status proc)
              (if (and out (> (string-length out) 0))
                (let ((buf (qt-buffer-create! "*Org-roam*" ed #f)))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer win) buf)
                  (qt-plain-text-edit-set-text! ed out)
                  (sci-send ed SCI_SETREADONLY 1))
                (echo-message! echo "No matching nodes found")))))))))

(def (cmd-org-roam-node-insert app)
  "Insert org-roam node link at point."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (echo (app-state-echo app))
         (target (qt-echo-read-string app "Insert node link: ")))
    (when (and target (not (string-empty? target)))
      (let ((pos (sci-send ed SCI_GETCURRENTPOS)))
        (sci-send/string ed SCI_INSERTTEXT pos (string-append "[[roam:" target "]]"))))))

;; Perspective / workspace management
(def *qt-perspectives* (make-hash-table))  ; name -> list of buffer names
(def *qt-current-perspective* "default")

(def (cmd-persp-switch app)
  "Switch perspective/workspace."
  (let* ((echo (app-state-echo app))
         (name (qt-echo-read-string app "Switch to perspective: ")))
    (when (and name (not (string-empty? name)))
      ;; Save current perspective's buffer list
      (hash-put! *qt-perspectives* *qt-current-perspective*
        (map buffer-name (buffer-list)))
      (set! *qt-current-perspective* name)
      (echo-message! echo (string-append "Perspective: " name)))))

(def (cmd-persp-add-buffer app)
  "Add current buffer to current perspective."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (qt-edit-window-buffer win))
         (echo (app-state-echo app)))
    (when buf
      (let ((existing (or (hash-get *qt-perspectives* *qt-current-perspective*) '())))
        (hash-put! *qt-perspectives* *qt-current-perspective*
          (cons (buffer-name buf) existing))
        (echo-message! echo
          (string-append "Added " (buffer-name buf) " to " *qt-current-perspective*))))))

(def (cmd-persp-remove-buffer app)
  "Remove current buffer from current perspective."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (qt-edit-window-buffer win))
         (echo (app-state-echo app)))
    (when buf
      (let* ((existing (or (hash-get *qt-perspectives* *qt-current-perspective*) '()))
             (name (buffer-name buf)))
        (hash-put! *qt-perspectives* *qt-current-perspective*
          (filter (lambda (n) (not (string=? n name))) existing))
        (echo-message! echo
          (string-append "Removed " name " from " *qt-current-perspective*))))))

;; EMMS (Emacs Multimedia System) - uses mpv, mplayer, or ffplay
(def *qt-emms-player-process* #f)
(def *qt-emms-current-file* #f)
(def *qt-emms-paused* #f)
(def *qt-emms-playlist* [])
(def *qt-emms-playlist-idx* 0)

(def (qt-emms-find-player)
  "Find available media player."
  (cond
    ((file-exists? "/usr/bin/mpv") "mpv")
    ((file-exists? "/usr/bin/mplayer") "mplayer")
    ((file-exists? "/usr/bin/ffplay") "ffplay")
    (else #f)))

(def (qt-emms-play-track! app file)
  "Play a specific track file, updating state."
  (let ((echo (app-state-echo app))
        (player (qt-emms-find-player)))
    (if (not player)
      (echo-error! echo "No media player found (mpv, mplayer, or ffplay)")
      (begin
        (when *qt-emms-player-process*
          (with-exception-catcher (lambda (e) #f)
            (lambda () (close-port *qt-emms-player-process*))))
        (set! *qt-emms-player-process*
          (open-process
            (list path: player
                  arguments: (list "--quiet" file)
                  stdin-redirection: #f
                  stdout-redirection: #f
                  stderr-redirection: #f)))
        (set! *qt-emms-current-file* file)
        (set! *qt-emms-paused* #f)
        (echo-message! echo (string-append "Playing: " (path-strip-directory file)))))))

(def (cmd-emms app)
  "Open EMMS player — show current track info."
  (let ((echo (app-state-echo app)))
    (if *qt-emms-current-file*
      (echo-message! echo (string-append "Now playing: " (path-strip-directory *qt-emms-current-file*)
                                        (if *qt-emms-paused* " [PAUSED]" "")))
      (echo-message! echo "No track playing. Use emms-play-file to start."))))

(def (cmd-emms-play-file app)
  "Play a media file using mpv or mplayer."
  (let* ((echo (app-state-echo app))
         (player (qt-emms-find-player))
         (file (qt-echo-read-string app "Media file: ")))
    (if (not player)
      (echo-error! echo "No media player found (mpv, mplayer, or ffplay)")
      (when (and file (not (string-empty? file)))
        (if (not (file-exists? file))
          (echo-error! echo "File not found")
          (begin
            (when *qt-emms-player-process*
              (with-exception-catcher (lambda (e) #f)
                (lambda () (close-port *qt-emms-player-process*))))
            (set! *qt-emms-player-process*
              (open-process
                (list path: player
                      arguments: (list "--quiet" file)
                      stdin-redirection: #f
                      stdout-redirection: #f
                      stderr-redirection: #f)))
            (set! *qt-emms-current-file* file)
            (set! *qt-emms-paused* #f)
            (unless (member file *qt-emms-playlist*)
              (set! *qt-emms-playlist* (append *qt-emms-playlist* (list file))))
            (let loop ((i 0) (pl *qt-emms-playlist*))
              (when (pair? pl)
                (if (equal? (car pl) file)
                  (set! *qt-emms-playlist-idx* i)
                  (loop (+ i 1) (cdr pl)))))
            (echo-message! echo (string-append "Playing: " (path-strip-directory file)))))))))

(def (cmd-emms-next app)
  "Play the next track in the playlist."
  (let ((echo (app-state-echo app)))
    (if (null? *qt-emms-playlist*)
      (echo-message! echo "Playlist is empty. Use emms-play-file to add tracks.")
      (begin
        (set! *qt-emms-playlist-idx*
          (modulo (+ *qt-emms-playlist-idx* 1) (length *qt-emms-playlist*)))
        (qt-emms-play-track! app (list-ref *qt-emms-playlist* *qt-emms-playlist-idx*))))))

(def (cmd-emms-previous app)
  "Play the previous track in the playlist."
  (let ((echo (app-state-echo app)))
    (if (null? *qt-emms-playlist*)
      (echo-message! echo "Playlist is empty. Use emms-play-file to add tracks.")
      (begin
        (set! *qt-emms-playlist-idx*
          (modulo (- *qt-emms-playlist-idx* 1) (length *qt-emms-playlist*)))
        (qt-emms-play-track! app (list-ref *qt-emms-playlist* *qt-emms-playlist-idx*))))))

(def (cmd-emms-pause app)
  "Pause/resume playback."
  (let ((echo (app-state-echo app)))
    (if (not *qt-emms-player-process*)
      (echo-message! echo "No track playing")
      (begin
        (set! *qt-emms-paused* (not *qt-emms-paused*))
        (echo-message! echo (if *qt-emms-paused* "Paused" "Resumed"))))))

(def (cmd-emms-stop app)
  "Stop playback."
  (let ((echo (app-state-echo app)))
    (when *qt-emms-player-process*
      (with-exception-catcher (lambda (e) #f)
        (lambda () (close-port *qt-emms-player-process*)))
      (set! *qt-emms-player-process* #f)
      (set! *qt-emms-current-file* #f)
      (set! *qt-emms-paused* #f))
    (echo-message! echo "Stopped")))

(def (cmd-eat app)
  (execute-command! app 'terminal))

(def (cmd-vundo app)
  (execute-command! app 'undo-tree-visualize))

(def (cmd-undo-fu-only-undo app)
  (execute-command! app 'undo))
(def (cmd-undo-fu-only-redo app)
  (execute-command! app 'redo))

(def (cmd-unexpand-abbrev app)
  (echo-message! (app-state-echo app) "Use C-/ to undo last expansion"))

(def (cmd-sp-forward-slurp-sexp app)
  (execute-command! app 'paredit-forward-slurp-sexp))
(def (cmd-sp-backward-slurp-sexp app)
  (execute-command! app 'paredit-backward-slurp-sexp))
(def (cmd-sp-forward-barf-sexp app)
  (execute-command! app 'paredit-forward-barf-sexp))
(def (cmd-sp-backward-barf-sexp app)
  (execute-command! app 'paredit-backward-barf-sexp))


;;;============================================================================
;;; Registration of all parity4 commands
;;;============================================================================

(def (qt-register-parity4-commands!)
  "Register all additional parity commands."
  ;; Mode toggles
  (qt-register-parity3-mode-toggles!)
  ;; Stubs
  (qt-register-parity3-stubs!)
  ;; Aliases
  (qt-register-parity3-aliases!)
  ;; Functional commands
  (for-each
    (lambda (pair)
      (register-command! (car pair) (cdr pair)))
    (list
      (cons 'tetris cmd-tetris)
      (cons 'snake cmd-snake)
      (cons 'hanoi cmd-hanoi)
      (cons 'life cmd-life)
      (cons 'dunnet cmd-dunnet)
      (cons 'doctor cmd-doctor)
      (cons 'proced cmd-proced)
      (cons 'proced-filter cmd-proced-filter)
      (cons 'proced-send-signal cmd-proced-send-signal)
      (cons 'calculator cmd-calculator)
      (cons 'calculator-inline cmd-calculator-inline)
      (cons 'calc-eval-region cmd-calc-eval-region)
      (cons 'calc-push cmd-calc-push)
      (cons 'calc-pop cmd-calc-pop)
      (cons 'calc-dup cmd-calc-dup)
      (cons 'calc-swap cmd-calc-swap)
      (cons 'server-start cmd-server-start)
      (cons 'server-edit cmd-server-edit)
      (cons 'server-force-delete cmd-server-force-delete)
      (cons 'eww-forward cmd-eww-forward)
      (cons 'eww-download cmd-eww-download)
      (cons 'eww-copy-page-url cmd-eww-copy-page-url)
      (cons 'eww-search-web cmd-eww-search-web)
      (cons 'gdb cmd-gdb)
      (cons 'gud-break cmd-gud-break)
      (cons 'gud-cont cmd-gud-cont)
      (cons 'gud-next cmd-gud-next)
      (cons 'gud-step cmd-gud-step)
      (cons 'gud-remove cmd-gud-remove)
      (cons 'mc-add-next cmd-mc-add-next)
      (cons 'mc-add-all cmd-mc-add-all)
      (cons 'mc-mark-next-like-this cmd-mc-mark-next-like-this)
      (cons 'mc-mark-previous-like-this cmd-mc-mark-previous-like-this)
      (cons 'mc-mark-all-like-this cmd-mc-mark-all-like-this)
      (cons 'mc-skip-and-add-next cmd-mc-skip-and-add-next)
      (cons 'mc-cursors-on-lines cmd-mc-cursors-on-lines)
      (cons 'scheme-send-buffer cmd-scheme-send-buffer)
      (cons 'scheme-send-region cmd-scheme-send-region)
      (cons 'inferior-lisp cmd-inferior-lisp)
      (cons 'duplicate-and-comment cmd-duplicate-and-comment)
      (cons 'smart-backspace cmd-smart-backspace)
      (cons 'smart-open-line-above cmd-smart-open-line-above)
      (cons 'smart-open-line-below cmd-smart-open-line-below)
      (cons 'fold-this cmd-fold-this)
      (cons 'fold-this-all cmd-fold-this-all)
      (cons 'fold-toggle-at-point cmd-fold-toggle-at-point)
      (cons 'wrap-region-with cmd-wrap-region-with)
      (cons 'unwrap-region cmd-unwrap-region)
      (cons 'vc-dir cmd-vc-dir)
      (cons 'vc-print-log cmd-vc-print-log)
      (cons 'vc-register cmd-vc-register)
      (cons 'vc-stash cmd-vc-stash)
      (cons 'vc-stash-pop cmd-vc-stash-pop)
      (cons 'treemacs-find-file cmd-treemacs-find-file)
      (cons 'project-tree-toggle-node cmd-project-tree-toggle-node)
      (cons 'rotate-frame cmd-rotate-frame)
      (cons 'window-save-layout cmd-window-save-layout)
      (cons 'window-restore-layout cmd-window-restore-layout)
      (cons 'uptime cmd-uptime)
      (cons 'world-clock cmd-world-clock)
      (cons 'memory-usage cmd-memory-usage)
      (cons 'generate-password cmd-generate-password)
      (cons 'epoch-to-date cmd-epoch-to-date)
      (cons 'detect-encoding cmd-detect-encoding)
      (cons 'open-containing-folder cmd-open-containing-folder)
      (cons 'display-prefix cmd-display-prefix)
      (cons 'display-prefix-help cmd-display-prefix-help)
      (cons 'push-mark-command cmd-push-mark-command)
      (cons 'exchange-dot-and-mark cmd-exchange-dot-and-mark)
      (cons 'move-to-window-center cmd-move-to-window-center)
      (cons 'set-goal-column cmd-set-goal-column)
      (cons 'isearch-occur cmd-isearch-occur)
      (cons 'isearch-toggle-case-fold cmd-isearch-toggle-case-fold)
      (cons 'isearch-toggle-regexp cmd-isearch-toggle-regexp)
      (cons 'copy-as-formatted cmd-copy-as-formatted)
      (cons 'copy-rectangle-to-clipboard cmd-copy-rectangle-to-clipboard)
      (cons 'canonically-space-region cmd-canonically-space-region)
      (cons 'format-region cmd-format-region)
      (cons 'csv-align-columns cmd-csv-align-columns)
      (cons 'json-sort-keys cmd-json-sort-keys)
      (cons 'jq-filter cmd-jq-filter)
      (cons 'html-encode-region cmd-html-encode-region)
      (cons 'html-decode-region cmd-html-decode-region)
      (cons 'encode-hex-string cmd-encode-hex-string)
      (cons 'decode-hex-string cmd-decode-hex-string)
      (cons 'increment-hex-at-point cmd-increment-hex-at-point)
      (cons 'titlecase-region cmd-titlecase-region)
      (cons 'reverse-region-chars cmd-reverse-region-chars)
      (cons 'reverse-words-in-region cmd-reverse-words-in-region)
      (cons 'sort-words-in-line cmd-sort-words-in-line)
      (cons 'sort-paragraphs cmd-sort-paragraphs)
      (cons 'goto-random-line cmd-goto-random-line)
      (cons 'open-line-below cmd-open-line-below)
      (cons 'open-recent-dir cmd-open-recent-dir)
      (cons 'scratch-with-mode cmd-scratch-with-mode)
      (cons 'what-tab-width cmd-what-tab-width)
      (cons 'cd cmd-cd)
      (cons 'eshell-here cmd-eshell-here)
      (cons 'suspend-emacs cmd-suspend-emacs)
      (cons 'mode-line-other-buffer cmd-mode-line-other-buffer)
      (cons 'minibuffer-complete cmd-minibuffer-complete)
      (cons 'minibuffer-keyboard-quit cmd-minibuffer-keyboard-quit)
      (cons 'display-fill-column cmd-display-fill-column)
      (cons 'gerbil-mode cmd-gerbil-mode)
      (cons 'set-buffer-mode cmd-set-buffer-mode)
      (cons 'set-face-attribute cmd-set-face-attribute)
      (cons 'symbol-overlay-put cmd-symbol-overlay-put)
      (cons 'symbol-overlay-remove-all cmd-symbol-overlay-remove-all)
      (cons 'unhighlight-regexp cmd-unhighlight-regexp)
      (cons 'untabify-region cmd-untabify-region)
      (cons 're-builder cmd-re-builder)
      (cons 'regex-builder cmd-regex-builder)
      (cons 'find-file-with-warnings cmd-find-file-with-warnings)
      (cons 'quick-run cmd-quick-run)
      (cons 'flyspell-auto-correct-word cmd-flyspell-auto-correct-word)
      (cons 'flyspell-goto-next-error cmd-flyspell-goto-next-error)
      (cons 'helpful-callable cmd-helpful-callable)
      (cons 'helpful-key cmd-helpful-key)
      (cons 'helpful-variable cmd-helpful-variable)
      (cons 'tags-search cmd-tags-search)
      (cons 'tags-query-replace cmd-tags-query-replace)
      (cons 'tramp-cleanup-connections cmd-tramp-cleanup-connections)
      (cons 'tramp-cleanup-all-connections cmd-tramp-cleanup-all-connections)
      (cons 'tramp-version cmd-tramp-version)
      (cons 'apply-macro-to-region-lines cmd-apply-macro-to-region-lines)
      (cons 'edit-kbd-macro cmd-edit-kbd-macro)
      (cons 'execute-named-macro cmd-execute-named-macro)
      (cons 'kmacro-add-counter cmd-kmacro-add-counter)
      (cons 'kmacro-insert-counter cmd-kmacro-insert-counter)
      (cons 'kmacro-set-counter cmd-kmacro-set-counter)
      (cons 'kmacro-set-format cmd-kmacro-set-format)
      (cons 'insert-mode-line cmd-insert-mode-line)
      (cons 'insert-random-line cmd-insert-random-line)
      (cons 'insert-register-content cmd-insert-register-content)
      (cons 'insert-scratch-message cmd-insert-scratch-message)
      (cons 'markdown-insert-header cmd-markdown-insert-header)
      (cons 'selection-info cmd-selection-info)
      (cons 'rename-symbol cmd-rename-symbol)
      (cons 'reopen-killed-buffer cmd-reopen-killed-buffer)
      (cons 'save-persistent-scratch cmd-save-persistent-scratch)
      (cons 'load-persistent-scratch cmd-load-persistent-scratch)
      (cons 'complete-word-from-buffer cmd-complete-word-from-buffer)
      (cons 'add-dir-local-variable cmd-add-dir-local-variable)
      (cons 'add-file-local-variable cmd-add-file-local-variable)
      (cons 'org-roam-node-find cmd-org-roam-node-find)
      (cons 'org-roam-node-insert cmd-org-roam-node-insert)
      (cons 'org-roam-buffer-toggle cmd-org-roam-node-find)
      (cons 'persp-switch cmd-persp-switch)
      (cons 'persp-add-buffer cmd-persp-add-buffer)
      (cons 'persp-remove-buffer cmd-persp-remove-buffer)
      (cons 'emms cmd-emms)
      (cons 'emms-play-file cmd-emms-play-file)
      (cons 'emms-next cmd-emms-next)
      (cons 'emms-previous cmd-emms-previous)
      (cons 'emms-pause cmd-emms-pause)
      (cons 'emms-stop cmd-emms-stop)
      (cons 'eat cmd-eat)
      (cons 'vundo cmd-vundo)
      (cons 'undo-fu-only-undo cmd-undo-fu-only-undo)
      (cons 'undo-fu-only-redo cmd-undo-fu-only-redo)
      (cons 'unexpand-abbrev cmd-unexpand-abbrev)
      (cons 'sp-forward-slurp-sexp cmd-sp-forward-slurp-sexp)
      (cons 'sp-backward-slurp-sexp cmd-sp-backward-slurp-sexp)
      (cons 'sp-forward-barf-sexp cmd-sp-forward-barf-sexp)
      (cons 'sp-backward-barf-sexp cmd-sp-backward-barf-sexp)
      )))
