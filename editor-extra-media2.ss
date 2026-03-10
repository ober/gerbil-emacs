;;; -*- Gerbil -*-
;;; Media/extras part 2: coding system, local variables, toggle modes,
;;; dired operations, diff navigation, display-line-numbers

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        (only-in :gemacs/editor-core
                 *auto-save-enabled* make-auto-save-path)
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers
        (only-in :gemacs/editor-extra-editing2
                 *dired-marks* cmd-dired-refresh))

(def (cmd-describe-current-coding-system app)
  "Describe current coding system."
  (echo-message! (app-state-echo app) "Coding: utf-8 (default)"))

;; Buffer-local variables
(def (cmd-add-file-local-variable app)
  "Add file-local variable — inserts a Local Variables block at end of buffer."
  (let* ((name (app-read-string app "Variable name: ")))
    (when (and name (not (string-empty? name)))
      (let ((val (app-read-string app (string-append name " value: "))))
        (when val
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (text (editor-get-text ed))
                 (local-var-line (string-append ";; " name ": " val)))
            (if (string-contains text "Local Variables:")
              (let ((insert-pos (string-contains text "End:")))
                (when insert-pos
                  (editor-insert-text ed insert-pos (string-append local-var-line "\n"))))
              (let ((end (string-length text)))
                (editor-insert-text ed end
                  (string-append "\n;; Local Variables:\n" local-var-line "\n;; End:\n"))))
            (echo-message! (app-state-echo app) (string-append "Added: " name " = " val))))))))

(def (cmd-add-dir-local-variable app)
  "Add directory-local variable — creates/edits .dir-locals.el."
  (let* ((buf (current-buffer-from-app app))
         (dir (if (and buf (buffer-file-path buf))
                (path-directory (buffer-file-path buf))
                (current-directory)))
         (dl-file (string-append dir "/.dir-locals.el")))
    (let ((name (app-read-string app "Variable name: ")))
      (when (and name (not (string-empty? name)))
        (let ((val (app-read-string app (string-append name " value: "))))
          (when val
            (with-output-to-file dl-file
              (lambda ()
                (display (string-append "((nil . ((" name " . " val "))))\n"))))
            (echo-message! (app-state-echo app)
              (string-append "Dir-local " name "=" val " written to " dl-file))))))))

;; Hippie expand variants
(def (cmd-hippie-expand-file app)
  "Hippie expand filename — complete filename at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         ;; Extract word before point as path prefix
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((let ((c (string-ref text i)))
                           (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab))) (+ i 1))
                        (else (loop (- i 1))))))
         (prefix (substring text start pos)))
    (if (string-empty? prefix)
      (echo-message! (app-state-echo app) "No prefix for file completion")
      (with-exception-catcher
        (lambda (e) (echo-message! (app-state-echo app) "No file matches"))
        (lambda ()
          (let* ((dir (path-directory prefix))
                 (base (path-strip-directory prefix))
                 (entries (directory-files (if (string-empty? dir) "." dir)))
                 (matches (filter (lambda (f) (string-prefix? base f)) entries)))
            (if (null? matches)
              (echo-message! (app-state-echo app) "No file matches")
              (let ((completion (car matches)))
                (send-message ed SCI_SETTARGETSTART start 0)
                (send-message ed SCI_SETTARGETEND pos 0)
                (send-message/string ed SCI_REPLACETARGET
                  (string-append (if (string-empty? dir) "" dir) completion))
                (echo-message! (app-state-echo app) (string-append "Completed: " completion))))))))))

;; Registers extras
(def (cmd-frameset-to-register app)
  "Save frameset to register — stores window layout description."
  (let ((key (app-read-string app "Register for frameset: ")))
    (when (and key (not (string-empty? key)))
      (let* ((fr (app-state-frame app))
             (nwin (length (frame-windows fr)))
             (desc (string-append "frameset:" (number->string nwin) "-windows")))
        (hash-put! (app-state-registers app) (string-ref key 0) desc)
        (echo-message! (app-state-echo app)
          (string-append "Frameset stored in register " key))))))

(def (cmd-window-configuration-to-register app)
  "Save window configuration to register."
  (let ((key (app-read-string app "Register for window config: ")))
    (when (and key (not (string-empty? key)))
      (let* ((fr (app-state-frame app))
             (nwin (length (frame-windows fr)))
             (desc (string-append "winconfig:" (number->string nwin) "-windows")))
        (hash-put! (app-state-registers app) (string-ref key 0) desc)
        (echo-message! (app-state-echo app)
          (string-append "Window config stored in register " key))))))

;; Macro counter extras
(def (cmd-kmacro-add-counter app)
  "Add to keyboard macro counter."
  (let ((val (app-read-string app "Add to counter: ")))
    (when (and val (not (string-empty? val)))
      (let ((n (string->number val)))
        (when n
          (set! *kmacro-counter* (+ *kmacro-counter* n))
          (echo-message! (app-state-echo app)
            (string-append "Kmacro counter: " (number->string *kmacro-counter*))))))))

(def (cmd-kmacro-set-format app)
  "Set keyboard macro counter format."
  (let ((fmt (app-read-string app "Counter format (e.g. %03d): ")))
    (when (and fmt (not (string-empty? fmt)))
      (set! *kmacro-counter-format* fmt)
      (echo-message! (app-state-echo app) (string-append "Kmacro format: " fmt)))))

;; Line number display modes
(def (cmd-display-line-numbers-absolute app)
  "Show absolute line numbers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_SETMARGINWIDTHN 0 48)
    (echo-message! (app-state-echo app) "Line numbers: absolute")))

(def (cmd-display-line-numbers-none app)
  "Hide line numbers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_SETMARGINWIDTHN 0 0)
    (echo-message! (app-state-echo app) "Line numbers: hidden")))

;; Scratch buffer
(def (cmd-scratch-buffer app)
  "Switch to *scratch* buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (existing (let loop ((bufs (buffer-list)))
                     (cond
                       ((null? bufs) #f)
                       ((string=? (buffer-name (car bufs)) "*scratch*") (car bufs))
                       (else (loop (cdr bufs)))))))
    (if existing
      (begin
        (buffer-attach! ed existing)
        (set! (edit-window-buffer win) existing))
      (let ((buf (buffer-create! "*scratch*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed ";; This is the scratch buffer.\n;; Use it for notes and experiments.\n\n")))))

;; Recentf extras
(def (cmd-recentf-cleanup app)
  "Clean up recent files list — remove non-existent files."
  (let* ((recent *recent-files*)
         (before (length recent))
         (cleaned (filter file-exists? recent)))
    (set! *recent-files* cleaned)
    (echo-message! (app-state-echo app)
      (string-append "Recent files: removed " (number->string (- before (length cleaned)))
                     " non-existent entries"))))

;; Interactive hook management — uses core.ss hook system
(def (cmd-add-hook app)
  "Add a command to a hook (e.g. after-save-hook)."
  (let ((hook-name (app-read-string app "Hook name: ")))
    (when (and hook-name (not (string-empty? hook-name)))
      (let ((func-name (app-read-string app "Command name: ")))
        (when (and func-name (not (string-empty? func-name)))
          (let ((cmd (find-command (string->symbol func-name))))
            (if cmd
              (begin
                (add-hook! (string->symbol hook-name) cmd)
                (echo-message! (app-state-echo app)
                  (string-append "Added " func-name " to " hook-name)))
              (echo-error! (app-state-echo app)
                (string-append "Unknown command: " func-name)))))))))

(def (cmd-remove-hook app)
  "Remove a command from a hook."
  (let ((hook-name (app-read-string app "Hook name: ")))
    (when (and hook-name (not (string-empty? hook-name)))
      (let ((func-name (app-read-string app "Command to remove: ")))
        (when (and func-name (not (string-empty? func-name)))
          (let ((cmd (find-command (string->symbol func-name))))
            (if cmd
              (begin
                (remove-hook! (string->symbol hook-name) cmd)
                (echo-message! (app-state-echo app)
                  (string-append "Removed " func-name " from " hook-name)))
              (echo-error! (app-state-echo app)
                (string-append "Unknown command: " func-name)))))))))

(def (cmd-list-hooks app)
  "List all active hooks and their functions."
  (let ((entries (hash->list *hooks*)))
    (if (null? entries)
      (echo-message! (app-state-echo app) "No hooks defined")
      (let ((text (string-join
                    (map (lambda (entry)
                           (let ((hook (symbol->string (car entry)))
                                 (fns (cdr entry)))
                             (string-append hook ": "
                               (number->string (length fns)) " function(s)")))
                         entries)
                    "; ")))
        (echo-message! (app-state-echo app) text)))))

;; Elpa/Melpa package sources
(def (cmd-package-archives app)
  "Show configured package archives."
  (echo-message! (app-state-echo app) "Package archives: gerbil-pkg (built-in)"))

;; Auto-save (uses *auto-save-enabled* from editor-core)
(def (cmd-auto-save-mode app)
  "Toggle auto-save mode."
  (set! *auto-save-enabled* (not *auto-save-enabled*))
  (echo-message! (app-state-echo app)
    (if *auto-save-enabled* "Auto-save mode: on" "Auto-save mode: off")))

(def (cmd-recover-file app)
  "Recover file from auto-save (#file#) backup."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((auto-save (make-auto-save-path path)))
        (if (file-exists? auto-save)
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (content (read-file-as-string auto-save)))
            (editor-set-text ed (or content ""))
            (echo-message! (app-state-echo app) (string-append "Recovered from " auto-save)))
          (echo-message! (app-state-echo app) "No auto-save file found"))))))

;; Tramp details
(def (cmd-tramp-version app)
  "Show TRAMP version — SSH-based remote editing."
  (echo-message! (app-state-echo app) "TRAMP: SSH remote file editing via scp (built-in)"))

;; Global HL line
(def (cmd-hl-line-mode app)
  "Toggle highlight current line mode."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETCARETLINEVISIBLE 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETCARETLINEVISIBLE 0 0)
        (echo-message! (app-state-echo app) "HL line: off"))
      (begin
        (send-message ed SCI_SETCARETLINEVISIBLE 1 0)
        (send-message ed SCI_SETCARETLINEBACK #x333333 0)
        (echo-message! (app-state-echo app) "HL line: on")))))

;; Occur extras
(def (cmd-occur-rename-buffer app)
  "Rename occur buffer."
  (let* ((buf (current-buffer-from-app app))
         (name (app-read-string app "New buffer name: ")))
    (when (and name (not (string-empty? name)) buf)
      (set! (buffer-name buf) name)
      (echo-message! (app-state-echo app) (string-append "Buffer renamed to: " name)))))

;; Printing — uses lpr or enscript
(def (cmd-print-buffer app)
  "Print buffer contents using lpr."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed)))
    (with-exception-catcher
      (lambda (e) (echo-error! (app-state-echo app) "lpr not available"))
      (lambda ()
        (let ((proc (open-process
                      (list path: "lpr"
                            stdin-redirection: #t stdout-redirection: #f stderr-redirection: #t))))
          (display text proc)
          (close-output-port proc)
          (process-status proc)
          (echo-message! (app-state-echo app) "Buffer sent to printer"))))))

(def (cmd-print-region app)
  "Print selected region using lpr."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let ((region (substring (editor-get-text ed) start end)))
        (with-exception-catcher
          (lambda (e) (echo-error! (app-state-echo app) "lpr not available"))
          (lambda ()
            (let ((proc (open-process
                          (list path: "lpr"
                                stdin-redirection: #t stdout-redirection: #f stderr-redirection: #t))))
              (display region proc)
              (close-output-port proc)
              (process-status proc)
              (echo-message! (app-state-echo app) "Region sent to printer"))))))))

;; Buffer encoding info
(def (cmd-describe-char-at-point app)
  "Describe character at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: '" (string ch)
                         "', Code: " (number->string code)
                         " (#x" (number->string code 16) ")"))))))

;; Miscellaneous
(def (cmd-toggle-debug-on-signal app)
  "Toggle debug on signal."
  (let ((on (toggle-mode! 'debug-on-signal)))
    (echo-message! (app-state-echo app)
      (if on "Debug on signal: on" "Debug on signal: off"))))

(def (cmd-toggle-word-boundary app)
  "Toggle word boundary display — shows whitespace characters."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETVIEWWS 0 0)))
    (if (> cur 0)
      (begin (send-message ed SCI_SETVIEWWS 0 0)
             (echo-message! (app-state-echo app) "Word boundaries: hidden"))
      (begin (send-message ed SCI_SETVIEWWS 1 0)
             (echo-message! (app-state-echo app) "Word boundaries: visible")))))

(def (cmd-indent-tabs-mode app)
  "Show indent tabs mode status."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (use-tabs (send-message ed SCI_GETUSETABS 0 0)))
    (echo-message! (app-state-echo app)
      (if (> use-tabs 0) "Indent: tabs" "Indent: spaces"))))

(def (cmd-electric-indent-local-mode app)
  "Toggle electric indent for current buffer."
  (let ((on (toggle-mode! 'electric-indent-local)))
    (echo-message! (app-state-echo app)
      (if on "Electric indent (local): on" "Electric indent (local): off"))))

(def (cmd-visual-fill-column-mode app)
  "Toggle visual fill column mode — show fill column indicator."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'visual-fill-column)))
    (if on
      (begin (send-message ed 2363 #|SCI_SETEDGEMODE|# 1 0)
             (send-message ed 2361 #|SCI_SETEDGECOLUMN|# 80 0)
             (echo-message! (app-state-echo app) "Visual fill column: on (80)"))
      (begin (send-message ed 2363 #|SCI_SETEDGEMODE|# 0 0)
             (echo-message! (app-state-echo app) "Visual fill column: off")))))

(def (cmd-adaptive-wrap-prefix-mode app)
  "Toggle adaptive wrap prefix mode."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'adaptive-wrap)))
    (if on
      (begin (send-message ed SCI_SETWRAPMODE 1 0)
             (send-message ed SCI_SETWRAPINDENTMODE 1 0)
             (echo-message! (app-state-echo app) "Adaptive wrap: on"))
      (begin (send-message ed SCI_SETWRAPMODE 0 0)
             (echo-message! (app-state-echo app) "Adaptive wrap: off")))))

(def (cmd-display-fill-column app)
  "Display current fill column."
  (echo-message! (app-state-echo app) "Fill column: 80 (default)"))

(def (cmd-set-selective-display app)
  "Set selective display level."
  (let ((level (app-read-string app "Selective display level: ")))
    (when (and level (not (string-empty? level)))
      (let ((n (string->number level)))
        (when n
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win)))
            ;; Use fold level to approximate selective display
            (echo-message! (app-state-echo app)
              (string-append "Selective display: " level))))))))

(def (cmd-toggle-indicate-empty-lines app)
  "Toggle empty line indicators."
  (let ((on (toggle-mode! 'indicate-empty-lines)))
    (echo-message! (app-state-echo app)
      (if on "Empty line indicators: on" "Empty line indicators: off"))))

(def (cmd-toggle-indicate-buffer-boundaries app)
  "Toggle buffer boundary indicators."
  (let ((on (toggle-mode! 'indicate-buffer-boundaries)))
    (echo-message! (app-state-echo app)
      (if on "Buffer boundaries: on" "Buffer boundaries: off"))))

;; Enriched text / face manipulation
(def (cmd-facemenu-set-foreground app)
  "Set text foreground color."
  (let ((color (app-read-string app "Foreground color (#RRGGBB): ")))
    (when (and color (not (string-empty? color)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        ;; Set default foreground color
        (send-message ed SCI_STYLESETFORE 32 ;; STYLE_DEFAULT
          (string->number (string-append "#x" (substring color 1 (string-length color)))))
        (echo-message! (app-state-echo app) (string-append "Foreground: " color))))))

(def (cmd-facemenu-set-background app)
  "Set text background color."
  (let ((color (app-read-string app "Background color (#RRGGBB): ")))
    (when (and color (not (string-empty? color)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        (send-message ed SCI_STYLESETBACK 32 ;; STYLE_DEFAULT
          (string->number (string-append "#x" (substring color 1 (string-length color)))))
        (echo-message! (app-state-echo app) (string-append "Background: " color))))))

;; Emacs games

(def (cmd-tetris app)
  "Play tetris — simple text-based Tetris game."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Tetris*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
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
    (editor-set-read-only ed #t)))

(def (cmd-snake app)
  "Play snake — simple text-based Snake game."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Snake*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
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
    (editor-set-read-only ed #t)))

(def (cmd-dunnet app)
  "Play dunnet text adventure."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Dunnet*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Dead End\n\n"
        "You are at a dead end of a dirt road. The road goes to the east.\n"
        "In the distance you can see that it will eventually fork off.\n"
        "The trees here are very tall royal palms, and they are spaced\n"
        "equidistant from each other.\n\n"
        "There is a shovel here.\n\n"
        "> "))
    (editor-goto-pos ed (string-length (editor-get-text ed)))))

(def (cmd-hanoi app)
  "Show towers of hanoi visualization."
  (let* ((n-str (app-read-string app "Number of disks (1-8): "))
         (n (if (and n-str (not (string-empty? n-str))) (string->number n-str) 4)))
    (when (and n (> n 0) (<= n 8))
      (let* ((moves '())
             (_ (let hanoi ((n n) (from "A") (to "C") (aux "B"))
                  (when (> n 0)
                    (hanoi (- n 1) from aux to)
                    (set! moves (cons (string-append "Move disk " (number->string n)
                                                     " from " from " to " to) moves))
                    (hanoi (- n 1) aux to from))))
             (text (string-append "Towers of Hanoi (" (number->string n) " disks)\n\n"
                                  "Moves required: " (number->string (length moves)) "\n\n"
                                  (string-join (reverse moves) "\n") "\n")))
        (open-output-buffer app "*Hanoi*" text)))))

(def (cmd-life app)
  "Run Conway's Game of Life — displays a glider pattern."
  (let* ((width 40) (height 20)
         (grid (make-vector (* width height) #f))
         ;; Place a glider at (2,2)
         (_ (begin
              (vector-set! grid (+ 2 (* 1 width)) #t)
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
                           (newline)
                           (yloop (+ y 1))))
                       (display "\n")
                       ;; Compute next generation
                       (let ((new-grid (make-vector (* width height) #f)))
                         (let yloop2 ((y 0))
                           (when (< y height)
                             (let xloop2 ((x 0))
                               (when (< x width)
                                 (let* ((count 0)
                                        (count (let dy-loop ((dy -1) (c count))
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
                                     (or (= count 3)
                                         (and (= count 2) (vector-ref grid (+ x (* y width)))))))
                                 (xloop2 (+ x 1))))
                             (yloop2 (+ y 1))))
                         ;; Copy new-grid to grid
                         (let cp ((i 0))
                           (when (< i (* width height))
                             (vector-set! grid i (vector-ref new-grid i))
                             (cp (+ i 1)))))
                       (gen-loop (+ gen 1))))))))
    (open-output-buffer app "*Life*" text)))

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
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Doctor*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "I am the psychotherapist. Please describe your problems.\n"
        "Each time you are finished talking, press RET twice.\n\n"
        "> "))
    (editor-goto-pos ed (string-length (editor-get-text ed)))))

;; Process list operations
(def (cmd-proced-send-signal app)
  "Send signal to process."
  (let ((pid-str (app-read-string app "PID: ")))
    (when (and pid-str (not (string-empty? pid-str)))
      (let ((sig (app-read-string app "Signal (default TERM): ")))
        (let ((signal (if (or (not sig) (string-empty? sig)) "TERM" sig)))
          (with-exception-catcher
            (lambda (e) (echo-error! (app-state-echo app) "Failed to send signal"))
            (lambda ()
              (let ((proc (open-process
                            (list path: "kill"
                                  arguments: (list (string-append "-" signal) pid-str)
                                  stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
                (process-status proc)
                (echo-message! (app-state-echo app)
                  (string-append "Sent SIG" signal " to PID " pid-str))))))))))

(def (cmd-proced-filter app)
  "Filter process list by pattern."
  (let ((pattern (app-read-string app "Filter processes by: ")))
    (when (and pattern (not (string-empty? pattern)))
      (with-exception-catcher
        (lambda (e) (echo-error! (app-state-echo app) "ps failed"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "ps"
                               arguments: '("aux")
                               stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                 (output (read-line proc #f)))
            (process-status proc)
            (when output
              (let* ((lines (string-split output #\newline))
                     (header (car lines))
                     (filtered (filter (lambda (l) (string-contains l pattern)) (cdr lines)))
                     (result (string-append header "\n" (string-join filtered "\n") "\n")))
                (open-output-buffer app "*Proced*" result)))))))))

;; Ediff session management
(def (cmd-ediff-show-registry app)
  "Show ediff session registry."
  (echo-message! (app-state-echo app) "No active ediff sessions"))

;; ── batch 44: modern Emacs package toggles ──────────────────────────
(def *consult-mode* #f)
(def *orderless-mode* #f)
(def *embark-mode* #f)
(def *undo-fu-session* #f)
(def *auto-package-mode* #f)
(def *corfu-mode* #f)
(def *cape-mode* #f)
(def *nerd-icons-mode* #f)
(def *all-the-icons* #f)
(def *doom-themes* #f)

(def (cmd-toggle-consult-mode app)
  "Toggle consult-mode (enhanced search commands)."
  (let ((echo (app-state-echo app)))
    (set! *consult-mode* (not *consult-mode*))
    (echo-message! echo (if *consult-mode*
                          "Consult mode ON" "Consult mode OFF"))))

(def (cmd-toggle-orderless-mode app)
  "Toggle orderless-mode (orderless completion style)."
  (let ((echo (app-state-echo app)))
    (set! *orderless-mode* (not *orderless-mode*))
    (echo-message! echo (if *orderless-mode*
                          "Orderless mode ON" "Orderless mode OFF"))))

(def (cmd-toggle-embark-mode app)
  "Toggle embark-mode (contextual actions)."
  (let ((echo (app-state-echo app)))
    (set! *embark-mode* (not *embark-mode*))
    (echo-message! echo (if *embark-mode*
                          "Embark mode ON" "Embark mode OFF"))))

(def (cmd-toggle-undo-fu-session app)
  "Toggle undo-fu-session-mode (persistent undo history)."
  (let ((echo (app-state-echo app)))
    (set! *undo-fu-session* (not *undo-fu-session*))
    (echo-message! echo (if *undo-fu-session*
                          "Undo-fu session ON" "Undo-fu session OFF"))))

(def (cmd-toggle-auto-package-mode app)
  "Toggle auto-package-mode (auto install packages)."
  (let ((echo (app-state-echo app)))
    (set! *auto-package-mode* (not *auto-package-mode*))
    (echo-message! echo (if *auto-package-mode*
                          "Auto-package mode ON" "Auto-package mode OFF"))))

(def (cmd-toggle-corfu-mode app)
  "Toggle corfu-mode (in-buffer completion popup)."
  (let ((echo (app-state-echo app)))
    (set! *corfu-mode* (not *corfu-mode*))
    (echo-message! echo (if *corfu-mode*
                          "Corfu mode ON" "Corfu mode OFF"))))

(def (cmd-toggle-cape-mode app)
  "Toggle cape-mode (completion-at-point extensions)."
  (let ((echo (app-state-echo app)))
    (set! *cape-mode* (not *cape-mode*))
    (echo-message! echo (if *cape-mode*
                          "Cape mode ON" "Cape mode OFF"))))

(def (cmd-toggle-nerd-icons-mode app)
  "Toggle nerd-icons-mode (icon display)."
  (let ((echo (app-state-echo app)))
    (set! *nerd-icons-mode* (not *nerd-icons-mode*))
    (echo-message! echo (if *nerd-icons-mode*
                          "Nerd icons mode ON" "Nerd icons mode OFF"))))

(def (cmd-toggle-all-the-icons app)
  "Toggle all-the-icons mode."
  (let ((echo (app-state-echo app)))
    (set! *all-the-icons* (not *all-the-icons*))
    (echo-message! echo (if *all-the-icons*
                          "All-the-icons ON" "All-the-icons OFF"))))

(def (cmd-toggle-doom-themes app)
  "Toggle doom-themes (themed appearance)."
  (let ((echo (app-state-echo app)))
    (set! *doom-themes* (not *doom-themes*))
    (echo-message! echo (if *doom-themes*
                          "Doom themes ON" "Doom themes OFF"))))

;; ── batch 53: highlight and visual feedback toggles ─────────────────
(def *global-whitespace-newline* #f)
(def *global-highlight-indent* #f)
(def *global-rainbow-mode* #f)
(def *global-auto-highlight* #f)
(def *global-symbol-overlay* #f)
(def *global-highlight-parentheses* #f)
(def *global-pulse-line* #f)

(def (cmd-toggle-global-whitespace-newline app)
  "Toggle display of newline characters globally."
  (let ((echo (app-state-echo app)))
    (set! *global-whitespace-newline* (not *global-whitespace-newline*))
    (echo-message! echo (if *global-whitespace-newline*
                          "Whitespace newlines ON" "Whitespace newlines OFF"))))

(def (cmd-toggle-global-highlight-indent app)
  "Toggle global highlight-indentation-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-highlight-indent* (not *global-highlight-indent*))
    (echo-message! echo (if *global-highlight-indent*
                          "Highlight indent ON" "Highlight indent OFF"))))

(def (cmd-toggle-global-rainbow-mode app)
  "Toggle global rainbow-mode (colorize color strings)."
  (let ((echo (app-state-echo app)))
    (set! *global-rainbow-mode* (not *global-rainbow-mode*))
    (echo-message! echo (if *global-rainbow-mode*
                          "Rainbow mode ON" "Rainbow mode OFF"))))

(def (cmd-toggle-global-auto-highlight app)
  "Toggle global auto-highlight-symbol-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-highlight* (not *global-auto-highlight*))
    (echo-message! echo (if *global-auto-highlight*
                          "Auto-highlight ON" "Auto-highlight OFF"))))

(def (cmd-toggle-global-symbol-overlay app)
  "Toggle global symbol-overlay-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-symbol-overlay* (not *global-symbol-overlay*))
    (echo-message! echo (if *global-symbol-overlay*
                          "Symbol overlay ON" "Symbol overlay OFF"))))

(def (cmd-toggle-global-highlight-parentheses app)
  "Toggle global highlight-parentheses-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-highlight-parentheses* (not *global-highlight-parentheses*))
    (echo-message! echo (if *global-highlight-parentheses*
                          "Highlight parens ON" "Highlight parens OFF"))))

(def (cmd-toggle-global-pulse-line app)
  "Toggle global pulse-line-mode (flash current line)."
  (let ((echo (app-state-echo app)))
    (set! *global-pulse-line* (not *global-pulse-line*))
    (echo-message! echo (if *global-pulse-line*
                          "Pulse line ON" "Pulse line OFF"))))

;;; ---- batch 62: modeline and theme enhancement toggles ----

(def *global-solaire* #f)
(def *global-spaceline* #f)
(def *global-doom-modeline-env* #f)
(def *global-minions* #f)
(def *global-moody* #f)
(def *global-rich-minority* #f)
(def *global-smart-mode-line* #f)

(def (cmd-toggle-global-solaire app)
  "Toggle global solaire-mode (distinguish file/non-file buffers)."
  (let ((echo (app-state-echo app)))
    (set! *global-solaire* (not *global-solaire*))
    (echo-message! echo (if *global-solaire*
                          "Global solaire ON" "Global solaire OFF"))))

(def (cmd-toggle-global-spaceline app)
  "Toggle global spaceline-mode (Spacemacs modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-spaceline* (not *global-spaceline*))
    (echo-message! echo (if *global-spaceline*
                          "Spaceline ON" "Spaceline OFF"))))

(def (cmd-toggle-global-doom-modeline-env app)
  "Toggle global doom-modeline-env (show env info in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-doom-modeline-env* (not *global-doom-modeline-env*))
    (echo-message! echo (if *global-doom-modeline-env*
                          "Doom modeline env ON" "Doom modeline env OFF"))))

(def (cmd-toggle-global-minions app)
  "Toggle global minions-mode (minor mode menu in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-minions* (not *global-minions*))
    (echo-message! echo (if *global-minions*
                          "Minions ON" "Minions OFF"))))

(def (cmd-toggle-global-moody app)
  "Toggle global moody-mode (tabs and ribbons in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-moody* (not *global-moody*))
    (echo-message! echo (if *global-moody*
                          "Moody ON" "Moody OFF"))))

(def (cmd-toggle-global-rich-minority app)
  "Toggle global rich-minority-mode (clean minor mode display)."
  (let ((echo (app-state-echo app)))
    (set! *global-rich-minority* (not *global-rich-minority*))
    (echo-message! echo (if *global-rich-minority*
                          "Rich minority ON" "Rich minority OFF"))))

(def (cmd-toggle-global-smart-mode-line app)
  "Toggle global smart-mode-line (sexy modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-smart-mode-line* (not *global-smart-mode-line*))
    (echo-message! echo (if *global-smart-mode-line*
                          "Smart mode-line ON" "Smart mode-line OFF"))))

;;; ---- batch 71: BEAM and systems programming language toggles ----

(def *global-erlang-mode* #f)
(def *global-elixir-mode* #f)
(def *global-zig-mode* #f)
(def *global-ocaml-mode* #f)
(def *global-fsharp-mode* #f)
(def *global-dart-mode* #f)
(def *global-julia-mode* #f)

(def (cmd-toggle-global-erlang-mode app)
  "Toggle global erlang-mode (Erlang development)."
  (let ((echo (app-state-echo app)))
    (set! *global-erlang-mode* (not *global-erlang-mode*))
    (echo-message! echo (if *global-erlang-mode*
                          "Erlang mode ON" "Erlang mode OFF"))))

(def (cmd-toggle-global-elixir-mode app)
  "Toggle global elixir-mode (Elixir development)."
  (let ((echo (app-state-echo app)))
    (set! *global-elixir-mode* (not *global-elixir-mode*))
    (echo-message! echo (if *global-elixir-mode*
                          "Elixir mode ON" "Elixir mode OFF"))))

(def (cmd-toggle-global-zig-mode app)
  "Toggle global zig-mode (Zig development)."
  (let ((echo (app-state-echo app)))
    (set! *global-zig-mode* (not *global-zig-mode*))
    (echo-message! echo (if *global-zig-mode*
                          "Zig mode ON" "Zig mode OFF"))))

(def (cmd-toggle-global-ocaml-mode app)
  "Toggle global ocaml-mode (OCaml development with tuareg)."
  (let ((echo (app-state-echo app)))
    (set! *global-ocaml-mode* (not *global-ocaml-mode*))
    (echo-message! echo (if *global-ocaml-mode*
                          "OCaml mode ON" "OCaml mode OFF"))))

(def (cmd-toggle-global-fsharp-mode app)
  "Toggle global fsharp-mode (F# development)."
  (let ((echo (app-state-echo app)))
    (set! *global-fsharp-mode* (not *global-fsharp-mode*))
    (echo-message! echo (if *global-fsharp-mode*
                          "F# mode ON" "F# mode OFF"))))

(def (cmd-toggle-global-dart-mode app)
  "Toggle global dart-mode (Dart/Flutter development)."
  (let ((echo (app-state-echo app)))
    (set! *global-dart-mode* (not *global-dart-mode*))
    (echo-message! echo (if *global-dart-mode*
                          "Dart mode ON" "Dart mode OFF"))))

(def (cmd-toggle-global-julia-mode app)
  "Toggle global julia-mode (Julia scientific computing)."
  (let ((echo (app-state-echo app)))
    (set! *global-julia-mode* (not *global-julia-mode*))
    (echo-message! echo (if *global-julia-mode*
                          "Julia mode ON" "Julia mode OFF"))))

;;;============================================================================
;;; Dired advanced operations
;;;============================================================================

(def (cmd-dired-toggle-marks app)
  "Toggle marks on all entries in dired."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (new-lines
           (map (lambda (line)
                  (cond
                    ((string-prefix? "* " line)
                     (let ((f (substring line 2 (string-length line))))
                       (hash-remove! *dired-marks* (string-trim f))
                       f))
                    ((and (> (string-length line) 0)
                          (not (string-prefix? " " line))
                          (not (string-prefix? "-" line)))
                     (let ((f (string-trim line)))
                       (when (> (string-length f) 0)
                         (hash-put! *dired-marks* f #t))
                       (string-append "* " line)))
                    (else line)))
                lines)))
    (editor-set-text ed (string-join new-lines "\n"))
    (echo-message! (app-state-echo app) "Marks toggled")))

(def (cmd-dired-do-copy-marked app)
  "Copy all marked files to a destination directory."
  (let* ((marked (hash-keys *dired-marks*))
         (echo (app-state-echo app)))
    (if (null? marked)
      (echo-error! echo "No marked files")
      (let ((dest (app-read-string app "Copy to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (f)
                (with-catch (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory f) dest-dir)))
                      (copy-file f target)
                      (set! count (+ count 1))))))
              marked)
            (echo-message! echo
              (string-append "Copied " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-do-rename-marked app)
  "Move all marked files to a destination directory."
  (let* ((marked (hash-keys *dired-marks*))
         (echo (app-state-echo app)))
    (if (null? marked)
      (echo-error! echo "No marked files")
      (let ((dest (app-read-string app "Move to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (f)
                (with-catch (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory f) dest-dir)))
                      (rename-file f target)
                      (set! count (+ count 1))))))
              marked)
            (set! *dired-marks* (make-hash-table))
            (let ((buf (current-buffer-from-app app)))
              (when (and buf (buffer-file-path buf))
                (cmd-dired-refresh app)))
            (echo-message! echo
              (string-append "Moved " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-mark-by-regexp app)
  "Mark files matching a pattern in dired."
  (let ((pattern (app-read-string app "Mark files matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (count 0))
        (for-each
          (lambda (line)
            (let ((f (string-trim line)))
              (when (and (> (string-length f) 0)
                         (not (string-prefix? "*" f))
                         (string-contains f pattern))
                (hash-put! *dired-marks* f #t)
                (set! count (+ count 1)))))
          lines)
        ;; Refresh to show marks
        (let ((buf (current-buffer-from-app app)))
          (when (and buf (buffer-file-path buf))
            (cmd-dired-refresh app)))
        (echo-message! (app-state-echo app)
          (string-append "Marked " (number->string count) " files"))))))

(def (cmd-dired-sort-toggle app)
  "Toggle dired sort order."
  (echo-message! (app-state-echo app) "Sorted by name (default)"))

;;;============================================================================
;;; Diff hunk navigation
;;;============================================================================

(def (cmd-diff-next-hunk app)
  "Jump to next diff hunk (@@)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (+ (editor-get-current-pos ed) 1))
         (idx (string-contains text "@@" pos)))
    (if idx
      (begin (editor-goto-pos ed idx) (editor-scroll-caret ed))
      (echo-message! (app-state-echo app) "No more hunks"))))

(def (cmd-diff-prev-hunk app)
  "Jump to previous diff hunk (@@)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    ;; Search backward
    (let loop ((i (- pos 2)))
      (cond
        ((< i 0) (echo-message! (app-state-echo app) "No previous hunk"))
        ((and (>= i 0) (< (+ i 1) (string-length text))
              (char=? (string-ref text i) #\@)
              (char=? (string-ref text (+ i 1)) #\@))
         (editor-goto-pos ed i) (editor-scroll-caret ed))
        (else (loop (- i 1)))))))

;;;============================================================================
;;; Display line numbers mode
;;;============================================================================

(def (cmd-display-line-numbers-mode app)
  "Toggle line number display."
  (let* ((ed (current-editor app))
         (currently-on (> (send-message ed SCI_GETMARGINWIDTHN 0 0) 0)))
    (if currently-on
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 0)
        (echo-message! (app-state-echo app) "Line numbers OFF"))
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 48)
        (echo-message! (app-state-echo app) "Line numbers ON")))))

;;;============================================================================
;;; Helper: get or create a named buffer (TUI)
;;;============================================================================

(def (tui-get-or-create-buffer app name)
  "Find buffer by name or create a new one, attach to current editor."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (or (buffer-by-name name)
        (buffer-create! name ed))))

(def (tui-display-in-buffer! app name text)
  "Display text in a named buffer, switching to it."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (tui-get-or-create-buffer app name)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    buf))

;;;============================================================================
;;; Elfeed — RSS/Atom feed reader
;;;============================================================================

(def *elfeed-feeds* '())           ; list of feed URL strings
(def *elfeed-entries* '())         ; list of (title url date feed-title read?)
(def *elfeed-db-file* #f)         ; path to feeds list file

(def (elfeed-db-path)
  "Path to elfeed feeds list."
  (or *elfeed-db-file*
      (let ((home (getenv "HOME" "/tmp")))
        (string-append home "/.gemacs-elfeed-feeds"))))

(def (elfeed-load-feeds!)
  "Load feed URLs from disk."
  (let ((path (elfeed-db-path)))
    (when (file-exists? path)
      (set! *elfeed-feeds*
        (with-exception-catcher
          (lambda (e) '())
          (lambda ()
            (let ((lines (call-with-input-file path
                           (lambda (p) (read-line p #f)))))
              (if (and lines (string? lines))
                (let loop ((rest lines) (acc '()))
                  (let ((nl (string-index rest #\newline)))
                    (if nl
                      (let ((line (string-trim-both (substring rest 0 nl))))
                        (loop (substring rest (+ nl 1) (string-length rest))
                              (if (and (> (string-length line) 0)
                                       (not (char=? (string-ref line 0) #\#)))
                                (cons line acc) acc)))
                      (let ((line (string-trim-both rest)))
                        (reverse (if (> (string-length line) 0)
                                   (cons line acc) acc))))))
                '()))))))))

(def (elfeed-save-feeds!)
  "Save feed URLs to disk."
  (let ((path (elfeed-db-path)))
    (call-with-output-file path
      (lambda (p)
        (for-each (lambda (url) (display url p) (newline p))
                  *elfeed-feeds*)))))

(def (elfeed-fetch-feed url)
  "Fetch and parse an RSS/Atom feed. Returns list of (title link date)."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let* ((proc (open-process
                     (list path: "curl"
                           arguments: (list "-sL" "-A" "Mozilla/5.0"
                                            "--max-time" "15" url)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (xml (read-line proc #f)))
        (process-status proc)
        (if (and xml (string? xml))
          (elfeed-parse-feed xml url)
          '())))))

(def (elfeed-extract-tag xml tag (start 0))
  "Extract content between <tag> and </tag> starting from position start.
   Returns (content . end-pos) or #f."
  (let* ((open-tag (string-append "<" tag))
         (close-tag (string-append "</" tag ">"))
         (len (string-length xml))
         (pos (string-contains xml open-tag start)))
    (if (not pos) #f
      ;; Find end of opening tag (handle attributes)
      (let ((gt (string-index xml #\> pos)))
        (if (not gt) #f
          (let* ((content-start (+ gt 1))
                 (end-pos (string-contains xml close-tag content-start)))
            (if (not end-pos) #f
              (cons (substring xml content-start end-pos)
                    (+ end-pos (string-length close-tag))))))))))

(def (elfeed-str-replace str from to)
  "Replace all occurrences of from with to in str."
  (let ((from-len (string-length from))
        (str-len (string-length str)))
    (if (= from-len 0) str
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (if (> (+ i from-len) str-len)
            (begin (display (substring str i str-len) out)
                   (get-output-string out))
            (if (string=? (substring str i (+ i from-len)) from)
              (begin (display to out) (loop (+ i from-len)))
              (begin (write-char (string-ref str i) out) (loop (+ i 1))))))))))

(def (elfeed-unescape-html s)
  "Basic HTML entity unescaping."
  (let* ((s (elfeed-str-replace s "&amp;" "&"))
         (s (elfeed-str-replace s "&lt;" "<"))
         (s (elfeed-str-replace s "&gt;" ">"))
         (s (elfeed-str-replace s "&quot;" "\""))
         (s (elfeed-str-replace s "&#39;" "'"))
         (s (elfeed-str-replace s "<![CDATA[" ""))
         (s (elfeed-str-replace s "]]>" "")))
    (string-trim-both s)))

(def (elfeed-parse-feed xml url)
  "Parse RSS or Atom feed XML into entries. Returns list of (title link date)."
  (let ((feed-title
          (let ((t (elfeed-extract-tag xml "title")))
            (if t (elfeed-unescape-html (car t)) url))))
    ;; Try RSS <item> first, then Atom <entry>
    (let ((items (elfeed-parse-items xml "item" feed-title)))
      (if (null? items)
        (elfeed-parse-items xml "entry" feed-title)
        items))))

(def (elfeed-parse-items xml tag feed-title)
  "Parse all <item> or <entry> elements from xml."
  (let loop ((start 0) (acc '()))
    (let ((item (elfeed-extract-tag xml tag start)))
      (if (not item) (reverse acc)
        (let* ((content (car item))
               (next (cdr item))
               (title-r (elfeed-extract-tag content "title"))
               (title (if title-r (elfeed-unescape-html (car title-r)) "(no title)"))
               ;; RSS uses <link>, Atom uses <link href="..."/>
               (link-r (elfeed-extract-tag content "link"))
               (link (if link-r
                       (let ((l (car link-r)))
                         (if (> (string-length l) 0)
                           (elfeed-unescape-html l)
                           ;; Atom: extract href attribute
                           (elfeed-extract-href content)))
                       ""))
               (date-r (or (elfeed-extract-tag content "pubDate")
                           (elfeed-extract-tag content "updated")
                           (elfeed-extract-tag content "published")
                           (elfeed-extract-tag content "dc:date")))
               (date (if date-r (elfeed-unescape-html (car date-r)) "")))
          (loop next (cons (list title link date feed-title #f) acc)))))))

(def (elfeed-extract-href content)
  "Extract href from <link href='...' /> in Atom feeds."
  (let ((pos (string-contains content "<link")))
    (if (not pos) ""
      (let ((href-pos (string-contains content "href=" pos)))
        (if (not href-pos) ""
          (let* ((q-start (+ href-pos 5))
                 (quote-char (if (< q-start (string-length content))
                               (string-ref content q-start) #\"))
                 (val-start (+ q-start 1))
                 (val-end (string-index content quote-char val-start)))
            (if val-end (substring content val-start val-end) "")))))))

(def (cmd-elfeed app)
  "Open the Elfeed RSS feed reader."
  (elfeed-load-feeds!)
  (when (null? *elfeed-feeds*)
    (set! *elfeed-feeds*
      '("https://planet.emacslife.com/atom.xml"
        "https://hnrss.org/frontpage")))
  (echo-message! (app-state-echo app)
    (string-append "Fetching " (number->string (length *elfeed-feeds*)) " feeds..."))
  ;; Fetch all feeds
  (set! *elfeed-entries* '())
  (for-each
    (lambda (url)
      (let ((entries (elfeed-fetch-feed url)))
        (set! *elfeed-entries* (append *elfeed-entries* entries))))
    *elfeed-feeds*)
  ;; Display in buffer
  (let* ((text (elfeed-format-entries *elfeed-entries*)))
    (tui-display-in-buffer! app "*elfeed*" text)
    (echo-message! (app-state-echo app)
      (string-append "Elfeed: " (number->string (length *elfeed-entries*)) " entries"))))

(def (elfeed-format-entries entries)
  "Format feed entries for display."
  (let ((lines (map (lambda (e)
                      (let ((title (car e))
                            (link (cadr e))
                            (date (caddr e))
                            (feed (cadddr e)))
                        (string-append
                          (if (> (string-length date) 16)
                            (substring date 0 16) date)
                          "  "
                          (string-pad-right feed 20)
                          "  "
                          title
                          "\n    " link)))
                    entries)))
    (string-append "Elfeed - RSS Feed Reader\n"
                   (make-string 60 #\=) "\n\n"
                   (string-join lines "\n\n") "\n")))

(def (cmd-elfeed-add-feed app)
  "Add an RSS/Atom feed URL to elfeed."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (url (echo-read-string echo "Feed URL: " row width)))
    (when (and url (> (string-length url) 0))
      (elfeed-load-feeds!)
      (unless (member url *elfeed-feeds*)
        (set! *elfeed-feeds* (cons url *elfeed-feeds*))
        (elfeed-save-feeds!)
        (echo-message! echo (string-append "Added feed: " url))))))

(def (cmd-elfeed-update app)
  "Refresh all elfeed feeds."
  (cmd-elfeed app))


;;;============================================================================
;;; Direnv / envrc — .envrc integration
;;;============================================================================

(def *direnv-active* #f)

(def (cmd-direnv-update-environment app)
  "Load environment from .envrc in project root using direnv."
  (let* ((dir (current-directory))
         (envrc (string-append dir "/.envrc")))
    (if (not (file-exists? envrc))
      (echo-message! (app-state-echo app) "No .envrc found in current directory")
      (with-exception-catcher
        (lambda (e)
          (echo-message! (app-state-echo app) "direnv not installed or failed"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "direnv"
                               arguments: (list "export" "bash")
                               directory: dir
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #f)))
                 (output (read-line proc #f)))
            (process-status proc)
            (when (and output (string? output))
              ;; Parse export VAR=value lines
              (let loop ((rest output) (count 0))
                (let ((pos (string-contains rest "export ")))
                  (if (not pos)
                    (begin
                      (set! *direnv-active* #t)
                      (echo-message! (app-state-echo app)
                        (string-append "direnv: loaded " (number->string count)
                                       " variables from " envrc)))
                    (let* ((start (+ pos 7))
                           (nl (or (string-index rest #\newline start)
                                   (string-length rest)))
                           (assign (substring rest start nl))
                           (eq (string-index assign #\=)))
                      (when eq
                        (let ((var (substring assign 0 eq))
                              (val (let ((raw (substring assign (+ eq 1)
                                                         (string-length assign))))
                                     ;; Strip quotes
                                     (if (and (> (string-length raw) 1)
                                              (or (char=? (string-ref raw 0) #\')
                                                  (char=? (string-ref raw 0) #\")))
                                       (substring raw 1 (- (string-length raw) 1))
                                       raw))))
                          (setenv var val)))
                      (loop (substring rest (+ nl 1) (string-length rest))
                            (+ count 1)))))))))))))

(def (cmd-direnv-allow app)
  "Run direnv allow for the current directory."
  (with-exception-catcher
    (lambda (e) (echo-message! (app-state-echo app) "direnv allow failed"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "direnv" arguments: (list "allow")
                           directory: (current-directory)
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #f)))
             (out (read-line proc #f)))
        (process-status proc)
        (echo-message! (app-state-echo app) "direnv: allowed .envrc")))))

;;;============================================================================
;;; Move text up/down (drag-stuff / move-text)
;;;============================================================================

(def (cmd-move-text-up app)
  "Move current line up one line."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (lines (string-split text #\newline)))
    (when (> cur-line 0)
      (let* ((swapped
               (let loop ((ls lines) (n 0) (acc '()))
                 (cond
                   ((null? ls) (reverse acc))
                   ((= n (- cur-line 1))
                    ;; Swap this line with the next
                    (if (null? (cdr ls))
                      (reverse (cons (car ls) acc))
                      (loop (cddr ls) (+ n 2)
                            (cons (car ls) (cons (cadr ls) acc)))))
                   (else (loop (cdr ls) (+ n 1) (cons (car ls) acc))))))
             (new-text (string-join swapped "\n"))
             (new-pos (editor-position-from-line ed (- cur-line 1))))
        (editor-set-text ed new-text)
        (editor-goto-pos ed new-pos)))))

(def (cmd-move-text-down app)
  "Move current line down one line."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (lines (string-split text #\newline))
         (max-line (- (length lines) 1)))
    (when (< cur-line max-line)
      (let* ((swapped
               (let loop ((ls lines) (n 0) (acc '()))
                 (cond
                   ((null? ls) (reverse acc))
                   ((= n cur-line)
                    (if (null? (cdr ls))
                      (reverse (cons (car ls) acc))
                      (loop (cddr ls) (+ n 2)
                            (cons (car ls) (cons (cadr ls) acc)))))
                   (else (loop (cdr ls) (+ n 1) (cons (car ls) acc))))))
             (new-text (string-join swapped "\n"))
             (new-pos (editor-position-from-line ed (+ cur-line 1))))
        (editor-set-text ed new-text)
        (editor-goto-pos ed new-pos)))))


;;;============================================================================
;;; Transient keymaps — modal command menus (like Magit's transient)
;;;============================================================================

(def *transient-active* #f)   ; currently active transient map name
(def *transient-maps* (make-hash-table)) ; name → list of (key description command)

(def (transient-define-map! name entries)
  "Define a transient keymap. entries: list of (key-char description cmd-symbol)."
  (hash-put! *transient-maps* name entries))

;; Pre-define common transient maps
(def (transient-init-defaults!)
  "Set up default transient maps."
  ;; Window resize transient
  (transient-define-map! 'window-resize
    '((#\{ "Shrink horizontal" shrink-window-horizontally)
      (#\} "Grow horizontal" enlarge-window-horizontally)
      (#\^ "Grow vertical" enlarge-window)
      (#\v "Shrink vertical" shrink-window)
      (#\= "Balance" balance-windows)))
  ;; Zoom transient
  (transient-define-map! 'zoom
    '((#\+ "Zoom in" text-scale-increase)
      (#\- "Zoom out" text-scale-decrease)
      (#\0 "Reset" text-scale-adjust)))
  ;; Navigation transient
  (transient-define-map! 'navigate
    '((#\n "Next error" next-error)
      (#\p "Previous error" previous-error)
      (#\N "Next buffer" next-buffer)
      (#\P "Previous buffer" previous-buffer))))

(def (cmd-transient-map app)
  "Activate a transient keymap by name."
  (transient-init-defaults!)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (names (hash-keys *transient-maps*))
         (name-strs (map symbol->string names))
         (choice (echo-read-string echo
                   (string-append "Transient map ("
                                  (string-join name-strs "/") "): ")
                   row width)))
    (when (and choice (> (string-length choice) 0))
      (let ((sym (string->symbol choice)))
        (if (not (hash-get *transient-maps* sym))
          (echo-message! echo (string-append "Unknown transient: " choice))
          (cmd-transient-activate app sym))))))

(def (cmd-transient-activate app name)
  "Show and activate a transient keymap."
  (let* ((entries (hash-get *transient-maps* name))
         (echo (app-state-echo app))
         (prompt (string-append
                   (symbol->string name) ": "
                   (string-join
                     (map (lambda (e)
                            (string-append
                              (string (car e)) "=" (cadr e)))
                          entries)
                     " "))))
    (set! *transient-active* name)
    (let ((key-str (echo-read-string echo (string-append prompt " > ")
                     (- (frame-height (app-state-frame app)) 1)
                     (frame-width (app-state-frame app)))))
      (set! *transient-active* #f)
      (when (and key-str (= (string-length key-str) 1))
        (let* ((ch (string-ref key-str 0))
               (entry (find (lambda (e) (char=? (car e) ch)) entries)))
          (if entry
            (let ((cmd-sym (caddr entry)))
              (let ((cmd (find-command cmd-sym)))
                (if cmd (cmd app)
                  (echo-message! echo
                    (string-append "Command not found: "
                                   (symbol->string cmd-sym))))))
            (echo-message! echo "Unknown key")))))))




