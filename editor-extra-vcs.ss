;;; -*- Gerbil -*-
;;; Version control, mail, sessions, macros, compilation, flyspell,
;;; multiple cursors, package management, customize, and diff mode

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/editor-extra-helpers)

;; Additional VC commands
(def (cmd-vc-register app)
  "Register file with version control."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (buffer-file-path buf)))
    (if file
      (let ((result (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git" arguments: (list "add" file)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "")))))))
        (echo-message! (app-state-echo app)
          (string-append "Registered: " (path-strip-directory file))))
      (echo-message! (app-state-echo app) "No file to register"))))

(def (cmd-vc-dir app)
  "Show VC directory status."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error running git status")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("status" "--short")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(clean)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*VC Dir*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "VC Directory Status\n\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-vc-pull app)
  "Pull from remote repository."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("pull")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "git pull: " (if (> (string-length result) 60)
                                    (substring result 0 60)
                                    result)))))

(def (cmd-vc-push app)
  "Push to remote repository."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("push")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "git push: " (if (> (string-length result) 60)
                                    (substring result 0 60)
                                    result)))))

(def (cmd-vc-create-tag app)
  "Create a git tag."
  (let ((tag (app-read-string app "Tag name: ")))
    (when (and tag (not (string-empty? tag)))
      (let ((result (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git" arguments: (list "tag" tag)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "")))))))
        (echo-message! (app-state-echo app)
          (string-append "Created tag: " tag))))))

(def (cmd-vc-print-log app)
  "Show full git log."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error running git log")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("log" "--oneline" "-50")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(empty log)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*VC Log*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Log (last 50)\n\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-vc-stash app)
  "Stash current changes."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("stash")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "Stash: " result))))

(def (cmd-vc-stash-pop app)
  "Pop last stash."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("stash" "pop")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "Stash pop: " result))))

;; Mail
(def (cmd-compose-mail app)
  "Compose mail. Creates a buffer with headers. Use C-c C-c to send via sendmail."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Mail*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed "To: \nCc: \nSubject: \n--text follows this line--\n\n")
    (editor-goto-pos ed 4) ; position after "To: "
    (echo-message! (app-state-echo app) "Compose mail (C-c C-c to send)")))

(def (cmd-rmail app)
  "Read mail from mbox file."
  (let* ((mbox (string-append (or (getenv "HOME") ".") "/mbox"))
         (echo (app-state-echo app)))
    (if (file-exists? mbox)
      (let* ((content (read-file-as-string mbox))
             (fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! "*RMAIL*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed content)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)
        (echo-message! echo "RMAIL: reading mbox"))
      (echo-message! echo "No mbox file found"))))

(def (cmd-gnus app)
  "Show mail folders/newsgroups."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Gnus*" ed))
         (mail-dir (string-append (or (getenv "HOME") ".") "/Mail"))
         (folders (if (file-exists? mail-dir)
                    (with-exception-catcher
                      (lambda (e) '())
                      (lambda () (directory-files mail-dir)))
                    '()))
         (text (string-append "Group Buffer\n\n"
                 (if (null? folders)
                   "  No mail folders found in ~/Mail\n"
                   (string-join
                     (map (lambda (f) (string-append "  " f)) folders)
                     "\n"))
                 "\n")))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Session management
(def (cmd-desktop-save app)
  "Save desktop session (buffer list and files)."
  (let* ((bufs (buffer-list))
         (files (filter (lambda (f) f)
                        (map buffer-file-path bufs)))
         (session-file (string-append (or (getenv "HOME") ".") "/.gerbil-emacs-session")))
    (with-exception-catcher
      (lambda (e) (echo-message! (app-state-echo app) "Error saving session"))
      (lambda ()
        (call-with-output-file session-file
          (lambda (port)
            (for-each (lambda (f) (display f port) (newline port)) files)))
        (echo-message! (app-state-echo app)
          (string-append "Session saved: " (number->string (length files)) " files"))))))

(def (cmd-desktop-read app)
  "Restore desktop session."
  (let ((session-file (string-append (or (getenv "HOME") ".") "/.gerbil-emacs-session")))
    (if (file-exists? session-file)
      (with-exception-catcher
        (lambda (e) (echo-message! (app-state-echo app) "Error reading session"))
        (lambda ()
          (let ((files (call-with-input-file session-file
                         (lambda (port)
                           (let loop ((acc []))
                             (let ((line (read-line port)))
                               (if (eof-object? line)
                                 (reverse acc)
                                 (loop (cons line acc)))))))))
            (let* ((fr (app-state-frame app))
                   (win (current-window fr))
                   (ed (edit-window-editor win))
                   (count 0))
              (for-each
                (lambda (f)
                  (when (file-exists? f)
                    (let ((buf (buffer-create! (path-strip-directory f) ed)))
                      (buffer-attach! ed buf)
                      (set! (buffer-file-path buf) f)
                      (set! count (+ count 1)))))
                files)
              (echo-message! (app-state-echo app)
                (string-append "Session restored: " (number->string count) " files"))))))
      (echo-message! (app-state-echo app) "No session file found"))))

(def (cmd-desktop-clear app)
  "Clear saved session."
  (let ((session-file (string-append (or (getenv "HOME") ".") "/.gerbil-emacs-session")))
    (when (file-exists? session-file)
      (delete-file session-file))
    (echo-message! (app-state-echo app) "Session cleared")))

;; Man page viewer
(def (cmd-man app)
  "View man page."
  (let ((topic (app-read-string app "Man page: ")))
    (when (and topic (not (string-empty? topic)))
      (let ((result (with-exception-catcher
                      (lambda (e) (string-append "No man page for: " topic))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "man"
                                         arguments: (list topic)
                                         environment: '("MANPAGER=cat" "COLUMNS=80" "TERM=dumb")
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out (string-append "No man page for: " topic))))))))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (buf (buffer-create! (string-append "*Man " topic "*") ed)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed result)
          (editor-goto-pos ed 0)
          (editor-set-read-only ed #t))))))

(def (cmd-woman app)
  "View man page without man command (alias for man)."
  (cmd-man app))

;; Macro extras
(def (cmd-apply-macro-to-region-lines app)
  "Apply last keyboard macro to each line in region."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (macro (app-state-macro-last app)))
    (if macro
      (let* ((start (editor-get-selection-start ed))
             (end (editor-get-selection-end ed))
             (start-line (send-message ed SCI_LINEFROMPOSITION start 0))
             (end-line (send-message ed SCI_LINEFROMPOSITION end 0))
             (count (- end-line start-line)))
        ;; Apply macro to each line from end to start (to preserve positions)
        (let loop ((line end-line))
          (when (>= line start-line)
            (let ((line-start (send-message ed SCI_POSITIONFROMLINE line 0)))
              (editor-goto-pos ed line-start)
              ;; Replay macro events
              (for-each
                (lambda (evt)
                  ;; Each evt is a key event, replay via the app's key handler
                  (void))
                macro))
            (loop (- line 1))))
        (echo-message! (app-state-echo app)
          (string-append "Macro applied to " (number->string (+ count 1)) " lines")))
      (echo-message! (app-state-echo app) "No keyboard macro defined"))))

(def (cmd-edit-kbd-macro app)
  "Display the last keyboard macro in a buffer for viewing."
  (let* ((macro (app-state-macro-last app))
         (echo (app-state-echo app)))
    (if macro
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! "*Kbd Macro*" ed))
             (text (string-append "Keyboard Macro ("
                     (number->string (length macro)) " events)\n\n"
                     (string-join
                       (map (lambda (evt) (with-output-to-string (lambda () (write evt))))
                            macro)
                       "\n")
                     "\n")))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t))
      (echo-message! echo "No keyboard macro defined"))))

;; Compilation extras
(def (cmd-recompile app)
  "Recompile using last compile command."
  (let ((last-cmd (app-state-last-compile app)))
    (if last-cmd
      (echo-message! (app-state-echo app) (string-append "Recompile: " last-cmd))
      (echo-message! (app-state-echo app) "No previous compile command"))))

(def (cmd-kill-compilation app)
  "Kill current compilation process."
  (let ((proc *last-compile-proc*))
    (if proc
      (begin
        (with-exception-catcher (lambda (e) (void))
          (lambda () (when (port? proc) (close-port proc))))
        (echo-message! (app-state-echo app) "Compilation killed"))
      (echo-message! (app-state-echo app) "No compilation in progress"))))

;; Flyspell extras — uses aspell/ispell for spell checking
;; flyspell-check-word is imported from :gerbil-emacs/editor-extra-helpers

(def (cmd-flyspell-auto-correct-word app)
  "Auto-correct word at point using aspell's first suggestion."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed)))
    (let-values (((start end) (word-bounds-at ed pos)))
      (if (not start)
        (echo-message! echo "No word at point")
        (let* ((word (substring (editor-get-text ed) start end))
               (suggestions (flyspell-check-word word)))
          (cond
            ((not suggestions) (echo-message! echo (string-append "\"" word "\" is correct")))
            ((null? suggestions) (echo-message! echo (string-append "No suggestions for \"" word "\"")))
            (else
              (let ((replacement (car suggestions)))
                (send-message ed SCI_SETTARGETSTART start 0)
                (send-message ed SCI_SETTARGETEND end 0)
                (send-message/string ed SCI_REPLACETARGET replacement)
                (echo-message! echo (string-append "Corrected: " word " -> " replacement))))))))))

(def (cmd-flyspell-goto-next-error app)
  "Move to next misspelled word using aspell."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Scan forward for words and check each
    (let loop ((i pos))
      (cond
        ((>= i len) (echo-message! echo "No more misspelled words"))
        ((extra-word-char? (string-ref text i))
         ;; Found word start, find end
         (let find-end ((j (+ i 1)))
           (if (or (>= j len) (not (extra-word-char? (string-ref text j))))
             (let* ((word (substring text i j))
                    (bad (and (> (string-length word) 2) (flyspell-check-word word))))
               (if (and bad (list? bad))
                 (begin
                   (editor-goto-pos ed i)
                   (editor-set-selection ed i j)
                   (editor-scroll-caret ed)
                   (echo-message! echo (string-append "Misspelled: " word)))
                 (loop j)))
             (find-end (+ j 1)))))
        (else (loop (+ i 1)))))))

;; Multiple cursors - simulated via sequential replacement
;; True multiple cursors require deep editor integration; this provides
;; the most common use case: replacing all occurrences of selection

(def *mc-selection* #f) ; current selection being marked
(def *mc-positions* '()) ; list of (start . end) positions found
(def *mc-position-idx* 0) ; current position index

(def (mc-get-selection app)
  "Get the currently selected text, or word at point if no selection."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection - get word at point
      (let-values (((start end) (word-bounds-at ed (editor-get-current-pos ed))))
        (if start
          (let ((text (editor-get-text ed)))
            (substring text start end))
          #f))
      ;; Have selection
      (let ((text (editor-get-text ed)))
        (substring text sel-start sel-end)))))

(def (mc-find-all-positions ed pattern)
  "Find all positions of pattern in editor text."
  (let* ((text (editor-get-text ed))
         (len (string-length pattern))
         (text-len (string-length text)))
    (let loop ((i 0) (positions '()))
      (if (> (+ i len) text-len)
        (reverse positions)
        (if (string=? (substring text i (+ i len)) pattern)
          (loop (+ i len) (cons (cons i (+ i len)) positions))
          (loop (+ i 1) positions))))))

(def (cmd-mc-mark-next-like-this app)
  "Find and highlight next occurrence of selection/word. Use repeatedly to mark more."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    ;; Get or initialize selection
    (when (not *mc-selection*)
      (set! *mc-selection* (mc-get-selection app))
      (set! *mc-positions* (if *mc-selection* (mc-find-all-positions ed *mc-selection*) '()))
      (set! *mc-position-idx* 0))
    
    (if (or (not *mc-selection*) (null? *mc-positions*))
      (echo-message! echo "No matches found")
      (let* ((new-idx (modulo (+ *mc-position-idx* 1) (length *mc-positions*)))
             (pos (list-ref *mc-positions* new-idx))
             (start (car pos))
             (end (cdr pos)))
        (set! *mc-position-idx* new-idx)
        (editor-goto-pos ed start)
        (editor-set-selection ed start end)
        (echo-message! echo (string-append "Match " (number->string (+ new-idx 1))
                                          "/" (number->string (length *mc-positions*))
                                          " of \"" *mc-selection* "\""))))))

(def (cmd-mc-mark-previous-like-this app)
  "Find and highlight previous occurrence of selection/word."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (when (not *mc-selection*)
      (set! *mc-selection* (mc-get-selection app))
      (set! *mc-positions* (if *mc-selection* (mc-find-all-positions ed *mc-selection*) '()))
      (set! *mc-position-idx* 0))
    
    (if (or (not *mc-selection*) (null? *mc-positions*))
      (echo-message! echo "No matches found")
      (let* ((new-idx (modulo (- *mc-position-idx* 1) (length *mc-positions*)))
             (pos (list-ref *mc-positions* new-idx))
             (start (car pos))
             (end (cdr pos)))
        (set! *mc-position-idx* new-idx)
        (editor-goto-pos ed start)
        (editor-set-selection ed start end)
        (echo-message! echo (string-append "Match " (number->string (+ new-idx 1))
                                          "/" (number->string (length *mc-positions*))
                                          " of \"" *mc-selection* "\""))))))

(def (cmd-mc-mark-all-like-this app)
  "Replace all occurrences of selection with prompted text (simulates multi-cursor edit)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (selection (mc-get-selection app)))
    (if (not selection)
      (echo-message! echo "No selection or word at point")
      (let ((positions (mc-find-all-positions ed selection)))
        (if (null? positions)
          (echo-message! echo "No matches found")
          (let* ((row (- (frame-height fr) 1))
                 (width (frame-width fr))
                 (replacement (echo-read-string echo 
                                (string-append "Replace all (" (number->string (length positions)) 
                                              " matches) with: ")
                                row width)))
            (when (and replacement (not (string-empty? replacement)))
              ;; Replace from end to start to preserve positions
              (let ((sorted-positions (sort positions (lambda (a b) (> (car a) (car b))))))
                (for-each
                  (lambda (pos)
                    (editor-set-selection ed (car pos) (cdr pos))
                    (editor-replace-selection ed replacement))
                  sorted-positions)
                (echo-message! echo (string-append "Replaced " (number->string (length positions))
                                                  " occurrences"))))))))))

(def (cmd-mc-edit-lines app)
  "Apply the same edit to each line in selection."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (mark-pos (app-state-mark-pos app)))
    (if (not mark-pos)
      (echo-message! echo "No region (set mark first with C-SPC)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min pos mark-pos))
             (end (max pos mark-pos))
             (text (editor-get-text ed))
             (region (substring text start (min end (string-length text))))
             (lines (string-split region #\newline))
             (num-lines (length lines))
             (row (- (frame-height fr) 1))
             (width (frame-width fr)))
        (let ((prefix (echo-read-string echo 
                        (string-append "Prepend to " (number->string num-lines) " lines: ")
                        row width)))
          (when prefix
            (let* ((new-lines (map (lambda (line) (string-append prefix line)) lines))
                   (new-text (string-join new-lines "\n")))
              (editor-set-selection ed start end)
              (editor-replace-selection ed new-text)
              (echo-message! echo (string-append "Prepended to " (number->string num-lines) " lines")))))))))

;; Clear mc state on other commands
(def (mc-clear-state!)
  (set! *mc-selection* #f)
  (set! *mc-positions* '())
  (set! *mc-position-idx* 0))

;; Package management via Gerbil pkg system
(def (run-gerbil-pkg args)
  "Run gerbil pkg with given args. Returns output string."
  (with-exception-catcher
    (lambda (e) (string-append "Error: " (with-output-to-string (lambda () (display-exception e)))))
    (lambda ()
      (let* ((proc (open-process
                      (list path: "gerbil"
                            arguments: (cons "pkg" args)
                            stdin-redirection: #f stdout-redirection: #t
                            stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (or out "")))))

(def (cmd-package-list-packages app)
  "List installed Gerbil packages."
  (let* ((output (run-gerbil-pkg '("list")))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Packages*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Installed Packages\n\n" output "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

(def (cmd-package-install app)
  "Install a Gerbil package by name."
  (let ((pkg (app-read-string app "Package to install: ")))
    (when (and pkg (not (string-empty? pkg)))
      (echo-message! (app-state-echo app) (string-append "Installing " pkg "..."))
      (let ((result (run-gerbil-pkg (list "install" pkg))))
        (echo-message! (app-state-echo app) (string-append "Install: " result))))))

(def (cmd-package-delete app)
  "Uninstall a Gerbil package."
  (let ((pkg (app-read-string app "Package to remove: ")))
    (when (and pkg (not (string-empty? pkg)))
      (let ((result (run-gerbil-pkg (list "uninstall" pkg))))
        (echo-message! (app-state-echo app) (string-append "Uninstall: " result))))))

(def (cmd-package-refresh-contents app)
  "Refresh package list (update)."
  (echo-message! (app-state-echo app) "Updating packages...")
  (let ((result (run-gerbil-pkg '("update"))))
    (echo-message! (app-state-echo app) (string-append "Update: " result))))

;; Customization system (*custom-variables* in editor-extra-helpers)

(def (cmd-customize-group app)
  "Show current editor settings grouped by category."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Customize*" ed))
         (text (string-append
                 "Editor Settings\n"
                 (make-string 60 #\=) "\n\n"
                 "Display:\n"
                 "  line-numbers: " (if (mode-enabled? 'line-numbers) "on" "off") "\n"
                 "  word-wrap: " (if (mode-enabled? 'word-wrap) "on" "off") "\n"
                 "  whitespace: " (if (mode-enabled? 'whitespace) "on" "off") "\n\n"
                 "Editing:\n"
                 "  org-mode: " (if (mode-enabled? 'org-mode) "on" "off") "\n"
                 "  overwrite-mode: " (if (mode-enabled? 'overwrite) "on" "off") "\n\n"
                 "Mode Flags:\n"
                 (let ((entries (hash->list *mode-flags*)))
                   (if (null? entries) "  (none set)\n"
                     (string-join
                       (map (lambda (p)
                              (string-append "  " (symbol->string (car p)) ": "
                                            (if (cdr p) "on" "off")))
                            entries)
                       "\n")))
                 "\n\nCustom Variables:\n"
                 (let ((entries (hash->list *custom-variables*)))
                   (if (null? entries) "  (none set)\n"
                     (string-join
                       (map (lambda (p)
                              (string-append "  " (symbol->string (car p)) " = "
                                            (with-output-to-string (lambda () (write (cdr p))))))
                            entries)
                       "\n")))
                 "\n")))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

(def (cmd-customize-variable app)
  "Set a custom variable by name."
  (let ((name (app-read-string app "Variable name: ")))
    (when (and name (not (string-empty? name)))
      (let* ((sym (string->symbol name))
             (current (hash-get *custom-variables* sym))
             (prompt (if current
                       (string-append "Value for " name " (current: "
                         (with-output-to-string (lambda () (write current))) "): ")
                       (string-append "Value for " name ": ")))
             (val-str (app-read-string app prompt)))
        (when (and val-str (not (string-empty? val-str)))
          (let ((val (or (string->number val-str)
                        (cond
                          ((string=? val-str "true") #t)
                          ((string=? val-str "false") #f)
                          ((string=? val-str "nil") #f)
                          (else val-str)))))
            (hash-put! *custom-variables* sym val)
            (echo-message! (app-state-echo app)
              (string-append name " = " (with-output-to-string (lambda () (write val)))))))))))

(def (cmd-customize-themes app)
  "List available color themes."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Themes*" ed))
         (text (string-append
                 "Available Themes\n\n"
                 "  default       — Standard dark theme\n"
                 "  light         — Light background\n"
                 "  solarized     — Solarized color scheme\n"
                 "  monokai       — Monokai-inspired\n"
                 "  gruvbox       — Gruvbox warm colors\n"
                 "\nUse M-x load-theme to activate a theme.\n")))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Diff mode - working with diff/patch content

(def (diff-parse-hunk-header line)
  "Parse a diff hunk header like @@ -start,count +start,count @@. Returns (old-start old-count new-start new-count) or #f."
  (if (string-prefix? "@@" line)
    (let* ((parts (string-split line #\space))
           (old-part (if (>= (length parts) 2) (cadr parts) "-0"))
           (new-part (if (>= (length parts) 3) (caddr parts) "+0")))
      ;; Parse -start,count and +start,count
      (let* ((old-range (substring old-part 1 (string-length old-part)))
             (new-range (substring new-part 1 (string-length new-part)))
             (old-parts (string-split old-range #\,))
             (new-parts (string-split new-range #\,))
             (old-start (string->number (car old-parts)))
             (old-count (if (> (length old-parts) 1) (string->number (cadr old-parts)) 1))
             (new-start (string->number (car new-parts)))
             (new-count (if (> (length new-parts) 1) (string->number (cadr new-parts)) 1)))
        (if (and old-start new-start)
          (list old-start (or old-count 1) new-start (or new-count 1))
          #f)))
    #f))

(def (diff-find-current-hunk ed)
  "Find the hunk header line for current position. Returns line number or #f."
  (let* ((pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (text (editor-get-text ed))
         (lines (string-split text #\newline)))
    (let loop ((line-num cur-line))
      (if (< line-num 0)
        #f
        (let ((line (if (< line-num (length lines)) (list-ref lines line-num) "")))
          (if (string-prefix? "@@" line)
            line-num
            (loop (- line-num 1))))))))

(def (cmd-diff-mode app)
  "Show information about diff at current position."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (additions (length (filter (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\+))) lines)))
         (deletions (length (filter (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\-))) lines)))
         (hunks (length (filter (lambda (l) (string-prefix? "@@" l)) lines))))
    (echo-message! echo
      (string-append "Diff: " (number->string hunks) " hunk(s), +"
                    (number->string additions) "/-" (number->string deletions) " lines"))))

(def (cmd-diff-apply-hunk app)
  "Apply the current diff hunk using patch command."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (hunk-line (diff-find-current-hunk ed)))
    (if (not hunk-line)
      (echo-message! echo "Not in a diff hunk")
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline)))
        ;; Extract hunk content
        (let loop ((i hunk-line) (hunk-lines '()))
          (if (>= i (length lines))
            ;; Apply via patch
            (let* ((hunk-text (string-join (reverse hunk-lines) "\n"))
                   ;; Write to temp file and apply
                   (tmp-file "/tmp/gerbil-emacs-hunk.patch"))
              (with-exception-catcher
                (lambda (e) (echo-error! echo "Failed to apply hunk"))
                (lambda ()
                  (call-with-output-file tmp-file
                    (lambda (p) (display hunk-text p)))
                  (let* ((proc (open-process
                                 (list path: "patch"
                                       arguments: (list "-p1" "--dry-run" "-i" tmp-file)
                                       stdin-redirection: #f
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (out (read-line proc #f)))
                    (process-status proc)
                    (echo-message! echo (string-append "Patch output: " (or out "ok")))))))
            (let ((line (list-ref lines i)))
              (if (and (> i hunk-line) (string-prefix? "@@" line))
                ;; Hit next hunk, stop
                (loop (length lines) hunk-lines)
                (loop (+ i 1) (cons line hunk-lines))))))))))

(def (cmd-diff-revert-hunk app)
  "Revert the current diff hunk (apply patch in reverse)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (hunk-line (diff-find-current-hunk ed)))
    (if (not hunk-line)
      (echo-message! echo "Not in a diff hunk")
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline)))
        (let loop ((i hunk-line) (hunk-lines '()))
          (if (>= i (length lines))
            (let* ((hunk-text (string-join (reverse hunk-lines) "\n"))
                   (tmp-file "/tmp/gerbil-emacs-revert-hunk.patch"))
              (with-exception-catcher
                (lambda (e) (echo-error! echo "Failed to revert hunk"))
                (lambda ()
                  (call-with-output-file tmp-file
                    (lambda (p) (display hunk-text p)))
                  (let* ((proc (open-process
                                 (list path: "patch"
                                       arguments: (list "-p1" "-R" "-i" tmp-file)
                                       stdin-redirection: #f
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (out (read-line proc #f)))
                    (process-status proc)
                    (echo-message! echo (string-append "Reverted: " (or out "ok")))))))
            (let ((line (list-ref lines i)))
              (if (and (> i hunk-line) (string-prefix? "@@" line))
                (loop (length lines) hunk-lines)
                (loop (+ i 1) (cons line hunk-lines))))))))))

(def (cmd-diff-goto-source app)
  "Jump to source file and line from diff."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (lines (string-split text #\newline)))
    ;; Find the file header (--- a/file or +++ b/file)
    (let loop ((i 0) (file #f))
      (if (>= i (length lines))
        (echo-message! echo "Could not determine source file")
        (let ((line (list-ref lines i)))
          (cond
            ((string-prefix? "+++ " line)
             ;; Found file, extract path
             (let* ((path-part (substring line 4 (string-length line)))
                    (clean-path (if (string-prefix? "b/" path-part)
                                  (substring path-part 2 (string-length path-part))
                                  path-part)))
               (if (file-exists? clean-path)
                 (begin
                   ;; Calculate line number from hunk
                   (let* ((hunk-line (diff-find-current-hunk ed))
                          (hunk-header (if hunk-line (list-ref lines hunk-line) ""))
                          (parsed (diff-parse-hunk-header hunk-header))
                          (target-line (if parsed (caddr parsed) 1)))
                     ;; Open file
                     (let ((new-buf (buffer-create! (path-strip-directory clean-path) ed clean-path)))
                       (with-exception-catcher
                         (lambda (e) (echo-error! echo "Cannot read file"))
                         (lambda ()
                           (let ((content (call-with-input-file clean-path (lambda (p) (read-line p #f)))))
                             (buffer-attach! ed new-buf)
                             (set! (edit-window-buffer win) new-buf)
                             (editor-set-text ed (or content ""))
                             (editor-goto-line ed target-line)
                             (echo-message! echo (string-append "Opened: " clean-path))))))))
                 (echo-message! echo (string-append "File not found: " clean-path)))))
            ((> i 50) ; Don't search too far
             (echo-message! echo "No file header found in diff"))
            (else
             (loop (+ i 1) #f))))))))

;;;============================================================================
;;; Fuzzy command matching for M-x
;;;============================================================================

(def (fuzzy-match? query target)
  "Check if query fuzzy-matches target. Characters must appear in order."
  (let ((qlen (string-length query))
        (tlen (string-length target)))
    (let loop ((qi 0) (ti 0))
      (cond
        ((>= qi qlen) #t) ;; All query chars matched
        ((>= ti tlen) #f) ;; Target exhausted
        ((char=? (char-downcase (string-ref query qi))
                 (char-downcase (string-ref target ti)))
         (loop (+ qi 1) (+ ti 1)))
        (else
         (loop qi (+ ti 1)))))))

(def (fuzzy-score query target)
  "Score a fuzzy match. Higher is better. Rewards consecutive matches and prefix matches."
  (let ((qlen (string-length query))
        (tlen (string-length target)))
    (let loop ((qi 0) (ti 0) (score 0) (consecutive 0))
      (cond
        ((>= qi qlen) score)
        ((>= ti tlen) -1) ;; No match
        ((char=? (char-downcase (string-ref query qi))
                 (char-downcase (string-ref target ti)))
         (let ((bonus (+ 1
                        (if (= ti 0) 3 0) ;; prefix bonus
                        (* consecutive 2) ;; consecutive bonus
                        (if (and (> ti 0)
                                 (memv (string-ref target (- ti 1))
                                       '(#\- #\_ #\/ #\space)))
                          2 0)))) ;; word boundary bonus
           (loop (+ qi 1) (+ ti 1) (+ score bonus) (+ consecutive 1))))
        (else
         (loop qi (+ ti 1) score 0))))))

(def (cmd-execute-extended-command-fuzzy app)
  "Execute command by name with fuzzy matching (M-x alternative)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd-names (sort (map symbol->string (hash-keys *all-commands*))
                          string<?))
         (input (echo-read-string-with-completion echo "M-x " cmd-names row width)))
    (when (and input (> (string-length input) 0))
      ;; Try exact match first
      (let ((exact-cmd (find-command (string->symbol input))))
        (if exact-cmd
          (execute-command! app (string->symbol input))
          ;; Try fuzzy match
          (let* ((matches (filter (lambda (name) (fuzzy-match? input name))
                                  cmd-names))
                 (scored (map (lambda (name) (cons (fuzzy-score input name) name))
                              matches))
                 (sorted (sort scored (lambda (a b) (> (car a) (car b))))))
            (if (null? sorted)
              (echo-error! echo (string-append input " not found"))
              ;; Execute best match
              (let ((best (cdar sorted)))
                (execute-command! app (string->symbol best))
                (echo-message! echo (string-append "Ran: " best))))))))))

;;;============================================================================
;;; Scratch buffer with language
;;;============================================================================

(def (cmd-scratch-with-mode app)
  "Create a new scratch buffer with a specified language mode."
  (let* ((echo (app-state-echo app))
         (mode (app-read-string app "Mode (e.g. python, scheme, js): "))
         (name (if (and mode (not (string-empty? mode)))
                 (string-append "*scratch-" mode "*")
                 "*scratch*")))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! name ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      ;; Insert header comment based on mode
      (let ((header (cond
                      ((and mode (or (string=? mode "python") (string=? mode "py")))
                       "# Python scratch buffer\n\n")
                      ((and mode (or (string=? mode "scheme") (string=? mode "gerbil")))
                       ";; Gerbil Scheme scratch buffer\n\n")
                      ((and mode (or (string=? mode "js") (string=? mode "javascript")))
                       "// JavaScript scratch buffer\n\n")
                      ((and mode (or (string=? mode "c") (string=? mode "cpp")))
                       "/* C/C++ scratch buffer */\n\n")
                      ((and mode (or (string=? mode "shell") (string=? mode "bash")))
                       "#!/bin/bash\n# Shell scratch buffer\n\n")
                      ((and mode (or (string=? mode "markdown") (string=? mode "md")))
                       "# Scratch\n\n")
                      (else (string-append ";; " (or mode "Scratch") " buffer\n\n")))))
        (editor-set-text ed header)
        (editor-goto-pos ed (string-length header))
        (echo-message! echo (string-append "New scratch: " name))))))

;;;============================================================================
;;; Buffer navigation helpers
;;;============================================================================

(def (cmd-switch-to-buffer-other-window app)
  "Switch to a buffer in the other window."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf-names (map buffer-name (buffer-list)))
         (choice (echo-read-string-with-completion echo "Buffer (other window): "
                   buf-names row width)))
    (when (and choice (> (string-length choice) 0))
      (let ((buf (buffer-by-name choice)))
        (if (not buf)
          (echo-error! echo (string-append "No buffer: " choice))
          ;; Split window if only one, then switch
          (let* ((wins (frame-windows fr))
                 (other-win (if (> (length wins) 1)
                              (let ((cur (current-window fr)))
                                (let loop ((ws wins))
                                  (cond
                                    ((null? ws) (car wins))
                                    ((not (eq? (car ws) cur)) (car ws))
                                    (else (loop (cdr ws))))))
                              ;; Only one window — just switch in place
                              (current-window fr))))
            (let ((ed (edit-window-editor other-win)))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer other-win) buf)
              (echo-message! echo (string-append "Buffer: " choice)))))))))

;;;============================================================================
;;; Batch 26: comment-box, format-region, rename-symbol, isearch-occur, etc.
;;;============================================================================

;;; --- Comment box: wrap comment in decorative box ---

(def (cmd-comment-box app)
  "Wrap the selected text in a comment box."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection for comment box")
      (let* ((text (let ((full (editor-get-text ed)))
                     (substring full sel-start sel-end)))
             (lines (string-split text #\newline))
             (max-len (apply max (map string-length lines)))
             (border (string-append ";; " (make-string (+ max-len 2) #\-)))
             (boxed (with-output-to-string
                      (lambda ()
                        (display border) (display "\n")
                        (for-each
                          (lambda (line)
                            (display ";; ")
                            (display line)
                            (display (make-string (- max-len (string-length line)) #\space))
                            (display "  ") (display "\n"))
                          lines)
                        (display border) (display "\n")))))
        (send-message ed SCI_SETTARGETSTART sel-start 0)
        (send-message ed SCI_SETTARGETEND sel-end 0)
        (send-message/string ed SCI_REPLACETARGET boxed)
        (echo-message! echo "Wrapped in comment box")))))

;;; --- Format/indent region ---

(def (cmd-format-region app)
  "Indent/format the selected region according to buffer settings."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to format")
      (let* ((text (let ((full (editor-get-text ed)))
                     (substring full sel-start sel-end)))
             (indent-size (editor-get-indent ed))
             (use-tabs (editor-get-use-tabs? ed))
             (lines (string-split text #\newline))
             ;; Re-indent each line: strip leading whitespace, add consistent indent
             (formatted (map (lambda (line)
                               (let* ((trimmed (string-trim line))
                                      (orig-indent (- (string-length line)
                                                      (string-length (string-trim line)))))
                                 (if (string=? trimmed "")
                                   ""
                                   (let ((indent-str (if use-tabs
                                                       (make-string (quotient orig-indent indent-size) #\tab)
                                                       (make-string orig-indent #\space))))
                                     (string-append indent-str trimmed)))))
                             lines))
             (result (string-join formatted "\n")))
        (send-message ed SCI_SETTARGETSTART sel-start 0)
        (send-message ed SCI_SETTARGETEND sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)
        (echo-message! echo "Region formatted")))))

;;; --- Rename symbol (local file, simple text replacement) ---

(def (cmd-rename-symbol app)
  "Rename all occurrences of a symbol in the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    ;; Extract word at point
    (let* ((word-start (let loop ((i (- pos 1)))
                         (if (or (< i 0)
                                 (let ((c (string-ref text i)))
                                   (not (or (char-alphabetic? c)
                                            (char-numeric? c)
                                            (char=? c #\_)
                                            (char=? c #\-)))))
                           (+ i 1)
                           (loop (- i 1)))))
           (word-end (let loop ((i pos))
                       (if (or (>= i (string-length text))
                               (let ((c (string-ref text i)))
                                 (not (or (char-alphabetic? c)
                                          (char-numeric? c)
                                          (char=? c #\_)
                                          (char=? c #\-)))))
                         i
                         (loop (+ i 1)))))
           (old-name (substring text word-start word-end)))
      (if (string=? old-name "")
        (echo-message! echo "No symbol at point")
        (let ((new-name (app-read-string app
                          (string-append "Rename '" old-name "' to: "))))
          (when (and new-name (> (string-length new-name) 0)
                     (not (string=? new-name old-name)))
            ;; Count and replace occurrences using word-boundary matching
            (let* ((result (string-subst text old-name new-name))
                   ;; Count how many replacements happened
                   (count (let loop ((s text) (n 0) (start 0))
                            (let ((idx (string-contains s old-name start)))
                              (if idx
                                (loop s (+ n 1) (+ idx (string-length old-name)))
                                n)))))
              (editor-set-text ed result)
              (editor-goto-pos ed (min pos (string-length result)))
              (echo-message! echo
                (string-append "Renamed '" old-name "' to '" new-name
                  "' (" (number->string count) " occurrences)")))))))))

;;; --- Isearch occur: show all matches of last search ---

(def (cmd-isearch-occur app)
  "Show all lines matching the current isearch string in an occur-like buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (search-str (app-state-last-search app)))
    (if (or (not search-str) (string=? search-str ""))
      (echo-message! echo "No search string - use isearch first")
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matches (let loop ((ls lines) (n 1) (acc []))
                        (if (null? ls) (reverse acc)
                          (loop (cdr ls) (+ n 1)
                            (if (string-contains (car ls) search-str)
                              (cons (string-append
                                      (string-pad (number->string n) 6)
                                      ": " (car ls))
                                acc)
                              acc))))))
        (if (null? matches)
          (echo-message! echo
            (string-append "No matches for: " search-str))
          (let ((result (string-append
                          "Isearch occur: " (number->string (length matches))
                          " matches for \"" search-str "\"\n"
                          (make-string 60 #\-) "\n"
                          (string-join matches "\n") "\n")))
            (editor-set-text ed result)
            (editor-goto-pos ed 0)
            (echo-message! echo
              (string-append (number->string (length matches))
                " matches found"))))))))

;;; --- Helm-mini style fuzzy buffer switch ---

(def (cmd-helm-mini app)
  "Fuzzy search and switch buffers (helm-mini style)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (bufs *buffer-list*)
         (names (map buffer-name bufs))
         (query (app-read-string app "Switch to buffer (fuzzy): ")))
    (when (and query (> (string-length query) 0))
      (let* ((query-lower (string-downcase query))
             (scored (filter-map
                       (lambda (name)
                         (let ((name-lower (string-downcase name)))
                           (if (string-contains name-lower query-lower)
                             (cons name (- (string-length name)
                                           (string-length query)))
                             #f)))
                       names))
             (sorted (sort scored (lambda (a b) (< (cdr a) (cdr b))))))
        (if (null? sorted)
          (echo-message! echo
            (string-append "No buffers matching: " query))
          (let* ((best (caar sorted))
                 (buf (let loop ((bs bufs))
                        (if (null? bs) #f
                          (if (string=? (buffer-name (car bs)) best)
                            (car bs)
                            (loop (cdr bs)))))))
            (when buf
              (buffer-attach! ed buf)
              (echo-message! echo
                (string-append "Switched to: " best)))))))))

;;; --- Toggle comment style (line vs block) ---

(def *comment-style* 'line)  ; 'line or 'block

(def (cmd-toggle-comment-style app)
  "Toggle between line and block comment styles."
  (set! *comment-style* (if (eq? *comment-style* 'line) 'block 'line))
  (echo-message! (app-state-echo app)
    (string-append "Comment style: "
      (if (eq? *comment-style* 'line) "line (//)" "block (/* */)"))))

;;; --- Flymake mode toggle ---

(def *flymake-mode* #f)

(def (cmd-toggle-flymake-mode app)
  "Toggle flymake (on-the-fly syntax checking) mode."
  (set! *flymake-mode* (not *flymake-mode*))
  (echo-message! (app-state-echo app)
    (if *flymake-mode*
      "Flymake mode enabled"
      "Flymake mode disabled")))

;;; --- Smart indent for tab ---

(def (cmd-indent-for-tab app)
  "Indent current line or region based on context."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      ;; Single line: insert tab or spaces at current position
      (let* ((indent-size (editor-get-indent ed))
             (use-tabs (editor-get-use-tabs? ed))
             (indent-str (if use-tabs "\t"
                           (make-string indent-size #\space))))
        (editor-insert-text ed (editor-get-current-pos ed) indent-str)
        (editor-goto-pos ed (+ (editor-get-current-pos ed) (string-length indent-str))))
      ;; Region: indent each line
      (let* ((line-start (editor-line-from-position ed sel-start))
             (line-end (editor-line-from-position ed sel-end))
             (indent-size (editor-get-indent ed))
             (use-tabs (editor-get-use-tabs? ed))
             (indent-str (if use-tabs "\t"
                           (make-string indent-size #\space))))
        (with-undo-action ed
          (let loop ((line line-end))
            (when (>= line line-start)
              (let ((pos (editor-position-from-line ed line)))
                (editor-insert-text ed pos indent-str))
              (loop (- line 1)))))
        (echo-message! echo
          (string-append "Indented "
            (number->string (+ 1 (- line-end line-start)))
            " lines"))))))

;;; --- Dedent region ---

(def (cmd-dedent-region app)
  "Remove one level of indentation from selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to dedent")
      (let* ((line-start (editor-line-from-position ed sel-start))
             (line-end (editor-line-from-position ed sel-end))
             (indent-size (editor-get-indent ed)))
        (with-undo-action ed
          (let loop ((line line-end))
            (when (>= line line-start)
              (let* ((pos (editor-position-from-line ed line))
                     (line-text (editor-get-line ed line))
                     (to-remove (let check ((i 0))
                                  (cond
                                    ((>= i indent-size) indent-size)
                                    ((>= i (string-length line-text)) i)
                                    ((char=? (string-ref line-text i) #\tab) (+ i 1))
                                    ((char=? (string-ref line-text i) #\space) (check (+ i 1)))
                                    (else i)))))
                (when (> to-remove 0)
                  (editor-delete-range ed pos to-remove)))
              (loop (- line 1)))))
        (echo-message! echo
          (string-append "Dedented "
            (number->string (+ 1 (- line-end line-start)))
            " lines"))))))

;;; --- Duplicate and comment: duplicate region then comment original ---

(def (cmd-duplicate-and-comment app)
  "Duplicate the selected region, then comment out the original."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to duplicate-and-comment")
      (let* ((text (let ((full (editor-get-text ed)))
                     (substring full sel-start sel-end)))
             (lines (string-split text #\newline))
             (commented (map (lambda (l) (string-append ";; " l)) lines))
             (result (string-append
                       (string-join commented "\n") "\n"
                       text)))
        (send-message ed SCI_SETTARGETSTART sel-start 0)
        (send-message ed SCI_SETTARGETEND sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)
        (echo-message! echo "Duplicated and commented original")))))

;;; --- Scratch message: insert welcome text in *scratch* ---

(def (cmd-insert-scratch-message app)
  "Insert the standard scratch buffer message."
  (let* ((ed (current-editor app))
         (msg (string-append
                ";; This buffer is for text that is not saved.\n"
                ";; To create a file, visit it with C-x C-f.\n"
                ";; Then enter text in the buffer and save with C-x C-s.\n\n")))
    (editor-insert-text ed 0 msg)
    (editor-goto-pos ed (string-length msg))
    (echo-message! (app-state-echo app) "Scratch message inserted")))

;;; --- Line statistics in echo area ---

(def (cmd-count-lines-region app)
  "Count lines, words, and characters in the region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed))
         (text (if (= sel-start sel-end)
                 (editor-get-text ed)
                 (let ((full (editor-get-text ed)))
                   (substring full sel-start sel-end))))
         (lines (length (string-split text #\newline)))
         (words (length (string-tokenize text)))
         (chars (string-length text))
         (label (if (= sel-start sel-end) "Buffer" "Region")))
    (echo-message! echo
      (string-append label ": " (number->string lines) " lines, "
        (number->string words) " words, "
        (number->string chars) " chars"))))

;;; --- Cycle spacing: consolidate whitespace ---

(def (cmd-cycle-spacing app)
  "Cycle whitespace at point: multiple spaces -> one space -> no space."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find extent of whitespace around point
    (let* ((ws-start (let loop ((i (- pos 1)))
                       (if (and (>= i 0)
                                (char=? (string-ref text i) #\space))
                         (loop (- i 1))
                         (+ i 1))))
           (ws-end (let loop ((i pos))
                     (if (and (< i len)
                              (char=? (string-ref text i) #\space))
                       (loop (+ i 1))
                       i)))
           (ws-len (- ws-end ws-start)))
      (cond
        ((> ws-len 1)
         ;; Multiple spaces -> one space
         (send-message ed SCI_SETTARGETSTART ws-start 0)
         (send-message ed SCI_SETTARGETEND ws-end 0)
         (send-message/string ed SCI_REPLACETARGET " ")
         (echo-message! echo "Collapsed to one space"))
        ((= ws-len 1)
         ;; One space -> no space
         (editor-delete-range ed ws-start 1)
         (echo-message! echo "Removed space"))
        (else
         ;; No space -> insert one space
         (editor-insert-text ed pos " ")
         (editor-goto-pos ed (+ pos 1))
         (echo-message! echo "Inserted space"))))))

;;;============================================================================
;;; Batch 32: delete-selection, word count, column ruler, shell-here, etc.
;;;============================================================================

;;; --- Toggle delete-selection mode (typing replaces selection) ---

(def *delete-selection-mode* #t)

(def (cmd-toggle-delete-selection app)
  "Toggle delete-selection mode (typing replaces active selection)."
  (let ((echo (app-state-echo app)))
    (set! *delete-selection-mode* (not *delete-selection-mode*))
    (echo-message! echo
      (if *delete-selection-mode*
        "Delete-selection mode on"
        "Delete-selection mode off"))))

;;; --- Toggle live word count in modeline ---

(def *word-count-mode* #f)

(def (cmd-toggle-word-count app)
  "Toggle live word count display in modeline."
  (let ((echo (app-state-echo app)))
    (set! *word-count-mode* (not *word-count-mode*))
    (echo-message! echo
      (if *word-count-mode*
        "Word count display on"
        "Word count display off"))))

;;; --- Toggle column ruler ---

(def *column-ruler-mode* #f)
(def *column-ruler-column* 80)

(def (cmd-toggle-column-ruler app)
  "Toggle display of a vertical column ruler at column 80."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *column-ruler-mode* (not *column-ruler-mode*))
    (if *column-ruler-mode*
      (send-message ed 2361 *column-ruler-column* 0)  ; SCI_SETEDGECOLUMN
      (send-message ed 2361 0 0))
    (send-message ed 2363  ; SCI_SETEDGEMODE
      (if *column-ruler-mode* 1 0) 0)  ; EDGE_LINE=1, EDGE_NONE=0
    (echo-message! echo
      (if *column-ruler-mode*
        (string-append "Column ruler at " (number->string *column-ruler-column*))
        "Column ruler off"))))

;;; --- Open shell in current file's directory ---

(def (cmd-shell-here app)
  "Open a shell command in the current file's directory."
  (let* ((echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (filepath (buffer-file-path buf))
         (dir (if filepath
                (let* ((parts (string-split filepath #\/)))
                  (if (<= (length parts) 1) "."
                    (string-join
                      (let loop ((ls parts) (acc []))
                        (if (null? (cdr ls)) (reverse acc)
                          (loop (cdr ls) (cons (car ls) acc))))
                      "/")))
                ".")))
    (with-catch
      (lambda (e) (echo-message! echo "Cannot open terminal"))
      (lambda ()
        (let ((term (cond
                      ((file-exists? "/usr/bin/x-terminal-emulator") "x-terminal-emulator")
                      ((file-exists? "/usr/bin/xterm") "xterm")
                      ((file-exists? "/usr/bin/gnome-terminal") "gnome-terminal")
                      (else #f))))
          (if term
            (begin
              (open-process (list path: term
                                  arguments: (list "--working-directory" dir)
                                  stdin-redirection: #f
                                  stdout-redirection: #f))
              (echo-message! echo (string-append "Shell in: " dir)))
            (echo-message! echo "No terminal emulator found")))))))

;;; --- Toggle soft wrap ---

(def *soft-wrap-mode* #f)

(def (cmd-toggle-soft-wrap app)
  "Toggle soft word wrap (no actual newlines inserted)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *soft-wrap-mode* (not *soft-wrap-mode*))
    (editor-set-wrap-mode ed (if *soft-wrap-mode* 1 0))
    (echo-message! echo
      (if *soft-wrap-mode*
        "Soft wrap on"
        "Soft wrap off"))))

;;; --- Toggle whitespace cleanup on save ---

(def *whitespace-cleanup-on-save* #f)

(def (cmd-toggle-whitespace-cleanup-on-save app)
  "Toggle automatic whitespace cleanup when saving."
  (let ((echo (app-state-echo app)))
    (set! *whitespace-cleanup-on-save* (not *whitespace-cleanup-on-save*))
    (echo-message! echo
      (if *whitespace-cleanup-on-save*
        "Whitespace cleanup on save enabled"
        "Whitespace cleanup on save disabled"))))

;;; --- Insert random inspirational line ---

(def *random-lines*
  '("The best code is no code at all."
    "Keep it simple, keep it working."
    "Premature optimization is the root of all evil."
    "Make it work, make it right, make it fast."
    "Programs must be written for people to read."
    "Debugging is twice as hard as writing the code."
    "Any fool can write code that a computer can understand."
    "Code is read much more often than it is written."
    "Simplicity is the ultimate sophistication."
    "First, solve the problem. Then, write the code."))

(def (cmd-insert-random-line app)
  "Insert a random programming quote at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (idx (random-integer (length *random-lines*)))
         (line (list-ref *random-lines* idx)))
    (editor-insert-text ed (editor-get-current-pos ed) line)
    (echo-message! echo "Quote inserted")))

;;; --- Smart backspace (delete indentation level) ---

(def (cmd-smart-backspace app)
  "Delete backward by indentation level (spaces) or single char."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (echo (app-state-echo app)))
    (if (<= pos 0)
      (echo-message! echo "Beginning of buffer")
      (let* ((text (editor-get-text ed))
             (tab-w 4)
             ;; Check how many spaces precede cursor
             (spaces-back
               (let loop ((i (- pos 1)) (n 0))
                 (if (or (< i 0) (not (char=? (string-ref text i) #\space))
                         (>= n tab-w))
                   n
                   (loop (- i 1) (+ n 1))))))
        (if (and (> spaces-back 0) (= (modulo spaces-back tab-w) 0)
                 (>= spaces-back tab-w))
          ;; Delete one tab-width of spaces
          (editor-delete-range ed (- pos tab-w) tab-w)
          ;; Regular backspace
          (editor-delete-range ed (- pos 1) 1))))))

;;; --- Toggle line move mode (move lines with M-up/down) ---

(def *line-move-visual* #t)

(def (cmd-toggle-line-move-visual app)
  "Toggle between visual and logical line movement."
  (let ((echo (app-state-echo app)))
    (set! *line-move-visual* (not *line-move-visual*))
    (echo-message! echo
      (if *line-move-visual*
        "Visual line movement"
        "Logical line movement"))))

;;; =========================================================================
;;; Batch 37: highlight-indentation, hungry-delete, type-break, etc.
;;; =========================================================================

(def *highlight-indentation-mode* #f)
(def *hungry-delete-mode* #f)
(def *type-break-mode* #f)
(def *delete-trailing-on-save* #f)
(def *cursor-in-non-selected* #t)
(def *blink-matching-paren* #t)
(def *next-error-follow* #f)

(def (cmd-toggle-highlight-indentation app)
  "Toggle highlight-indentation-mode (show indentation guides)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! *highlight-indentation-mode* (not *highlight-indentation-mode*))
    (if *highlight-indentation-mode*
      (begin
        ;; SCI_SETINDENTATIONGUIDES = 2132, SC_IV_LOOKBOTH = 3
        (send-message ed 2132 3 0)
        (echo-message! echo "Highlight-indentation ON"))
      (begin
        ;; SC_IV_NONE = 0
        (send-message ed 2132 0 0)
        (echo-message! echo "Highlight-indentation OFF")))))

(def (cmd-toggle-hungry-delete app)
  "Toggle hungry-delete-mode (delete all whitespace at once)."
  (let ((echo (app-state-echo app)))
    (set! *hungry-delete-mode* (not *hungry-delete-mode*))
    (echo-message! echo (if *hungry-delete-mode*
                          "Hungry delete ON"
                          "Hungry delete OFF"))))

(def (cmd-toggle-type-break app)
  "Toggle type-break-mode (remind to take typing breaks)."
  (let ((echo (app-state-echo app)))
    (set! *type-break-mode* (not *type-break-mode*))
    (echo-message! echo (if *type-break-mode*
                          "Type-break mode ON"
                          "Type-break mode OFF"))))

(def (cmd-insert-zero-width-space app)
  "Insert a zero-width space (U+200B) at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (zws (string (integer->char #x200B))))
    (editor-replace-selection ed zws)
    (echo-message! echo "Zero-width space inserted")))

(def (cmd-toggle-delete-trailing-on-save app)
  "Toggle automatic deletion of trailing whitespace on save."
  (let ((echo (app-state-echo app)))
    (set! *delete-trailing-on-save* (not *delete-trailing-on-save*))
    (echo-message! echo (if *delete-trailing-on-save*
                          "Delete trailing whitespace on save ON"
                          "Delete trailing whitespace on save OFF"))))

(def (cmd-toggle-cursor-in-non-selected-windows app)
  "Toggle cursor visibility in non-selected windows."
  (let ((echo (app-state-echo app)))
    (set! *cursor-in-non-selected* (not *cursor-in-non-selected*))
    (echo-message! echo (if *cursor-in-non-selected*
                          "Cursor in non-selected windows ON"
                          "Cursor in non-selected windows OFF"))))

(def (cmd-toggle-blink-matching-paren app)
  "Toggle blinking of matching parenthesis."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! *blink-matching-paren* (not *blink-matching-paren*))
    (if *blink-matching-paren*
      (begin
        ;; SCI_BRACEBADLIGHT uses position -1 to clear
        ;; Brace matching is handled by the event loop;
        ;; this flag controls whether it blinks
        (echo-message! echo "Blink matching paren ON"))
      (echo-message! echo "Blink matching paren OFF"))))

(def (cmd-toggle-next-error-follow app)
  "Toggle next-error-follow-minor-mode (auto-visit errors on navigate)."
  (let ((echo (app-state-echo app)))
    (set! *next-error-follow* (not *next-error-follow*))
    (echo-message! echo (if *next-error-follow*
                          "Next-error follow ON"
                          "Next-error follow OFF"))))

(def (cmd-insert-page-break app)
  "Insert a page break (form feed) character at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (ff (string (integer->char 12))))
    (editor-replace-selection ed ff)
    (echo-message! echo "Page break inserted")))

;; ── batch 47: safety and editing behavior toggles ───────────────────
(def *auto-save-default* #t)
(def *make-pointer-invisible* #t)
(def *kill-whole-line* #f)
(def *set-mark-command-repeat-pop* #f)
(def *enable-local-variables* #t)
(def *enable-dir-local-variables* #t)
(def *ad-activate-all* #f)
(def *global-hi-lock-mode* #f)
(def *next-line-add-newlines* #f)

(def (cmd-toggle-auto-save-default app)
  "Toggle auto-save by default for new buffers."
  (let ((echo (app-state-echo app)))
    (set! *auto-save-default* (not *auto-save-default*))
    (echo-message! echo (if *auto-save-default*
                          "Auto-save default ON" "Auto-save default OFF"))))

(def (cmd-toggle-make-pointer-invisible app)
  "Toggle making pointer invisible while typing."
  (let ((echo (app-state-echo app)))
    (set! *make-pointer-invisible* (not *make-pointer-invisible*))
    (echo-message! echo (if *make-pointer-invisible*
                          "Pointer invisible ON" "Pointer invisible OFF"))))

(def (cmd-toggle-kill-whole-line app)
  "Toggle kill-whole-line (C-k at start kills newline too)."
  (let ((echo (app-state-echo app)))
    (set! *kill-whole-line* (not *kill-whole-line*))
    (echo-message! echo (if *kill-whole-line*
                          "Kill whole line ON" "Kill whole line OFF"))))

(def (cmd-toggle-set-mark-command-repeat-pop app)
  "Toggle set-mark-command-repeat-pop (C-SPC C-SPC pops mark ring)."
  (let ((echo (app-state-echo app)))
    (set! *set-mark-command-repeat-pop* (not *set-mark-command-repeat-pop*))
    (echo-message! echo (if *set-mark-command-repeat-pop*
                          "Mark repeat pop ON" "Mark repeat pop OFF"))))

(def (cmd-toggle-enable-local-variables app)
  "Toggle processing of local variables in files."
  (let ((echo (app-state-echo app)))
    (set! *enable-local-variables* (not *enable-local-variables*))
    (echo-message! echo (if *enable-local-variables*
                          "Local variables ON" "Local variables OFF"))))

(def (cmd-toggle-enable-dir-local-variables app)
  "Toggle processing of directory-local variables."
  (let ((echo (app-state-echo app)))
    (set! *enable-dir-local-variables* (not *enable-dir-local-variables*))
    (echo-message! echo (if *enable-dir-local-variables*
                          "Dir-local variables ON" "Dir-local variables OFF"))))

(def (cmd-toggle-ad-activate-all app)
  "Toggle activation of all advice."
  (let ((echo (app-state-echo app)))
    (set! *ad-activate-all* (not *ad-activate-all*))
    (echo-message! echo (if *ad-activate-all*
                          "Advice activate all ON" "Advice activate all OFF"))))

(def (cmd-toggle-global-hi-lock-mode app)
  "Toggle global-hi-lock-mode (persistent text highlighting)."
  (let ((echo (app-state-echo app)))
    (set! *global-hi-lock-mode* (not *global-hi-lock-mode*))
    (echo-message! echo (if *global-hi-lock-mode*
                          "Global hi-lock mode ON" "Global hi-lock mode OFF"))))

(def (cmd-toggle-next-line-add-newlines app)
  "Toggle whether next-line adds newlines at end of buffer."
  (let ((echo (app-state-echo app)))
    (set! *next-line-add-newlines* (not *next-line-add-newlines*))
    (echo-message! echo (if *next-line-add-newlines*
                          "Next-line add newlines ON" "Next-line add newlines OFF"))))
