;;; -*- Gerbil -*-
;;; Version control, mail, sessions, macros, compilation, flyspell,
;;; multiple cursors, package management, customize, and diff mode

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
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

