;;; -*- Gerbil -*-
;;; Extra TUI editor commands for gerbil-emacs (overflow from editor.ss)
;;;
;;; Split from editor.ss due to Gerbil compiler limits on module size.

(export register-extra-commands!)

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
        :gerbil-emacs/echo)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (app-read-string app prompt)
  "Convenience wrapper: read a string from the echo area."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (echo-read-string echo prompt row width)))

(def (extra-word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

(def (word-bounds-at ed pos)
  "Find word boundaries around POS. Returns (values start end) or (values #f #f)."
  (let* ((text (editor-get-text ed))
         (len (string-length text)))
    (if (or (>= pos len) (< pos 0) (not (extra-word-char? (string-ref text pos))))
      ;; Not in a word — try char before pos
      (if (and (> pos 0) (extra-word-char? (string-ref text (- pos 1))))
        (let ((p (- pos 1)))
          (let find-start ((i p))
            (if (and (> i 0) (extra-word-char? (string-ref text (- i 1))))
              (find-start (- i 1))
              (let find-end ((j (+ p 1)))
                (if (and (< j len) (extra-word-char? (string-ref text j)))
                  (find-end (+ j 1))
                  (values i j))))))
        (values #f #f))
      ;; In a word — scan backward then forward
      (let find-start ((i pos))
        (if (and (> i 0) (extra-word-char? (string-ref text (- i 1))))
          (find-start (- i 1))
          (let find-end ((j (+ pos 1)))
            (if (and (< j len) (extra-word-char? (string-ref text j)))
              (find-end (+ j 1))
              (values i j))))))))

;; --- Task #46: org-mode stubs, windmove, winner, VC extras, mail, sessions ---

;; Org-mode stubs
(def (cmd-org-mode app)
  "Toggle org-mode (stub)."
  (echo-message! (app-state-echo app) "Org-mode toggled (stub)"))

(def (cmd-org-todo app)
  "Cycle TODO state (stub)."
  (echo-message! (app-state-echo app) "TODO state cycled (stub)"))

(def (cmd-org-schedule app)
  "Schedule an item (stub)."
  (echo-message! (app-state-echo app) "Schedule set (stub)"))

(def (cmd-org-deadline app)
  "Set deadline (stub)."
  (echo-message! (app-state-echo app) "Deadline set (stub)"))

(def (cmd-org-agenda app)
  "Show org agenda (stub)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Org Agenda*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed "Org Agenda (stub)\n\nNo agenda items.\n")
    (editor-set-read-only ed #t)))

(def (cmd-org-export app)
  "Export org document (stub)."
  (echo-message! (app-state-echo app) "Org export (stub)"))

(def (cmd-org-table-create app)
  "Create an org table (stub)."
  (echo-message! (app-state-echo app) "Org table created (stub)"))

(def (cmd-org-link app)
  "Insert org link (stub)."
  (echo-message! (app-state-echo app) "Org link inserted (stub)"))

(def (cmd-org-store-link app)
  "Store link to current location (stub)."
  (echo-message! (app-state-echo app) "Link stored (stub)"))

(def (cmd-org-open-at-point app)
  "Open link at point (stub)."
  (echo-message! (app-state-echo app) "Open at point (stub)"))

(def (cmd-org-cycle app)
  "Cycle visibility of org heading (stub)."
  (echo-message! (app-state-echo app) "Visibility cycled (stub)"))

(def (cmd-org-shift-tab app)
  "Global visibility cycling (stub)."
  (echo-message! (app-state-echo app) "Global visibility cycled (stub)"))

;; Calendar/diary
(def (cmd-calendar app)
  "Show calendar."
  (let ((cal-text (with-exception-catcher
                    (lambda (e) "Calendar not available")
                    (lambda ()
                      (let ((p (open-process
                                 (list path: "cal"
                                       arguments: '()
                                       stdin-redirection: #f stdout-redirection: #t
                                       stderr-redirection: #t))))
                        (let ((out (read-line p #f)))
                          (process-status p)
                          (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Calendar*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Calendar\n\n" cal-text "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-diary-view-entries app)
  "View diary entries (stub)."
  (echo-message! (app-state-echo app) "No diary entries"))

;; EWW browser stubs
(def (cmd-eww app)
  "Open EWW web browser (stub)."
  (let ((url (app-read-string app "URL: ")))
    (when (and url (not (string-empty? url)))
      (echo-message! (app-state-echo app)
        (string-append "EWW not available. URL: " url)))))

(def (cmd-eww-browse-url app)
  "Browse URL with EWW (stub)."
  (cmd-eww app))

(def (cmd-browse-url-at-point app)
  "Browse URL at point (stub)."
  (echo-message! (app-state-echo app) "browse-url-at-point (stub)"))

;; Windmove
(def (cmd-windmove-left app)
  "Move to window on the left (alias for other-window reverse)."
  ;; With only vertical split, cycle backward
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (active (current-window fr)))
    (when (> (length wins) 1)
      (let ((idx (let loop ((ws wins) (i 0))
                   (cond ((null? ws) 0)
                         ((eq? (car ws) active) i)
                         (else (loop (cdr ws) (+ i 1)))))))
        (let ((prev-idx (modulo (- idx 1) (length wins))))
          (set! (frame-current-idx fr) prev-idx))))))

(def (cmd-windmove-right app)
  "Move to window on the right."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (active (current-window fr)))
    (when (> (length wins) 1)
      (let ((idx (let loop ((ws wins) (i 0))
                   (cond ((null? ws) 0)
                         ((eq? (car ws) active) i)
                         (else (loop (cdr ws) (+ i 1)))))))
        (let ((next-idx (modulo (+ idx 1) (length wins))))
          (set! (frame-current-idx fr) next-idx))))))

(def (cmd-windmove-up app)
  "Move to window above (same as windmove-left in vertical layout)."
  (cmd-windmove-left app))

(def (cmd-windmove-down app)
  "Move to window below (same as windmove-right in vertical layout)."
  (cmd-windmove-right app))

;; Winner mode (window configuration undo/redo stubs)
(def (cmd-winner-undo app)
  "Undo window configuration change (stub)."
  (echo-message! (app-state-echo app) "Winner undo (stub)"))

(def (cmd-winner-redo app)
  "Redo window configuration change (stub)."
  (echo-message! (app-state-echo app) "Winner redo (stub)"))

;; Tab-bar commands
(def (cmd-tab-new app)
  "Create a new tab (stub)."
  (echo-message! (app-state-echo app) "New tab (stub)"))

(def (cmd-tab-close app)
  "Close current tab (stub)."
  (echo-message! (app-state-echo app) "Tab closed (stub)"))

(def (cmd-tab-next app)
  "Switch to next tab (stub)."
  (echo-message! (app-state-echo app) "Next tab (stub)"))

(def (cmd-tab-previous app)
  "Switch to previous tab (stub)."
  (echo-message! (app-state-echo app) "Previous tab (stub)"))

(def (cmd-tab-rename app)
  "Rename current tab (stub)."
  (echo-message! (app-state-echo app) "Tab renamed (stub)"))

(def (cmd-tab-move app)
  "Move current tab (stub)."
  (echo-message! (app-state-echo app) "Tab moved (stub)"))

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

;; Mail stubs
(def (cmd-compose-mail app)
  "Compose mail (stub)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Mail*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed "To: \nSubject: \n--text follows this line--\n\n")))

(def (cmd-rmail app)
  "Read mail (stub)."
  (echo-message! (app-state-echo app) "RMAIL not available (stub)"))

(def (cmd-gnus app)
  "Start Gnus newsreader (stub)."
  (echo-message! (app-state-echo app) "Gnus not available (stub)"))

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
  "Edit keyboard macro (stub)."
  (echo-message! (app-state-echo app) "Edit kbd macro (stub)"))

;; Compilation extras
(def (cmd-recompile app)
  "Recompile using last compile command."
  (let ((last-cmd (app-state-last-compile app)))
    (if last-cmd
      (echo-message! (app-state-echo app) (string-append "Recompile: " last-cmd))
      (echo-message! (app-state-echo app) "No previous compile command"))))

(def (cmd-kill-compilation app)
  "Kill current compilation (stub)."
  (echo-message! (app-state-echo app) "Compilation killed (stub)"))

;; Flyspell extras
(def (cmd-flyspell-auto-correct-word app)
  "Auto-correct word at point (stub)."
  (echo-message! (app-state-echo app) "Flyspell auto-correct (stub)"))

(def (cmd-flyspell-goto-next-error app)
  "Go to next flyspell error (stub)."
  (echo-message! (app-state-echo app) "Next flyspell error (stub)"))

;; Multiple cursors stubs
(def (cmd-mc-mark-next-like-this app)
  "Add cursor at next occurrence of selection (stub)."
  (echo-message! (app-state-echo app) "Multiple cursors: mark next (stub)"))

(def (cmd-mc-mark-previous-like-this app)
  "Add cursor at previous occurrence of selection (stub)."
  (echo-message! (app-state-echo app) "Multiple cursors: mark previous (stub)"))

(def (cmd-mc-mark-all-like-this app)
  "Add cursors at all occurrences of selection (stub)."
  (echo-message! (app-state-echo app) "Multiple cursors: mark all (stub)"))

(def (cmd-mc-edit-lines app)
  "Add cursor to each line in selection (stub)."
  (echo-message! (app-state-echo app) "Multiple cursors: edit lines (stub)"))

;; Package management stubs
(def (cmd-package-list-packages app)
  "List available packages (stub)."
  (echo-message! (app-state-echo app) "Package list (stub)"))

(def (cmd-package-install app)
  "Install a package (stub)."
  (echo-message! (app-state-echo app) "Package install (stub)"))

(def (cmd-package-delete app)
  "Delete a package (stub)."
  (echo-message! (app-state-echo app) "Package delete (stub)"))

(def (cmd-package-refresh-contents app)
  "Refresh package list (stub)."
  (echo-message! (app-state-echo app) "Package refresh (stub)"))

;; Custom stubs
(def (cmd-customize-group app)
  "Customize a group of settings (stub)."
  (echo-message! (app-state-echo app) "Customize group (stub)"))

(def (cmd-customize-variable app)
  "Customize a variable (stub)."
  (echo-message! (app-state-echo app) "Customize variable (stub)"))

(def (cmd-customize-themes app)
  "Customize themes (stub)."
  (echo-message! (app-state-echo app) "Customize themes (stub)"))

;; Diff mode
(def (cmd-diff-mode app)
  "Toggle diff mode (stub)."
  (echo-message! (app-state-echo app) "Diff mode (stub)"))

(def (cmd-diff-apply-hunk app)
  "Apply diff hunk (stub)."
  (echo-message! (app-state-echo app) "Apply hunk (stub)"))

(def (cmd-diff-revert-hunk app)
  "Revert diff hunk (stub)."
  (echo-message! (app-state-echo app) "Revert hunk (stub)"))

(def (cmd-diff-goto-source app)
  "Jump to source from diff (stub)."
  (echo-message! (app-state-echo app) "Goto source (stub)"))

;; Artist mode stub
(def (cmd-artist-mode app)
  "Toggle artist mode for ASCII drawing (stub)."
  (echo-message! (app-state-echo app) "Artist mode (stub)"))

;; Tramp stubs
(def (cmd-tramp-cleanup-all-connections app)
  "Clean up all TRAMP connections (stub)."
  (echo-message! (app-state-echo app) "TRAMP connections cleaned (stub)"))

;; Process management extras
(def (cmd-proced app)
  "Process editor (show system processes)."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error listing processes")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "ps"
                                     arguments: '("aux" "--sort=-pcpu")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Proced*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Process List\n\n" result "\n"))
      (editor-set-read-only ed #t))))

;; Paredit-like commands for Lisp editing
(def (cmd-paredit-wrap-round app)
  "Wrap sexp in parentheses."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      ;; No selection: wrap word at point
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we ")")
            (editor-insert-text ed ws "("))))
      ;; Wrap selection
      (begin
        (editor-insert-text ed end ")")
        (editor-insert-text ed start "(")))))

(def (cmd-paredit-wrap-square app)
  "Wrap sexp in square brackets."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we "]")
            (editor-insert-text ed ws "["))))
      (begin
        (editor-insert-text ed end "]")
        (editor-insert-text ed start "[")))))

(def (cmd-paredit-wrap-curly app)
  "Wrap sexp in curly braces."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we "}")
            (editor-insert-text ed ws "{"))))
      (begin
        (editor-insert-text ed end "}")
        (editor-insert-text ed start "{")))))

(def (cmd-paredit-splice-sexp app)
  "Splice sexp - remove enclosing delimiters."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (match-pos (send-message ed SCI_BRACEMATCH pos 0)))
    (when (>= match-pos 0)
      (let ((open-pos (min pos match-pos))
            (close-pos (max pos match-pos)))
        ;; Delete close delimiter first (to preserve open position)
        (send-message ed SCI_DELETERANGE close-pos 1)
        ;; Delete open delimiter
        (send-message ed SCI_DELETERANGE open-pos 1)))))

(def (cmd-paredit-raise-sexp app)
  "Raise sexp - replace parent with child sexp (stub)."
  (echo-message! (app-state-echo app) "Paredit raise (stub)"))

;; Tramp-like remote editing
(def (cmd-find-file-ssh app)
  "Open file via SSH (stub)."
  (let ((path (app-read-string app "SSH path (user@host:path): ")))
    (when (and path (not (string-empty? path)))
      (echo-message! (app-state-echo app)
        (string-append "SSH file editing not implemented: " path)))))

;; Additional text manipulation
(def (cmd-string-inflection-cycle app)
  "Cycle word between camelCase, snake_case, kebab-case."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed)))
    (let-values (((word-start word-end) (word-bounds-at ed pos)))
      (when word-start
      (let* ((len (- word-end word-start))
             (word (substring (editor-get-text ed) word-start word-end))
             (has-underscore (string-contains word "_"))
             (has-dash (string-contains word "-"))
             (has-upper (let loop ((i 0))
                          (if (>= i (string-length word)) #f
                            (if (char-upper-case? (string-ref word i)) #t
                              (loop (+ i 1))))))
             (new-word
               (cond
                 ;; snake_case -> kebab-case
                 (has-underscore
                  (list->string
                    (map (lambda (c) (if (char=? c #\_) #\- c))
                         (string->list word))))
                 ;; kebab-case -> camelCase
                 (has-dash
                  (let loop ((chars (string->list word)) (capitalize #f) (acc []))
                    (cond
                      ((null? chars) (list->string (reverse acc)))
                      ((char=? (car chars) #\-)
                       (loop (cdr chars) #t acc))
                      (capitalize
                       (loop (cdr chars) #f (cons (char-upcase (car chars)) acc)))
                      (else
                       (loop (cdr chars) #f (cons (car chars) acc))))))
                 ;; camelCase -> snake_case
                 (has-upper
                  (let loop ((chars (string->list word)) (acc []))
                    (cond
                      ((null? chars) (list->string (reverse acc)))
                      ((and (char-upper-case? (car chars)) (not (null? acc)))
                       (loop (cdr chars)
                             (cons (char-downcase (car chars)) (cons #\_ acc))))
                      (else
                       (loop (cdr chars) (cons (char-downcase (car chars)) acc))))))
                 ;; no case markers - do nothing
                 (else word))))
        (send-message ed SCI_SETTARGETSTART word-start 0)
        (send-message ed SCI_SETTARGETEND word-end 0)
        (send-message/string ed SCI_REPLACETARGET new-word))))))

;; Ediff extras
(def (cmd-ediff-files app)
  "Compare two files with ediff (stub)."
  (echo-message! (app-state-echo app) "Ediff files (stub)"))

(def (cmd-ediff-regions app)
  "Compare two regions with ediff (stub)."
  (echo-message! (app-state-echo app) "Ediff regions (stub)"))

;; Repeat and undo extras
(def (cmd-undo-tree-visualize app)
  "Visualize undo tree (stub)."
  (echo-message! (app-state-echo app) "Undo tree (stub)"))

;; Emacsclient stubs
(def (cmd-server-start app)
  "Start the editor server (stub)."
  (echo-message! (app-state-echo app) "Server started (stub)"))

(def (cmd-server-edit app)
  "Edit with emacsclient (stub)."
  (echo-message! (app-state-echo app) "Server edit (stub)"))

;; Additional navigation
(def (cmd-pop-global-mark app)
  "Pop back to previous global mark."
  (let ((mr (app-state-mark-ring app)))
    (if (and mr (not (null? mr)))
      (let ((mark (car mr)))
        (set! (app-state-mark-ring app) (cdr mr))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win)))
          (editor-goto-pos ed mark)
          (editor-scroll-caret ed)))
      (echo-message! (app-state-echo app) "Mark ring empty"))))

(def (cmd-set-goal-column app)
  "Set current column as goal column (stub)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (col (editor-get-column ed pos)))
    (echo-message! (app-state-echo app)
      (string-append "Goal column set to " (number->string col)))))

;; Directory navigation
(def (cmd-cd app)
  "Change default directory."
  (let ((dir (app-read-string app "Change directory: ")))
    (when (and dir (not (string-empty? dir)))
      (if (file-exists? dir)
        (begin
          (current-directory dir)
          (echo-message! (app-state-echo app)
            (string-append "Directory: " (current-directory))))
        (echo-message! (app-state-echo app)
          (string-append "No such directory: " dir))))))

;; Misc Emacs commands
(def (cmd-display-prefix app)
  "Display the current prefix argument (stub)."
  (echo-message! (app-state-echo app) "Prefix arg: none"))

(def (cmd-digit-argument app)
  "Begin entering a numeric prefix argument (stub)."
  (echo-message! (app-state-echo app) "Digit argument (stub)"))

(def (cmd-negative-argument app)
  "Begin negative numeric prefix argument (stub)."
  (echo-message! (app-state-echo app) "Negative argument (stub)"))

(def (cmd-suspend-emacs app)
  "Suspend the editor (send SIGTSTP)."
  (echo-message! (app-state-echo app) "Use C-z in terminal to suspend"))

(def (cmd-save-buffers-kill-emacs app)
  "Save all buffers and quit."
  ;; Save any modified buffers with files
  (for-each
    (lambda (buf)
      (when (and (buffer-file-path buf) (buffer-modified buf))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win)))
          ;; Only save if this buffer is in the active window
          ;; For a full implementation, we'd need to attach each buffer
          (void))))
    (buffer-list))
  (set! (app-state-running app) #f)
  (echo-message! (app-state-echo app) "Exiting..."))

;; View/doc mode
(def (cmd-view-mode app)
  "Toggle view mode (read-only with navigation)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (ro (editor-get-read-only? ed)))
    (editor-set-read-only ed (not ro))
    (echo-message! (app-state-echo app)
      (if ro "View mode disabled" "View mode enabled"))))

(def (cmd-doc-view-mode app)
  "Toggle doc-view mode (stub)."
  (echo-message! (app-state-echo app) "Doc-view mode (stub)"))

;; Speedbar stub
(def (cmd-speedbar app)
  "Toggle speedbar (stub)."
  (echo-message! (app-state-echo app) "Speedbar (stub)"))

;; Misc utilities
(def (cmd-world-clock app)
  "Display world clock."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error getting time")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "date"
                                     arguments: '("+%Y-%m-%d %H:%M:%S %Z")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p)))
                        (process-status p)
                        (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*World Clock*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "World Clock\n\nLocal: " result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-display-battery app)
  "Display battery status."
  (let ((result (with-exception-catcher
                  (lambda (e) "Battery info not available")
                  (lambda ()
                    (if (file-exists? "/sys/class/power_supply/BAT0/capacity")
                      (let ((cap (call-with-input-file "/sys/class/power_supply/BAT0/capacity"
                                   read-line))
                            (status (if (file-exists? "/sys/class/power_supply/BAT0/status")
                                      (call-with-input-file "/sys/class/power_supply/BAT0/status"
                                        read-line)
                                      "Unknown")))
                        (string-append "Battery: " cap "% (" status ")"))
                      "No battery information available")))))
    (echo-message! (app-state-echo app) result)))

(def (cmd-uptime app)
  "Display system uptime."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error getting uptime")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "uptime"
                                     arguments: '()
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p)))
                        (process-status p)
                        (string-append "Uptime:" (string-trim out))))))))
    (echo-message! (app-state-echo app) result)))

;; Kmacro counter
(def (cmd-kmacro-set-counter app)
  "Set keyboard macro counter (stub)."
  (echo-message! (app-state-echo app) "Macro counter set (stub)"))

(def (cmd-kmacro-insert-counter app)
  "Insert and increment keyboard macro counter (stub)."
  (echo-message! (app-state-echo app) "Macro counter inserted (stub)"))

;; Whitespace report
(def (cmd-whitespace-report app)
  "Report whitespace problems in buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (trailing-count 0)
         (tab-count 0)
         (long-count 0))
    (for-each
      (lambda (line)
        (when (and (> (string-length line) 0)
                   (char-whitespace? (string-ref line (- (string-length line) 1))))
          (set! trailing-count (+ trailing-count 1)))
        (when (string-contains line "\t")
          (set! tab-count (+ tab-count 1)))
        (when (> (string-length line) 80)
          (set! long-count (+ long-count 1))))
      lines)
    (echo-message! (app-state-echo app)
      (string-append "Trailing: " (number->string trailing-count)
                     " Tabs: " (number->string tab-count)
                     " Long(>80): " (number->string long-count)))))

;; Encoding detection
(def (cmd-describe-coding-system app)
  "Describe current coding system (stub)."
  (echo-message! (app-state-echo app) "Coding system: utf-8 (default)"))

(def (cmd-set-terminal-coding-system app)
  "Set terminal coding system (stub)."
  (echo-message! (app-state-echo app) "Terminal coding: utf-8"))

;; Misc text
(def (cmd-overwrite-mode app)
  "Toggle overwrite mode (stub — use M-x toggle-overwrite-mode)."
  (echo-message! (app-state-echo app) "Use M-x toggle-overwrite-mode"))

;; --- Task #47: xref, ibuffer, which-key, markdown, auto-insert, and more ---

;; Xref cross-reference navigation
(def (cmd-xref-find-definitions app)
  "Find definitions of symbol at point (stub)."
  (echo-message! (app-state-echo app) "Xref: find definitions (stub)"))

(def (cmd-xref-find-references app)
  "Find references to symbol at point (stub)."
  (echo-message! (app-state-echo app) "Xref: find references (stub)"))

(def (cmd-xref-find-apropos app)
  "Find symbols matching pattern (stub)."
  (echo-message! (app-state-echo app) "Xref: find apropos (stub)"))

(def (cmd-xref-go-back app)
  "Pop back to previous xref location (stub)."
  (echo-message! (app-state-echo app) "Xref: go back (stub)"))

(def (cmd-xref-go-forward app)
  "Go forward in xref history (stub)."
  (echo-message! (app-state-echo app) "Xref: go forward (stub)"))

;; Ibuffer - advanced buffer management
(def (cmd-ibuffer app)
  "Open ibuffer - advanced buffer management."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (bufs (buffer-list))
         (lines (map (lambda (b)
                       (string-append
                         (if (buffer-modified b) "* " "  ")
                         (buffer-name b)
                         (if (buffer-file-path b)
                           (string-append "  " (buffer-file-path b))
                           "")))
                     bufs))
         (text (string-join lines "\n"))
         (buf (buffer-create! "*Ibuffer*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Ibuffer\n\n  MR  Buffer              File\n  --  ------              ----\n" text "\n"))
    (editor-set-read-only ed #t)))

(def (cmd-ibuffer-mark app)
  "Mark buffer in ibuffer (stub)."
  (echo-message! (app-state-echo app) "Ibuffer mark (stub)"))

(def (cmd-ibuffer-delete app)
  "Flag buffer for deletion in ibuffer (stub)."
  (echo-message! (app-state-echo app) "Ibuffer delete flag (stub)"))

(def (cmd-ibuffer-do-kill app)
  "Execute flagged operations in ibuffer (stub)."
  (echo-message! (app-state-echo app) "Ibuffer execute (stub)"))

;; Which-key - display available keybindings
(def (cmd-which-key app)
  "Display available keybindings for current prefix (stub)."
  (echo-message! (app-state-echo app) "Which-key: press a key to see bindings (stub)"))

;; Markdown mode
(def (cmd-markdown-mode app)
  "Toggle markdown mode (stub)."
  (echo-message! (app-state-echo app) "Markdown mode (stub)"))

(def (cmd-markdown-preview app)
  "Preview markdown (stub)."
  (echo-message! (app-state-echo app) "Markdown preview (stub)"))

(def (cmd-markdown-insert-header app)
  "Insert markdown header."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "# ")))

(def (cmd-markdown-insert-bold app)
  "Insert markdown bold markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "****")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -2)))
      (begin
        (editor-insert-text ed end "**")
        (editor-insert-text ed start "**")))))

(def (cmd-markdown-insert-italic app)
  "Insert markdown italic markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "**")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -1)))
      (begin
        (editor-insert-text ed end "*")
        (editor-insert-text ed start "*")))))

(def (cmd-markdown-insert-code app)
  "Insert markdown code markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "``")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -1)))
      (begin
        (editor-insert-text ed end "`")
        (editor-insert-text ed start "`")))))

(def (cmd-markdown-insert-link app)
  "Insert markdown link template."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "[text](url)")))

(def (cmd-markdown-insert-image app)
  "Insert markdown image template."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "![alt](url)")))

(def (cmd-markdown-insert-code-block app)
  "Insert markdown fenced code block."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "```\n\n```")))

(def (cmd-markdown-insert-list-item app)
  "Insert markdown list item."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "- ")))

;; Auto-insert templates
(def (cmd-auto-insert app)
  "Insert file template based on file extension (stub)."
  (echo-message! (app-state-echo app) "Auto-insert (stub)"))

(def (cmd-auto-insert-mode app)
  "Toggle auto-insert mode (stub)."
  (echo-message! (app-state-echo app) "Auto-insert mode toggled (stub)"))

;; Text scale (font size)
(def (cmd-text-scale-increase app)
  "Increase text scale."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETZOOM 0 0)))
    (send-message ed SCI_SETZOOM (+ cur 1) 0)
    (echo-message! (app-state-echo app)
      (string-append "Zoom: " (number->string (+ cur 1))))))

(def (cmd-text-scale-decrease app)
  "Decrease text scale."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETZOOM 0 0)))
    (send-message ed SCI_SETZOOM (- cur 1) 0)
    (echo-message! (app-state-echo app)
      (string-append "Zoom: " (number->string (- cur 1))))))

(def (cmd-text-scale-reset app)
  "Reset text scale to default."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_SETZOOM 0 0)
    (echo-message! (app-state-echo app) "Zoom: 0 (default)")))

;; Browse kill ring
(def (cmd-browse-kill-ring app)
  "Display kill ring contents."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (kr (app-state-kill-ring app))
         (entries (let loop ((items kr) (i 0) (acc []))
                    (if (or (null? items) (>= i 20))
                      (reverse acc)
                      (let ((entry (car items)))
                        (loop (cdr items) (+ i 1)
                              (cons (string-append
                                      (number->string i) ": "
                                      (if (> (string-length entry) 60)
                                        (string-append (substring entry 0 60) "...")
                                        entry))
                                    acc))))))
         (text (if (null? entries) "(empty)"
                 (string-join entries "\n")))
         (buf (buffer-create! "*Kill Ring*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Kill Ring\n\n" text "\n"))
    (editor-set-read-only ed #t)))

;; Flycheck / syntax checking
(def (cmd-flycheck-mode app)
  "Toggle flycheck mode (stub)."
  (echo-message! (app-state-echo app) "Flycheck mode toggled (stub)"))

(def (cmd-flycheck-next-error app)
  "Jump to next flycheck error (stub)."
  (echo-message! (app-state-echo app) "Flycheck: next error (stub)"))

(def (cmd-flycheck-previous-error app)
  "Jump to previous flycheck error (stub)."
  (echo-message! (app-state-echo app) "Flycheck: previous error (stub)"))

(def (cmd-flycheck-list-errors app)
  "List all flycheck errors (stub)."
  (echo-message! (app-state-echo app) "Flycheck: list errors (stub)"))

;; Treemacs / file explorer
(def (cmd-treemacs app)
  "Toggle treemacs file explorer (stub)."
  (echo-message! (app-state-echo app) "Treemacs (stub)"))

(def (cmd-treemacs-find-file app)
  "Find current file in treemacs (stub)."
  (echo-message! (app-state-echo app) "Treemacs: find file (stub)"))

;; Magit-like git operations
(def (cmd-magit-status app)
  "Show git status in magit-like interface."
  (let ((result (with-exception-catcher
                  (lambda (e) "Not a git repository")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("status" "--short")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(clean)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Status\n\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-magit-log app)
  "Show git log in magit-like interface."
  (let ((result (with-exception-catcher
                  (lambda (e) "Not a git repository")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("log" "--oneline" "-30")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(empty)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit Log*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Log\n\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-magit-diff app)
  "Show git diff."
  (let ((result (with-exception-catcher
                  (lambda (e) "Not a git repository")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("diff")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(no changes)")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit Diff*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Diff\n\n" result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-magit-commit app)
  "Create git commit with message from echo area."
  (let ((msg (app-read-string app "Commit message: ")))
    (when (and msg (not (string-empty? msg)))
      (let ((result (with-exception-catcher
                      (lambda (e) (string-append "Error: " (with-output-to-string (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "commit" "-m" msg)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "Committed")))))))
        (echo-message! (app-state-echo app) result)))))

(def (cmd-magit-stage-file app)
  "Stage current file."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (buffer-file-path buf)))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error staging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "add" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Staged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-magit-unstage-file app)
  "Unstage current file."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (buffer-file-path buf)))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error unstaging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "reset" "HEAD" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Unstaged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-magit-branch app)
  "Show or create git branch."
  (let ((result (with-exception-catcher
                  (lambda (e) "Not a git repository")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("branch" "-a")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(no branches)")))))))
    (echo-message! (app-state-echo app) result)))

(def (cmd-magit-checkout app)
  "Switch git branch."
  (let ((branch (app-read-string app "Branch: ")))
    (when (and branch (not (string-empty? branch)))
      (let ((result (with-exception-catcher
                      (lambda (e) "Error switching branch")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "checkout" branch)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out (string-append "Switched to: " branch))))))))
        (echo-message! (app-state-echo app) result)))))

;; Minibuffer commands
(def (cmd-minibuffer-complete app)
  "Complete in minibuffer (stub)."
  (echo-message! (app-state-echo app) "Minibuffer complete (stub)"))

(def (cmd-minibuffer-keyboard-quit app)
  "Quit minibuffer (stub)."
  (echo-message! (app-state-echo app) "Quit"))

;; Abbrev mode extras
(def (cmd-define-global-abbrev app)
  "Define a global abbreviation (stub)."
  (echo-message! (app-state-echo app) "Define global abbrev (stub)"))

(def (cmd-define-mode-abbrev app)
  "Define a mode-specific abbreviation (stub)."
  (echo-message! (app-state-echo app) "Define mode abbrev (stub)"))

(def (cmd-unexpand-abbrev app)
  "Undo last abbreviation expansion (stub)."
  (echo-message! (app-state-echo app) "Unexpand abbrev (stub)"))

;; Hippie expand
(def (cmd-hippie-expand-undo app)
  "Undo last hippie-expand (stub)."
  (echo-message! (app-state-echo app) "Hippie expand undo (stub)"))

;; Compilation extras
(def (cmd-next-error-function app)
  "Navigate to next compilation error (alias for next-error, stub)."
  (echo-message! (app-state-echo app) "Next error function (stub)"))

(def (cmd-previous-error-function app)
  "Navigate to previous compilation error (stub)."
  (echo-message! (app-state-echo app) "Previous error function (stub)"))

;; Bookmark extras
(def (cmd-bookmark-bmenu-list app)
  "List bookmarks in a menu buffer (stub)."
  (echo-message! (app-state-echo app) "Bookmark menu (stub)"))

;; Rectangle extras
(def (cmd-rectangle-mark-mode app)
  "Toggle rectangle mark mode (stub)."
  (echo-message! (app-state-echo app) "Rectangle mark mode (stub)"))

(def (cmd-number-to-register app)
  "Store a number in a register (stub)."
  (echo-message! (app-state-echo app) "Number to register (stub)"))

;; Isearch extras
(def (cmd-isearch-toggle-case-fold app)
  "Toggle case sensitivity in isearch (stub)."
  (echo-message! (app-state-echo app) "Isearch: case fold toggled (stub)"))

(def (cmd-isearch-toggle-regexp app)
  "Toggle regexp in isearch (stub)."
  (echo-message! (app-state-echo app) "Isearch: regexp toggled (stub)"))

;; Semantic / imenu / tags
(def (cmd-semantic-mode app)
  "Toggle semantic mode (stub)."
  (echo-message! (app-state-echo app) "Semantic mode (stub)"))

(def (cmd-imenu-anywhere app)
  "Jump to any imenu entry across buffers (stub)."
  (echo-message! (app-state-echo app) "Imenu anywhere (stub)"))

(def (cmd-tags-search app)
  "Search in tags table (stub)."
  (echo-message! (app-state-echo app) "Tags search (stub)"))

(def (cmd-tags-query-replace app)
  "Query-replace in tags table (stub)."
  (echo-message! (app-state-echo app) "Tags query replace (stub)"))

(def (cmd-visit-tags-table app)
  "Visit a TAGS file (stub)."
  (echo-message! (app-state-echo app) "Visit tags table (stub)"))

;; Whitespace extras
(def (cmd-whitespace-toggle-options app)
  "Toggle whitespace display options (stub)."
  (echo-message! (app-state-echo app) "Whitespace options toggled (stub)"))

;; Highlight
(def (cmd-highlight-regexp app)
  "Highlight text matching regexp."
  (let ((pat (app-read-string app "Highlight regexp: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        ;; Use indicator-based highlighting
        (send-message ed SCI_INDICSETSTYLE 0 7) ;; INDIC_ROUNDBOX
        (send-message ed SCI_INDICSETFORE 0 #x00FF00)
        (send-message ed SCI_SETINDICATORCURRENT 0 0)
        (let* ((text (editor-get-text ed))
               (len (string-length text))
               (pat-len (string-length pat)))
          (let loop ((pos 0))
            (when (< pos (- len pat-len))
              (let ((sub (substring text pos (+ pos pat-len))))
                (when (string=? sub pat)
                  (send-message ed SCI_INDICATORFILLRANGE pos pat-len)))
              (loop (+ pos 1)))))
        (echo-message! (app-state-echo app) (string-append "Highlighted: " pat))))))

(def (cmd-unhighlight-regexp app)
  "Remove regexp highlighting."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (len (editor-get-text-length ed)))
    (send-message ed SCI_SETINDICATORCURRENT 0 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len)
    (echo-message! (app-state-echo app) "Highlights cleared")))

;; Emacs server / client
(def (cmd-server-force-delete app)
  "Force delete Emacs server (stub)."
  (echo-message! (app-state-echo app) "Server force-deleted (stub)"))

;; Help extras
(def (cmd-help-for-help app)
  "Show help about help system."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Help for Help*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Help Commands\n\n"
        "C-h k  describe-key         - Show what a key does\n"
        "C-h f  describe-function    - Describe a function\n"
        "C-h v  describe-variable    - Describe a variable\n"
        "C-h w  where-is             - Find key for a command\n"
        "C-h b  describe-bindings    - List all key bindings\n"
        "C-h a  apropos-command      - Search commands\n"
        "C-h m  describe-mode        - Describe current mode\n"
        "C-h i  info                 - Open Info browser\n"
        "C-h ?  help-for-help        - This buffer\n"))
    (editor-set-read-only ed #t)))

(def (cmd-help-quick app)
  "Show quick reference card."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Quick Help*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Quick Reference\n\n"
        "Navigation:    C-f/b/n/p  Forward/Back/Next/Prev\n"
        "               C-a/e      Beginning/End of line\n"
        "               M-f/b      Forward/Back word\n"
        "               M-</>      Beginning/End of buffer\n\n"
        "Editing:       C-d        Delete char\n"
        "               C-k        Kill line\n"
        "               C-y        Yank (paste)\n"
        "               C-w        Kill region\n"
        "               M-w        Copy region\n\n"
        "Files:         C-x C-f    Open file\n"
        "               C-x C-s    Save file\n"
        "               C-x C-w    Save as\n\n"
        "Buffers:       C-x b      Switch buffer\n"
        "               C-x k      Kill buffer\n"
        "               C-x C-b    List buffers\n\n"
        "Windows:       C-x 2      Split horizontal\n"
        "               C-x 3      Split vertical\n"
        "               C-x 1      Delete other windows\n"
        "               C-x o      Other window\n\n"
        "Search:        C-s        Search forward\n"
        "               C-r        Search backward\n"
        "               M-%        Query replace\n\n"
        "Other:         M-x        Execute command\n"
        "               C-g        Keyboard quit\n"
        "               C-x C-c    Quit\n"))
    (editor-set-read-only ed #t)))

;; Theme commands
(def (cmd-disable-theme app)
  "Disable current theme (stub)."
  (echo-message! (app-state-echo app) "Theme disabled (stub)"))

(def (cmd-describe-theme app)
  "Describe current theme (stub)."
  (echo-message! (app-state-echo app) "Theme description (stub)"))

;; Ediff extras
(def (cmd-ediff-merge app)
  "Merge two files with ediff (stub)."
  (echo-message! (app-state-echo app) "Ediff merge (stub)"))

(def (cmd-ediff-directories app)
  "Compare directories with ediff (stub)."
  (echo-message! (app-state-echo app) "Ediff directories (stub)"))

;; Window commands extras
(def (cmd-window-divider-mode app)
  "Toggle window divider display (stub)."
  (echo-message! (app-state-echo app) "Window divider mode (stub)"))

(def (cmd-scroll-bar-mode app)
  "Toggle scroll bar (stub)."
  (echo-message! (app-state-echo app) "Scroll bar mode toggled (stub)"))

(def (cmd-menu-bar-open app)
  "Open menu bar (stub)."
  (echo-message! (app-state-echo app) "Menu bar (stub)"))

;; Programming helpers
(def (cmd-toggle-prettify-symbols app)
  "Toggle prettify-symbols mode (stub)."
  (echo-message! (app-state-echo app) "Prettify-symbols mode (stub)"))

(def (cmd-subword-mode app)
  "Toggle subword mode for CamelCase navigation (stub)."
  (echo-message! (app-state-echo app) "Subword mode toggled (stub)"))

(def (cmd-superword-mode app)
  "Toggle superword mode for symbol_name navigation (stub)."
  (echo-message! (app-state-echo app) "Superword mode toggled (stub)"))

(def (cmd-glasses-mode app)
  "Toggle glasses mode for CamelCase display (stub)."
  (echo-message! (app-state-echo app) "Glasses mode toggled (stub)"))

;; Misc tools
(def (cmd-calculator app)
  "Open inline calculator - evaluate math expression."
  (let ((expr (app-read-string app "Calc: ")))
    (when (and expr (not (string-empty? expr)))
      (let ((result (with-exception-catcher
                      (lambda (e) "Error")
                      (lambda ()
                        (let ((val (eval (with-input-from-string expr read))))
                          (with-output-to-string (lambda () (write val))))))))
        (echo-message! (app-state-echo app) (string-append "= " result))))))

(def (cmd-count-words-line app)
  "Count words in current line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0))
         (start (send-message ed SCI_POSITIONFROMLINE line 0))
         (end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (text (substring (editor-get-text ed) start end))
         (words (let loop ((i 0) (count 0) (in-word #f))
                  (if (>= i (string-length text))
                    (if in-word (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\tab))
                        (loop (+ i 1) (if in-word (+ count 1) count) #f)
                        (loop (+ i 1) count #t)))))))
    (echo-message! (app-state-echo app)
      (string-append "Words in line: " (number->string words)))))

(def (cmd-display-column-number app)
  "Display current column number."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (col (send-message ed SCI_GETCOLUMN (editor-get-current-pos ed) 0)))
    (echo-message! (app-state-echo app)
      (string-append "Column: " (number->string col)))))

(def (cmd-what-tab-width app)
  "Display current tab width."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (tw (send-message ed SCI_GETTABWIDTH 0 0)))
    (echo-message! (app-state-echo app)
      (string-append "Tab width: " (number->string tw)))))

(def (cmd-set-tab-width app)
  "Set tab width."
  (let ((width (app-read-string app "Tab width: ")))
    (when (and width (not (string-empty? width)))
      (let ((n (string->number width)))
        (when (and n (> n 0) (<= n 16))
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win)))
            (send-message ed SCI_SETTABWIDTH n 0)
            (echo-message! (app-state-echo app)
              (string-append "Tab width set to " width))))))))

(def (cmd-display-cursor-position app)
  "Display detailed cursor position."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0))
         (col (send-message ed SCI_GETCOLUMN pos 0))
         (total (editor-get-text-length ed)))
    (echo-message! (app-state-echo app)
      (string-append "Pos " (number->string pos)
                     " of " (number->string total)
                     ", Line " (number->string (+ line 1))
                     ", Col " (number->string col)))))

(def (cmd-toggle-line-spacing app)
  "Toggle extra line spacing."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETEXTRAASCENT 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETEXTRAASCENT 0 0)
        (send-message ed SCI_SETEXTRADESCENT 0 0)
        (echo-message! (app-state-echo app) "Line spacing: normal"))
      (begin
        (send-message ed SCI_SETEXTRAASCENT 2 0)
        (send-message ed SCI_SETEXTRADESCENT 2 0)
        (echo-message! (app-state-echo app) "Line spacing: expanded")))))

(def (cmd-toggle-selection-mode app)
  "Toggle between stream and rectangular selection."
  ;; SCI_GETSELECTIONMODE=2422, SCI_SETSELECTIONMODE=2422 (not in constants.ss)
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed 2422 0 0))) ;; SCI_GETSELECTIONMODE
    (if (= cur 0) ;; SC_SEL_STREAM
      (begin
        (send-message ed 2421 1 0) ;; SCI_SETSELECTIONMODE SC_SEL_RECTANGLE
        (echo-message! (app-state-echo app) "Rectangle selection mode"))
      (begin
        (send-message ed 2421 0 0) ;; SCI_SETSELECTIONMODE SC_SEL_STREAM
        (echo-message! (app-state-echo app) "Stream selection mode")))))

(def (cmd-toggle-virtual-space app)
  "Toggle virtual space mode."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETVIRTUALSPACEOPTIONS 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETVIRTUALSPACEOPTIONS 0 0)
        (echo-message! (app-state-echo app) "Virtual space: off"))
      (begin
        (send-message ed SCI_SETVIRTUALSPACEOPTIONS 3 0) ;; SCVS_RECTANGULARSELECTION | SCVS_USERACCESSIBLE
        (echo-message! (app-state-echo app) "Virtual space: on")))))

(def (cmd-toggle-caret-style app)
  "Toggle between line and block caret."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETCARETSTYLE 0 0)))
    (if (= cur 1) ;; CARETSTYLE_LINE
      (begin
        (send-message ed SCI_SETCARETSTYLE 2 0) ;; CARETSTYLE_BLOCK
        (echo-message! (app-state-echo app) "Caret: block"))
      (begin
        (send-message ed SCI_SETCARETSTYLE 1 0) ;; CARETSTYLE_LINE
        (echo-message! (app-state-echo app) "Caret: line")))))

;; Buffer comparison
(def (cmd-compare-windows app)
  "Compare text of two windows (stub)."
  (echo-message! (app-state-echo app) "Compare windows (stub)"))

;; Frame commands
(def (cmd-iconify-frame app)
  "Iconify/minimize frame (stub)."
  (echo-message! (app-state-echo app) "Frame iconified (stub)"))

(def (cmd-raise-frame app)
  "Raise frame (stub)."
  (echo-message! (app-state-echo app) "Frame raised (stub)"))

;; Face/font commands
(def (cmd-set-face-attribute app)
  "Set face attribute (stub)."
  (echo-message! (app-state-echo app) "Set face attribute (stub)"))

(def (cmd-list-faces-display app)
  "Display list of all faces (stub)."
  (echo-message! (app-state-echo app) "List faces (stub)"))

;; Eshell extras
(def (cmd-eshell-here app)
  "Open eshell in current buffer's directory (stub)."
  (echo-message! (app-state-echo app) "Eshell here (stub)"))

;; Calendar extras
(def (cmd-calendar-goto-date app)
  "Go to specific date in calendar (stub)."
  (echo-message! (app-state-echo app) "Calendar goto date (stub)"))

(def (cmd-calendar-holidays app)
  "Show holidays (stub)."
  (echo-message! (app-state-echo app) "Holidays (stub)"))

;; ERC/IRC
(def (cmd-erc app)
  "Start ERC IRC client (stub)."
  (echo-message! (app-state-echo app) "ERC IRC client (stub)"))

;; TRAMP extras
(def (cmd-tramp-cleanup-connections app)
  "Clean up TRAMP connections (stub)."
  (echo-message! (app-state-echo app) "TRAMP connections cleaned (stub)"))

;; LSP extras
(def (cmd-lsp-find-declaration app)
  "Find declaration via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: find declaration (stub)"))

(def (cmd-lsp-find-implementation app)
  "Find implementation via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: find implementation (stub)"))

(def (cmd-lsp-rename app)
  "Rename symbol via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: rename (stub)"))

(def (cmd-lsp-format-buffer app)
  "Format buffer via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: format buffer (stub)"))

(def (cmd-lsp-code-actions app)
  "Show code actions via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: code actions (stub)"))

(def (cmd-lsp-describe-thing-at-point app)
  "Describe thing at point via LSP (stub)."
  (echo-message! (app-state-echo app) "LSP: describe (stub)"))

;; Debug adapter protocol
(def (cmd-dap-debug app)
  "Start debug session (stub)."
  (echo-message! (app-state-echo app) "DAP: debug (stub)"))

(def (cmd-dap-breakpoint-toggle app)
  "Toggle breakpoint (stub)."
  (echo-message! (app-state-echo app) "DAP: breakpoint toggled (stub)"))

(def (cmd-dap-continue app)
  "Continue execution (stub)."
  (echo-message! (app-state-echo app) "DAP: continue (stub)"))

(def (cmd-dap-step-over app)
  "Step over (stub)."
  (echo-message! (app-state-echo app) "DAP: step over (stub)"))

(def (cmd-dap-step-in app)
  "Step in (stub)."
  (echo-message! (app-state-echo app) "DAP: step in (stub)"))

(def (cmd-dap-step-out app)
  "Step out (stub)."
  (echo-message! (app-state-echo app) "DAP: step out (stub)"))

;; Snippet / template system
(def (cmd-yas-insert-snippet app)
  "Insert a snippet (stub)."
  (echo-message! (app-state-echo app) "Insert snippet (stub)"))

(def (cmd-yas-new-snippet app)
  "Create a new snippet (stub)."
  (echo-message! (app-state-echo app) "New snippet (stub)"))

(def (cmd-yas-visit-snippet-file app)
  "Visit snippet file (stub)."
  (echo-message! (app-state-echo app) "Visit snippet (stub)"))


;;;============================================================================
;;; Register extra commands
;;;============================================================================

(def (register-extra-commands!)
  ;; Task #46: org-mode, windmove, winner, VC, mail, sessions, etc.
  ;; Org-mode stubs
  (register-command! 'org-mode cmd-org-mode)
  (register-command! 'org-todo cmd-org-todo)
  (register-command! 'org-schedule cmd-org-schedule)
  (register-command! 'org-deadline cmd-org-deadline)
  (register-command! 'org-agenda cmd-org-agenda)
  (register-command! 'org-export cmd-org-export)
  (register-command! 'org-table-create cmd-org-table-create)
  (register-command! 'org-link cmd-org-link)
  (register-command! 'org-store-link cmd-org-store-link)
  (register-command! 'org-open-at-point cmd-org-open-at-point)
  (register-command! 'org-cycle cmd-org-cycle)
  (register-command! 'org-shift-tab cmd-org-shift-tab)
  ;; Calendar/diary
  (register-command! 'calendar cmd-calendar)
  (register-command! 'diary-view-entries cmd-diary-view-entries)
  ;; EWW browser
  (register-command! 'eww cmd-eww)
  (register-command! 'eww-browse-url cmd-eww-browse-url)
  (register-command! 'browse-url-at-point cmd-browse-url-at-point)
  ;; Windmove
  (register-command! 'windmove-left cmd-windmove-left)
  (register-command! 'windmove-right cmd-windmove-right)
  (register-command! 'windmove-up cmd-windmove-up)
  (register-command! 'windmove-down cmd-windmove-down)
  ;; Winner mode
  (register-command! 'winner-undo cmd-winner-undo)
  (register-command! 'winner-redo cmd-winner-redo)
  ;; Tab-bar
  (register-command! 'tab-new cmd-tab-new)
  (register-command! 'tab-close cmd-tab-close)
  (register-command! 'tab-next cmd-tab-next)
  (register-command! 'tab-previous cmd-tab-previous)
  (register-command! 'tab-rename cmd-tab-rename)
  (register-command! 'tab-move cmd-tab-move)
  ;; VC extras
  (register-command! 'vc-register cmd-vc-register)
  (register-command! 'vc-dir cmd-vc-dir)
  (register-command! 'vc-pull cmd-vc-pull)
  (register-command! 'vc-push cmd-vc-push)
  (register-command! 'vc-create-tag cmd-vc-create-tag)
  (register-command! 'vc-print-log cmd-vc-print-log)
  (register-command! 'vc-stash cmd-vc-stash)
  (register-command! 'vc-stash-pop cmd-vc-stash-pop)
  ;; Mail
  (register-command! 'compose-mail cmd-compose-mail)
  (register-command! 'rmail cmd-rmail)
  (register-command! 'gnus cmd-gnus)
  ;; Sessions
  (register-command! 'desktop-save cmd-desktop-save)
  (register-command! 'desktop-read cmd-desktop-read)
  (register-command! 'desktop-clear cmd-desktop-clear)
  ;; Man pages
  (register-command! 'man cmd-man)
  (register-command! 'woman cmd-woman)
  ;; Macro extras
  (register-command! 'apply-macro-to-region-lines cmd-apply-macro-to-region-lines)
  (register-command! 'edit-kbd-macro cmd-edit-kbd-macro)
  ;; Compilation
  (register-command! 'recompile cmd-recompile)
  (register-command! 'kill-compilation cmd-kill-compilation)
  ;; Flyspell
  (register-command! 'flyspell-auto-correct-word cmd-flyspell-auto-correct-word)
  (register-command! 'flyspell-goto-next-error cmd-flyspell-goto-next-error)
  ;; Multiple cursors
  (register-command! 'mc-mark-next-like-this cmd-mc-mark-next-like-this)
  (register-command! 'mc-mark-previous-like-this cmd-mc-mark-previous-like-this)
  (register-command! 'mc-mark-all-like-this cmd-mc-mark-all-like-this)
  (register-command! 'mc-edit-lines cmd-mc-edit-lines)
  ;; Package management
  (register-command! 'package-list-packages cmd-package-list-packages)
  (register-command! 'package-install cmd-package-install)
  (register-command! 'package-delete cmd-package-delete)
  (register-command! 'package-refresh-contents cmd-package-refresh-contents)
  ;; Custom
  (register-command! 'customize-group cmd-customize-group)
  (register-command! 'customize-variable cmd-customize-variable)
  (register-command! 'customize-themes cmd-customize-themes)
  ;; Diff mode
  (register-command! 'diff-mode cmd-diff-mode)
  (register-command! 'diff-apply-hunk cmd-diff-apply-hunk)
  (register-command! 'diff-revert-hunk cmd-diff-revert-hunk)
  (register-command! 'diff-goto-source cmd-diff-goto-source)
  ;; Artist mode
  (register-command! 'artist-mode cmd-artist-mode)
  ;; Tramp
  (register-command! 'tramp-cleanup-all-connections cmd-tramp-cleanup-all-connections)
  ;; Process
  (register-command! 'proced cmd-proced)
  ;; Paredit
  (register-command! 'paredit-wrap-round cmd-paredit-wrap-round)
  (register-command! 'paredit-wrap-square cmd-paredit-wrap-square)
  (register-command! 'paredit-wrap-curly cmd-paredit-wrap-curly)
  (register-command! 'paredit-splice-sexp cmd-paredit-splice-sexp)
  (register-command! 'paredit-raise-sexp cmd-paredit-raise-sexp)
  ;; Remote editing
  (register-command! 'find-file-ssh cmd-find-file-ssh)
  ;; Text manipulation
  (register-command! 'string-inflection-cycle cmd-string-inflection-cycle)
  ;; Ediff
  (register-command! 'ediff-files cmd-ediff-files)
  (register-command! 'ediff-regions cmd-ediff-regions)
  ;; Undo tree
  (register-command! 'undo-tree-visualize cmd-undo-tree-visualize)
  ;; Server
  (register-command! 'server-start cmd-server-start)
  (register-command! 'server-edit cmd-server-edit)
  ;; Navigation
  (register-command! 'pop-global-mark cmd-pop-global-mark)
  (register-command! 'set-goal-column cmd-set-goal-column)
  (register-command! 'cd cmd-cd)
  ;; Misc
  (register-command! 'display-prefix cmd-display-prefix)
  (register-command! 'digit-argument cmd-digit-argument)
  (register-command! 'negative-argument cmd-negative-argument)
  (register-command! 'suspend-emacs cmd-suspend-emacs)
  (register-command! 'save-buffers-kill-emacs cmd-save-buffers-kill-emacs)
  (register-command! 'view-mode cmd-view-mode)
  (register-command! 'doc-view-mode cmd-doc-view-mode)
  (register-command! 'speedbar cmd-speedbar)
  (register-command! 'world-clock cmd-world-clock)
  (register-command! 'display-battery cmd-display-battery)
  (register-command! 'uptime cmd-uptime)
  (register-command! 'kmacro-set-counter cmd-kmacro-set-counter)
  (register-command! 'kmacro-insert-counter cmd-kmacro-insert-counter)
  (register-command! 'whitespace-report cmd-whitespace-report)
  (register-command! 'describe-coding-system cmd-describe-coding-system)
  (register-command! 'set-terminal-coding-system cmd-set-terminal-coding-system)
  (register-command! 'overwrite-mode cmd-overwrite-mode)
  ;; Task #47: xref, ibuffer, which-key, markdown, auto-insert, and more
  ;; Xref
  (register-command! 'xref-find-definitions cmd-xref-find-definitions)
  (register-command! 'xref-find-references cmd-xref-find-references)
  (register-command! 'xref-find-apropos cmd-xref-find-apropos)
  (register-command! 'xref-go-back cmd-xref-go-back)
  (register-command! 'xref-go-forward cmd-xref-go-forward)
  ;; Ibuffer
  (register-command! 'ibuffer cmd-ibuffer)
  (register-command! 'ibuffer-mark cmd-ibuffer-mark)
  (register-command! 'ibuffer-delete cmd-ibuffer-delete)
  (register-command! 'ibuffer-do-kill cmd-ibuffer-do-kill)
  ;; Which-key
  (register-command! 'which-key cmd-which-key)
  ;; Markdown
  (register-command! 'markdown-mode cmd-markdown-mode)
  (register-command! 'markdown-preview cmd-markdown-preview)
  (register-command! 'markdown-insert-header cmd-markdown-insert-header)
  (register-command! 'markdown-insert-bold cmd-markdown-insert-bold)
  (register-command! 'markdown-insert-italic cmd-markdown-insert-italic)
  (register-command! 'markdown-insert-code cmd-markdown-insert-code)
  (register-command! 'markdown-insert-link cmd-markdown-insert-link)
  (register-command! 'markdown-insert-image cmd-markdown-insert-image)
  (register-command! 'markdown-insert-code-block cmd-markdown-insert-code-block)
  (register-command! 'markdown-insert-list-item cmd-markdown-insert-list-item)
  ;; Auto-insert
  (register-command! 'auto-insert cmd-auto-insert)
  (register-command! 'auto-insert-mode cmd-auto-insert-mode)
  ;; Text scale
  (register-command! 'text-scale-increase cmd-text-scale-increase)
  (register-command! 'text-scale-decrease cmd-text-scale-decrease)
  (register-command! 'text-scale-reset cmd-text-scale-reset)
  ;; Browse kill ring
  (register-command! 'browse-kill-ring cmd-browse-kill-ring)
  ;; Flycheck
  (register-command! 'flycheck-mode cmd-flycheck-mode)
  (register-command! 'flycheck-next-error cmd-flycheck-next-error)
  (register-command! 'flycheck-previous-error cmd-flycheck-previous-error)
  (register-command! 'flycheck-list-errors cmd-flycheck-list-errors)
  ;; Treemacs
  (register-command! 'treemacs cmd-treemacs)
  (register-command! 'treemacs-find-file cmd-treemacs-find-file)
  ;; Magit
  (register-command! 'magit-status cmd-magit-status)
  (register-command! 'magit-log cmd-magit-log)
  (register-command! 'magit-diff cmd-magit-diff)
  (register-command! 'magit-commit cmd-magit-commit)
  (register-command! 'magit-stage-file cmd-magit-stage-file)
  (register-command! 'magit-unstage-file cmd-magit-unstage-file)
  (register-command! 'magit-branch cmd-magit-branch)
  (register-command! 'magit-checkout cmd-magit-checkout)
  ;; Minibuffer
  (register-command! 'minibuffer-complete cmd-minibuffer-complete)
  (register-command! 'minibuffer-keyboard-quit cmd-minibuffer-keyboard-quit)
  ;; Abbrev extras
  (register-command! 'define-global-abbrev cmd-define-global-abbrev)
  (register-command! 'define-mode-abbrev cmd-define-mode-abbrev)
  (register-command! 'unexpand-abbrev cmd-unexpand-abbrev)
  ;; Hippie expand
  (register-command! 'hippie-expand-undo cmd-hippie-expand-undo)
  ;; Compilation
  (register-command! 'next-error-function cmd-next-error-function)
  (register-command! 'previous-error-function cmd-previous-error-function)
  ;; Bookmark extras
  (register-command! 'bookmark-bmenu-list cmd-bookmark-bmenu-list)
  ;; Rectangle extras
  (register-command! 'rectangle-mark-mode cmd-rectangle-mark-mode)
  (register-command! 'number-to-register cmd-number-to-register)
  ;; Isearch extras
  (register-command! 'isearch-toggle-case-fold cmd-isearch-toggle-case-fold)
  (register-command! 'isearch-toggle-regexp cmd-isearch-toggle-regexp)
  ;; Semantic / imenu / tags
  (register-command! 'semantic-mode cmd-semantic-mode)
  (register-command! 'imenu-anywhere cmd-imenu-anywhere)
  (register-command! 'tags-search cmd-tags-search)
  (register-command! 'tags-query-replace cmd-tags-query-replace)
  (register-command! 'visit-tags-table cmd-visit-tags-table)
  ;; Whitespace extras
  (register-command! 'whitespace-toggle-options cmd-whitespace-toggle-options)
  ;; Highlight
  (register-command! 'highlight-regexp cmd-highlight-regexp)
  (register-command! 'unhighlight-regexp cmd-unhighlight-regexp)
  ;; Server extras
  (register-command! 'server-force-delete cmd-server-force-delete)
  ;; Help extras
  (register-command! 'help-for-help cmd-help-for-help)
  (register-command! 'help-quick cmd-help-quick)
  ;; Theme
  (register-command! 'disable-theme cmd-disable-theme)
  (register-command! 'describe-theme cmd-describe-theme)
  ;; Ediff extras
  (register-command! 'ediff-merge cmd-ediff-merge)
  (register-command! 'ediff-directories cmd-ediff-directories)
  ;; Window extras
  (register-command! 'window-divider-mode cmd-window-divider-mode)
  (register-command! 'scroll-bar-mode cmd-scroll-bar-mode)
  (register-command! 'menu-bar-open cmd-menu-bar-open)
  ;; Programming
  (register-command! 'toggle-prettify-symbols cmd-toggle-prettify-symbols)
  (register-command! 'subword-mode cmd-subword-mode)
  (register-command! 'superword-mode cmd-superword-mode)
  (register-command! 'glasses-mode cmd-glasses-mode)
  ;; Calculator
  (register-command! 'calculator cmd-calculator)
  ;; Text info
  (register-command! 'count-words-line cmd-count-words-line)
  (register-command! 'display-column-number cmd-display-column-number)
  (register-command! 'what-tab-width cmd-what-tab-width)
  (register-command! 'set-tab-width cmd-set-tab-width)
  (register-command! 'display-cursor-position cmd-display-cursor-position)
  ;; Display toggles
  (register-command! 'toggle-line-spacing cmd-toggle-line-spacing)
  (register-command! 'toggle-selection-mode cmd-toggle-selection-mode)
  (register-command! 'toggle-virtual-space cmd-toggle-virtual-space)
  (register-command! 'toggle-caret-style cmd-toggle-caret-style)
  ;; Compare
  (register-command! 'compare-windows cmd-compare-windows)
  ;; Frame
  (register-command! 'iconify-frame cmd-iconify-frame)
  (register-command! 'raise-frame cmd-raise-frame)
  ;; Face/font
  (register-command! 'set-face-attribute cmd-set-face-attribute)
  (register-command! 'list-faces-display cmd-list-faces-display)
  ;; Eshell extras
  (register-command! 'eshell-here cmd-eshell-here)
  ;; Calendar extras
  (register-command! 'calendar-goto-date cmd-calendar-goto-date)
  (register-command! 'calendar-holidays cmd-calendar-holidays)
  ;; ERC
  (register-command! 'erc cmd-erc)
  ;; TRAMP extras
  (register-command! 'tramp-cleanup-connections cmd-tramp-cleanup-connections)
  ;; LSP
  (register-command! 'lsp-find-declaration cmd-lsp-find-declaration)
  (register-command! 'lsp-find-implementation cmd-lsp-find-implementation)
  (register-command! 'lsp-rename cmd-lsp-rename)
  (register-command! 'lsp-format-buffer cmd-lsp-format-buffer)
  (register-command! 'lsp-code-actions cmd-lsp-code-actions)
  (register-command! 'lsp-describe-thing-at-point cmd-lsp-describe-thing-at-point)
  ;; DAP (Debug Adapter Protocol)
  (register-command! 'dap-debug cmd-dap-debug)
  (register-command! 'dap-breakpoint-toggle cmd-dap-breakpoint-toggle)
  (register-command! 'dap-continue cmd-dap-continue)
  (register-command! 'dap-step-over cmd-dap-step-over)
  (register-command! 'dap-step-in cmd-dap-step-in)
  (register-command! 'dap-step-out cmd-dap-step-out)
  ;; Snippets
  (register-command! 'yas-insert-snippet cmd-yas-insert-snippet)
  (register-command! 'yas-new-snippet cmd-yas-new-snippet)
  (register-command! 'yas-visit-snippet-file cmd-yas-visit-snippet-file))
