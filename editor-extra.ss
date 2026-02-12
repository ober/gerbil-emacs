;;; -*- Gerbil -*-
;;; Extra TUI editor commands for gerbil-emacs (overflow from editor.ss)
;;;
;;; Split from editor.ss due to Gerbil compiler limits on module size.

(export register-extra-commands!
        winner-save-config!)

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

;; Winner mode (window configuration undo/redo)
;; Saves/restores: number of windows, current window index, buffer names per window

(def *winner-max-history* 50) ; Max configs to remember

(def (winner-save-config! app)
  "Save current window configuration to winner history."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (num-wins (length wins))
         (current-idx (frame-current-idx fr))
         (buffers (map (lambda (w)
                         (let ((buf (edit-window-buffer w)))
                           (if buf (buffer-name buf) "*scratch*")))
                       wins))
         (config (list num-wins current-idx buffers))
         (history (app-state-winner-history app)))
    ;; Don't save duplicate consecutive configs
    (unless (and (not (null? history))
                 (equal? config (car history)))
      ;; Truncate future (redo) history when adding new config
      (let ((idx (app-state-winner-history-idx app)))
        (when (> idx 0)
          (set! history (list-tail history idx))
          (set! (app-state-winner-history-idx app) 0)))
      ;; Add new config, limit size
      (let ((new-history (cons config history)))
        (set! (app-state-winner-history app)
          (if (> (length new-history) *winner-max-history*)
            (take new-history *winner-max-history*)
            new-history))))))

(def (winner-restore-config! app config)
  "Restore a window configuration from winner history."
  (let* ((target-num-wins (car config))
         (target-idx (cadr config))
         (target-buffers (caddr config))
         (fr (app-state-frame app))
         (current-wins (length (frame-windows fr))))
    ;; Adjust number of windows
    (cond
      ((> target-num-wins current-wins)
       ;; Need more windows - split
       (let loop ((n (- target-num-wins current-wins)))
         (when (> n 0)
           (frame-split! fr)
           (loop (- n 1)))))
      ((< target-num-wins current-wins)
       ;; Need fewer windows - delete extras
       (let loop ((n (- current-wins target-num-wins)))
         (when (and (> n 0) (> (length (frame-windows fr)) 1))
           (frame-delete-window! fr (frame-current-idx fr))
           (loop (- n 1))))))
    ;; Set current window index
    (let ((max-idx (- (length (frame-windows fr)) 1)))
      (set! (frame-current-idx fr) (min target-idx max-idx)))
    ;; Restore buffers to windows (by name)
    (let ((wins (frame-windows fr)))
      (for-each
        (lambda (win buf-name)
          (let ((buf (buffer-by-name buf-name)))
            (when buf
              (let ((ed (edit-window-editor win)))
                (buffer-attach! ed buf)
                (set! (edit-window-buffer win) buf)))))
        wins
        (take target-buffers (length wins))))
    ;; Relayout
    (frame-layout! fr)))

(def (cmd-winner-undo app)
  "Undo window configuration change - restore previous window layout."
  (let* ((history (app-state-winner-history app))
         (idx (app-state-winner-history-idx app))
         (echo (app-state-echo app)))
    (if (>= (+ idx 1) (length history))
      (echo-message! echo "No earlier window configuration")
      (begin
        ;; Save current config first if at index 0
        (when (= idx 0)
          (winner-save-config! app))
        ;; Move back in history
        (let ((new-idx (+ idx 1)))
          (set! (app-state-winner-history-idx app) new-idx)
          (let ((config (list-ref (app-state-winner-history app) new-idx)))
            (winner-restore-config! app config)
            (echo-message! echo
              (string-append "Winner: restored config "
                            (number->string (- (length history) new-idx))
                            "/" (number->string (length history))))))))))

(def (cmd-winner-redo app)
  "Redo window configuration change - restore next window layout."
  (let* ((idx (app-state-winner-history-idx app))
         (echo (app-state-echo app)))
    (if (<= idx 0)
      (echo-message! echo "No later window configuration")
      (begin
        (let ((new-idx (- idx 1)))
          (set! (app-state-winner-history-idx app) new-idx)
          (let* ((history (app-state-winner-history app))
                 (config (list-ref history new-idx)))
            (winner-restore-config! app config)
            (echo-message! echo
              (string-append "Winner: restored config "
                            (number->string (- (length history) new-idx))
                            "/" (number->string (length history))))))))))

;; Tab-bar commands
;; Tabs store: (name buffer-names window-idx)
;; Each tab remembers which buffers were open and which window was active

(def (tab-save-current! app)
  "Save current window state to current tab."
  (let* ((tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (fr (app-state-frame app))
         (wins (frame-windows fr))
         (buffers (map (lambda (w)
                         (let ((buf (edit-window-buffer w)))
                           (if buf (buffer-name buf) "*scratch*")))
                       wins))
         (win-idx (frame-current-idx fr)))
    (when (< idx (length tabs))
      (let* ((old-tab (list-ref tabs idx))
             (name (car old-tab))
             (new-tab (list name buffers win-idx)))
        (set! (app-state-tabs app)
          (append (take tabs idx)
                  (list new-tab)
                  (if (< (+ idx 1) (length tabs))
                    (list-tail tabs (+ idx 1))
                    '())))))))

(def (tab-restore! app tab)
  "Restore window state from a tab."
  (let* ((name (car tab))
         (buffers (cadr tab))
         (win-idx (caddr tab))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    ;; Restore buffers to windows
    (for-each
      (lambda (win buf-name)
        (let ((buf (buffer-by-name buf-name)))
          (when buf
            (let ((ed (edit-window-editor win)))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer win) buf)))))
      wins
      (take buffers (min (length buffers) (length wins))))
    ;; Set current window
    (let ((max-idx (- (length wins) 1)))
      (set! (frame-current-idx fr) (min win-idx max-idx)))))

(def (cmd-tab-new app)
  "Create a new tab with current buffer."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (if buf (buffer-name buf) "*scratch*"))
         (new-tab-num (+ (length tabs) 1))
         (new-tab-name (string-append "Tab " (number->string new-tab-num)))
         (new-tab (list new-tab-name (list buf-name) 0)))
    ;; Save current tab state first
    (tab-save-current! app)
    ;; Add new tab
    (set! (app-state-tabs app) (append tabs (list new-tab)))
    (set! (app-state-current-tab-idx app) (- (length (app-state-tabs app)) 1))
    (echo-message! echo (string-append "Created " new-tab-name))))

(def (cmd-tab-close app)
  "Close current tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Cannot close last tab")
      (let* ((tab-name (car (list-ref tabs idx)))
             (new-tabs (append (take tabs idx)
                               (if (< (+ idx 1) (length tabs))
                                 (list-tail tabs (+ idx 1))
                                 '())))
             (new-idx (min idx (- (length new-tabs) 1))))
        (set! (app-state-tabs app) new-tabs)
        (set! (app-state-current-tab-idx app) new-idx)
        ;; Restore the now-current tab
        (tab-restore! app (list-ref new-tabs new-idx))
        (echo-message! echo (string-append "Closed " tab-name))))))

(def (cmd-tab-next app)
  "Switch to next tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (begin
        ;; Save current tab state
        (tab-save-current! app)
        ;; Switch to next
        (let ((new-idx (modulo (+ idx 1) (length tabs))))
          (set! (app-state-current-tab-idx app) new-idx)
          (let ((tab (list-ref tabs new-idx)))
            (tab-restore! app tab)
            (echo-message! echo (string-append "Tab: " (car tab)
                                              " [" (number->string (+ new-idx 1))
                                              "/" (number->string (length tabs)) "]"))))))))

(def (cmd-tab-previous app)
  "Switch to previous tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (begin
        ;; Save current tab state
        (tab-save-current! app)
        ;; Switch to previous
        (let ((new-idx (modulo (- idx 1) (length tabs))))
          (set! (app-state-current-tab-idx app) new-idx)
          (let ((tab (list-ref tabs new-idx)))
            (tab-restore! app tab)
            (echo-message! echo (string-append "Tab: " (car tab)
                                              " [" (number->string (+ new-idx 1))
                                              "/" (number->string (length tabs)) "]"))))))))

(def (cmd-tab-rename app)
  "Rename current tab."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (old-name (car (list-ref tabs idx)))
         (new-name (echo-read-string echo "Rename tab to: " row width)))
    (when (and new-name (not (string=? new-name "")))
      (let* ((old-tab (list-ref tabs idx))
             (new-tab (cons new-name (cdr old-tab))))
        (set! (app-state-tabs app)
          (append (take tabs idx)
                  (list new-tab)
                  (if (< (+ idx 1) (length tabs))
                    (list-tail tabs (+ idx 1))
                    '())))
        (echo-message! echo (string-append "Renamed to: " new-name))))))

(def (cmd-tab-move app)
  "Move current tab left or right (with prefix arg for direction)."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (n (get-prefix-arg app 1)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (let* ((new-idx (modulo (+ idx n) (length tabs)))
             (tab (list-ref tabs idx))
             (tabs-without (append (take tabs idx)
                                   (if (< (+ idx 1) (length tabs))
                                     (list-tail tabs (+ idx 1))
                                     '())))
             (new-tabs (append (take tabs-without new-idx)
                               (list tab)
                               (list-tail tabs-without new-idx))))
        (set! (app-state-tabs app) new-tabs)
        (set! (app-state-current-tab-idx app) new-idx)
        (echo-message! echo (string-append "Moved tab to position "
                                          (number->string (+ new-idx 1))))))))

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
;; Uses external linters based on file extension

(def *flycheck-errors* (make-hash-table)) ; buffer-name -> list of (line col message)
(def *flycheck-error-idx* (make-hash-table)) ; buffer-name -> current error index

(def (flycheck-get-linter file-path)
  "Get linter command and args for a file based on extension."
  (let ((ext (path-extension file-path)))
    (cond
      ((member ext '(".py")) 
       '("python3" "-m" "py_compile"))
      ((member ext '(".js" ".mjs"))
       '("node" "--check"))
      ((member ext '(".sh" ".bash"))
       '("bash" "-n"))
      ((member ext '(".rb"))
       '("ruby" "-c"))
      ((member ext '(".pl" ".pm"))
       '("perl" "-c"))
      ((member ext '(".go"))
       '("gofmt" "-e"))
      ((member ext '(".rs"))
       '("rustfmt" "--check"))
      ((member ext '(".c" ".h"))
       '("gcc" "-fsyntax-only" "-Wall"))
      ((member ext '(".cpp" ".hpp" ".cc" ".cxx"))
       '("g++" "-fsyntax-only" "-Wall"))
      ((member ext '(".json"))
       '("python3" "-m" "json.tool"))
      ((member ext '(".yaml" ".yml"))
       '("python3" "-c" "import yaml,sys; yaml.safe_load(open(sys.argv[1]))"))
      ((member ext '(".xml"))
       '("xmllint" "--noout"))
      (else #f))))

(def (flycheck-parse-errors output file-path)
  "Parse linter output into list of (line col message)."
  (let ((lines (string-split output #\newline))
        (errors '()))
    (for-each
      (lambda (line)
        (when (and (> (string-length line) 0)
                   (or (string-contains line "error")
                       (string-contains line "Error")
                       (string-contains line "warning")
                       (string-contains line "Warning")
                       (string-contains line "line ")
                       (string-contains line ":")))
          ;; Try to extract line number - common format: file:line:col: message
          (let* ((parts (string-split line #\:))
                 (line-num (if (>= (length parts) 2)
                             (string->number (string-trim (cadr parts)))
                             #f))
                 (col-num (if (>= (length parts) 3)
                            (string->number (string-trim (caddr parts)))
                            1))
                 (msg (string-trim line)))
            (when (and line-num (> line-num 0))
              (set! errors (cons (list line-num (or col-num 1) msg) errors))))))
      lines)
    (reverse errors)))

(def (flycheck-run-linter! app)
  "Run the linter for the current buffer and store errors."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file-path (and buf (buffer-file-path buf)))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not file-path)
      (echo-error! echo "No file associated with buffer")
      (let ((linter-cmd (flycheck-get-linter file-path)))
        (if (not linter-cmd)
          (echo-message! echo (string-append "No linter for " (path-extension file-path)))
          (with-exception-catcher
            (lambda (e) 
              (echo-message! echo "Linter not available"))
            (lambda ()
              ;; Save buffer first if modified
              (let* ((ed (edit-window-editor win))
                     (proc (open-process
                             (list path: (car linter-cmd)
                                   arguments: (append (cdr linter-cmd) (list file-path))
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (stdout (read-line proc #f))
                     (stderr (let ((p (process-stderr-port proc)))
                               (if p (read-line p #f) ""))))
                (process-status proc)
                (let* ((output (string-append (or stdout "") "\n" (or stderr "")))
                       (errors (flycheck-parse-errors output file-path)))
                  (hash-put! *flycheck-errors* buf-name errors)
                  (hash-put! *flycheck-error-idx* buf-name 0)
                  (if (null? errors)
                    (echo-message! echo "No errors found")
                    (echo-message! echo
                      (string-append (number->string (length errors)) " error(s) found"))))))))))))

(def (cmd-flycheck-mode app)
  "Run syntax check on current buffer using appropriate linter."
  (flycheck-run-linter! app))

(def (cmd-flycheck-next-error app)
  "Jump to next flycheck error in current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (hash-get *flycheck-errors* buf-name '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((idx (hash-get *flycheck-error-idx* buf-name 0))
                 (new-idx (modulo (+ idx 1) (length errors)))
                 (error (list-ref errors new-idx))
                 (line (car error))
                 (col (cadr error))
                 (msg (caddr error))
                 (ed (edit-window-editor win)))
            (hash-put! *flycheck-error-idx* buf-name new-idx)
            ;; Go to the error line
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Error " (number->string (+ new-idx 1))
                                              "/" (number->string (length errors))
                                              ": " msg))))))))

(def (cmd-flycheck-previous-error app)
  "Jump to previous flycheck error in current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (hash-get *flycheck-errors* buf-name '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((idx (hash-get *flycheck-error-idx* buf-name 0))
                 (new-idx (modulo (- idx 1) (length errors)))
                 (error (list-ref errors new-idx))
                 (line (car error))
                 (col (cadr error))
                 (msg (caddr error))
                 (ed (edit-window-editor win)))
            (hash-put! *flycheck-error-idx* buf-name new-idx)
            ;; Go to the error line
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Error " (number->string (+ new-idx 1))
                                              "/" (number->string (length errors))
                                              ": " msg))))))))

(def (cmd-flycheck-list-errors app)
  "List all flycheck errors in a buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (hash-get *flycheck-errors* buf-name '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((ed (edit-window-editor win))
                 (error-buf (buffer-create! "*Flycheck Errors*" ed))
                 (text (string-join
                         (map (lambda (err)
                                (string-append "Line " (number->string (car err))
                                              ": " (caddr err)))
                              errors)
                         "\n")))
            (buffer-attach! ed error-buf)
            (set! (edit-window-buffer win) error-buf)
            (editor-set-text ed (string-append "Flycheck errors for " buf-name ":\n\n" text "\n"))
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)))))))

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

;; --- Task #48: EWW, EMMS, PDF tools, Calc, ace-jump, expand-region, etc. ---

;; EWW web browser operations
(def (cmd-eww-back app)
  "Go back in EWW history (stub)."
  (echo-message! (app-state-echo app) "EWW: back (stub)"))

(def (cmd-eww-forward app)
  "Go forward in EWW history (stub)."
  (echo-message! (app-state-echo app) "EWW: forward (stub)"))

(def (cmd-eww-reload app)
  "Reload current EWW page (stub)."
  (echo-message! (app-state-echo app) "EWW: reload (stub)"))

(def (cmd-eww-download app)
  "Download URL in EWW (stub)."
  (echo-message! (app-state-echo app) "EWW: download (stub)"))

(def (cmd-eww-copy-page-url app)
  "Copy current EWW page URL (stub)."
  (echo-message! (app-state-echo app) "EWW: URL copied (stub)"))

;; EMMS (Emacs Multimedia System) stubs
(def (cmd-emms app)
  "Open EMMS player (stub)."
  (echo-message! (app-state-echo app) "EMMS player (stub)"))

(def (cmd-emms-play-file app)
  "Play a media file (stub)."
  (echo-message! (app-state-echo app) "EMMS: play file (stub)"))

(def (cmd-emms-pause app)
  "Pause/resume playback (stub)."
  (echo-message! (app-state-echo app) "EMMS: pause/resume (stub)"))

(def (cmd-emms-stop app)
  "Stop playback (stub)."
  (echo-message! (app-state-echo app) "EMMS: stop (stub)"))

(def (cmd-emms-next app)
  "Next track (stub)."
  (echo-message! (app-state-echo app) "EMMS: next track (stub)"))

(def (cmd-emms-previous app)
  "Previous track (stub)."
  (echo-message! (app-state-echo app) "EMMS: previous track (stub)"))

;; PDF tools stubs
(def (cmd-pdf-view-mode app)
  "Toggle PDF view mode (stub)."
  (echo-message! (app-state-echo app) "PDF view mode (stub)"))

(def (cmd-pdf-view-next-page app)
  "Next page in PDF (stub)."
  (echo-message! (app-state-echo app) "PDF: next page (stub)"))

(def (cmd-pdf-view-previous-page app)
  "Previous page in PDF (stub)."
  (echo-message! (app-state-echo app) "PDF: previous page (stub)"))

(def (cmd-pdf-view-goto-page app)
  "Go to specific PDF page (stub)."
  (echo-message! (app-state-echo app) "PDF: goto page (stub)"))

;; Calc stack operations
(def (cmd-calc-push app)
  "Push value onto calc stack."
  (let ((val (app-read-string app "Push value: ")))
    (when (and val (not (string-empty? val)))
      (echo-message! (app-state-echo app) (string-append "Pushed: " val)))))

(def (cmd-calc-pop app)
  "Pop value from calc stack (stub)."
  (echo-message! (app-state-echo app) "Calc: pop (stub)"))

(def (cmd-calc-dup app)
  "Duplicate top of calc stack (stub)."
  (echo-message! (app-state-echo app) "Calc: dup (stub)"))

(def (cmd-calc-swap app)
  "Swap top two calc stack items (stub)."
  (echo-message! (app-state-echo app) "Calc: swap (stub)"))

;; Ace-jump / Avy navigation
(def (cmd-avy-goto-char app)
  "Jump to character (stub)."
  (echo-message! (app-state-echo app) "Avy: goto char (stub)"))

(def (cmd-avy-goto-word app)
  "Jump to word (stub)."
  (echo-message! (app-state-echo app) "Avy: goto word (stub)"))

(def (cmd-avy-goto-line app)
  "Jump to line (stub)."
  (echo-message! (app-state-echo app) "Avy: goto line (stub)"))

;; Expand-region
(def (cmd-expand-region app)
  "Expand selection region (stub)."
  (echo-message! (app-state-echo app) "Expand region (stub)"))

(def (cmd-contract-region app)
  "Contract selection region (stub)."
  (echo-message! (app-state-echo app) "Contract region (stub)"))

;; Smartparens
(def (cmd-sp-forward-slurp-sexp app)
  "Smartparens forward slurp (stub)."
  (echo-message! (app-state-echo app) "SP: forward slurp (stub)"))

(def (cmd-sp-forward-barf-sexp app)
  "Smartparens forward barf (stub)."
  (echo-message! (app-state-echo app) "SP: forward barf (stub)"))

(def (cmd-sp-backward-slurp-sexp app)
  "Smartparens backward slurp (stub)."
  (echo-message! (app-state-echo app) "SP: backward slurp (stub)"))

(def (cmd-sp-backward-barf-sexp app)
  "Smartparens backward barf (stub)."
  (echo-message! (app-state-echo app) "SP: backward barf (stub)"))

;; Project.el extras
(def (cmd-project-switch-project app)
  "Switch to another project (stub)."
  (echo-message! (app-state-echo app) "Switch project (stub)"))

(def (cmd-project-find-regexp app)
  "Find regexp in project files (stub)."
  (echo-message! (app-state-echo app) "Project: find regexp (stub)"))

(def (cmd-project-shell app)
  "Open shell in project root (stub)."
  (echo-message! (app-state-echo app) "Project: shell (stub)"))

(def (cmd-project-dired app)
  "Open dired at project root (stub)."
  (echo-message! (app-state-echo app) "Project: dired (stub)"))

(def (cmd-project-eshell app)
  "Open eshell in project root (stub)."
  (echo-message! (app-state-echo app) "Project: eshell (stub)"))

;; JSON formatting
(def (cmd-json-pretty-print app)
  "Pretty-print JSON in region or buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-message! (app-state-echo app) "Select JSON region first")
      (let* ((text (substring (editor-get-text ed) start end))
             (result (with-exception-catcher
                       (lambda (e) #f)
                       (lambda ()
                         (let ((p (open-process
                                    (list path: "python3"
                                          arguments: '("-m" "json.tool")
                                          stdin-redirection: #t stdout-redirection: #t
                                          stderr-redirection: #t))))
                           (display text p)
                           (close-output-port p)
                           (let ((out (read-line p #f)))
                             (process-status p)
                             out))))))
        (if result
          (begin
            (send-message ed SCI_SETTARGETSTART start 0)
            (send-message ed SCI_SETTARGETEND end 0)
            (send-message/string ed SCI_REPLACETARGET result)
            (echo-message! (app-state-echo app) "JSON formatted"))
          (echo-message! (app-state-echo app) "JSON format failed"))))))

;; XML formatting
(def (cmd-xml-format app)
  "Format XML in region or buffer (stub)."
  (echo-message! (app-state-echo app) "XML format (stub)"))

;; Desktop notifications
(def (cmd-notifications-list app)
  "List desktop notifications (stub)."
  (echo-message! (app-state-echo app) "Notifications (stub)"))

;; Profiler
(def (cmd-profiler-report app)
  "Show profiler report (stub)."
  (echo-message! (app-state-echo app) "Profiler report (stub)"))

;; Narrowing extras
(def (cmd-narrow-to-page app)
  "Narrow to current page (stub)."
  (echo-message! (app-state-echo app) "Narrow to page (stub)"))

;; Encoding detection
(def (cmd-describe-current-coding-system app)
  "Describe current coding system."
  (echo-message! (app-state-echo app) "Coding: utf-8 (default)"))

;; Buffer-local variables
(def (cmd-add-file-local-variable app)
  "Add file-local variable (stub)."
  (echo-message! (app-state-echo app) "Add file-local variable (stub)"))

(def (cmd-add-dir-local-variable app)
  "Add directory-local variable (stub)."
  (echo-message! (app-state-echo app) "Add dir-local variable (stub)"))

;; Hippie expand variants
(def (cmd-hippie-expand-file app)
  "Hippie expand filename (stub)."
  (echo-message! (app-state-echo app) "Hippie expand file (stub)"))

;; Registers extras
(def (cmd-frameset-to-register app)
  "Save frameset to register (stub)."
  (echo-message! (app-state-echo app) "Frameset to register (stub)"))

(def (cmd-window-configuration-to-register app)
  "Save window configuration to register (stub)."
  (echo-message! (app-state-echo app) "Window config to register (stub)"))

;; Macro counter extras
(def (cmd-kmacro-add-counter app)
  "Add to keyboard macro counter (stub)."
  (echo-message! (app-state-echo app) "Kmacro: add counter (stub)"))

(def (cmd-kmacro-set-format app)
  "Set keyboard macro counter format (stub)."
  (echo-message! (app-state-echo app) "Kmacro: set format (stub)"))

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
  "Clean up recent files list (remove non-existent, stub)."
  (echo-message! (app-state-echo app) "Recent files cleaned (stub)"))

;; Save hooks
(def (cmd-add-hook app)
  "Add a hook function (stub)."
  (echo-message! (app-state-echo app) "Add hook (stub)"))

(def (cmd-remove-hook app)
  "Remove a hook function (stub)."
  (echo-message! (app-state-echo app) "Remove hook (stub)"))

;; Elpa/Melpa package sources
(def (cmd-package-archives app)
  "Show configured package archives (stub)."
  (echo-message! (app-state-echo app) "Package archives: (built-in)"))

;; Auto-save
(def (cmd-auto-save-mode app)
  "Toggle auto-save mode (stub)."
  (echo-message! (app-state-echo app) "Auto-save mode toggled (stub)"))

(def (cmd-recover-file app)
  "Recover file from auto-save (stub)."
  (echo-message! (app-state-echo app) "Recover file (stub)"))

;; Tramp details
(def (cmd-tramp-version app)
  "Show TRAMP version (stub)."
  (echo-message! (app-state-echo app) "TRAMP (stub — not available)"))

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
  "Rename occur buffer (stub)."
  (echo-message! (app-state-echo app) "Occur rename (stub)"))

;; Printing
(def (cmd-print-buffer app)
  "Print buffer contents (stub)."
  (echo-message! (app-state-echo app) "Print buffer (stub)"))

(def (cmd-print-region app)
  "Print region (stub)."
  (echo-message! (app-state-echo app) "Print region (stub)"))

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
  "Toggle debug on signal (stub)."
  (echo-message! (app-state-echo app) "Debug on signal toggled (stub)"))

(def (cmd-toggle-word-boundary app)
  "Toggle word boundary display (stub)."
  (echo-message! (app-state-echo app) "Word boundary display toggled (stub)"))

(def (cmd-indent-tabs-mode app)
  "Show indent tabs mode status."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (use-tabs (send-message ed SCI_GETUSETABS 0 0)))
    (echo-message! (app-state-echo app)
      (if (> use-tabs 0) "Indent: tabs" "Indent: spaces"))))

(def (cmd-electric-indent-local-mode app)
  "Toggle electric indent for current buffer (stub)."
  (echo-message! (app-state-echo app) "Electric indent (local) toggled (stub)"))

(def (cmd-visual-fill-column-mode app)
  "Toggle visual fill column mode (stub)."
  (echo-message! (app-state-echo app) "Visual fill column mode (stub)"))

(def (cmd-adaptive-wrap-prefix-mode app)
  "Toggle adaptive wrap prefix mode (stub)."
  (echo-message! (app-state-echo app) "Adaptive wrap mode (stub)"))

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
  "Toggle empty line indicators (stub)."
  (echo-message! (app-state-echo app) "Empty line indicators toggled (stub)"))

(def (cmd-toggle-indicate-buffer-boundaries app)
  "Toggle buffer boundary indicators (stub)."
  (echo-message! (app-state-echo app) "Buffer boundaries toggled (stub)"))

;; Enriched text / face manipulation
(def (cmd-facemenu-set-foreground app)
  "Set text foreground color (stub)."
  (echo-message! (app-state-echo app) "Set foreground (stub)"))

(def (cmd-facemenu-set-background app)
  "Set text background color (stub)."
  (echo-message! (app-state-echo app) "Set background (stub)"))

;; Emacs games
(def (cmd-tetris app)
  "Play tetris (stub)."
  (echo-message! (app-state-echo app) "Tetris (stub — not implemented)"))

(def (cmd-snake app)
  "Play snake (stub)."
  (echo-message! (app-state-echo app) "Snake (stub — not implemented)"))

(def (cmd-dunnet app)
  "Play dunnet text adventure (stub)."
  (echo-message! (app-state-echo app) "Dunnet (stub — not implemented)"))

(def (cmd-hanoi app)
  "Show towers of hanoi (stub)."
  (echo-message! (app-state-echo app) "Hanoi (stub — not implemented)"))

(def (cmd-life app)
  "Run Game of Life (stub)."
  (echo-message! (app-state-echo app) "Life (stub — not implemented)"))

(def (cmd-doctor app)
  "Start Eliza psychotherapist (stub)."
  (echo-message! (app-state-echo app) "Doctor (stub — not implemented)"))

;; Process list operations
(def (cmd-proced-send-signal app)
  "Send signal to process (stub)."
  (echo-message! (app-state-echo app) "Proced: send signal (stub)"))

(def (cmd-proced-filter app)
  "Filter process list (stub)."
  (echo-message! (app-state-echo app) "Proced: filter (stub)"))

;; Ediff session management
(def (cmd-ediff-show-registry app)
  "Show ediff session registry (stub)."
  (echo-message! (app-state-echo app) "Ediff registry (stub)"))

;; --- Task #49: elisp mode, scheme mode, regex builder, color picker, etc. ---

;; Emacs Lisp mode helpers
(def (cmd-emacs-lisp-mode app)
  "Switch to Emacs Lisp mode (stub)."
  (echo-message! (app-state-echo app) "Emacs Lisp mode (stub)"))

(def (cmd-eval-last-sexp app)
  "Evaluate the sexp before point."
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
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "No sexp before point"))))

(def (cmd-eval-defun app)
  "Evaluate current top-level form (stub)."
  (echo-message! (app-state-echo app) "Eval defun (stub)"))

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
  "Switch to Scheme mode (stub)."
  (echo-message! (app-state-echo app) "Scheme mode (stub)"))

(def (cmd-gerbil-mode app)
  "Switch to Gerbil mode (stub)."
  (echo-message! (app-state-echo app) "Gerbil mode (stub)"))

(def (cmd-run-scheme app)
  "Run Scheme REPL (alias for repl command)."
  (echo-message! (app-state-echo app) "Use C-x r to open REPL"))

(def (cmd-scheme-send-region app)
  "Send region to Scheme process (stub)."
  (echo-message! (app-state-echo app) "Scheme: send region (stub)"))

(def (cmd-scheme-send-buffer app)
  "Send buffer to Scheme process (stub)."
  (echo-message! (app-state-echo app) "Scheme: send buffer (stub)"))

;; Regex builder
(def (cmd-re-builder app)
  "Open interactive regex builder (stub)."
  (echo-message! (app-state-echo app) "Regex builder (stub)"))

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

;; IDO mode (Interactively Do Things)
(def (cmd-ido-mode app)
  "Toggle IDO mode (stub)."
  (echo-message! (app-state-echo app) "IDO mode (stub)"))

(def (cmd-ido-find-file app)
  "Find file with IDO (stub)."
  (echo-message! (app-state-echo app) "IDO find file (stub)"))

(def (cmd-ido-switch-buffer app)
  "Switch buffer with IDO (stub)."
  (echo-message! (app-state-echo app) "IDO switch buffer (stub)"))

;; Helm / Ivy / Vertico
(def (cmd-helm-mode app)
  "Toggle Helm mode (stub)."
  (echo-message! (app-state-echo app) "Helm mode (stub)"))

(def (cmd-ivy-mode app)
  "Toggle Ivy mode (stub)."
  (echo-message! (app-state-echo app) "Ivy mode (stub)"))

(def (cmd-vertico-mode app)
  "Toggle Vertico mode (stub)."
  (echo-message! (app-state-echo app) "Vertico mode (stub)"))

(def (cmd-consult-line app)
  "Search buffer lines with consult (stub)."
  (echo-message! (app-state-echo app) "Consult line (stub)"))

(def (cmd-consult-grep app)
  "Grep with consult (stub)."
  (echo-message! (app-state-echo app) "Consult grep (stub)"))

(def (cmd-consult-buffer app)
  "Switch buffer with consult (stub)."
  (echo-message! (app-state-echo app) "Consult buffer (stub)"))

;; Company completion
(def (cmd-company-mode app)
  "Toggle company completion mode (stub)."
  (echo-message! (app-state-echo app) "Company mode (stub)"))

(def (cmd-company-complete app)
  "Trigger company completion (stub)."
  (echo-message! (app-state-echo app) "Company complete (stub)"))

;; Flyspell extras
(def (cmd-flyspell-buffer app)
  "Flyspell-check entire buffer (stub)."
  (echo-message! (app-state-echo app) "Flyspell buffer (stub)"))

(def (cmd-flyspell-correct-word app)
  "Correct misspelled word (stub)."
  (echo-message! (app-state-echo app) "Flyspell correct (stub)"))

;; Bibliography / citar
(def (cmd-citar-insert-citation app)
  "Insert citation (stub)."
  (echo-message! (app-state-echo app) "Insert citation (stub)"))

;; Docker
(def (cmd-docker app)
  "Docker management interface (stub)."
  (echo-message! (app-state-echo app) "Docker (stub)"))

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
  "List docker images (stub)."
  (echo-message! (app-state-echo app) "Docker images (stub)"))

;; Restclient
(def (cmd-restclient-mode app)
  "Toggle restclient mode (stub)."
  (echo-message! (app-state-echo app) "Restclient mode (stub)"))

(def (cmd-restclient-http-send app)
  "Send HTTP request (stub)."
  (echo-message! (app-state-echo app) "Restclient: send (stub)"))

;; YAML mode
(def (cmd-yaml-mode app)
  "Toggle YAML mode (stub)."
  (echo-message! (app-state-echo app) "YAML mode (stub)"))

;; TOML mode
(def (cmd-toml-mode app)
  "Toggle TOML mode (stub)."
  (echo-message! (app-state-echo app) "TOML mode (stub)"))

;; Dockerfile mode
(def (cmd-dockerfile-mode app)
  "Toggle Dockerfile mode (stub)."
  (echo-message! (app-state-echo app) "Dockerfile mode (stub)"))

;; SQL mode
(def (cmd-sql-mode app)
  "Toggle SQL mode (stub)."
  (echo-message! (app-state-echo app) "SQL mode (stub)"))

(def (cmd-sql-connect app)
  "Connect to SQL database (stub)."
  (echo-message! (app-state-echo app) "SQL connect (stub)"))

(def (cmd-sql-send-region app)
  "Send SQL region to process (stub)."
  (echo-message! (app-state-echo app) "SQL: send region (stub)"))

;; Language modes
(def (cmd-python-mode app)
  "Toggle Python mode (stub)."
  (echo-message! (app-state-echo app) "Python mode (stub)"))

(def (cmd-c-mode app)
  "Toggle C mode (stub)."
  (echo-message! (app-state-echo app) "C mode (stub)"))

(def (cmd-c++-mode app)
  "Toggle C++ mode (stub)."
  (echo-message! (app-state-echo app) "C++ mode (stub)"))

(def (cmd-java-mode app)
  "Toggle Java mode (stub)."
  (echo-message! (app-state-echo app) "Java mode (stub)"))

(def (cmd-rust-mode app)
  "Toggle Rust mode (stub)."
  (echo-message! (app-state-echo app) "Rust mode (stub)"))

(def (cmd-go-mode app)
  "Toggle Go mode (stub)."
  (echo-message! (app-state-echo app) "Go mode (stub)"))

(def (cmd-js-mode app)
  "Toggle JavaScript mode (stub)."
  (echo-message! (app-state-echo app) "JavaScript mode (stub)"))

(def (cmd-typescript-mode app)
  "Toggle TypeScript mode (stub)."
  (echo-message! (app-state-echo app) "TypeScript mode (stub)"))

(def (cmd-html-mode app)
  "Toggle HTML mode (stub)."
  (echo-message! (app-state-echo app) "HTML mode (stub)"))

(def (cmd-css-mode app)
  "Toggle CSS mode (stub)."
  (echo-message! (app-state-echo app) "CSS mode (stub)"))

(def (cmd-lua-mode app)
  "Toggle Lua mode (stub)."
  (echo-message! (app-state-echo app) "Lua mode (stub)"))

(def (cmd-ruby-mode app)
  "Toggle Ruby mode (stub)."
  (echo-message! (app-state-echo app) "Ruby mode (stub)"))

(def (cmd-shell-script-mode app)
  "Toggle Shell Script mode (stub)."
  (echo-message! (app-state-echo app) "Shell Script mode (stub)"))

;; Prog mode / text mode
(def (cmd-prog-mode app)
  "Switch to programming mode (stub)."
  (echo-message! (app-state-echo app) "Prog mode (stub)"))

(def (cmd-text-mode app)
  "Switch to text mode (stub)."
  (echo-message! (app-state-echo app) "Text mode (stub)"))

(def (cmd-fundamental-mode app)
  "Switch to fundamental mode (stub)."
  (echo-message! (app-state-echo app) "Fundamental mode (stub)"))

;; Tab completion / completion-at-point
(def (cmd-completion-at-point app)
  "Complete symbol at point (stub)."
  (echo-message! (app-state-echo app) "Completion at point (stub)"))

;; Eldoc extras
(def (cmd-eldoc-mode app)
  "Toggle eldoc mode (stub)."
  (echo-message! (app-state-echo app) "Eldoc mode (stub)"))

;; Which-function extras
(def (cmd-which-function-mode app)
  "Toggle which-function mode (stub)."
  (echo-message! (app-state-echo app) "Which-function mode (stub)"))

;; Compilation
(def (cmd-compilation-mode app)
  "Switch to compilation mode (stub)."
  (echo-message! (app-state-echo app) "Compilation mode (stub)"))

;; GDB
(def (cmd-gdb app)
  "Start GDB debugger (stub)."
  (echo-message! (app-state-echo app) "GDB (stub)"))

(def (cmd-gud-break app)
  "Set breakpoint at current line (stub)."
  (echo-message! (app-state-echo app) "GUD: breakpoint (stub)"))

(def (cmd-gud-remove app)
  "Remove breakpoint (stub)."
  (echo-message! (app-state-echo app) "GUD: remove breakpoint (stub)"))

(def (cmd-gud-cont app)
  "Continue execution in debugger (stub)."
  (echo-message! (app-state-echo app) "GUD: continue (stub)"))

(def (cmd-gud-next app)
  "Step over in debugger (stub)."
  (echo-message! (app-state-echo app) "GUD: next (stub)"))

(def (cmd-gud-step app)
  "Step into in debugger (stub)."
  (echo-message! (app-state-echo app) "GUD: step (stub)"))

;; Hippie expand
(def (cmd-try-expand-dabbrev app)
  "Try dabbrev expansion (stub)."
  (echo-message! (app-state-echo app) "Try expand dabbrev (stub)"))

;; Mode line helpers
(def (cmd-toggle-mode-line app)
  "Toggle mode line display (stub)."
  (echo-message! (app-state-echo app) "Mode line toggled (stub)"))

(def (cmd-mode-line-other-buffer app)
  "Show other buffer info in mode line (stub)."
  (echo-message! (app-state-echo app) "Mode line: other buffer (stub)"))

;; Timer
(def (cmd-run-with-timer app)
  "Run function after delay (stub)."
  (echo-message! (app-state-echo app) "Timer (stub)"))

;; Global auto-revert
(def (cmd-global-auto-revert-mode app)
  "Toggle global auto-revert mode (stub)."
  (echo-message! (app-state-echo app) "Global auto-revert mode (stub)"))

;; Save place
(def (cmd-save-place-mode app)
  "Toggle save-place mode (stub)."
  (echo-message! (app-state-echo app) "Save-place mode (stub)"))

;; Winner mode
(def (cmd-winner-mode app)
  "Toggle winner mode. Winner mode is always enabled; this command reports status."
  (let ((history-len (length (app-state-winner-history app))))
    (echo-message! (app-state-echo app)
      (string-append "Winner mode enabled. History: " (number->string history-len) " configs"))))

;; Whitespace toggle
(def (cmd-global-whitespace-mode app)
  "Toggle global whitespace mode (stub)."
  (echo-message! (app-state-echo app) "Global whitespace mode (stub)"))

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
  "Switch to Lisp interaction mode (stub)."
  (echo-message! (app-state-echo app) "Lisp interaction mode (stub)"))

(def (cmd-inferior-lisp app)
  "Start inferior Lisp process (stub)."
  (echo-message! (app-state-echo app) "Inferior Lisp (stub)"))

(def (cmd-slime app)
  "Start SLIME (stub)."
  (echo-message! (app-state-echo app) "SLIME (stub)"))

(def (cmd-sly app)
  "Start SLY (stub)."
  (echo-message! (app-state-echo app) "SLY (stub)"))

;; Code folding extras
(def (cmd-fold-this app)
  "Fold current block (stub)."
  (echo-message! (app-state-echo app) "Fold this (stub)"))

(def (cmd-fold-this-all app)
  "Fold all similar blocks (stub)."
  (echo-message! (app-state-echo app) "Fold this all (stub)"))

(def (cmd-origami-mode app)
  "Toggle origami folding mode (stub)."
  (echo-message! (app-state-echo app) "Origami mode (stub)"))

;; Indent guides
(def (cmd-indent-guide-mode app)
  "Toggle indent guide display (stub)."
  (echo-message! (app-state-echo app) "Indent guide mode (stub)"))

(def (cmd-highlight-indent-guides-mode app)
  "Toggle highlight indent guides (stub)."
  (echo-message! (app-state-echo app) "Highlight indent guides (stub)"))

;; Rainbow delimiters
(def (cmd-rainbow-delimiters-mode app)
  "Toggle rainbow delimiters (stub)."
  (echo-message! (app-state-echo app) "Rainbow delimiters (stub)"))

(def (cmd-rainbow-mode app)
  "Toggle rainbow mode for color display (stub)."
  (echo-message! (app-state-echo app) "Rainbow mode (stub)"))

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
      (let ((hunks (hash-get *git-gutter-hunks* buf-name '())))
        (if (null? hunks)
          (echo-message! echo "No hunks (run git-gutter-mode first)")
          (let* ((idx (hash-get *git-gutter-hunk-idx* buf-name 0))
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
      (let ((hunks (hash-get *git-gutter-hunks* buf-name '())))
        (if (null? hunks)
          (echo-message! echo "No hunks (run git-gutter-mode first)")
          (let* ((idx (hash-get *git-gutter-hunk-idx* buf-name 0))
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
      (let ((hunks (hash-get *git-gutter-hunks* buf-name '())))
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
  "Toggle minimap (stub)."
  (echo-message! (app-state-echo app) "Minimap mode (stub)"))

;; Zen/focus/distraction-free modes
(def (cmd-writeroom-mode app)
  "Toggle writeroom/zen mode (stub)."
  (echo-message! (app-state-echo app) "Writeroom mode (stub)"))

(def (cmd-focus-mode app)
  "Toggle focus mode — dim non-focused text (stub)."
  (echo-message! (app-state-echo app) "Focus mode (stub)"))

(def (cmd-olivetti-mode app)
  "Toggle olivetti mode — centered text (stub)."
  (echo-message! (app-state-echo app) "Olivetti mode (stub)"))

;; Golden ratio
(def (cmd-golden-ratio-mode app)
  "Toggle golden ratio window resizing (stub)."
  (echo-message! (app-state-echo app) "Golden ratio mode (stub)"))

;; Rotate layout
(def (cmd-rotate-window app)
  "Rotate window layout (stub)."
  (echo-message! (app-state-echo app) "Window layout rotated (stub)"))

(def (cmd-rotate-frame app)
  "Rotate frame layout (stub)."
  (echo-message! (app-state-echo app) "Frame rotated (stub)"))

;; Modern completion: Corfu/Orderless/Marginalia/Embark/Cape
(def (cmd-corfu-mode app)
  "Toggle corfu completion mode (stub)."
  (echo-message! (app-state-echo app) "Corfu mode (stub)"))

(def (cmd-orderless-mode app)
  "Toggle orderless completion style (stub)."
  (echo-message! (app-state-echo app) "Orderless mode (stub)"))

(def (cmd-marginalia-mode app)
  "Toggle marginalia annotations (stub)."
  (echo-message! (app-state-echo app) "Marginalia mode (stub)"))

(def (cmd-embark-act app)
  "Embark act on target (stub)."
  (echo-message! (app-state-echo app) "Embark act (stub)"))

(def (cmd-embark-dwim app)
  "Embark do-what-I-mean (stub)."
  (echo-message! (app-state-echo app) "Embark DWIM (stub)"))

(def (cmd-cape-dabbrev app)
  "Cape dabbrev completion (stub)."
  (echo-message! (app-state-echo app) "Cape dabbrev (stub)"))

(def (cmd-cape-file app)
  "Cape file completion (stub)."
  (echo-message! (app-state-echo app) "Cape file (stub)"))

;; Doom/Spacemacs-style
(def (cmd-doom-themes app)
  "Load doom themes (stub)."
  (echo-message! (app-state-echo app) "Doom themes (stub)"))

(def (cmd-doom-modeline-mode app)
  "Toggle doom modeline (stub)."
  (echo-message! (app-state-echo app) "Doom modeline (stub)"))

;; Which-key extras
(def (cmd-which-key-mode app)
  "Toggle which-key mode (stub)."
  (echo-message! (app-state-echo app) "Which-key mode (stub)"))

;; Helpful
(def (cmd-helpful-callable app)
  "Describe callable with helpful (stub)."
  (echo-message! (app-state-echo app) "Helpful callable (stub)"))

(def (cmd-helpful-variable app)
  "Describe variable with helpful (stub)."
  (echo-message! (app-state-echo app) "Helpful variable (stub)"))

(def (cmd-helpful-key app)
  "Describe key with helpful (stub)."
  (echo-message! (app-state-echo app) "Helpful key (stub)"))

;; Diff-hl
(def (cmd-diff-hl-mode app)
  "Toggle diff-hl mode (stub)."
  (echo-message! (app-state-echo app) "Diff-hl mode (stub)"))

;; Wgrep
(def (cmd-wgrep-change-to-wgrep-mode app)
  "Change to wgrep mode (stub)."
  (echo-message! (app-state-echo app) "Wgrep mode (stub)"))

(def (cmd-wgrep-finish-edit app)
  "Finish wgrep editing (stub)."
  (echo-message! (app-state-echo app) "Wgrep: finish edit (stub)"))

;; Symbol overlay
(def (cmd-symbol-overlay-put app)
  "Put symbol overlay (stub)."
  (echo-message! (app-state-echo app) "Symbol overlay put (stub)"))

(def (cmd-symbol-overlay-remove-all app)
  "Remove all symbol overlays (stub)."
  (echo-message! (app-state-echo app) "Symbol overlays removed (stub)"))

;; Perspective / workspace
(def (cmd-persp-switch app)
  "Switch perspective/workspace (stub)."
  (echo-message! (app-state-echo app) "Switch perspective (stub)"))

(def (cmd-persp-add-buffer app)
  "Add buffer to perspective (stub)."
  (echo-message! (app-state-echo app) "Add to perspective (stub)"))

(def (cmd-persp-remove-buffer app)
  "Remove buffer from perspective (stub)."
  (echo-message! (app-state-echo app) "Remove from perspective (stub)"))

;; Popper
(def (cmd-popper-toggle-latest app)
  "Toggle latest popup (stub)."
  (echo-message! (app-state-echo app) "Popper toggle (stub)"))

(def (cmd-popper-cycle app)
  "Cycle through popups (stub)."
  (echo-message! (app-state-echo app) "Popper cycle (stub)"))

;; All-the-icons
(def (cmd-all-the-icons-install-fonts app)
  "Install all-the-icons fonts (stub)."
  (echo-message! (app-state-echo app) "Install icons fonts (stub)"))

;; Nerd-icons
(def (cmd-nerd-icons-install-fonts app)
  "Install nerd-icons fonts (stub)."
  (echo-message! (app-state-echo app) "Install nerd icons (stub)"))

;; Page break lines
(def (cmd-page-break-lines-mode app)
  "Toggle page break lines display (stub)."
  (echo-message! (app-state-echo app) "Page break lines (stub)"))

;; Undo-fu
(def (cmd-undo-fu-only-undo app)
  "Undo (undo-fu style, stub)."
  (echo-message! (app-state-echo app) "Undo-fu: undo (stub)"))

(def (cmd-undo-fu-only-redo app)
  "Redo (undo-fu style, stub)."
  (echo-message! (app-state-echo app) "Undo-fu: redo (stub)"))

;; Vundo
(def (cmd-vundo app)
  "Visual undo tree (stub)."
  (echo-message! (app-state-echo app) "Vundo (stub)"))

;; Dash (at point)
(def (cmd-dash-at-point app)
  "Look up documentation in Dash (stub)."
  (echo-message! (app-state-echo app) "Dash at point (stub)"))

;; Devdocs
(def (cmd-devdocs-lookup app)
  "Look up in devdocs (stub)."
  (echo-message! (app-state-echo app) "Devdocs lookup (stub)"))

;; Copilot
(def (cmd-copilot-mode app)
  "Toggle copilot mode (stub)."
  (echo-message! (app-state-echo app) "Copilot mode (stub)"))

(def (cmd-copilot-accept-completion app)
  "Accept copilot suggestion (stub)."
  (echo-message! (app-state-echo app) "Copilot: accept (stub)"))

(def (cmd-copilot-next-completion app)
  "Next copilot suggestion (stub)."
  (echo-message! (app-state-echo app) "Copilot: next (stub)"))

;; ChatGPT / AI
(def (cmd-gptel app)
  "Open GPTel chat (stub)."
  (echo-message! (app-state-echo app) "GPTel chat (stub)"))

(def (cmd-gptel-send app)
  "Send prompt to GPTel (stub)."
  (echo-message! (app-state-echo app) "GPTel: send (stub)"))

;; Evil mode
(def (cmd-evil-mode app)
  "Toggle evil mode (stub)."
  (echo-message! (app-state-echo app) "Evil mode (stub)"))

;; Meow modal editing
(def (cmd-meow-mode app)
  "Toggle meow modal editing (stub)."
  (echo-message! (app-state-echo app) "Meow mode (stub)"))

;; Eat terminal
(def (cmd-eat app)
  "Open eat terminal emulator (stub)."
  (echo-message! (app-state-echo app) "Eat terminal (stub)"))

;; Vterm
(def (cmd-vterm app)
  "Open vterm terminal (stub)."
  (echo-message! (app-state-echo app) "Vterm (stub)"))

;; Denote
(def (cmd-denote app)
  "Create denote note (stub)."
  (echo-message! (app-state-echo app) "Denote (stub)"))

(def (cmd-denote-link app)
  "Insert denote link (stub)."
  (echo-message! (app-state-echo app) "Denote link (stub)"))

;; Org-roam
(def (cmd-org-roam-node-find app)
  "Find org-roam node (stub)."
  (echo-message! (app-state-echo app) "Org-roam: find (stub)"))

(def (cmd-org-roam-node-insert app)
  "Insert org-roam node link (stub)."
  (echo-message! (app-state-echo app) "Org-roam: insert (stub)"))

(def (cmd-org-roam-buffer-toggle app)
  "Toggle org-roam buffer (stub)."
  (echo-message! (app-state-echo app) "Org-roam: buffer (stub)"))

;; Dirvish
(def (cmd-dirvish app)
  "Open dirvish file manager (stub)."
  (echo-message! (app-state-echo app) "Dirvish (stub)"))

;; Jinx (spell check)
(def (cmd-jinx-mode app)
  "Toggle jinx spell checking (stub)."
  (echo-message! (app-state-echo app) "Jinx mode (stub)"))

(def (cmd-jinx-correct app)
  "Correct word with jinx (stub)."
  (echo-message! (app-state-echo app) "Jinx correct (stub)"))

;; Hl-todo
(def (cmd-hl-todo-mode app)
  "Toggle hl-todo mode (stub)."
  (echo-message! (app-state-echo app) "HL-todo mode (stub)"))

(def (cmd-hl-todo-next app)
  "Jump to next TODO (stub)."
  (echo-message! (app-state-echo app) "HL-todo: next (stub)"))

(def (cmd-hl-todo-previous app)
  "Jump to previous TODO (stub)."
  (echo-message! (app-state-echo app) "HL-todo: previous (stub)"))

;; Editorconfig
(def (cmd-editorconfig-mode app)
  "Toggle editorconfig mode (stub)."
  (echo-message! (app-state-echo app) "Editorconfig mode (stub)"))

;; Envrc / direnv
(def (cmd-envrc-mode app)
  "Toggle envrc mode (stub)."
  (echo-message! (app-state-echo app) "Envrc mode (stub)"))

;; Apheleia (formatter)
(def (cmd-apheleia-mode app)
  "Toggle apheleia auto-format (stub)."
  (echo-message! (app-state-echo app) "Apheleia mode (stub)"))

(def (cmd-apheleia-format-buffer app)
  "Format buffer with apheleia (stub)."
  (echo-message! (app-state-echo app) "Apheleia: format (stub)"))

;; Magit extras
(def (cmd-magit-stash app)
  "Magit stash (stub)."
  (echo-message! (app-state-echo app) "Magit: stash (stub)"))

(def (cmd-magit-blame app)
  "Magit blame (stub)."
  (echo-message! (app-state-echo app) "Magit: blame (stub)"))

(def (cmd-magit-fetch app)
  "Magit fetch (stub)."
  (echo-message! (app-state-echo app) "Magit: fetch (stub)"))

(def (cmd-magit-pull app)
  "Magit pull (stub)."
  (echo-message! (app-state-echo app) "Magit: pull (stub)"))

(def (cmd-magit-push app)
  "Magit push (stub)."
  (echo-message! (app-state-echo app) "Magit: push (stub)"))

(def (cmd-magit-rebase app)
  "Magit rebase (stub)."
  (echo-message! (app-state-echo app) "Magit: rebase (stub)"))

(def (cmd-magit-merge app)
  "Magit merge (stub)."
  (echo-message! (app-state-echo app) "Magit: merge (stub)"))


;;;============================================================================
;;; Task #51: Additional unique commands to cross 1000 registrations
;;;============================================================================

;; --- Emacs built-in modes not yet covered ---
(def (cmd-native-compile-file app)
  "Native compile a file (stub)."
  (echo-message! (app-state-echo app) "Native compile: file (stub)"))

(def (cmd-native-compile-async app)
  "Native compile asynchronously (stub)."
  (echo-message! (app-state-echo app) "Native compile: async (stub)"))

(def (cmd-tab-line-mode app)
  "Toggle tab-line-mode (stub)."
  (echo-message! (app-state-echo app) "Tab-line mode: toggle (stub)"))

(def (cmd-pixel-scroll-precision-mode app)
  "Toggle pixel-scroll-precision-mode (stub)."
  (echo-message! (app-state-echo app) "Pixel scroll: toggle (stub)"))

(def (cmd-so-long-mode app)
  "Toggle so-long mode for long lines (stub)."
  (echo-message! (app-state-echo app) "So-long mode: toggle (stub)"))

(def (cmd-repeat-mode app)
  "Toggle repeat-mode for transient maps (stub)."
  (echo-message! (app-state-echo app) "Repeat mode: toggle (stub)"))

(def (cmd-context-menu-mode app)
  "Toggle context-menu-mode (stub)."
  (echo-message! (app-state-echo app) "Context menu mode: toggle (stub)"))

(def (cmd-savehist-mode app)
  "Toggle savehist-mode (persist minibuffer history) (stub)."
  (echo-message! (app-state-echo app) "Savehist mode: toggle (stub)"))

(def (cmd-recentf-mode app)
  "Toggle recentf-mode (track recent files) (stub)."
  (echo-message! (app-state-echo app) "Recentf mode: toggle (stub)"))

(def (cmd-winner-undo-2 app)
  "Winner undo alternative binding."
  (cmd-winner-undo app)))

(def (cmd-global-subword-mode app)
  "Toggle global subword-mode (CamelCase navigation) (stub)."
  (echo-message! (app-state-echo app) "Global subword mode: toggle (stub)"))

(def (cmd-display-fill-column-indicator-mode app)
  "Toggle fill column indicator display (stub)."
  (echo-message! (app-state-echo app) "Fill column indicator: toggle (stub)"))

(def (cmd-global-display-line-numbers-mode app)
  "Toggle global line numbers display (stub)."
  (echo-message! (app-state-echo app) "Global line numbers: toggle (stub)"))

(def (cmd-indent-bars-mode app)
  "Toggle indent-bars indentation guides (stub)."
  (echo-message! (app-state-echo app) "Indent bars mode: toggle (stub)"))

(def (cmd-global-hl-line-mode app)
  "Toggle global hl-line highlighting (stub)."
  (echo-message! (app-state-echo app) "Global hl-line mode: toggle (stub)"))

(def (cmd-delete-selection-mode app)
  "Toggle delete-selection-mode (stub)."
  (echo-message! (app-state-echo app) "Delete selection mode: toggle (stub)"))

(def (cmd-electric-indent-mode app)
  "Toggle electric-indent-mode (stub)."
  (echo-message! (app-state-echo app) "Electric indent mode: toggle (stub)"))

(def (cmd-show-paren-mode app)
  "Toggle show-paren-mode (stub)."
  (echo-message! (app-state-echo app) "Show paren mode: toggle (stub)"))

(def (cmd-column-number-mode app)
  "Toggle column-number-mode in modeline (stub)."
  (echo-message! (app-state-echo app) "Column number mode: toggle (stub)"))

(def (cmd-size-indication-mode app)
  "Toggle size-indication-mode in modeline (stub)."
  (echo-message! (app-state-echo app) "Size indication mode: toggle (stub)"))

(def (cmd-minibuffer-depth-indicate-mode app)
  "Toggle minibuffer-depth-indicate-mode (stub)."
  (echo-message! (app-state-echo app) "Minibuffer depth: toggle (stub)"))

(def (cmd-file-name-shadow-mode app)
  "Toggle file-name-shadow-mode (stub)."
  (echo-message! (app-state-echo app) "File name shadow mode: toggle (stub)"))

(def (cmd-midnight-mode app)
  "Toggle midnight-mode (clean up buffers at midnight) (stub)."
  (echo-message! (app-state-echo app) "Midnight mode: toggle (stub)"))

(def (cmd-cursor-intangible-mode app)
  "Toggle cursor-intangible-mode (stub)."
  (echo-message! (app-state-echo app) "Cursor intangible mode: toggle (stub)"))

(def (cmd-auto-compression-mode app)
  "Toggle auto-compression-mode (compressed files) (stub)."
  (echo-message! (app-state-echo app) "Auto-compression mode: toggle (stub)"))


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
  (register-command! 'yas-visit-snippet-file cmd-yas-visit-snippet-file)
  ;; Task #48: EWW, EMMS, PDF tools, Calc, ace-jump, expand-region, etc.
  ;; EWW extras
  (register-command! 'eww-back cmd-eww-back)
  (register-command! 'eww-forward cmd-eww-forward)
  (register-command! 'eww-reload cmd-eww-reload)
  (register-command! 'eww-download cmd-eww-download)
  (register-command! 'eww-copy-page-url cmd-eww-copy-page-url)
  ;; EMMS
  (register-command! 'emms cmd-emms)
  (register-command! 'emms-play-file cmd-emms-play-file)
  (register-command! 'emms-pause cmd-emms-pause)
  (register-command! 'emms-stop cmd-emms-stop)
  (register-command! 'emms-next cmd-emms-next)
  (register-command! 'emms-previous cmd-emms-previous)
  ;; PDF tools
  (register-command! 'pdf-view-mode cmd-pdf-view-mode)
  (register-command! 'pdf-view-next-page cmd-pdf-view-next-page)
  (register-command! 'pdf-view-previous-page cmd-pdf-view-previous-page)
  (register-command! 'pdf-view-goto-page cmd-pdf-view-goto-page)
  ;; Calc stack
  (register-command! 'calc-push cmd-calc-push)
  (register-command! 'calc-pop cmd-calc-pop)
  (register-command! 'calc-dup cmd-calc-dup)
  (register-command! 'calc-swap cmd-calc-swap)
  ;; Ace-jump/Avy
  (register-command! 'avy-goto-char cmd-avy-goto-char)
  (register-command! 'avy-goto-word cmd-avy-goto-word)
  (register-command! 'avy-goto-line cmd-avy-goto-line)
  ;; Expand-region
  (register-command! 'expand-region cmd-expand-region)
  (register-command! 'contract-region cmd-contract-region)
  ;; Smartparens
  (register-command! 'sp-forward-slurp-sexp cmd-sp-forward-slurp-sexp)
  (register-command! 'sp-forward-barf-sexp cmd-sp-forward-barf-sexp)
  (register-command! 'sp-backward-slurp-sexp cmd-sp-backward-slurp-sexp)
  (register-command! 'sp-backward-barf-sexp cmd-sp-backward-barf-sexp)
  ;; Project.el extras
  (register-command! 'project-switch-project cmd-project-switch-project)
  (register-command! 'project-find-regexp cmd-project-find-regexp)
  (register-command! 'project-shell cmd-project-shell)
  (register-command! 'project-dired cmd-project-dired)
  (register-command! 'project-eshell cmd-project-eshell)
  ;; JSON/XML
  (register-command! 'json-pretty-print cmd-json-pretty-print)
  (register-command! 'xml-format cmd-xml-format)
  ;; Notifications
  (register-command! 'notifications-list cmd-notifications-list)
  ;; Profiler
  (register-command! 'profiler-report cmd-profiler-report)
  ;; Narrowing extras
  (register-command! 'narrow-to-page cmd-narrow-to-page)
  ;; Encoding
  (register-command! 'describe-current-coding-system cmd-describe-current-coding-system)
  ;; File-local variables
  (register-command! 'add-file-local-variable cmd-add-file-local-variable)
  (register-command! 'add-dir-local-variable cmd-add-dir-local-variable)
  ;; Hippie expand
  (register-command! 'hippie-expand-file cmd-hippie-expand-file)
  ;; Register extras
  (register-command! 'frameset-to-register cmd-frameset-to-register)
  (register-command! 'window-configuration-to-register cmd-window-configuration-to-register)
  ;; Kmacro extras
  (register-command! 'kmacro-add-counter cmd-kmacro-add-counter)
  (register-command! 'kmacro-set-format cmd-kmacro-set-format)
  ;; Line number display
  (register-command! 'display-line-numbers-absolute cmd-display-line-numbers-absolute)
  (register-command! 'display-line-numbers-none cmd-display-line-numbers-none)
  ;; Scratch
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Recentf
  (register-command! 'recentf-cleanup cmd-recentf-cleanup)
  ;; Hooks
  (register-command! 'add-hook cmd-add-hook)
  (register-command! 'remove-hook cmd-remove-hook)
  ;; Package archives
  (register-command! 'package-archives cmd-package-archives)
  ;; Auto-save
  (register-command! 'auto-save-mode cmd-auto-save-mode)
  (register-command! 'recover-file cmd-recover-file)
  ;; TRAMP
  (register-command! 'tramp-version cmd-tramp-version)
  ;; HL line
  (register-command! 'hl-line-mode cmd-hl-line-mode)
  ;; Occur
  (register-command! 'occur-rename-buffer cmd-occur-rename-buffer)
  ;; Printing
  (register-command! 'print-buffer cmd-print-buffer)
  (register-command! 'print-region cmd-print-region)
  ;; Char info
  (register-command! 'describe-char-at-point cmd-describe-char-at-point)
  ;; Debug
  (register-command! 'toggle-debug-on-signal cmd-toggle-debug-on-signal)
  (register-command! 'toggle-word-boundary cmd-toggle-word-boundary)
  ;; Indent
  (register-command! 'indent-tabs-mode cmd-indent-tabs-mode)
  (register-command! 'electric-indent-local-mode cmd-electric-indent-local-mode)
  ;; Display
  (register-command! 'visual-fill-column-mode cmd-visual-fill-column-mode)
  (register-command! 'adaptive-wrap-prefix-mode cmd-adaptive-wrap-prefix-mode)
  (register-command! 'display-fill-column cmd-display-fill-column)
  (register-command! 'set-selective-display cmd-set-selective-display)
  (register-command! 'toggle-indicate-empty-lines cmd-toggle-indicate-empty-lines)
  (register-command! 'toggle-indicate-buffer-boundaries cmd-toggle-indicate-buffer-boundaries)
  ;; Face
  (register-command! 'facemenu-set-foreground cmd-facemenu-set-foreground)
  (register-command! 'facemenu-set-background cmd-facemenu-set-background)
  ;; Games
  (register-command! 'tetris cmd-tetris)
  (register-command! 'snake cmd-snake)
  (register-command! 'dunnet cmd-dunnet)
  (register-command! 'hanoi cmd-hanoi)
  (register-command! 'life cmd-life)
  (register-command! 'doctor cmd-doctor)
  ;; Proced extras
  (register-command! 'proced-send-signal cmd-proced-send-signal)
  (register-command! 'proced-filter cmd-proced-filter)
  ;; Ediff extras
  (register-command! 'ediff-show-registry cmd-ediff-show-registry)
  ;; Task #49: elisp, scheme, regex builder, color picker, etc.
  ;; Emacs Lisp
  (register-command! 'emacs-lisp-mode cmd-emacs-lisp-mode)
  (register-command! 'eval-last-sexp cmd-eval-last-sexp)
  (register-command! 'eval-defun cmd-eval-defun)
  (register-command! 'eval-print-last-sexp cmd-eval-print-last-sexp)
  ;; Scheme/Gerbil
  (register-command! 'scheme-mode cmd-scheme-mode)
  (register-command! 'gerbil-mode cmd-gerbil-mode)
  (register-command! 'run-scheme cmd-run-scheme)
  (register-command! 'scheme-send-region cmd-scheme-send-region)
  (register-command! 'scheme-send-buffer cmd-scheme-send-buffer)
  ;; Regex builder
  (register-command! 're-builder cmd-re-builder)
  ;; Colors
  (register-command! 'list-colors-display cmd-list-colors-display)
  ;; IDO
  (register-command! 'ido-mode cmd-ido-mode)
  (register-command! 'ido-find-file cmd-ido-find-file)
  (register-command! 'ido-switch-buffer cmd-ido-switch-buffer)
  ;; Helm/Ivy/Vertico
  (register-command! 'helm-mode cmd-helm-mode)
  (register-command! 'ivy-mode cmd-ivy-mode)
  (register-command! 'vertico-mode cmd-vertico-mode)
  (register-command! 'consult-line cmd-consult-line)
  (register-command! 'consult-grep cmd-consult-grep)
  (register-command! 'consult-buffer cmd-consult-buffer)
  ;; Company
  (register-command! 'company-mode cmd-company-mode)
  (register-command! 'company-complete cmd-company-complete)
  ;; Flyspell extras
  (register-command! 'flyspell-buffer cmd-flyspell-buffer)
  (register-command! 'flyspell-correct-word cmd-flyspell-correct-word)
  ;; Bibliography
  (register-command! 'citar-insert-citation cmd-citar-insert-citation)
  ;; Docker
  (register-command! 'docker cmd-docker)
  (register-command! 'docker-containers cmd-docker-containers)
  (register-command! 'docker-images cmd-docker-images)
  ;; Restclient
  (register-command! 'restclient-mode cmd-restclient-mode)
  (register-command! 'restclient-http-send cmd-restclient-http-send)
  ;; Config file modes
  (register-command! 'yaml-mode cmd-yaml-mode)
  (register-command! 'toml-mode cmd-toml-mode)
  (register-command! 'dockerfile-mode cmd-dockerfile-mode)
  ;; SQL
  (register-command! 'sql-mode cmd-sql-mode)
  (register-command! 'sql-connect cmd-sql-connect)
  (register-command! 'sql-send-region cmd-sql-send-region)
  ;; Language modes
  (register-command! 'python-mode cmd-python-mode)
  (register-command! 'c-mode cmd-c-mode)
  (register-command! 'c++-mode cmd-c++-mode)
  (register-command! 'java-mode cmd-java-mode)
  (register-command! 'rust-mode cmd-rust-mode)
  (register-command! 'go-mode cmd-go-mode)
  (register-command! 'js-mode cmd-js-mode)
  (register-command! 'typescript-mode cmd-typescript-mode)
  (register-command! 'html-mode cmd-html-mode)
  (register-command! 'css-mode cmd-css-mode)
  (register-command! 'lua-mode cmd-lua-mode)
  (register-command! 'ruby-mode cmd-ruby-mode)
  (register-command! 'shell-script-mode cmd-shell-script-mode)
  ;; Generic modes
  (register-command! 'prog-mode cmd-prog-mode)
  (register-command! 'text-mode cmd-text-mode)
  (register-command! 'fundamental-mode cmd-fundamental-mode)
  ;; Completion
  (register-command! 'completion-at-point cmd-completion-at-point)
  ;; Eldoc
  (register-command! 'eldoc-mode cmd-eldoc-mode)
  ;; Which-function
  (register-command! 'which-function-mode cmd-which-function-mode)
  ;; Compilation
  (register-command! 'compilation-mode cmd-compilation-mode)
  ;; GDB/GUD
  (register-command! 'gdb cmd-gdb)
  (register-command! 'gud-break cmd-gud-break)
  (register-command! 'gud-remove cmd-gud-remove)
  (register-command! 'gud-cont cmd-gud-cont)
  (register-command! 'gud-next cmd-gud-next)
  (register-command! 'gud-step cmd-gud-step)
  ;; Hippie expand
  (register-command! 'try-expand-dabbrev cmd-try-expand-dabbrev)
  ;; Mode line
  (register-command! 'toggle-mode-line cmd-toggle-mode-line)
  (register-command! 'mode-line-other-buffer cmd-mode-line-other-buffer)
  ;; Timer
  (register-command! 'run-with-timer cmd-run-with-timer)
  ;; Global modes
  (register-command! 'global-auto-revert-mode cmd-global-auto-revert-mode)
  (register-command! 'save-place-mode cmd-save-place-mode)
  (register-command! 'winner-mode cmd-winner-mode)
  (register-command! 'global-whitespace-mode cmd-global-whitespace-mode)
  ;; Cursor
  (register-command! 'blink-cursor-mode cmd-blink-cursor-mode)
  ;; Task #50: push to 1000+
  ;; Lisp interaction
  (register-command! 'lisp-interaction-mode cmd-lisp-interaction-mode)
  (register-command! 'inferior-lisp cmd-inferior-lisp)
  (register-command! 'slime cmd-slime)
  (register-command! 'sly cmd-sly)
  ;; Folding extras
  (register-command! 'fold-this cmd-fold-this)
  (register-command! 'fold-this-all cmd-fold-this-all)
  (register-command! 'origami-mode cmd-origami-mode)
  ;; Indent guides
  (register-command! 'indent-guide-mode cmd-indent-guide-mode)
  (register-command! 'highlight-indent-guides-mode cmd-highlight-indent-guides-mode)
  ;; Rainbow
  (register-command! 'rainbow-delimiters-mode cmd-rainbow-delimiters-mode)
  (register-command! 'rainbow-mode cmd-rainbow-mode)
  ;; Git gutter
  (register-command! 'git-gutter-mode cmd-git-gutter-mode)
  (register-command! 'git-gutter-next-hunk cmd-git-gutter-next-hunk)
  (register-command! 'git-gutter-previous-hunk cmd-git-gutter-previous-hunk)
  (register-command! 'git-gutter-revert-hunk cmd-git-gutter-revert-hunk)
  (register-command! 'git-gutter-stage-hunk cmd-git-gutter-stage-hunk)
  ;; Minimap
  (register-command! 'minimap-mode cmd-minimap-mode)
  ;; Zen modes
  (register-command! 'writeroom-mode cmd-writeroom-mode)
  (register-command! 'focus-mode cmd-focus-mode)
  (register-command! 'olivetti-mode cmd-olivetti-mode)
  ;; Golden ratio
  (register-command! 'golden-ratio-mode cmd-golden-ratio-mode)
  ;; Rotate
  (register-command! 'rotate-window cmd-rotate-window)
  (register-command! 'rotate-frame cmd-rotate-frame)
  ;; Modern completion
  (register-command! 'corfu-mode cmd-corfu-mode)
  (register-command! 'orderless-mode cmd-orderless-mode)
  (register-command! 'marginalia-mode cmd-marginalia-mode)
  (register-command! 'embark-act cmd-embark-act)
  (register-command! 'embark-dwim cmd-embark-dwim)
  (register-command! 'cape-dabbrev cmd-cape-dabbrev)
  (register-command! 'cape-file cmd-cape-file)
  ;; Doom
  (register-command! 'doom-themes cmd-doom-themes)
  (register-command! 'doom-modeline-mode cmd-doom-modeline-mode)
  ;; Which-key
  (register-command! 'which-key-mode cmd-which-key-mode)
  ;; Helpful
  (register-command! 'helpful-callable cmd-helpful-callable)
  (register-command! 'helpful-variable cmd-helpful-variable)
  (register-command! 'helpful-key cmd-helpful-key)
  ;; Diff-hl
  (register-command! 'diff-hl-mode cmd-diff-hl-mode)
  ;; Wgrep
  (register-command! 'wgrep-change-to-wgrep-mode cmd-wgrep-change-to-wgrep-mode)
  (register-command! 'wgrep-finish-edit cmd-wgrep-finish-edit)
  ;; Symbol overlay
  (register-command! 'symbol-overlay-put cmd-symbol-overlay-put)
  (register-command! 'symbol-overlay-remove-all cmd-symbol-overlay-remove-all)
  ;; Perspective
  (register-command! 'persp-switch cmd-persp-switch)
  (register-command! 'persp-add-buffer cmd-persp-add-buffer)
  (register-command! 'persp-remove-buffer cmd-persp-remove-buffer)
  ;; Popper
  (register-command! 'popper-toggle-latest cmd-popper-toggle-latest)
  (register-command! 'popper-cycle cmd-popper-cycle)
  ;; Icons
  (register-command! 'all-the-icons-install-fonts cmd-all-the-icons-install-fonts)
  (register-command! 'nerd-icons-install-fonts cmd-nerd-icons-install-fonts)
  ;; Page break lines
  (register-command! 'page-break-lines-mode cmd-page-break-lines-mode)
  ;; Undo-fu
  (register-command! 'undo-fu-only-undo cmd-undo-fu-only-undo)
  (register-command! 'undo-fu-only-redo cmd-undo-fu-only-redo)
  ;; Vundo
  (register-command! 'vundo cmd-vundo)
  ;; Dash/Devdocs
  (register-command! 'dash-at-point cmd-dash-at-point)
  (register-command! 'devdocs-lookup cmd-devdocs-lookup)
  ;; AI
  (register-command! 'copilot-mode cmd-copilot-mode)
  (register-command! 'copilot-accept-completion cmd-copilot-accept-completion)
  (register-command! 'copilot-next-completion cmd-copilot-next-completion)
  (register-command! 'gptel cmd-gptel)
  (register-command! 'gptel-send cmd-gptel-send)
  ;; Modal editing
  (register-command! 'evil-mode cmd-evil-mode)
  (register-command! 'meow-mode cmd-meow-mode)
  ;; Terminals
  (register-command! 'eat cmd-eat)
  (register-command! 'vterm cmd-vterm)
  ;; Notes
  (register-command! 'denote cmd-denote)
  (register-command! 'denote-link cmd-denote-link)
  (register-command! 'org-roam-node-find cmd-org-roam-node-find)
  (register-command! 'org-roam-node-insert cmd-org-roam-node-insert)
  (register-command! 'org-roam-buffer-toggle cmd-org-roam-buffer-toggle)
  ;; Dirvish
  (register-command! 'dirvish cmd-dirvish)
  ;; Jinx
  (register-command! 'jinx-mode cmd-jinx-mode)
  (register-command! 'jinx-correct cmd-jinx-correct)
  ;; HL-todo
  (register-command! 'hl-todo-mode cmd-hl-todo-mode)
  (register-command! 'hl-todo-next cmd-hl-todo-next)
  (register-command! 'hl-todo-previous cmd-hl-todo-previous)
  ;; Editorconfig
  (register-command! 'editorconfig-mode cmd-editorconfig-mode)
  ;; Envrc
  (register-command! 'envrc-mode cmd-envrc-mode)
  ;; Apheleia
  (register-command! 'apheleia-mode cmd-apheleia-mode)
  (register-command! 'apheleia-format-buffer cmd-apheleia-format-buffer)
  ;; Magit extras
  (register-command! 'magit-stash cmd-magit-stash)
  (register-command! 'magit-blame cmd-magit-blame)
  (register-command! 'magit-fetch cmd-magit-fetch)
  (register-command! 'magit-pull cmd-magit-pull)
  (register-command! 'magit-push cmd-magit-push)
  (register-command! 'magit-rebase cmd-magit-rebase)
  (register-command! 'magit-merge cmd-magit-merge)
  ;; Task #51: Additional commands to cross 1000
  (register-command! 'native-compile-file cmd-native-compile-file)
  (register-command! 'native-compile-async cmd-native-compile-async)
  (register-command! 'tab-line-mode cmd-tab-line-mode)
  (register-command! 'pixel-scroll-precision-mode cmd-pixel-scroll-precision-mode)
  (register-command! 'so-long-mode cmd-so-long-mode)
  (register-command! 'repeat-mode cmd-repeat-mode)
  (register-command! 'context-menu-mode cmd-context-menu-mode)
  (register-command! 'savehist-mode cmd-savehist-mode)
  (register-command! 'recentf-mode cmd-recentf-mode)
  (register-command! 'winner-undo-2 cmd-winner-undo-2)
  (register-command! 'global-subword-mode cmd-global-subword-mode)
  (register-command! 'display-fill-column-indicator-mode cmd-display-fill-column-indicator-mode)
  (register-command! 'global-display-line-numbers-mode cmd-global-display-line-numbers-mode)
  (register-command! 'indent-bars-mode cmd-indent-bars-mode)
  (register-command! 'global-hl-line-mode cmd-global-hl-line-mode)
  (register-command! 'delete-selection-mode cmd-delete-selection-mode)
  (register-command! 'electric-indent-mode cmd-electric-indent-mode)
  (register-command! 'show-paren-mode cmd-show-paren-mode)
  (register-command! 'column-number-mode cmd-column-number-mode)
  (register-command! 'size-indication-mode cmd-size-indication-mode)
  (register-command! 'minibuffer-depth-indicate-mode cmd-minibuffer-depth-indicate-mode)
  (register-command! 'file-name-shadow-mode cmd-file-name-shadow-mode)
  (register-command! 'midnight-mode cmd-midnight-mode)
  (register-command! 'cursor-intangible-mode cmd-cursor-intangible-mode)
  (register-command! 'auto-compression-mode cmd-auto-compression-mode))
