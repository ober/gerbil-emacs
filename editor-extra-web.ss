;;; -*- Gerbil -*-
;;; EWW browser, windmove, winner, and tab-bar commands

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


;; EWW browser - text-mode web browser using curl + html2text
;; Maintains history for back/forward navigation

(def *eww-history* '())        ; list of URLs visited
(def *eww-history-idx* 0)      ; current position in history
(def *eww-current-url* #f)     ; currently displayed URL

(def (eww-fetch-url url)
  "Fetch URL content and convert to text. Returns text or #f."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "curl"
                           arguments: (list "-sL" "-A" "Mozilla/5.0" url)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (html (read-line proc #f)))
        (process-status proc)
        (if (not html)
          #f
          ;; Try to convert HTML to text using various tools
          (let* ((text-proc (open-process
                              (list path: "lynx"
                                    arguments: (list "-dump" "-stdin")
                                    stdin-redirection: #t
                                    stdout-redirection: #t
                                    stderr-redirection: #f))))
            (display html text-proc)
            (close-output-port text-proc)
            (let ((text (read-line text-proc #f)))
              (process-status text-proc)
              (or text html))))))))

(def (eww-display-page app url content)
  "Display web content in EWW buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*EWW*")
                  (buffer-create! "*EWW*" ed)))
         (text (string-append "URL: " url "\n"
                             (make-string 60 #\-) "\n\n"
                             (or content "Failed to fetch page")
                             "\n\n[q: quit, g: goto URL, b: back, f: forward]")))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (set! *eww-current-url* url)))

(def (cmd-eww app)
  "Open EWW web browser."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (url (echo-read-string echo "URL: " row width)))
    (when (and url (not (string-empty? url)))
      ;; Add http:// if no protocol
      (let ((full-url (if (or (string-prefix? "http://" url)
                              (string-prefix? "https://" url))
                        url
                        (string-append "https://" url))))
        (echo-message! echo (string-append "Fetching: " full-url))
        (let ((content (eww-fetch-url full-url)))
          (if content
            (begin
              ;; Update history
              (set! *eww-history* (cons full-url *eww-history*))
              (set! *eww-history-idx* 0)
              (eww-display-page app full-url content))
            (echo-error! echo "Failed to fetch URL")))))))

(def (cmd-eww-browse-url app)
  "Browse URL with EWW."
  (cmd-eww app))

(def (cmd-browse-url-at-point app)
  "Browse URL at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Find URL-like text around point
    (let loop ((start pos))
      (if (or (< start 0) (char-whitespace? (string-ref text start)))
        (let find-end ((end (+ pos 1)))
          (if (or (>= end (string-length text))
                  (char-whitespace? (string-ref text end)))
            ;; Extract potential URL
            (let* ((url-text (substring text (+ start 1) end)))
              (if (or (string-prefix? "http://" url-text)
                      (string-prefix? "https://" url-text)
                      (string-contains url-text ".com")
                      (string-contains url-text ".org")
                      (string-contains url-text ".net"))
                (begin
                  (echo-message! echo (string-append "Opening: " url-text))
                  ;; Use xdg-open or browser
                  (with-exception-catcher
                    (lambda (e) (echo-error! echo "Failed to open URL"))
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "xdg-open"
                                          arguments: (list url-text)
                                          stdin-redirection: #f
                                          stdout-redirection: #f
                                          stderr-redirection: #f))))
                        (echo-message! echo "Opened in browser")))))
                (echo-message! echo "No URL at point")))
            (find-end (+ end 1))))
        (loop (- start 1))))))

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
           (frame-delete-window! fr)
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

