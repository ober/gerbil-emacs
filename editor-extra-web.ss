;;; -*- Gerbil -*-
;;; EWW browser, windmove, winner, and tab-bar commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        :std/text/json
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

;;;============================================================================
;;; URL encode/decode
;;;============================================================================

(def (url-encode str)
  "Percent-encode a string for URLs."
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (when (< i (string-length str))
        (let ((ch (string-ref str i)))
          (cond
            ((or (char-alphabetic? ch) (char-numeric? ch)
                 (memv ch '(#\- #\_ #\. #\~)))
             (write-char ch out))
            ((char=? ch #\space)
             (write-char #\+ out))
            (else
             (let ((b (char->integer ch)))
               (display "%" out)
               (when (< b 16) (write-char #\0 out))
               (display (number->string b 16) out)))))
        (loop (+ i 1))))
    (get-output-string out)))

(def (hex-digit-value ch)
  "Convert hex digit char to integer value."
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9))
     (- (char->integer ch) (char->integer #\0)))
    ((and (char>=? ch #\a) (char<=? ch #\f))
     (+ 10 (- (char->integer ch) (char->integer #\a))))
    ((and (char>=? ch #\A) (char<=? ch #\F))
     (+ 10 (- (char->integer ch) (char->integer #\A))))
    (else #f)))

(def (url-decode str)
  "Decode a percent-encoded URL string."
  (let ((out (open-output-string))
        (len (string-length str)))
    (let loop ((i 0))
      (when (< i len)
        (let ((ch (string-ref str i)))
          (cond
            ((and (char=? ch #\%) (< (+ i 2) len))
             (let ((h1 (hex-digit-value (string-ref str (+ i 1))))
                   (h2 (hex-digit-value (string-ref str (+ i 2)))))
               (if (and h1 h2)
                 (begin
                   (write-char (integer->char (+ (* h1 16) h2)) out)
                   (loop (+ i 3)))
                 (begin (write-char ch out) (loop (+ i 1))))))
            ((char=? ch #\+)
             (write-char #\space out)
             (loop (+ i 1)))
            (else
             (write-char ch out)
             (loop (+ i 1)))))))
    (get-output-string out)))

(def (cmd-url-encode-region app)
  "URL-encode the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-text ed))
             (region (substring text start end))
             (encoded (url-encode region)))
        (editor-set-selection ed start end)
        (send-message/string ed 2170 encoded) ;; SCI_REPLACESEL
        (echo-message! (app-state-echo app) "URL encoded")))))

(def (cmd-url-decode-region app)
  "URL-decode the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-text ed))
             (region (substring text start end))
             (decoded (url-decode region)))
        (editor-set-selection ed start end)
        (send-message/string ed 2170 decoded) ;; SCI_REPLACESEL
        (echo-message! (app-state-echo app) "URL decoded")))))

;;;============================================================================
;;; JSON format / minify
;;;============================================================================

(def (json-pretty-print obj indent)
  "Pretty-print a JSON value with indentation."
  (let ((out (open-output-string)))
    (let pp ((val obj) (level 0))
      (let ((prefix (make-string (* level indent) #\space)))
        (cond
          ((hash-table? val)
           (display "{\n" out)
           (let ((keys (sort (hash-keys val) string<?))
                 (first #t))
             (for-each
               (lambda (k)
                 (unless first (display ",\n" out))
                 (display (make-string (* (+ level 1) indent) #\space) out)
                 (write k out)
                 (display ": " out)
                 (pp (hash-ref val k) (+ level 1))
                 (set! first #f))
               keys))
           (display "\n" out)
           (display prefix out)
           (display "}" out))
          ((list? val)
           (if (null? val)
             (display "[]" out)
             (begin
               (display "[\n" out)
               (let ((first #t))
                 (for-each
                   (lambda (item)
                     (unless first (display ",\n" out))
                     (display (make-string (* (+ level 1) indent) #\space) out)
                     (pp item (+ level 1))
                     (set! first #f))
                   val))
               (display "\n" out)
               (display prefix out)
               (display "]" out))))
          ((string? val) (write val out))
          ((number? val) (display val out))
          ((boolean? val) (display (if val "true" "false") out))
          ((not val) (display "null" out))
          (else (write val out)))))
    (get-output-string out)))

(def (cmd-json-format-buffer app)
  "Pretty-print JSON in the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (formatted (json-pretty-print obj 2))
               (pos (editor-get-current-pos ed)))
          (editor-set-text ed (string-append formatted "\n"))
          (editor-goto-pos ed (min pos (string-length formatted)))
          (echo-message! echo "JSON formatted"))))))

(def (cmd-json-minify-buffer app)
  "Minify JSON in the current buffer (remove whitespace)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (minified (call-with-output-string
                           (lambda (port) (write-json obj port))))
               (pos (editor-get-current-pos ed)))
          (editor-set-text ed minified)
          (editor-goto-pos ed (min pos (string-length minified)))
          (echo-message! echo
            (string-append "JSON minified ("
              (number->string (string-length minified)) " bytes)")))))))

;;;============================================================================
;;; HTML entity encode/decode
;;;============================================================================

(def *html-entities*
  '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
    ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'")
    ("&nbsp;" . " ") ("&copy;" . "(c)") ("&reg;" . "(R)")
    ("&ndash;" . "-") ("&mdash;" . "--") ("&hellip;" . "...")
    ("&laquo;" . "<<") ("&raquo;" . ">>")))

(def (html-decode-entities str)
  "Decode common HTML entities in a string."
  (let ((result str))
    (for-each
      (lambda (pair)
        (set! result (string-subst result (car pair) (cdr pair))))
      *html-entities*)
    result))

(def (html-encode-entities str)
  "Encode special characters as HTML entities."
  (let ((result str))
    (set! result (string-subst result "&" "&amp;"))
    (set! result (string-subst result "<" "&lt;"))
    (set! result (string-subst result ">" "&gt;"))
    (set! result (string-subst result "\"" "&quot;"))
    result))

(def (cmd-html-encode-region app)
  "Encode HTML entities in the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-text ed))
             (region (substring text start end))
             (encoded (html-encode-entities region)))
        (editor-set-selection ed start end)
        (send-message/string ed 2170 encoded)
        (echo-message! (app-state-echo app) "HTML encoded")))))

(def (cmd-html-decode-region app)
  "Decode HTML entities in the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-text ed))
             (region (substring text start end))
             (decoded (html-decode-entities region)))
        (editor-set-selection ed start end)
        (send-message/string ed 2170 decoded)
        (echo-message! (app-state-echo app) "HTML decoded")))))

;;;============================================================================
;;; Backup file before save (helper)
;;;============================================================================

(def (create-backup-file! path)
  "Create a backup of a file (file~ naming convention)."
  (when (file-exists? path)
    (let ((backup-path (string-append path "~")))
      (with-catch
        (lambda (e) (void)) ;; Silently fail — backup is best-effort
        (lambda ()
          (run-process/batch ["cp" "-p" path backup-path]))))))

;;;============================================================================
;;; Large file and binary file warnings
;;;============================================================================

(def *large-file-threshold* (* 1024 1024)) ;; 1 MB

(def (large-file? path)
  "Check if a file exceeds the large file threshold."
  (and (file-exists? path)
       (with-catch
         (lambda (e) #f)
         (lambda ()
           (> (file-info-size (file-info path)) *large-file-threshold*)))))

(def (binary-file? path)
  "Heuristic: check if a file appears to be binary (has null bytes in first 8KB)."
  (and (file-exists? path)
       (with-catch
         (lambda (e) #f)
         (lambda ()
           (call-with-input-file path
             (lambda (port)
               (let loop ((i 0))
                 (if (>= i 8192) #f
                   (let ((ch (read-char port)))
                     (cond
                       ((eof-object? ch) #f)
                       ((= (char->integer ch) 0) #t)
                       (else (loop (+ i 1)))))))))))))

(def (cmd-find-file-with-warnings app)
  "Open file with warnings for large or binary files."
  (let* ((echo (app-state-echo app))
         (path (app-read-string app "Find file: ")))
    (when (and path (> (string-length path) 0))
      (let ((expanded (path-expand path)))
        (cond
          ((large-file? expanded)
           (echo-message! echo
             (string-append "Warning: Large file ("
               (number->string (quotient (file-info-size (file-info expanded)) 1024))
               " KB). Opening anyway..."))
           (execute-command! app 'find-file))
          ((binary-file? expanded)
           (echo-message! echo "Warning: Binary file detected. Opening in hex view may be better."))
          (else
           (execute-command! app 'find-file)))))))

;;;============================================================================
;;; Encoding detection
;;;============================================================================

(def (detect-file-encoding path)
  "Detect file encoding using heuristics. Returns encoding name string."
  (if (not (file-exists? path)) "unknown"
    (with-catch
      (lambda (e) "utf-8")
      (lambda ()
        (call-with-input-file path
          (lambda (port)
            (let ((b1 (read-u8 port))
                  (b2 (read-u8 port))
                  (b3 (read-u8 port)))
              (cond
                ;; UTF-8 BOM
                ((and (eqv? b1 #xEF) (eqv? b2 #xBB) (eqv? b3 #xBF))
                 "utf-8-bom")
                ;; UTF-16 LE BOM
                ((and (eqv? b1 #xFF) (eqv? b2 #xFE))
                 "utf-16-le")
                ;; UTF-16 BE BOM
                ((and (eqv? b1 #xFE) (eqv? b2 #xFF))
                 "utf-16-be")
                ;; Default: assume UTF-8
                (else "utf-8")))))))))

(def (cmd-detect-encoding app)
  "Show the detected encoding of the current file."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (echo-message! echo
        (string-append "Encoding: " (detect-file-encoding path))))))

;;;============================================================================
;;; Sort JSON keys
;;;============================================================================

(def (cmd-json-sort-keys app)
  "Sort all JSON object keys alphabetically."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               ;; json-pretty-print already sorts keys
               (sorted (json-pretty-print obj 2))
               (pos (editor-get-current-pos ed)))
          (editor-set-text ed (string-append sorted "\n"))
          (editor-goto-pos ed (min pos (string-length sorted)))
          (echo-message! echo "JSON keys sorted"))))))

;;;============================================================================
;;; CSV mode helpers
;;;============================================================================

(def (csv-split-line line)
  "Split a CSV line into fields (handles simple cases)."
  (let ((fields [])
        (current (open-output-string))
        (in-quotes #f)
        (len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
        (reverse (cons (get-output-string current) fields))
        (let ((ch (string-ref line i)))
          (cond
            ((and (char=? ch (integer->char 34)) (not in-quotes))
             (set! in-quotes #t)
             (loop (+ i 1)))
            ((and (char=? ch (integer->char 34)) in-quotes)
             (set! in-quotes #f)
             (loop (+ i 1)))
            ((and (char=? ch #\,) (not in-quotes))
             (set! fields (cons (get-output-string current) fields))
             (set! current (open-output-string))
             (loop (+ i 1)))
            (else
             (write-char ch current)
             (loop (+ i 1)))))))))

(def (cmd-csv-align-columns app)
  "Align CSV columns for better readability."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (rows (map csv-split-line (filter (lambda (l) (> (string-length l) 0)) lines))))
    (if (null? rows)
      (echo-message! echo "No CSV data")
      ;; Calculate max width for each column
      (let* ((num-cols (apply max (map length rows)))
             (widths (let loop ((col 0) (acc []))
                       (if (>= col num-cols)
                         (reverse acc)
                         (loop (+ col 1)
                               (cons (apply max
                                       (map (lambda (row)
                                              (if (< col (length row))
                                                (string-length (list-ref row col))
                                                0))
                                            rows))
                                     acc))))))
        ;; Build aligned output
        (let ((out (open-output-string)))
          (for-each
            (lambda (row)
              (let field-loop ((i 0) (fields row))
                (unless (null? fields)
                  (when (> i 0) (display " | " out))
                  (let* ((field (car fields))
                         (width (if (< i (length widths))
                                  (list-ref widths i) 0))
                         (pad (max 0 (- width (string-length field)))))
                    (display field out)
                    (display (make-string pad #\space) out))
                  (field-loop (+ i 1) (cdr fields))))
              (newline out))
            rows)
          (let ((result (get-output-string out))
                (pos (editor-get-current-pos ed)))
            (editor-set-text ed result)
            (editor-goto-pos ed (min pos (string-length result)))
            (echo-message! echo
              (string-append "Aligned " (number->string (length rows))
                " rows, " (number->string num-cols) " columns"))))))))

;;;============================================================================
;;; Epoch timestamp conversion
;;;============================================================================

(def (cmd-epoch-to-date app)
  "Convert Unix epoch timestamp at point or in region to human-readable date."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed))
         (text (editor-get-text ed)))
    (let ((num-str (if (= start end)
                     ;; No selection: try to find number at point
                     (let ((pos (editor-get-current-pos ed)))
                       (let loop ((s pos))
                         (if (or (< s 0) (not (char-numeric? (string-ref text s))))
                           (let loop2 ((e (+ s 1)))
                             (if (or (>= e (string-length text))
                                     (not (char-numeric? (string-ref text e))))
                               (substring text (+ s 1) e)
                               (loop2 (+ e 1))))
                           (loop (- s 1)))))
                     (substring text start end))))
      (let ((ts (string->number num-str)))
        (if (not ts)
          (echo-error! echo "No timestamp at point")
          (with-catch
            (lambda (e) (echo-error! echo "Invalid timestamp"))
            (lambda ()
              (let ((output (run-process ["date" "-d"
                              (string-append "@" (number->string (inexact->exact (floor ts))))
                              "+%Y-%m-%d %H:%M:%S %Z"])))
                (echo-message! echo
                  (string-append (number->string (inexact->exact (floor ts)))
                    " = " (string-trim-both output)))))))))))

;;;============================================================================
;;; Pipe buffer through jq
;;;============================================================================

(def (cmd-jq-filter app)
  "Run jq filter on current buffer's JSON content."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (filter-str (app-read-string app "jq filter (e.g. '.key'): ")))
    (when (and filter-str (> (string-length filter-str) 0))
      (let ((text (editor-get-text ed)))
        (with-catch
          (lambda (e) (echo-error! echo "jq error (is jq installed?)"))
          (lambda ()
            (let ((output (filter-with-process ["jq" filter-str]
                            (lambda (port) (display text port))
                            (lambda (port) (read-line port #f)))))
              (when (and output (> (string-length output) 0))
                (let ((pos (editor-get-current-pos ed)))
                  (editor-set-text ed output)
                  (editor-goto-pos ed (min pos (string-length output)))
                  (echo-message! echo "jq filter applied"))))))))))

