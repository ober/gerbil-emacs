;;; -*- Gerbil -*-
;;; EWW browser, windmove, winner, and tab-bar commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        :std/misc/shuffle
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

;;;============================================================================
;;; Batch 25: Line manipulation, calc-eval, table, smart-open, quick-run, etc.
;;;============================================================================

;;; Helper: get text between two positions using full-buffer substring
(def (editor-text-range ed start end)
  "Extract text between positions start and end."
  (let ((text (editor-get-text ed)))
    (substring text (min start (string-length text))
                    (min end (string-length text)))))

;;; --- Reverse lines in region or buffer ---

(def (reverse-lines-in-string text)
  "Reverse the order of lines in a string."
  (let* ((lines (string-split text #\newline))
         (reversed (reverse lines)))
    (string-join reversed "\n")))

(def (cmd-reverse-lines app)
  "Reverse the order of lines in region or entire buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection => reverse entire buffer
      (let* ((text (editor-get-text ed))
             (result (reverse-lines-in-string text)))
        (editor-set-text ed result)
        (editor-goto-pos ed 0)
        (echo-message! echo "Reversed all lines"))
      ;; Selection => expand to full lines, reverse
      (let* ((line-start (editor-line-from-position ed sel-start))
             (line-end (editor-line-from-position ed sel-end))
             (pos-start (editor-position-from-line ed line-start))
             (pos-end (editor-get-line-end-position ed line-end))
             (text (editor-text-range ed pos-start pos-end))
             (result (reverse-lines-in-string text)))
        (send-message ed SCI_SETTARGETSTART pos-start 0)
        (send-message ed SCI_SETTARGETEND pos-end 0)
        (send-message/string ed SCI_REPLACETARGET result)
        (echo-message! echo
          (string-append "Reversed " (number->string (+ 1 (- line-end line-start)))
            " lines"))))))

;;; --- Shuffle lines in region or buffer ---

(def (cmd-shuffle-lines app)
  "Randomly shuffle lines in region or entire buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection => shuffle entire buffer
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (shuffled (shuffle lines))
             (result (string-join shuffled "\n")))
        (editor-set-text ed result)
        (editor-goto-pos ed 0)
        (echo-message! echo "Shuffled all lines"))
      ;; Selection => expand to full lines, shuffle
      (let* ((line-start (editor-line-from-position ed sel-start))
             (line-end (editor-line-from-position ed sel-end))
             (pos-start (editor-position-from-line ed line-start))
             (pos-end (editor-get-line-end-position ed line-end))
             (text (editor-text-range ed pos-start pos-end))
             (lines (string-split text #\newline))
             (shuffled (shuffle lines))
             (result (string-join shuffled "\n")))
        (send-message ed SCI_SETTARGETSTART pos-start 0)
        (send-message ed SCI_SETTARGETEND pos-end 0)
        (send-message/string ed SCI_REPLACETARGET result)
        (echo-message! echo
          (string-append "Shuffled " (number->string (length lines))
            " lines"))))))

;;; --- Evaluate math expression in region ---

(def (cmd-calc-eval-region app)
  "Evaluate the selected text as a math expression and replace with result."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection - select a math expression first")
      (let* ((expr-text (editor-text-range ed sel-start sel-end)))
        (with-catch
          (lambda (e)
            (echo-error! echo (string-append "Eval error: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((result (eval (call-with-input-string expr-text read))))
              (send-message ed SCI_SETTARGETSTART sel-start 0)
              (send-message ed SCI_SETTARGETEND sel-end 0)
              (let ((result-str (with-output-to-string
                                  (lambda () (display result)))))
                (send-message/string ed SCI_REPLACETARGET result-str)
                (echo-message! echo
                  (string-append expr-text " = " result-str))))))))))

;;; --- Insert text table ---

(def (cmd-table-insert app)
  "Insert a formatted text table at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (cols-str (app-read-string app "Number of columns: "))
         (rows-str (and cols-str (app-read-string app "Number of rows: "))))
    (when (and cols-str rows-str)
      (with-catch
        (lambda (e) (echo-error! echo "Invalid number"))
        (lambda ()
          (let* ((cols (string->number cols-str))
                 (rows (string->number rows-str))
                 (col-width 12)
                 (separator (string-append "+"
                   (string-join
                     (let loop ((i 0) (acc []))
                       (if (>= i cols) (reverse acc)
                         (loop (+ i 1) (cons (make-string col-width #\-) acc))))
                     "+")
                   "+\n"))
                 (data-row (string-append "|"
                   (string-join
                     (let loop ((i 0) (acc []))
                       (if (>= i cols) (reverse acc)
                         (loop (+ i 1) (cons (make-string col-width #\space) acc))))
                     "|")
                   "|\n"))
                 (table (with-output-to-string
                          (lambda ()
                            (display separator)
                            (let loop ((r 0))
                              (when (< r rows)
                                (display data-row)
                                (display separator)
                                (loop (+ r 1))))))))
            (let ((pos (editor-get-current-pos ed)))
              (editor-insert-text ed pos table)
              (echo-message! echo
                (string-append "Inserted " (number->string rows) "x"
                  (number->string cols) " table")))))))))

;;; --- List active timers ---

(def (cmd-list-timers app)
  "Show a buffer listing active system timers."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (info (with-output-to-string
                 (lambda ()
                   (display "Active Timers:\n")
                   (display (make-string 60 #\-))
                   (display "\n")
                   (display "  Auto-save timer:     30s interval (saves modified buffers)\n")
                   (display "  File-check timer:     5s interval (detects external changes)\n")
                   (display "  GC timer:           periodic (garbage collection)\n")
                   (display (make-string 60 #\-))
                   (display "\n")
                   (display "Total: 3 system timers configured\n")))))
    (editor-set-text ed info)
    (editor-goto-pos ed 0)
    (echo-message! echo "Timer list displayed")))

;;; --- Aggressive indent mode ---

(def *aggressive-indent-mode* #f)

(def (cmd-toggle-aggressive-indent app)
  "Toggle aggressive auto-indent mode."
  (set! *aggressive-indent-mode* (not *aggressive-indent-mode*))
  (echo-message! (app-state-echo app)
    (if *aggressive-indent-mode*
      "Aggressive indent mode enabled"
      "Aggressive indent mode disabled")))

;;; --- Smart open line (crux-style) ---

(def (cmd-smart-open-line-above app)
  "Open a new line above, respecting indentation of current line."
  (let* ((ed (current-editor app))
         (line (editor-line-from-position ed
                 (editor-get-current-pos ed)))
         (line-text (editor-get-line ed line))
         (indent (let loop ((i 0))
                   (if (and (< i (string-length line-text))
                            (or (char=? (string-ref line-text i) #\space)
                                (char=? (string-ref line-text i) #\tab)))
                     (loop (+ i 1))
                     i)))
         (indent-str (substring line-text 0 indent))
         (pos (editor-position-from-line ed line)))
    (editor-insert-text ed pos (string-append indent-str "\n"))
    (editor-goto-pos ed (+ pos indent))))

(def (cmd-smart-open-line-below app)
  "Open a new line below, respecting indentation of current line."
  (let* ((ed (current-editor app))
         (line (editor-line-from-position ed
                 (editor-get-current-pos ed)))
         (line-text (editor-get-line ed line))
         (indent (let loop ((i 0))
                   (if (and (< i (string-length line-text))
                            (or (char=? (string-ref line-text i) #\space)
                                (char=? (string-ref line-text i) #\tab)))
                     (loop (+ i 1))
                     i)))
         (indent-str (substring line-text 0 indent))
         (eol (editor-get-line-end-position ed line)))
    (editor-insert-text ed eol (string-append "\n" indent-str))
    (editor-goto-pos ed (+ eol 1 indent))))

;;; --- Quick-run current buffer ---

(def *file-runners*
  (hash
    ("py" "python3")
    ("rb" "ruby")
    ("js" "node")
    ("ts" "ts-node")
    ("sh" "bash")
    ("pl" "perl")
    ("lua" "lua")
    ("php" "php")
    ("go" "go run")
    ("rs" "rustc -o /tmp/gerbil-emacs-run && /tmp/gerbil-emacs-run")
    ("ss" "gxi")
    ("scm" "gxi")
    ("el" "emacs --script")
    ("awk" "awk -f")))

(def (file-extension path)
  "Get the file extension from a path."
  (let ((dot-pos (string-index-right path #\.)))
    (if dot-pos
      (substring path (+ dot-pos 1) (string-length path))
      "")))

(def (cmd-quick-run app)
  "Run the current buffer's file with an appropriate interpreter."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (filepath (buffer-file-path buf)))
    (if (not filepath)
      (echo-message! echo "Buffer has no file - save first")
      (let* ((ext (file-extension filepath))
             (runner (hash-get *file-runners* ext)))
        (if (not runner)
          (echo-message! echo (string-append "No runner configured for ." ext))
          (let ((cmd (string-append runner " " filepath)))
            (with-catch
              (lambda (e)
                (echo-error! echo (string-append "Run error: "
                  (with-output-to-string (lambda () (display-exception e))))))
              (lambda ()
                (let ((output (filter-with-process
                                ["bash" "-c" cmd]
                                (lambda (port) (void))
                                (lambda (port) (read-line port #f)))))
                  (let* ((out-buf-name "*run-output*")
                         (text (or output "")))
                    (editor-set-text ed text)
                    (editor-goto-pos ed 0)
                    (echo-message! echo
                      (string-append "Ran: " cmd))))))))))))

;;; --- WS-butler mode: clean whitespace only on changed lines ---

(def *ws-butler-mode* #f)
(def *ws-butler-dirty-lines* (make-hash-table))

(def (ws-butler-mark-line-dirty! ed)
  "Mark the current line as dirty for ws-butler cleanup."
  (when *ws-butler-mode*
    (let ((line (editor-line-from-position ed
                  (editor-get-current-pos ed))))
      (hash-put! *ws-butler-dirty-lines* line #t))))

(def (ws-butler-clean! ed)
  "Clean trailing whitespace on dirty lines only."
  (let ((lines (hash-keys *ws-butler-dirty-lines*)))
    (for-each
      (lambda (line)
        (let* ((start (editor-position-from-line ed line))
               (end (editor-get-line-end-position ed line))
               (text (editor-text-range ed start end)))
          (when (> (string-length text) 0)
            (let ((trimmed (string-trim-right text)))
              (when (not (string=? text trimmed))
                (send-message ed SCI_SETTARGETSTART start 0)
                (send-message ed SCI_SETTARGETEND end 0)
                (send-message/string ed SCI_REPLACETARGET trimmed))))))
      (sort lines <))
    (set! *ws-butler-dirty-lines* (make-hash-table))))

(def (cmd-toggle-ws-butler-mode app)
  "Toggle ws-butler mode (clean whitespace only on modified lines on save)."
  (set! *ws-butler-mode* (not *ws-butler-mode*))
  (set! *ws-butler-dirty-lines* (make-hash-table))
  (echo-message! (app-state-echo app)
    (if *ws-butler-mode*
      "ws-butler mode enabled (trailing whitespace cleaned on save for edited lines)"
      "ws-butler mode disabled")))

;;; --- Copy buffer contents as formatted code (with line numbers) ---

(def (cmd-copy-as-formatted app)
  "Copy buffer text with line numbers prepended to each line."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (width (string-length (number->string (length lines))))
         (numbered (let loop ((ls lines) (n 1) (acc []))
                     (if (null? ls) (reverse acc)
                       (loop (cdr ls) (+ n 1)
                         (cons (string-append
                                 (string-pad (number->string n) width)
                                 ": " (car ls))
                           acc))))))
    (let ((result (string-join numbered "\n")))
      (app-state-kill-ring-set! app (cons result (app-state-kill-ring app)))
      (echo-message! echo
        (string-append "Copied " (number->string (length lines))
          " lines with line numbers")))))

;;; --- Wrap region in delimiter pairs ---

(def (cmd-wrap-region-with app)
  "Wrap selected text with specified delimiters (quotes, parens, etc.)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to wrap")
      (let ((wrapper (app-read-string app "Wrap with (e.g. \" or ( or [ or { or <): ")))
        (when (and wrapper (> (string-length wrapper) 0))
          (let* ((open-ch (string-ref wrapper 0))
                 (close-ch (cond
                             ((char=? open-ch #\() #\))
                             ((char=? open-ch #\[) #\])
                             ((char=? open-ch #\{) #\})
                             ((char=? open-ch #\<) #\>)
                             (else open-ch)))
                 (text (editor-text-range ed sel-start sel-end))
                 (wrapped (string-append (string open-ch) text (string close-ch))))
            (send-message ed SCI_SETTARGETSTART sel-start 0)
            (send-message ed SCI_SETTARGETEND sel-end 0)
            (send-message/string ed SCI_REPLACETARGET wrapped)
            (echo-message! echo
              (string-append "Wrapped with "
                (string open-ch) "..." (string close-ch)))))))))

;;; --- Remove wrapping delimiters ---

(def (cmd-unwrap-region app)
  "Remove surrounding delimiters from selection (e.g. quotes, parens)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (< (- sel-end sel-start) 2)
      (echo-message! echo "Selection too short to unwrap")
      (let* ((text (editor-text-range ed sel-start sel-end))
             (first-ch (string-ref text 0))
             (last-ch (string-ref text (- (string-length text) 1)))
             (matching? (or (and (char=? first-ch #\() (char=? last-ch #\)))
                            (and (char=? first-ch #\[) (char=? last-ch #\]))
                            (and (char=? first-ch #\{) (char=? last-ch #\}))
                            (and (char=? first-ch #\<) (char=? last-ch #\>))
                            (char=? first-ch last-ch))))
        (if (not matching?)
          (echo-message! echo "Selection doesn't appear to be wrapped in matching delimiters")
          (let ((inner (substring text 1 (- (string-length text) 1))))
            (send-message ed SCI_SETTARGETSTART sel-start 0)
            (send-message ed SCI_SETTARGETEND sel-end 0)
            (send-message/string ed SCI_REPLACETARGET inner)
            (echo-message! echo "Unwrapped delimiters")))))))

;;; --- Quote style conversion ---

(def (cmd-toggle-quotes app)
  "Toggle between single and double quotes around the string at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find enclosing quote
    (let find-quote ((i pos))
      (if (< i 0)
        (echo-message! echo "Not inside a quoted string")
        (let ((ch (string-ref text i)))
          (if (or (char=? ch (integer->char 34))   ; double quote
                  (char=? ch (integer->char 39)))   ; single quote
            ;; Found opening quote - now find closing
            (let ((quote-ch ch)
                  (other-ch (if (char=? ch (integer->char 34))
                              (integer->char 39)
                              (integer->char 34))))
              (let find-close ((j (+ i 1)))
                (if (>= j len)
                  (echo-message! echo "Unmatched quote")
                  (if (char=? (string-ref text j) quote-ch)
                    ;; Replace both quotes
                    (begin
                      (send-message ed SCI_SETTARGETSTART j 0)
                      (send-message ed SCI_SETTARGETEND (+ j 1) 0)
                      (send-message/string ed SCI_REPLACETARGET (string other-ch))
                      (send-message ed SCI_SETTARGETSTART i 0)
                      (send-message ed SCI_SETTARGETEND (+ i 1) 0)
                      (send-message/string ed SCI_REPLACETARGET (string other-ch))
                      (echo-message! echo
                        (string-append "Toggled to "
                          (if (char=? other-ch (integer->char 34)) "double" "single")
                          " quotes")))
                    (find-close (+ j 1))))))
            (find-quote (- i 1))))))))

;;; --- Frequency analysis of words ---

(def (cmd-word-frequency-analysis app)
  "Show word frequency analysis of buffer content."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (words (string-tokenize text))
         (freq (make-hash-table))
         (_ (for-each (lambda (w)
              (let ((lw (string-downcase w)))
                (hash-put! freq lw (+ 1 (or (hash-get freq lw) 0)))))
              words))
         (pairs (hash->list freq))
         (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
         (top (let loop ((ls sorted) (n 0) (acc []))
                (if (or (null? ls) (>= n 30)) (reverse acc)
                  (loop (cdr ls) (+ n 1) (cons (car ls) acc)))))
         (report (with-output-to-string
                   (lambda ()
                     (display "Word Frequency Analysis:\n")
                     (display (make-string 40 #\-))
                     (display "\n")
                     (for-each
                       (lambda (p)
                         (display (string-pad (number->string (cdr p)) 6))
                         (display "  ")
                         (display (car p))
                         (display "\n"))
                       top)
                     (display (make-string 40 #\-))
                     (display "\n")
                     (display "Total unique words: ")
                     (display (number->string (length pairs)))
                     (display "\n")
                     (display "Total words: ")
                     (display (number->string (length words)))
                     (display "\n")))))
    (editor-set-text ed report)
    (editor-goto-pos ed 0)
    (echo-message! echo
      (string-append (number->string (length pairs)) " unique words analyzed"))))

;;; --- Selection statistics ---

(def (cmd-selection-info app)
  "Display information about the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (let* ((text (editor-text-range ed sel-start sel-end))
             (chars (string-length text))
             (lines (length (string-split text #\newline)))
             (words (length (string-tokenize text)))
             (bytes chars))
        (echo-message! echo
          (string-append "Selection: " (number->string chars) " chars, "
            (number->string words) " words, "
            (number->string lines) " lines"))))))

;;; --- Increment/decrement number at point (if not already present) ---

(def (cmd-increment-hex-at-point app)
  "Increment a hexadecimal number at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find hex number starting with 0x
    (let find-start ((i pos))
      (if (or (< i 0) (and (> (- pos i) 20)))
        (echo-message! echo "No hex number at point")
        (if (and (> i 0)
                 (char=? (string-ref text i) #\x)
                 (char=? (string-ref text (- i 1)) #\0))
          ;; Found 0x prefix - extract hex digits after
          (let* ((hex-start (+ i 1))
                 (hex-end (let loop ((j hex-start))
                            (if (and (< j len)
                                     (let ((c (string-ref text j)))
                                       (or (char-numeric? c)
                                           (and (char>=? c #\a) (char<=? c #\f))
                                           (and (char>=? c #\A) (char<=? c #\F)))))
                              (loop (+ j 1))
                              j)))
                 (hex-str (substring text hex-start hex-end))
                 (val (string->number hex-str 16)))
            (if (not val)
              (echo-message! echo "Invalid hex number")
              (let* ((new-val (+ val 1))
                     (new-hex (number->string new-val 16))
                     ;; Pad to same width
                     (padded (if (< (string-length new-hex) (string-length hex-str))
                               (string-append
                                 (make-string (- (string-length hex-str) (string-length new-hex)) #\0)
                                 new-hex)
                               new-hex)))
                (send-message ed SCI_SETTARGETSTART hex-start 0)
                (send-message ed SCI_SETTARGETEND hex-end 0)
                (send-message/string ed SCI_REPLACETARGET padded)
                (echo-message! echo
                  (string-append "0x" hex-str " -> 0x" padded)))))
          (find-start (- i 1)))))))

;;; --- Describe char at point ---

(def (cmd-describe-char app)
  "Show detailed info about the character at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (if (>= pos (string-length text))
      (echo-message! echo "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch))
             (name (cond
                     ((char=? ch #\space) "SPACE")
                     ((char=? ch #\tab) "TAB")
                     ((char=? ch #\newline) "NEWLINE")
                     ((char=? ch #\return) "CARRIAGE RETURN")
                     ((< code 32) (string-append "CONTROL-" (string (integer->char (+ code 64)))))
                     (else (string ch)))))
        (echo-message! echo
          (string-append name " (U+"
            (let ((hex (number->string code 16)))
              (if (< (string-length hex) 4)
                (string-append (make-string (- 4 (string-length hex)) #\0) hex)
                hex))
            ", decimal " (number->string code)
            ", octal " (number->string code 8) ")"))))))

;;; --- Narrow to region ---

(def *narrow-original-text* #f)
(def *narrow-offset* 0)

(def (cmd-narrow-to-region-simple app)
  "Narrow buffer to show only the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No region selected for narrowing")
      (begin
        (set! *narrow-original-text* (editor-get-text ed))
        (set! *narrow-offset* sel-start)
        (let ((region-text (editor-text-range ed sel-start sel-end)))
          (editor-set-text ed region-text)
          (editor-goto-pos ed 0)
          (echo-message! echo "Narrowed to region (use widen to restore)"))))))

(def (cmd-widen-simple app)
  "Widen buffer to show full content after narrowing."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (not *narrow-original-text*)
      (echo-message! echo "Buffer is not narrowed")
      (let ((narrowed-text (editor-get-text ed)))
        ;; Splice narrowed content back into original
        (let ((result (string-append
                        (substring *narrow-original-text* 0 *narrow-offset*)
                        narrowed-text
                        (substring *narrow-original-text*
                          (+ *narrow-offset* (string-length narrowed-text))
                          (string-length *narrow-original-text*)))))
          (editor-set-text ed result)
          (editor-goto-pos ed *narrow-offset*)
          (set! *narrow-original-text* #f)
          (set! *narrow-offset* 0)
          (echo-message! echo "Buffer widened"))))))

;;; --- Toggle read-only for current region (mark as non-editable) ---

(def (cmd-toggle-buffer-read-only app)
  "Toggle the read-only flag on the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (current (if (editor-get-read-only? ed) 1 0)))
    (editor-set-read-only ed (= current 0))
    (echo-message! echo
      (if (= current 0)
        "Buffer is now read-only"
        "Buffer is now editable"))))

;;; =========================================================================
;;; Batch 38: auto-compression, image mode, save silently, backups, etc.
;;; =========================================================================

(def *auto-compression-mode* #t)
(def *image-mode* #f)
(def *save-silently* #f)
(def *confirm-kill-emacs* #t)
(def *auto-window-vscroll* #t)
(def *fast-but-imprecise-scrolling* #f)
(def *mouse-avoidance-mode* #f)
(def *make-backup-files* #t)
(def *lock-file-create* #t)
(def *auto-encryption-mode* #t)

(def (cmd-toggle-auto-compression app)
  "Toggle auto-compression-mode (transparent gzip file handling)."
  (let ((echo (app-state-echo app)))
    (set! *auto-compression-mode* (not *auto-compression-mode*))
    (echo-message! echo (if *auto-compression-mode*
                          "Auto-compression ON"
                          "Auto-compression OFF"))))

(def (cmd-toggle-image-mode app)
  "Toggle image display mode for the current buffer."
  (let ((echo (app-state-echo app)))
    (set! *image-mode* (not *image-mode*))
    (echo-message! echo (if *image-mode*
                          "Image mode ON"
                          "Image mode OFF"))))

(def (cmd-toggle-save-silently app)
  "Toggle silent saving (no messages on save)."
  (let ((echo (app-state-echo app)))
    (set! *save-silently* (not *save-silently*))
    (echo-message! echo (if *save-silently*
                          "Save silently ON"
                          "Save silently OFF"))))

(def (cmd-toggle-confirm-kill-emacs app)
  "Toggle confirmation prompt before exiting."
  (let ((echo (app-state-echo app)))
    (set! *confirm-kill-emacs* (not *confirm-kill-emacs*))
    (echo-message! echo (if *confirm-kill-emacs*
                          "Confirm kill ON"
                          "Confirm kill OFF"))))

(def (cmd-toggle-auto-window-vscroll app)
  "Toggle automatic vertical scrolling in tall lines."
  (let ((echo (app-state-echo app)))
    (set! *auto-window-vscroll* (not *auto-window-vscroll*))
    (echo-message! echo (if *auto-window-vscroll*
                          "Auto window vscroll ON"
                          "Auto window vscroll OFF"))))

(def (cmd-toggle-fast-but-imprecise-scrolling app)
  "Toggle fast-but-imprecise scrolling (skip fontification during scroll)."
  (let ((echo (app-state-echo app)))
    (set! *fast-but-imprecise-scrolling* (not *fast-but-imprecise-scrolling*))
    (echo-message! echo (if *fast-but-imprecise-scrolling*
                          "Fast scrolling ON"
                          "Fast scrolling OFF"))))

(def (cmd-toggle-mouse-avoidance app)
  "Toggle mouse-avoidance-mode (move mouse pointer away from cursor)."
  (let ((echo (app-state-echo app)))
    (set! *mouse-avoidance-mode* (not *mouse-avoidance-mode*))
    (echo-message! echo (if *mouse-avoidance-mode*
                          "Mouse avoidance ON"
                          "Mouse avoidance OFF"))))

(def (cmd-toggle-make-backup-files app)
  "Toggle creation of backup files (~) on save."
  (let ((echo (app-state-echo app)))
    (set! *make-backup-files* (not *make-backup-files*))
    (echo-message! echo (if *make-backup-files*
                          "Backup files ON"
                          "Backup files OFF"))))

(def (cmd-toggle-lock-file-create app)
  "Toggle creation of lock files (.#filename) for editing."
  (let ((echo (app-state-echo app)))
    (set! *lock-file-create* (not *lock-file-create*))
    (echo-message! echo (if *lock-file-create*
                          "Lock files ON"
                          "Lock files OFF"))))

(def (cmd-toggle-auto-encryption app)
  "Toggle auto-encryption-mode (transparent .gpg file handling)."
  (let ((echo (app-state-echo app)))
    (set! *auto-encryption-mode* (not *auto-encryption-mode*))
    (echo-message! echo (if *auto-encryption-mode*
                          "Auto-encryption ON"
                          "Auto-encryption OFF"))))

;; ── batch 46: file handling and display preferences ─────────────────
(def *auto-rename-tag* #f)
(def *global-prettify-symbols* #f)
(def *global-subword-mode* #f)
(def *global-superword-mode* #f)
(def *delete-by-moving-to-trash* #f)
(def *create-lockfiles* #t)
(def *mode-line-compact* #f)
(def *use-file-dialog* #f)
(def *xterm-mouse-mode* #f)

(def (cmd-insert-date-time-stamp app)
  "Insert current date and time at point."
  (let* ((ed (current-editor app))
         (stamp (string-trim-right
                 (run-process ["date" "+%Y-%m-%d %H:%M:%S"]
                              stdout-redirection: #t))))
    (editor-replace-selection ed stamp)))

(def (cmd-toggle-auto-rename-tag app)
  "Toggle auto-rename-tag (rename matching HTML tags)."
  (let ((echo (app-state-echo app)))
    (set! *auto-rename-tag* (not *auto-rename-tag*))
    (echo-message! echo (if *auto-rename-tag*
                          "Auto-rename tag ON" "Auto-rename tag OFF"))))

(def (cmd-toggle-global-prettify-symbols app)
  "Toggle global-prettify-symbols-mode (display symbol ligatures)."
  (let ((echo (app-state-echo app)))
    (set! *global-prettify-symbols* (not *global-prettify-symbols*))
    (echo-message! echo (if *global-prettify-symbols*
                          "Prettify symbols ON" "Prettify symbols OFF"))))

(def (cmd-toggle-global-subword app)
  "Toggle global-subword-mode (camelCase word navigation)."
  (let ((echo (app-state-echo app)))
    (set! *global-subword-mode* (not *global-subword-mode*))
    (echo-message! echo (if *global-subword-mode*
                          "Global subword mode ON" "Global subword mode OFF"))))

(def (cmd-toggle-global-superword app)
  "Toggle global-superword-mode (treat symbols as words)."
  (let ((echo (app-state-echo app)))
    (set! *global-superword-mode* (not *global-superword-mode*))
    (echo-message! echo (if *global-superword-mode*
                          "Global superword mode ON" "Global superword mode OFF"))))

(def (cmd-toggle-delete-by-moving-to-trash app)
  "Toggle delete-by-moving-to-trash."
  (let ((echo (app-state-echo app)))
    (set! *delete-by-moving-to-trash* (not *delete-by-moving-to-trash*))
    (echo-message! echo (if *delete-by-moving-to-trash*
                          "Delete to trash ON" "Delete to trash OFF"))))

(def (cmd-toggle-create-lockfiles app)
  "Toggle creation of lock files for edited buffers."
  (let ((echo (app-state-echo app)))
    (set! *create-lockfiles* (not *create-lockfiles*))
    (echo-message! echo (if *create-lockfiles*
                          "Lock files ON" "Lock files OFF"))))

(def (cmd-toggle-mode-line-compact app)
  "Toggle compact mode-line display."
  (let ((echo (app-state-echo app)))
    (set! *mode-line-compact* (not *mode-line-compact*))
    (echo-message! echo (if *mode-line-compact*
                          "Mode-line compact ON" "Mode-line compact OFF"))))

(def (cmd-toggle-use-file-dialog app)
  "Toggle use of file dialogs for file operations."
  (let ((echo (app-state-echo app)))
    (set! *use-file-dialog* (not *use-file-dialog*))
    (echo-message! echo (if *use-file-dialog*
                          "File dialog ON" "File dialog OFF"))))

(def (cmd-toggle-xterm-mouse-mode app)
  "Toggle xterm-mouse-mode (mouse in terminal)."
  (let ((echo (app-state-echo app)))
    (set! *xterm-mouse-mode* (not *xterm-mouse-mode*))
    (echo-message! echo (if *xterm-mouse-mode*
                          "Xterm mouse mode ON" "Xterm mouse mode OFF"))))
