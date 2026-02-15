;;; -*- Gerbil -*-
;;; Qt commands modes - abbreviations, recentf, calendar, EWW, man, spelling
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/editor
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/modeline
        :gerbil-emacs/qt/commands-core
        :gerbil-emacs/qt/commands-edit
        :gerbil-emacs/qt/commands-search
        :gerbil-emacs/qt/commands-file
        :gerbil-emacs/qt/commands-sexp
        :gerbil-emacs/qt/commands-ide
        :gerbil-emacs/qt/commands-vcs
        :gerbil-emacs/qt/commands-shell)


(def (cmd-expand-abbrev app)
  "Expand abbreviation before cursor."
  (let* ((ed (current-qt-editor app))
         (prefix (get-word-prefix ed)))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No abbrev at point")
      (let ((expansion (hash-get *abbrev-table* prefix)))
        (if (not expansion)
          (echo-message! (app-state-echo app)
            (string-append "No abbrev for \"" prefix "\""))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (- pos (string-length prefix)))
                 (text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring text 0 start)
                             expansion
                             (substring text pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed
              (+ start (string-length expansion)))))))))

(def (cmd-list-abbrevs app)
  "List all defined abbreviations in a buffer."
  (let* ((pairs (sort (hash->list *abbrev-table*)
                  (lambda (a b) (string<? (car a) (car b)))))
         (text (if (null? pairs)
                 "No abbreviations defined.\n"
                 (string-append
                   "Abbreviations:\n"
                   (string-append
                     (string-join
                       (map (lambda (p)
                              (string-append "  " (car p) " => " (cdr p)))
                            pairs)
                       "\n")
                     "\n")))))
    (let* ((fr (app-state-frame app))
           (ed (current-qt-editor app))
           (buf (or (buffer-by-name "*abbrevs*")
                    (qt-buffer-create! "*abbrevs*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;; --- Man page viewer ---
(def (cmd-man app)
  "View a man page in a buffer."
  (let ((topic (qt-echo-read-string app "Man page: ")))
    (when (and topic (not (string=? topic "")))
      (let* ((parts (string-split topic #\space))
             (args (if (= (length parts) 2)
                     [(car parts) (cadr parts)]  ;; section + topic
                     parts))
             (port (open-process
                     (list path: "/usr/bin/man"
                           arguments: args
                           environment: ["MANPAGER=cat" "COLUMNS=80"
                                         "MAN_KEEP_FORMATTING=1"
                                         (string-append "HOME=" (or (getenv "HOME" #f) "/tmp"))
                                         (string-append "PATH=" (or (getenv "PATH" #f) "/usr/bin"))]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f))
             (_ (process-status port)))
        (close-port port)
        (if (or (not output) (string=? output ""))
          (echo-error! (app-state-echo app)
            (string-append "No man page for \"" topic "\""))
          ;; Strip backspace-based formatting (bold: X^HX, underline: _^HX)
          (let* ((clean (man-strip-formatting output))
                 (buf-name (string-append "*Man " topic "*"))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (or (buffer-by-name buf-name)
                          (qt-buffer-create! buf-name ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed clean)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! (app-state-echo app) buf-name)))))))

(def (man-strip-formatting text)
  "Remove backspace-based man page formatting (bold: X^HX, underline: _^HX)."
  (let* ((len (string-length text))
         (out (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string out))
        ((and (< (+ i 2) len)
              (char=? (string-ref text (+ i 1)) #\backspace))
         ;; Skip the overstriking: take the character after ^H
         (write-char (string-ref text (+ i 2)) out)
         (loop (+ i 3)))
        (else
         (write-char (string-ref text i) out)
         (loop (+ i 1)))))))

;; --- EWW-style web browser ---
(def *eww-history* [])
(def *eww-current-url* #f)

(def (eww-fetch-url url)
  "Fetch a URL using curl and return the raw HTML."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((port (open-process
                     (list path: "curl"
                           arguments: ["-sL" "-m" "10"
                                       "-A" "Mozilla/5.0 (compatible; gerbil-emacs eww)"
                                       url]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f))
             (_ (process-status port)))
        (close-port port)
        output))))

(def (eww-html-to-text html)
  "Simple HTML to text converter. Strips tags, decodes basic entities, adds newlines."
  (let* ((len (string-length html))
         (out (open-output-string))
         (col 0)
         (fill-col 78))
    (let loop ((i 0) (in-tag #f) (in-pre #f) (tag-buf ""))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref html i)))
          (cond
            ;; Start of tag
            ((and (not in-tag) (char=? ch #\<))
             (loop (+ i 1) #t in-pre ""))
            ;; End of tag
            ((and in-tag (char=? ch #\>))
             (let* ((tag (string-downcase tag-buf))
                    (tag-name (let ((sp (string-index tag #\space)))
                                (if sp (substring tag 0 sp) tag))))
               (cond
                 ((or (string=? tag-name "br") (string=? tag-name "br/"))
                  (write-char #\newline out) (set! col 0))
                 ((or (string=? tag-name "p") (string=? tag-name "/p")
                      (string=? tag-name "div") (string=? tag-name "/div")
                      (string=? tag-name "h1") (string=? tag-name "h2")
                      (string=? tag-name "h3") (string=? tag-name "h4")
                      (string=? tag-name "/h1") (string=? tag-name "/h2")
                      (string=? tag-name "/h3") (string=? tag-name "/h4")
                      (string=? tag-name "tr") (string=? tag-name "/tr")
                      (string=? tag-name "li"))
                  (when (> col 0)
                    (write-char #\newline out) (set! col 0))
                  (when (string=? tag-name "li")
                    (display "  * " out) (set! col 4)))
                 ((string=? tag-name "pre")
                  (loop (+ i 1) #f #t ""))
                 ((string=? tag-name "/pre")
                  (loop (+ i 1) #f #f "")))
               (loop (+ i 1) #f in-pre "")))
            ;; Inside tag
            (in-tag
             (loop (+ i 1) #t in-pre (string-append tag-buf (string ch))))
            ;; HTML entity
            ((char=? ch #\&)
             (let entity ((j (+ i 1)) (ebuf ""))
               (if (or (>= j len) (> (- j i) 10))
                 (begin (write-char #\& out) (set! col (+ col 1))
                        (loop (+ i 1) #f in-pre ""))
                 (let ((ec (string-ref html j)))
                   (if (char=? ec #\;)
                     (let ((entity-str ebuf))
                       (cond
                         ((string=? entity-str "amp") (write-char #\& out))
                         ((string=? entity-str "lt") (write-char #\< out))
                         ((string=? entity-str "gt") (write-char #\> out))
                         ((string=? entity-str "quot") (write-char #\" out))
                         ((string=? entity-str "apos") (write-char #\' out))
                         ((string=? entity-str "nbsp") (write-char #\space out))
                         ((string=? entity-str "#39") (write-char #\' out))
                         (else (display entity-str out)))
                       (set! col (+ col 1))
                       (loop (+ j 1) #f in-pre ""))
                     (entity (+ j 1) (string-append ebuf (string ec))))))))
            ;; Whitespace handling
            ((and (not in-pre) (char-whitespace? ch))
             (when (> col 0)
               (write-char #\space out) (set! col (+ col 1)))
             ;; Skip consecutive whitespace
             (let skip ((k (+ i 1)))
               (if (and (< k len) (char-whitespace? (string-ref html k)))
                 (skip (+ k 1))
                 (loop k #f in-pre ""))))
            ;; Regular character
            (else
             (write-char ch out)
             (set! col (+ col 1))
             ;; Word wrap at fill column
             (when (and (not in-pre) (>= col fill-col) (char=? ch #\space))
               (write-char #\newline out) (set! col 0))
             (loop (+ i 1) #f in-pre ""))))))))

(def (cmd-eww app)
  "Open a URL in the text browser."
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (and url (not (string=? url "")))
      ;; Prepend https:// if no scheme
      (let ((full-url (if (or (string-prefix? "http://" url)
                              (string-prefix? "https://" url))
                        url
                        (string-append "https://" url))))
        (echo-message! (app-state-echo app) (string-append "Fetching " full-url "..."))
        (let ((html (eww-fetch-url full-url)))
          (if (not html)
            (echo-error! (app-state-echo app) "Failed to fetch URL")
            (let* ((text (eww-html-to-text html))
                   (buf-name "*eww*")
                   (fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (buf (or (buffer-by-name buf-name)
                            (qt-buffer-create! buf-name ed #f))))
              (set! *eww-current-url* full-url)
              (set! *eww-history* (cons full-url *eww-history*))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "URL: " full-url "\n\n" text))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (echo-message! (app-state-echo app) full-url))))))))

(def (cmd-eww-back app)
  "Go back in eww browsing history."
  (if (or (null? *eww-history*) (null? (cdr *eww-history*)))
    (echo-message! (app-state-echo app) "No previous page")
    (begin
      (set! *eww-history* (cdr *eww-history*))
      (let ((url (car *eww-history*)))
        (set! *eww-current-url* url)
        (echo-message! (app-state-echo app) (string-append "Fetching " url "..."))
        (let ((html (eww-fetch-url url)))
          (when html
            (let* ((text (eww-html-to-text html))
                   (ed (current-qt-editor app))
                   (fr (app-state-frame app)))
              (qt-plain-text-edit-set-text! ed
                (string-append "URL: " url "\n\n" text))
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-eww-reload app)
  "Reload the current eww page."
  (when *eww-current-url*
    (echo-message! (app-state-echo app) "Reloading...")
    (let ((html (eww-fetch-url *eww-current-url*)))
      (when html
        (let* ((text (eww-html-to-text html))
               (ed (current-qt-editor app)))
          (qt-plain-text-edit-set-text! ed
            (string-append "URL: " *eww-current-url* "\n\n" text))
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;; --- Remote file editing (tramp-style) ---
(def (tramp-path? path)
  "Check if PATH is a tramp-style remote path (/ssh:host:path or /scp:host:path)."
  (or (string-prefix? "/ssh:" path)
      (string-prefix? "/scp:" path)))

(def (tramp-parse-path path)
  "Parse /ssh:host:path into (values host remote-path).
   Also supports /ssh:user@host:path."
  (let* ((rest (cond
                 ((string-prefix? "/ssh:" path) (substring path 5 (string-length path)))
                 ((string-prefix? "/scp:" path) (substring path 5 (string-length path)))
                 (else path)))
         (colon-pos (string-index rest #\:)))
    (if colon-pos
      (values (substring rest 0 colon-pos)
              (substring rest (+ colon-pos 1) (string-length rest)))
      (values rest "/"))))

(def (tramp-read-file host remote-path)
  "Read a remote file via scp into a string."
  (let* ((tmp (path-expand
                (string-append "tramp-" (number->string (random-integer 100000)))
                (or (getenv "TMPDIR" #f) "/tmp")))
         (src (string-append host ":" remote-path))
         (proc (open-process
                 (list path: "/usr/bin/scp"
                       arguments: ["-q" src tmp]
                       stdout-redirection: #t
                       stderr-redirection: #f
                       pseudo-terminal: #f)))
         (_ (process-status proc)))
    (close-port proc)
    (if (file-exists? tmp)
      (let ((content (call-with-input-file tmp (lambda (p) (read-line p #f)))))
        (delete-file tmp)
        content)
      #f)))

(def (tramp-write-file host remote-path content)
  "Write content to a remote file via scp."
  (let* ((tmp (path-expand
                (string-append "tramp-" (number->string (random-integer 100000)))
                (or (getenv "TMPDIR" #f) "/tmp")))
         (dst (string-append host ":" remote-path)))
    (call-with-output-file tmp (lambda (p) (display content p)))
    (let* ((proc (open-process
                   (list path: "/usr/bin/scp"
                         arguments: ["-q" tmp dst]
                         stdout-redirection: #t
                         stderr-redirection: #f
                         pseudo-terminal: #f)))
           (status (process-status proc)))
      (close-port proc)
      (when (file-exists? tmp) (delete-file tmp))
      (= status 0))))

(def (cmd-find-file-remote app)
  "Open a remote file via SSH/SCP. Use /ssh:host:path or /scp:host:path syntax."
  (let ((path (qt-echo-read-string app "Remote file (/ssh:host:path): ")))
    (when (and path (not (string=? path "")))
      (if (not (tramp-path? path))
        (echo-error! (app-state-echo app) "Use /ssh:host:path syntax")
        (let-values (((host remote-path) (tramp-parse-path path)))
          (echo-message! (app-state-echo app)
            (string-append "Fetching " host ":" remote-path "..."))
          (let ((content (tramp-read-file host remote-path)))
            (if (not content)
              (echo-error! (app-state-echo app)
                (string-append "Failed to fetch " remote-path " from " host))
              (let* ((name (string-append (path-strip-directory remote-path) " [" host "]"))
                     (fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (qt-buffer-create! name ed #f)))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed content)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                ;; Store remote info in buffer for save-back
                (set! (buffer-file-path buf) path)
                (echo-message! (app-state-echo app)
                  (string-append "Loaded " remote-path " from " host))))))))))

(def (cmd-save-remote-buffer app)
  "Save buffer back to remote host if it has a tramp-style path."
  (let* ((buf (current-qt-buffer app))
         (fpath (buffer-file-path buf)))
    (if (or (not fpath) (not (tramp-path? fpath)))
      (echo-error! (app-state-echo app) "Not a remote buffer")
      (let-values (((host remote-path) (tramp-parse-path fpath)))
        (let ((text (qt-plain-text-edit-text (current-qt-editor app))))
          (echo-message! (app-state-echo app)
            (string-append "Saving to " host ":" remote-path "..."))
          (if (tramp-write-file host remote-path text)
            (begin
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! (app-state-echo app) "Remote file saved"))
            (echo-error! (app-state-echo app) "Failed to save remote file")))))))

;; --- Calendar ---
(def *calendar-year* #f)
(def *calendar-month* #f)

(def (calendar-current-year-month)
  "Get current year and month from system date command."
  (with-catch
    (lambda (e) (values 2026 1))
    (lambda ()
      (let* ((port (open-process
                     (list path: "/bin/date"
                           arguments: ["+%Y %m"]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (line (read-line port)))
        (close-port port)
        (if (eof-object? line)
          (values 2026 1)
          (let* ((parts (string-split line #\space))
                 (year (string->number (car parts)))
                 (month (string->number (cadr parts))))
            (values year month)))))))

(def (calendar-render year month)
  "Render a 3-month calendar centered on the given month."
  (with-catch
    (lambda (e) (string-append "Calendar error\n"))
    (lambda ()
      (let* ((port (open-process
                     (list path: "/usr/bin/cal"
                           arguments: ["-3" (number->string month) (number->string year)]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f)))
        (close-port port)
        (or output "")))))

(def (cmd-calendar app)
  "Display a calendar."
  (when (not *calendar-year*)
    (let-values (((y m) (calendar-current-year-month)))
      (set! *calendar-year* y)
      (set! *calendar-month* m)))
  (let* ((text (string-append
                 (calendar-render *calendar-year* *calendar-month*)
                 "\n\nNavigation: p=prev month  n=next month  <=prev year  >=next year  .=today"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name "*calendar*")
                  (qt-buffer-create! "*calendar*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (cmd-calendar-prev-month app)
  "Go to previous month in calendar."
  (when *calendar-month*
    (set! *calendar-month* (- *calendar-month* 1))
    (when (< *calendar-month* 1)
      (set! *calendar-month* 12)
      (set! *calendar-year* (- *calendar-year* 1)))
    (cmd-calendar app)))

(def (cmd-calendar-next-month app)
  "Go to next month in calendar."
  (when *calendar-month*
    (set! *calendar-month* (+ *calendar-month* 1))
    (when (> *calendar-month* 12)
      (set! *calendar-month* 1)
      (set! *calendar-year* (+ *calendar-year* 1)))
    (cmd-calendar app)))

(def (cmd-calendar-prev-year app)
  "Go to previous year in calendar."
  (when *calendar-year*
    (set! *calendar-year* (- *calendar-year* 1))
    (cmd-calendar app)))

(def (cmd-calendar-next-year app)
  "Go to next year in calendar."
  (when *calendar-year*
    (set! *calendar-year* (+ *calendar-year* 1))
    (cmd-calendar app)))

(def (cmd-calendar-today app)
  "Go to current month in calendar."
  (let-values (((y m) (calendar-current-year-month)))
    (set! *calendar-year* y)
    (set! *calendar-month* m)
    (cmd-calendar app)))

;; --- Runtime key rebinding ---
(def *custom-keys-path*
  (path-expand ".gerbil-emacs-keys" (user-info-home (user-info (user-name)))))

(def (cmd-global-set-key app)
  "Bind a key to a command interactively."
  (let* ((key-str (qt-echo-read-string app "Key (e.g. C-c a, M-g t): "))
         (cmds (sort (map (lambda (p) (symbol->string (car p)))
                       (hash->list *all-commands*))
                 string<?))
         (cmd-name (qt-echo-read-string-with-completion app "Command: " cmds)))
    (when (and key-str (not (string=? key-str ""))
               cmd-name (not (string=? cmd-name "")))
      (let ((cmd-sym (string->symbol cmd-name)))
        ;; Determine which keymap to bind in
        (cond
          ((string-prefix? "C-x " key-str)
           (keymap-bind! *ctrl-x-map* (substring key-str 4 (string-length key-str)) cmd-sym))
          ((string-prefix? "C-c " key-str)
           (keymap-bind! *ctrl-c-map* (substring key-str 4 (string-length key-str)) cmd-sym))
          (else
           (keymap-bind! *global-keymap* key-str cmd-sym)))
        ;; Record and save to persistent file
        (set! *custom-key-bindings*
          (cons (cons key-str cmd-name)
                (filter (lambda (p) (not (string=? (car p) key-str)))
                        *custom-key-bindings*)))
        (custom-keys-save!)
        (echo-message! (app-state-echo app)
          (string-append key-str " → " cmd-name))))))

(def (cmd-global-unset-key app)
  "Unbind a key."
  (let ((key-str (qt-echo-read-string app "Key to unbind: ")))
    (when (and key-str (not (string=? key-str "")))
      (cond
        ((string-prefix? "C-x " key-str)
         (hash-remove! *ctrl-x-map* (substring key-str 4 (string-length key-str))))
        ((string-prefix? "C-c " key-str)
         (hash-remove! *ctrl-c-map* (substring key-str 4 (string-length key-str))))
        (else
         (hash-remove! *global-keymap* key-str)))
      (set! *custom-key-bindings*
        (filter (lambda (p) (not (string=? (car p) key-str)))
                *custom-key-bindings*))
      (custom-keys-save!)
      (echo-message! (app-state-echo app)
        (string-append key-str " unbound")))))

(def *custom-key-bindings* []) ;; list of (key-str . cmd-name) pairs

(def (custom-keys-save!)
  "Save custom key bindings to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *custom-keys-path*
        (lambda (port)
          (for-each
            (lambda (pair)
              (display (car pair) port) (display "\t" port)
              (display (cdr pair) port) (newline port))
            *custom-key-bindings*))))))

(def (custom-keys-load!)
  "Load custom key bindings from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *custom-keys-path*)
        (call-with-input-file *custom-keys-path*
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (unless (eof-object? line)
                  (let ((tab-pos (string-index line #\tab)))
                    (when tab-pos
                      (let* ((key-str (substring line 0 tab-pos))
                             (cmd-name (substring line (+ tab-pos 1) (string-length line)))
                             (cmd-sym (string->symbol cmd-name)))
                        (cond
                          ((string-prefix? "C-x " key-str)
                           (keymap-bind! *ctrl-x-map*
                             (substring key-str 4 (string-length key-str)) cmd-sym))
                          ((string-prefix? "C-c " key-str)
                           (keymap-bind! *ctrl-c-map*
                             (substring key-str 4 (string-length key-str)) cmd-sym))
                          (else
                           (keymap-bind! *global-keymap* key-str cmd-sym))))))
                  (loop))))))))))

;;; ============================================================================
;;; Init file loading
;;; ============================================================================

(def *init-file-path*
  (path-expand ".gerbil-emacs-init.ss" (user-info-home (user-info (user-name)))))

(def (load-init-file!)
  "Load user init file (~/.gerbil-emacs-init.ss) if it exists."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *init-file-path*)
        (let ((text (read-file-as-string *init-file-path*)))
          (when text
            (let ((port (open-input-string text)))
              (let loop ()
                (let ((form (read port)))
                  (unless (eof-object? form)
                    (with-catch
                      (lambda (e) #f) ;; skip forms that error
                      (lambda () (eval form)))
                    (loop)))))))))))

(def (cmd-load-init-file app)
  "Reload user init file."
  (if (file-exists? *init-file-path*)
    (begin
      (load-init-file!)
      (echo-message! (app-state-echo app)
        (string-append "Loaded " *init-file-path*)))
    (echo-error! (app-state-echo app)
      (string-append "No init file: " *init-file-path*))))

(def (cmd-find-init-file app)
  "Open user init file for editing."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (name (path-strip-directory *init-file-path*))
         (buf (or (buffer-by-name name)
                  (qt-buffer-create! name ed *init-file-path*))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (when (file-exists? *init-file-path*)
      (let ((text (read-file-as-string *init-file-path*)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))
    (echo-message! (app-state-echo app) *init-file-path*)))

;;; ============================================================================
;;; Persistent scratch buffer
;;; ============================================================================

(def *scratch-file-path*
  (path-expand ".gerbil-emacs-scratch" (user-info-home (user-info (user-name)))))

(def (scratch-save!)
  "Save scratch buffer contents to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((buf (buffer-by-name "*scratch*")))
        (when buf
          ;; Find a window showing this buffer to get text
          (let ((text #f))
            ;; Try each buffer in buffer-list to find one with a doc-pointer
            (when (buffer-doc-pointer buf)
              ;; We need the text from the widget, but we may not have the editor
              ;; Use the last known text approach: save it on each access
              (void))
            ;; Fallback: write from *scratch-last-text*
            (when *scratch-last-text*
              (call-with-output-file *scratch-file-path*
                (lambda (port) (display *scratch-last-text* port))))))))))

(def *scratch-last-text* #f)

(def (scratch-update-text! text)
  "Update cached scratch buffer text for persistence."
  (set! *scratch-last-text* text))

(def (scratch-restore!)
  "Restore scratch buffer contents from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (if (file-exists? *scratch-file-path*)
        (let ((text (read-file-as-string *scratch-file-path*)))
          (if text
            (begin (set! *scratch-last-text* text) text)
            #f))
        #f))))

;;; ============================================================================
;;; Basic Org-mode support
;;; ============================================================================

(def *org-todo-keywords* '("TODO" "IN-PROGRESS" "DONE"))

(def (org-buffer? buf)
  "Check if buffer is an org file."
  (let ((name (buffer-name buf)))
    (and (> (string-length name) 4)
         (string-suffix? ".org" name))))

(def (org-get-current-line ed)
  "Get the current line text and its start/end positions."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1))))))
    (values (substring text line-start line-end) line-start line-end)))

(def (org-heading-level line)
  "Return the heading level (number of leading *'s) or 0 if not a heading."
  (let loop ((i 0))
    (if (or (>= i (string-length line))
            (not (char=? (string-ref line i) #\*)))
      (if (and (> i 0) (< i (string-length line))
               (char=? (string-ref line i) #\space))
        i 0)
      (loop (+ i 1)))))

(def (cmd-org-todo-cycle app)
  "Cycle TODO state on current heading: none -> TODO -> IN-PROGRESS -> DONE -> none."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          ;; Extract the part after "*** "
          (let* ((after-stars (substring line (+ level 1) (string-length line)))
                 ;; Check if line starts with a TODO keyword
                 (current-kw
                   (let loop ((kws *org-todo-keywords*))
                     (if (null? kws) #f
                       (let ((kw (car kws)))
                         (if (string-prefix? (string-append kw " ") after-stars)
                           kw (loop (cdr kws)))))))
                 ;; Determine next keyword
                 (next-kw
                   (if (not current-kw)
                     (car *org-todo-keywords*)
                     (let loop ((kws *org-todo-keywords*))
                       (cond
                         ((null? kws) #f)  ;; cycle back to none
                         ((string=? (car kws) current-kw)
                          (if (null? (cdr kws)) #f (cadr kws)))
                         (else (loop (cdr kws)))))))
                 ;; Build new line
                 (heading-prefix (string-append (make-string level #\*) " "))
                 (rest (if current-kw
                         (substring after-stars
                           (+ (string-length current-kw) 1)
                           (string-length after-stars))
                         after-stars))
                 (new-line (if next-kw
                            (string-append heading-prefix next-kw " " rest)
                            (string-append heading-prefix rest)))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (+ line-start (string-length new-line))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-promote app)
  "Decrease heading level (remove one *)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 1)
          (let* ((new-line (substring line 1 (string-length line)))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (max line-start (- pos 1)))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-demote app)
  "Increase heading level (add one *)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          (let* ((new-line (string-append "*" line))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-toggle-checkbox app)
  "Toggle checkbox: [ ] <-> [X]."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((check-pos (string-contains line "[ ]"))
            (checked-pos (string-contains line "[X]")))
        (cond
          (check-pos
           (let* ((abs-pos (+ line-start check-pos))
                  (new-text (string-append
                              (substring text 0 abs-pos) "[X]"
                              (substring text (+ abs-pos 3) (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          (checked-pos
           (let* ((abs-pos (+ line-start checked-pos))
                  (new-text (string-append
                              (substring text 0 abs-pos) "[ ]"
                              (substring text (+ abs-pos 3) (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          (else
           (echo-message! (app-state-echo app) "No checkbox on this line")))))))

(def (cmd-org-insert-heading app)
  "Insert a new heading at the same level as the current one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let* ((level (org-heading-level line))
             (stars (if (> level 0) level 1))
             (heading (string-append "\n" (make-string stars #\*) " "))
             (new-text (string-append
                         (substring text 0 line-end) heading
                         (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (+ line-end (string-length heading)))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-org-next-heading app)
  "Move to next heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Skip past current line
    (let ((next-nl (let loop ((i pos))
                     (if (or (>= i len) (char=? (string-ref text i) #\newline))
                       (+ i 1) (loop (+ i 1))))))
      ;; Find next line starting with *
      (let loop ((i next-nl))
        (cond
          ((>= i len)
           (echo-message! (app-state-echo app) "No more headings"))
          ((and (char=? (string-ref text i) #\*)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (+ i 1))))))))

(def (cmd-org-prev-heading app)
  "Move to previous heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Go back to start of current line
    (let ((line-start (let loop ((i (- pos 1)))
                        (if (or (< i 0) (char=? (string-ref text i) #\newline))
                          i (loop (- i 1))))))
      ;; Search backward for a line starting with *
      (let loop ((i (- line-start 1)))
        (cond
          ((< i 0)
           (echo-message! (app-state-echo app) "No previous heading"))
          ((and (char=? (string-ref text i) #\*)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

(def (cmd-org-move-subtree-up app)
  "Move current heading and its subtree up (swap with previous sibling)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          ;; Find end of current subtree
          (let* ((len (string-length text))
                 (subtree-end
                   (let loop ((i (+ line-end 1)))
                     (cond
                       ((>= i len) len)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        ;; Check if this is same level or higher (not deeper)
                        (let ((sub-line-end (let lp ((j i))
                                              (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                j (lp (+ j 1))))))
                          (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                            (if (<= sub-level level) i
                              (loop (+ sub-line-end 1))))))
                       (else (loop (+ i 1))))))
                 ;; Find start of previous sibling
                 (prev-start
                   (let loop ((i (- line-start 2)))
                     (cond
                       ((< i 0) #f)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        (let ((prev-line-end (let lp ((j i))
                                               (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                 j (lp (+ j 1))))))
                          (let ((prev-level (org-heading-level (substring text i prev-line-end))))
                            (if (= prev-level level) i
                              (if (< prev-level level) #f
                                (loop (- i 1)))))))
                       (else (loop (- i 1)))))))
            (when prev-start
              ;; Swap: previous sibling subtree with current subtree
              (let* ((current-subtree (substring text line-start subtree-end))
                     (prev-subtree (substring text prev-start line-start))
                     (new-text (string-append
                                 (substring text 0 prev-start)
                                 current-subtree prev-subtree
                                 (substring text subtree-end (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed prev-start)
                (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-org-move-subtree-down app)
  "Move current heading and its subtree down (swap with next sibling)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          (let* ((len (string-length text))
                 ;; Find end of current subtree
                 (subtree-end
                   (let loop ((i (+ line-end 1)))
                     (cond
                       ((>= i len) len)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        (let ((sub-line-end (let lp ((j i))
                                              (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                j (lp (+ j 1))))))
                          (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                            (if (<= sub-level level) i
                              (loop (+ sub-line-end 1))))))
                       (else (loop (+ i 1)))))))
            ;; Find end of next sibling subtree
            (when (< subtree-end len)
              (let* ((next-heading-end
                       (let loop ((i subtree-end))
                         (if (or (>= i len) (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
                     (next-subtree-end
                       (let loop ((i (+ next-heading-end 1)))
                         (cond
                           ((>= i len) len)
                           ((and (char=? (string-ref text i) #\*)
                                 (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                            (let ((sub-line-end (let lp ((j i))
                                                  (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                    j (lp (+ j 1))))))
                              (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                                (if (<= sub-level level) i
                                  (loop (+ sub-line-end 1))))))
                           (else (loop (+ i 1)))))))
                ;; Swap
                (let* ((current-subtree (substring text line-start subtree-end))
                       (next-subtree (substring text subtree-end next-subtree-end))
                       (new-text (string-append
                                   (substring text 0 line-start)
                                   next-subtree current-subtree
                                   (substring text next-subtree-end (string-length text)))))
                  (qt-plain-text-edit-set-text! ed new-text)
                  (qt-plain-text-edit-set-cursor-position! ed
                    (+ line-start (string-length next-subtree)))
                  (qt-plain-text-edit-ensure-cursor-visible! ed))))))))))

;;; ============================================================================
;;; Markdown mode
;;; ============================================================================

(def (md-heading-level line)
  "Return the heading level (1-6) of LINE, or 0 if not a heading."
  (let ((len (string-length line)))
    (if (= len 0) 0
      (let loop ((i 0))
        (cond
          ((>= i len) 0)
          ((>= i 6) 0) ;; max 6 levels
          ((char=? (string-ref line i) #\#) (loop (+ i 1)))
          ((and (> i 0) (char=? (string-ref line i) #\space)) i)
          (else 0))))))

(def (md-get-current-line ed)
  "Get current line text, start position, and end position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (line-start (let loop ((i pos))
                       (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                         i (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i len) (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1))))))
    (values (substring text line-start line-end) line-start line-end)))

(def (cmd-markdown-promote app)
  "Decrease heading level (remove a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (if (<= level 1)
          (echo-error! (app-state-echo app) "Cannot promote further")
          (let* ((text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring text 0 line-start)
                             (substring line 1 (string-length line))
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed line-start)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-markdown-demote app)
  "Increase heading level (add a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (cond
          ((= level 0)
           ;; Not a heading — make it one
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "# " line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed)))
          ((>= level 6)
           (echo-error! (app-state-echo app) "Cannot demote further (max level 6)"))
          (else
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "#" line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start level 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed))))))))

(def (cmd-markdown-next-heading app)
  "Jump to the next markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Skip to next line first
    (let ((start (let loop ((i pos))
                   (if (or (>= i len) (char=? (string-ref text i) #\newline))
                     (+ i 1) (loop (+ i 1))))))
      (let loop ((i start))
        (cond
          ((>= i len)
           (echo-message! (app-state-echo app) "No more headings"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (+ i 1))))))))

(def (cmd-markdown-prev-heading app)
  "Jump to the previous markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Go to start of current line, then one more
    (let ((start (let loop ((i pos))
                   (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                     (- i 1) (loop (- i 1))))))
      (let loop ((i (max 0 start)))
        (cond
          ((< i 0)
           (echo-message! (app-state-echo app) "No previous heading"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

(def (cmd-markdown-insert-heading app)
  "Insert a heading at the same level as the current one."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((level (md-heading-level line))
             (prefix (if (> level 0) (string-append (make-string level #\#) " ") "## "))
             (text (qt-plain-text-edit-text ed))
             (insert-text (string-append "\n" prefix))
             (new-text (string-append
                         (substring text 0 line-end)
                         insert-text
                         (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end (string-length insert-text)))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-markdown-toggle-bold app)
  "Toggle bold (**) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection, insert **cursor**
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "****"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 2)))
      ;; Wrap selection in **
      (let* ((selected (substring text sel-start sel-end))
             ;; Check if already bold
             (already-bold (and (>= (string-length selected) 4)
                                (string-prefix? "**" selected)
                                (string-suffix? "**" selected)))
             (replacement (if already-bold
                            (substring selected 2 (- (string-length selected) 2))
                            (string-append "**" selected "**")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-italic app)
  "Toggle italic (*) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "**"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-italic (and (>= (string-length selected) 2)
                                   (string-prefix? "*" selected)
                                   (string-suffix? "*" selected)
                                   (not (string-prefix? "**" selected))))
             (replacement (if already-italic
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "*" selected "*")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-code app)
  "Toggle inline code (`) around selection or insert backticks."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "``"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-code (and (>= (string-length selected) 2)
                                 (string-prefix? "`" selected)
                                 (string-suffix? "`" selected)))
             (replacement (if already-code
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "`" selected "`")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-insert-link app)
  "Insert a markdown link [text](url)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (link-text (if (= sel-start sel-end) ""
                      (substring text sel-start sel-end)))
         (url (qt-echo-read-string app "URL: ")))
    (when (> (string-length url) 0)
      (let* ((md-link (string-append "[" (if (string=? link-text "") "link" link-text) "](" url ")"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         md-link
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length md-link)))))))

(def (cmd-markdown-insert-code-block app)
  "Insert a fenced code block."
  (let* ((ed (current-qt-editor app))
         (lang (qt-echo-read-string app "Language (empty for none): "))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (block (string-append "\n```" lang "\n\n```\n"))
         (new-text (string-append
                     (substring text 0 pos)
                     block
                     (substring text pos (string-length text)))))
    (qt-plain-text-edit-set-text! ed new-text)
    ;; Place cursor inside the code block
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 4 (string-length lang) 1))
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-markdown-toggle-checkbox app)
  "Toggle a markdown checkbox [ ] / [x]."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((text (qt-plain-text-edit-text ed))
             (trimmed (string-trim line))
             (has-unchecked (string-contains trimmed "[ ]"))
             (has-checked (string-contains trimmed "[x]")))
        (cond
          (has-unchecked
           (let* ((idx (string-contains line "[ ]"))
                  (new-line (string-append
                              (substring line 0 idx) "[x]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (has-checked
           (let* ((idx (string-contains line "[x]"))
                  (new-line (string-append
                              (substring line 0 idx) "[ ]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (else
           ;; Add checkbox prefix
           (let* ((new-line (string-append "- [ ] " trimmed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text))))))))

(def (cmd-markdown-outline app)
  "Show an outline of all headings in the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (lines
           (let loop ((i 0) (acc []))
             (if (>= i len) (reverse acc)
               (let* ((line-end (let lp ((j i))
                                  (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                    j (lp (+ j 1)))))
                      (line (substring text i line-end)))
                 (if (and (> (string-length line) 0)
                          (char=? (string-ref line 0) #\#))
                   (let ((level (md-heading-level line)))
                     (if (> level 0)
                       (loop (+ line-end 1) (cons (cons i line) acc))
                       (loop (+ line-end 1) acc)))
                   (loop (+ line-end 1) acc)))))))
    (if (null? lines)
      (echo-message! (app-state-echo app) "No headings found")
      (let* ((fr (app-state-frame app))
             (outline-buf (or (buffer-by-name "*MD Outline*")
                              (qt-buffer-create! "*MD Outline*" ed #f)))
             (outline-text
               (string-join
                 (map (lambda (entry)
                        (let* ((pos (car entry))
                               (line (cdr entry))
                               (level (md-heading-level line))
                               (indent (make-string (* 2 (- level 1)) #\space)))
                          (string-append indent (number->string pos) ": " line)))
                      lines)
                 "\n")))
        (qt-buffer-attach! ed outline-buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) outline-buf)
        (qt-plain-text-edit-set-text! ed outline-text)
        (qt-text-document-set-modified! (buffer-doc-pointer outline-buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-markdown-preview app)
  "Generate and display an HTML preview of the current markdown buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (fr (app-state-frame app))
         (preview-buf (or (buffer-by-name "*MD Preview*")
                          (qt-buffer-create! "*MD Preview*" ed #f))))
    ;; Simple markdown to text conversion for preview
    (let* ((lines (string-split text #\newline))
           (rendered
             (string-join
               (map (lambda (line)
                      (let ((level (md-heading-level line)))
                        (cond
                          ;; Heading: underline with = or -
                          ((> level 0)
                           (let* ((heading-text (substring line (+ level 1) (string-length line)))
                                  (underline (make-string (string-length heading-text)
                                                          (if (= level 1) #\= #\-))))
                             (string-append "\n" heading-text "\n" underline)))
                          ;; Horizontal rule
                          ((or (string-prefix? "---" line) (string-prefix? "***" line)
                               (string-prefix? "___" line))
                           (make-string 72 #\-))
                          ;; Code block markers
                          ((string-prefix? "```" line)
                           (string-append "--- " (substring line 3 (string-length line)) " ---"))
                          ;; List items
                          ((string-prefix? "- " line) line)
                          ((string-prefix? "* " line) (string-append "- " (substring line 2 (string-length line))))
                          ;; Quote blocks
                          ((string-prefix? "> " line) (string-append "  | " (substring line 2 (string-length line))))
                          ;; Regular line
                          (else line))))
                    lines)
               "\n")))
      (qt-buffer-attach! ed preview-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) preview-buf)
      (qt-plain-text-edit-set-text! ed rendered)
      (qt-text-document-set-modified! (buffer-doc-pointer preview-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "Markdown preview"))))

;;; ============================================================================
;;; Snippet/template expansion system

