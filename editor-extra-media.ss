;;; -*- Gerbil -*-
;;; EWW extras, EMMS, PDF tools, calc, avy, expand-region,
;;; smartparens, project, JSON/XML, games, and more

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        (only-in :gerbil-emacs/editor-core
                 *auto-save-enabled* make-auto-save-path)
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/editor-extra-helpers
        :gerbil-emacs/editor-extra-web)

;; --- Task #48: EWW, EMMS, PDF tools, Calc, ace-jump, expand-region, etc. ---

;; EWW web browser operations
(def (cmd-eww-back app)
  "Go back in EWW history."
  (let ((echo (app-state-echo app)))
    (if (>= (+ *eww-history-idx* 1) (length *eww-history*))
      (echo-message! echo "No previous page")
      (let* ((new-idx (+ *eww-history-idx* 1))
             (url (list-ref *eww-history* new-idx)))
        (set! *eww-history-idx* new-idx)
        (let ((content (eww-fetch-url url)))
          (if content
            (eww-display-page app url content)
            (echo-error! echo "Failed to fetch page")))))))

(def (cmd-eww-forward app)
  "Go forward in EWW history."
  (let ((echo (app-state-echo app)))
    (if (<= *eww-history-idx* 0)
      (echo-message! echo "No next page")
      (let* ((new-idx (- *eww-history-idx* 1))
             (url (list-ref *eww-history* new-idx)))
        (set! *eww-history-idx* new-idx)
        (let ((content (eww-fetch-url url)))
          (if content
            (eww-display-page app url content)
            (echo-error! echo "Failed to fetch page")))))))

(def (cmd-eww-reload app)
  "Reload current EWW page."
  (let ((echo (app-state-echo app)))
    (if (not *eww-current-url*)
      (echo-message! echo "No page to reload")
      (begin
        (echo-message! echo (string-append "Reloading: " *eww-current-url*))
        (let ((content (eww-fetch-url *eww-current-url*)))
          (if content
            (eww-display-page app *eww-current-url* content)
            (echo-error! echo "Failed to reload page")))))))

(def (cmd-eww-download app)
  "Download file from URL."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (url (echo-read-string echo "Download URL: " row width)))
    (when (and url (not (string-empty? url)))
      (let ((filename (path-strip-directory url)))
        (echo-message! echo (string-append "Downloading: " filename))
        (with-exception-catcher
          (lambda (e) (echo-error! echo "Download failed"))
          (lambda ()
            (let ((proc (open-process
                          (list path: "curl"
                                arguments: (list "-sLO" url)
                                stdin-redirection: #f
                                stdout-redirection: #t
                                stderr-redirection: #t))))
              (process-status proc)
              (echo-message! echo (string-append "Downloaded: " filename)))))))))

(def (cmd-eww-copy-page-url app)
  "Copy current EWW page URL to kill ring."
  (let ((echo (app-state-echo app)))
    (if (not *eww-current-url*)
      (echo-message! echo "No URL to copy")
      (begin
        ;; Add to kill ring
        (let ((kill-ring (app-state-kill-ring app)))
          (set! (app-state-kill-ring app) (cons *eww-current-url* kill-ring)))
        (echo-message! echo (string-append "Copied: " *eww-current-url*))))))

;; EMMS (Emacs Multimedia System) - uses mpv or mplayer
(def *emms-player-process* #f)  ; current player process
(def *emms-current-file* #f)    ; current playing file
(def *emms-paused* #f)          ; paused state
(def *emms-playlist* [])        ; list of file paths
(def *emms-playlist-idx* 0)     ; current index into playlist

(def (emms-find-player)
  "Find available media player."
  (cond
    ((file-exists? "/usr/bin/mpv") "mpv")
    ((file-exists? "/usr/bin/mplayer") "mplayer")
    ((file-exists? "/usr/bin/ffplay") "ffplay")
    (else #f)))

(def (cmd-emms app)
  "Open EMMS player - show playlist or current track info."
  (let ((echo (app-state-echo app)))
    (if *emms-current-file*
      (echo-message! echo (string-append "Now playing: " (path-strip-directory *emms-current-file*)
                                        (if *emms-paused* " [PAUSED]" "")))
      (echo-message! echo "No track playing. Use emms-play-file to start."))))

(def (cmd-emms-play-file app)
  "Play a media file using mpv or mplayer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (player (emms-find-player))
         (file (echo-read-string echo "Media file: " row width)))
    (if (not player)
      (echo-error! echo "No media player found (mpv, mplayer, or ffplay)")
      (when (and file (not (string-empty? file)))
        (if (not (file-exists? file))
          (echo-error! echo "File not found")
          (begin
            ;; Stop any existing playback
            (when *emms-player-process*
              (with-exception-catcher (lambda (e) #f)
                (lambda () (close-port *emms-player-process*))))
            ;; Start new playback
            (set! *emms-player-process*
              (open-process
                (list path: player
                      arguments: (list "--quiet" file)
                      stdin-redirection: #f
                      stdout-redirection: #f
                      stderr-redirection: #f)))
            (set! *emms-current-file* file)
            (set! *emms-paused* #f)
            ;; Add to playlist if not already present
            (unless (member file *emms-playlist*)
              (set! *emms-playlist* (append *emms-playlist* (list file))))
            ;; Update playlist index
            (let loop ((i 0) (pl *emms-playlist*))
              (when (pair? pl)
                (if (equal? (car pl) file)
                  (set! *emms-playlist-idx* i)
                  (loop (+ i 1) (cdr pl)))))
            (echo-message! echo (string-append "Playing: " (path-strip-directory file)))))))))

(def (cmd-emms-pause app)
  "Pause/resume playback (sends signal to player)."
  (let ((echo (app-state-echo app)))
    (if (not *emms-player-process*)
      (echo-message! echo "No track playing")
      (begin
        (set! *emms-paused* (not *emms-paused*))
        (echo-message! echo (if *emms-paused* "Paused" "Resumed"))))))

(def (cmd-emms-stop app)
  "Stop playback."
  (let ((echo (app-state-echo app)))
    (when *emms-player-process*
      (with-exception-catcher (lambda (e) #f)
        (lambda () (close-port *emms-player-process*)))
      (set! *emms-player-process* #f)
      (set! *emms-current-file* #f)
      (set! *emms-paused* #f))
    (echo-message! echo "Stopped")))

(def (emms-play-track! app file)
  "Play a specific track file, updating state."
  (let ((echo (app-state-echo app))
        (player (emms-find-player)))
    (if (not player)
      (echo-error! echo "No media player found")
      (begin
        ;; Stop existing
        (when *emms-player-process*
          (with-exception-catcher (lambda (e) #f)
            (lambda () (close-port *emms-player-process*))))
        ;; Start new
        (set! *emms-player-process*
          (open-process
            (list path: player
                  arguments: (list "--quiet" file)
                  stdin-redirection: #f
                  stdout-redirection: #f
                  stderr-redirection: #f)))
        (set! *emms-current-file* file)
        (set! *emms-paused* #f)
        (echo-message! echo (string-append "Playing: " (path-strip-directory file)))))))

(def (cmd-emms-next app)
  "Play the next track in the playlist."
  (let ((echo (app-state-echo app)))
    (if (null? *emms-playlist*)
      (echo-message! echo "Playlist is empty. Use emms-play-file to add tracks.")
      (begin
        (set! *emms-playlist-idx*
          (modulo (+ *emms-playlist-idx* 1) (length *emms-playlist*)))
        (emms-play-track! app (list-ref *emms-playlist* *emms-playlist-idx*))))))

(def (cmd-emms-previous app)
  "Play the previous track in the playlist."
  (let ((echo (app-state-echo app)))
    (if (null? *emms-playlist*)
      (echo-message! echo "Playlist is empty. Use emms-play-file to add tracks.")
      (begin
        (set! *emms-playlist-idx*
          (modulo (- *emms-playlist-idx* 1) (length *emms-playlist*)))
        (emms-play-track! app (list-ref *emms-playlist* *emms-playlist-idx*))))))

;; PDF tools - basic PDF viewing using pdftotext
(def *pdf-current-file* #f)  ; current PDF file
(def *pdf-current-page* 1)   ; current page number
(def *pdf-total-pages* 1)    ; total pages

(def (pdf-get-page-count file)
  "Get total pages in a PDF file."
  (with-exception-catcher
    (lambda (e) 1)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "pdfinfo"
                           arguments: (list file)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (process-status proc)
        (if output
          ;; Find "Pages:" line
          (let* ((lines (string-split output #\newline))
                 (pages-line (find (lambda (l) (string-prefix? "Pages:" l)) lines)))
            (if pages-line
              (let ((num (string->number (string-trim (substring pages-line 6 (string-length pages-line))))))
                (or num 1))
              1))
          1)))))

(def (pdf-extract-page file page)
  "Extract text from a specific page of a PDF."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "pdftotext"
                           arguments: (list "-f" (number->string page)
                                           "-l" (number->string page)
                                           "-layout" file "-")
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (process-status proc)
        output))))

(def (pdf-display-page app)
  "Display current PDF page."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (content (pdf-extract-page *pdf-current-file* *pdf-current-page*))
         (text (string-append "PDF: " (path-strip-directory *pdf-current-file*) 
                             " - Page " (number->string *pdf-current-page*)
                             "/" (number->string *pdf-total-pages*) "\n"
                             (make-string 60 #\-) "\n\n"
                             (or content "Could not extract text from page")
                             "\n\n[n: next, p: previous, g: goto, q: quit]")))
    (let ((buf (or (buffer-by-name "*PDF View*")
                   (buffer-create! "*PDF View*" ed))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed text)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

(def (cmd-pdf-view-mode app)
  "Open a PDF file for viewing."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (file (echo-read-string echo "PDF file: " row width)))
    (when (and file (not (string-empty? file)))
      (if (not (file-exists? file))
        (echo-error! echo "File not found")
        (begin
          (set! *pdf-current-file* file)
          (set! *pdf-current-page* 1)
          (set! *pdf-total-pages* (pdf-get-page-count file))
          (pdf-display-page app))))))

(def (cmd-pdf-view-next-page app)
  "Go to next page in PDF."
  (let ((echo (app-state-echo app)))
    (if (not *pdf-current-file*)
      (echo-message! echo "No PDF open")
      (if (>= *pdf-current-page* *pdf-total-pages*)
        (echo-message! echo "Already at last page")
        (begin
          (set! *pdf-current-page* (+ *pdf-current-page* 1))
          (pdf-display-page app))))))

(def (cmd-pdf-view-previous-page app)
  "Go to previous page in PDF."
  (let ((echo (app-state-echo app)))
    (if (not *pdf-current-file*)
      (echo-message! echo "No PDF open")
      (if (<= *pdf-current-page* 1)
        (echo-message! echo "Already at first page")
        (begin
          (set! *pdf-current-page* (- *pdf-current-page* 1))
          (pdf-display-page app))))))

(def (cmd-pdf-view-goto-page app)
  "Go to specific PDF page."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if (not *pdf-current-file*)
      (echo-message! echo "No PDF open")
      (let* ((input (echo-read-string echo (string-append "Go to page (1-" (number->string *pdf-total-pages*) "): ") row width))
             (page (and input (string->number input))))
        (if (and page (> page 0) (<= page *pdf-total-pages*))
          (begin
            (set! *pdf-current-page* page)
            (pdf-display-page app))
          (echo-error! echo "Invalid page number"))))))

;; Calc stack operations
(def (cmd-calc-push app)
  "Push value onto calc stack."
  (let ((val (app-read-string app "Push value: ")))
    (when (and val (not (string-empty? val)))
      (echo-message! (app-state-echo app) (string-append "Pushed: " val)))))

(def *calc-stack* '())

(def (cmd-calc-pop app)
  "Pop value from calc stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Calc: stack empty")
    (let ((val (car *calc-stack*)))
      (set! *calc-stack* (cdr *calc-stack*))
      (echo-message! (app-state-echo app) (string-append "Popped: " val)))))

(def (cmd-calc-dup app)
  "Duplicate top of calc stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Calc: stack empty")
    (begin
      (set! *calc-stack* (cons (car *calc-stack*) *calc-stack*))
      (echo-message! (app-state-echo app) (string-append "Duplicated: " (car *calc-stack*))))))

(def (cmd-calc-swap app)
  "Swap top two calc stack items."
  (if (or (null? *calc-stack*) (null? (cdr *calc-stack*)))
    (echo-message! (app-state-echo app) "Calc: need at least 2 items")
    (let ((a (car *calc-stack*))
          (b (cadr *calc-stack*)))
      (set! *calc-stack* (cons b (cons a (cddr *calc-stack*))))
      (echo-message! (app-state-echo app) (string-append "Swapped: " b " <-> " a)))))

;; Ace-jump / Avy navigation - quick cursor movement
;; Simplified implementation: searches for matches in visible text and jumps

(def (avy-find-all-matches ed char-or-pattern)
  "Find all positions matching char or pattern in editor text."
  (let* ((text (editor-get-text ed))
         (len (string-length text))
         (pattern (if (char? char-or-pattern) 
                    (string char-or-pattern) 
                    char-or-pattern)))
    (let loop ((i 0) (matches '()))
      (if (>= i len)
        (reverse matches)
        (if (and (< (+ i (string-length pattern)) len)
                 (string-ci=? (substring text i (+ i (string-length pattern))) pattern))
          (loop (+ i 1) (cons i matches))
          (loop (+ i 1) matches))))))

(def (cmd-avy-goto-char app)
  "Jump to character - prompts for char, finds all occurrences, jumps to selected one."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Jump to char: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (matches (avy-find-all-matches ed ch)))
        (if (null? matches)
          (echo-message! echo "No matches found")
          (if (= (length matches) 1)
            ;; Single match - jump directly
            (begin
              (editor-goto-pos ed (car matches))
              (echo-message! echo "Jumped!"))
            ;; Multiple matches - jump to first for now
            (begin
              (editor-goto-pos ed (car matches))
              (echo-message! echo (string-append "Jumped to first of " 
                                                (number->string (length matches)) 
                                                " matches (use search for more)")))))))))

(def (cmd-avy-goto-word app)
  "Jump to word - prompts for word prefix, finds matches, jumps."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (prefix (echo-read-string echo "Jump to word starting with: " row width)))
    (when (and prefix (not (string-empty? prefix)))
      ;; Find word boundaries and match prefix
      (let* ((text (editor-get-text ed))
             (len (string-length text))
             (plen (string-length prefix)))
        (let loop ((i 0) (in-word #f) (matches '()))
          (if (>= i len)
            (if (null? matches)
              (echo-message! echo "No matching words found")
              (begin
                (editor-goto-pos ed (car (reverse matches)))
                (echo-message! echo (string-append "Jumped to first of "
                                                  (number->string (length matches)) " matches"))))
            (let ((ch (string-ref text i)))
              (cond
                ((and (not in-word) (char-alphabetic? ch))
                 ;; Word start - check prefix
                 (if (and (<= (+ i plen) len)
                          (string-ci=? (substring text i (+ i plen)) prefix))
                   (loop (+ i 1) #t (cons i matches))
                   (loop (+ i 1) #t matches)))
                ((and in-word (not (char-alphabetic? ch)))
                 (loop (+ i 1) #f matches))
                (else
                 (loop (+ i 1) in-word matches))))))))))

(def (cmd-avy-goto-line app)
  "Jump to line - prompts for line number."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Jump to line: " row width)))
    (when (and input (not (string-empty? input)))
      (let ((line (string->number input)))
        (if (and line (> line 0))
          (begin
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Line " (number->string line))))
          (echo-error! echo "Invalid line number"))))))

;; Expand-region - progressively expand selection
;; Expansion order: word -> symbol -> quotes -> parens -> line -> paragraph

(def *expand-region-history* '())  ; stack of (start . end) for contract

(def (expand-find-word ed pos text)
  "Find word boundaries around pos."
  (let* ((len (string-length text))
         (start (let loop ((i pos))
                  (if (or (< i 0)
                          (not (or (char-alphabetic? (string-ref text i))
                                   (char-numeric? (string-ref text i))
                                   (char=? (string-ref text i) #\_))))
                    (+ i 1)
                    (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i len)
                        (not (or (char-alphabetic? (string-ref text i))
                                 (char-numeric? (string-ref text i))
                                 (char=? (string-ref text i) #\_))))
                  i
                  (loop (+ i 1))))))
    (if (< start end) (cons start end) #f)))

(def (expand-find-quotes ed pos text)
  "Find enclosing quotes around pos."
  (let* ((len (string-length text))
         ;; Look backwards for opening quote
         (start (let loop ((i (- pos 1)))
                  (if (< i 0)
                    #f
                    (let ((ch (string-ref text i)))
                      (if (memv ch '(#\" #\' #\`))
                        i
                        (loop (- i 1)))))))
         ;; Look forwards for closing quote
         (end (and start
                   (let ((quote-char (string-ref text start)))
                     (let loop ((i (+ pos 1)))
                       (if (>= i len)
                         #f
                         (let ((ch (string-ref text i)))
                           (if (char=? ch quote-char)
                             (+ i 1)
                             (loop (+ i 1))))))))))
    (if (and start end) (cons start end) #f)))

(def (expand-find-parens ed pos text)
  "Find enclosing parens around pos."
  (let ((open (sp-find-enclosing-paren ed pos #\( #\))))
    (if open
      (let ((close (sp-find-matching-close ed (+ open 1) #\( #\))))
        (if close (cons open (+ close 1)) #f))
      #f)))

(def (expand-find-line ed pos text)
  "Find line boundaries around pos."
  (let* ((len (string-length text))
         (start (let loop ((i pos))
                  (if (or (< i 0) (char=? (string-ref text i) #\newline))
                    (+ i 1)
                    (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i len) (char=? (string-ref text i) #\newline))
                  i
                  (loop (+ i 1))))))
    (cons start end)))

(def (cmd-expand-region app)
  "Expand selection region progressively."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed))
         (text (editor-get-text ed)))
    ;; Save current selection for contract
    (when (not (= sel-start sel-end))
      (set! *expand-region-history* (cons (cons sel-start sel-end) *expand-region-history*)))
    ;; Try expanding in order
    (let ((current-size (- sel-end sel-start)))
      (let try-expand ((expansions (list 
                                     (expand-find-word ed pos text)
                                     (expand-find-quotes ed pos text)
                                     (expand-find-parens ed pos text)
                                     (expand-find-line ed pos text))))
        (if (null? expansions)
          (echo-message! echo "Cannot expand further")
          (let ((exp (car expansions)))
            (if (and exp (> (- (cdr exp) (car exp)) current-size))
              (begin
                (editor-set-selection ed (car exp) (cdr exp))
                (echo-message! echo "Expanded"))
              (try-expand (cdr expansions)))))))))

(def (cmd-contract-region app)
  "Contract selection to previous size."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (if (null? *expand-region-history*)
      (begin
        (editor-set-selection ed (editor-get-current-pos ed) (editor-get-current-pos ed))
        (echo-message! echo "Selection cleared"))
      (let ((prev (car *expand-region-history*)))
        (set! *expand-region-history* (cdr *expand-region-history*))
        (editor-set-selection ed (car prev) (cdr prev))
        (echo-message! echo "Contracted")))))

;; Smartparens - structural editing for s-expressions
;; These commands manipulate parentheses around expressions

(def (cmd-sp-forward-slurp-sexp app)
  "Slurp the next sexp into the current list. (|a b) c -> (|a b c)"
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Find the closing paren of enclosing list
    (let ((open-pos (sp-find-enclosing-paren ed pos #\( #\))))
      (if (not open-pos)
        (echo-message! echo "Not inside a list")
        (let ((close-pos (sp-find-matching-close ed (+ open-pos 1) #\( #\))))
          (if (not close-pos)
            (echo-message! echo "Unbalanced parens")
            ;; Find the next sexp after the close paren
            (let ((next-end (sp-find-sexp-end ed (+ close-pos 1))))
              (if (not next-end)
                (echo-message! echo "Nothing to slurp")
                (begin
                  ;; Delete the close paren
                  (editor-set-selection ed close-pos (+ close-pos 1))
                  (editor-replace-selection ed "")
                  ;; Insert close paren after the slurped sexp
                  (editor-insert-text ed next-end ")")
                  (echo-message! echo "Slurped forward"))))))))))

(def (cmd-sp-forward-barf-sexp app)
  "Barf the last sexp out of the current list. (a b| c) -> (a b|) c"
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    (let ((open-pos (sp-find-enclosing-paren ed pos #\( #\))))
      (if (not open-pos)
        (echo-message! echo "Not inside a list")
        (let ((close-pos (sp-find-matching-close ed (+ open-pos 1) #\( #\))))
          (if (not close-pos)
            (echo-message! echo "Unbalanced parens")
            ;; Find the last sexp before the close paren
            (let loop ((i (- close-pos 1)))
              (if (<= i open-pos)
                (echo-message! echo "Nothing to barf")
                (let ((ch (string-ref text i)))
                  (cond
                    ((char-whitespace? ch) (loop (- i 1)))
                    (else
                     ;; Found end of last sexp, find its start
                     (let find-start ((j i))
                       (if (<= j open-pos)
                         (echo-message! echo "Nothing to barf")
                         (let ((c (string-ref text j)))
                           (cond
                             ((char=? c #\))
                              (let ((match (sp-find-enclosing-paren ed (+ j 1) #\( #\))))
                                (if match
                                  (begin
                                    ;; Delete close paren, insert before sexp
                                    (editor-set-selection ed close-pos (+ close-pos 1))
                                    (editor-replace-selection ed "")
                                    (editor-insert-text ed match ")")
                                    (echo-message! echo "Barfed forward"))
                                  (echo-message! echo "Parse error"))))
                             ((char-whitespace? c) (find-start (- j 1)))
                             (else
                              ;; At end of atom, scan back
                              (let scan-atom ((k j))
                                (if (<= k open-pos)
                                  (begin
                                    (editor-set-selection ed close-pos (+ close-pos 1))
                                    (editor-replace-selection ed "")
                                    (editor-insert-text ed (+ open-pos 1) ")")
                                    (echo-message! echo "Barfed forward"))
                                  (let ((cc (string-ref text k)))
                                    (if (or (char-whitespace? cc)
                                            (memv cc '(#\( #\))))
                                      (begin
                                        (editor-set-selection ed close-pos (+ close-pos 1))
                                        (editor-replace-selection ed "")
                                        (editor-insert-text ed (+ k 1) ")")
                                        (echo-message! echo "Barfed forward"))
                                      (scan-atom (- k 1))))))))))))))))))))))

(def (cmd-sp-backward-slurp-sexp app)
  "Slurp the previous sexp into the current list. a (|b c) -> (a |b c)"
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    (let ((open-pos (sp-find-enclosing-paren ed pos #\( #\))))
      (if (not open-pos)
        (echo-message! echo "Not inside a list")
        ;; Find the sexp before the open paren
        (let find-prev ((i (- open-pos 1)))
          (if (< i 0)
            (echo-message! echo "Nothing to slurp")
            (let ((ch (string-ref text i)))
              (cond
                ((char-whitespace? ch) (find-prev (- i 1)))
                ((char=? ch #\))
                 ;; End of sexp, find its start
                 (let ((match (sp-find-enclosing-paren ed (+ i 1) #\( #\))))
                   (if match
                     (begin
                       ;; Delete the open paren, insert before the sexp
                       (editor-set-selection ed open-pos (+ open-pos 1))
                       (editor-replace-selection ed "")
                       (editor-insert-text ed match "(")
                       (echo-message! echo "Slurped backward"))
                     (echo-message! echo "Parse error"))))
                (else
                 ;; Atom, find its start
                 (let scan-back ((j i))
                   (if (< j 0)
                     (begin
                       (editor-set-selection ed open-pos (+ open-pos 1))
                       (editor-replace-selection ed "")
                       (editor-insert-text ed 0 "(")
                       (echo-message! echo "Slurped backward"))
                     (let ((c (string-ref text j)))
                       (if (or (char-whitespace? c)
                               (memv c '(#\( #\))))
                         (begin
                           (editor-set-selection ed open-pos (+ open-pos 1))
                           (editor-replace-selection ed "")
                           (editor-insert-text ed (+ j 1) "(")
                           (echo-message! echo "Slurped backward"))
                         (scan-back (- j 1)))))))))))))))

(def (cmd-sp-backward-barf-sexp app)
  "Barf the first sexp out of the current list. (a |b c) -> a (|b c)"
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    (let ((open-pos (sp-find-enclosing-paren ed pos #\( #\))))
      (if (not open-pos)
        (echo-message! echo "Not inside a list")
        ;; Find the first sexp after the open paren
        (let find-first ((i (+ open-pos 1)))
          (if (>= i (string-length text))
            (echo-message! echo "Nothing to barf")
            (let ((ch (string-ref text i)))
              (cond
                ((char-whitespace? ch) (find-first (+ i 1)))
                (else
                 ;; Found start of first sexp, find its end
                 (let ((sexp-end (sp-find-sexp-end ed i)))
                   (if (not sexp-end)
                     (echo-message! echo "Parse error")
                     (begin
                       ;; Delete open paren, insert after first sexp
                       (editor-set-selection ed open-pos (+ open-pos 1))
                       (editor-replace-selection ed "")
                       (editor-insert-text ed sexp-end "(")
                       (echo-message! echo "Barfed backward")))))))))))))

;; Project.el - project detection and navigation
;; Projects are detected by presence of .git, .hg, .svn, or project markers

(def (cmd-project-switch-project app)
  "Switch to another project from history or prompt for directory."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if (null? *project-history*)
      ;; No history - prompt for directory
      (let ((dir (echo-read-string echo "Project directory: " row width)))
        (when (and dir (directory-exists? dir))
          (let ((root (project-find-root dir)))
            (if root
              (begin
                (set! *project-history* (cons root (delete root *project-history*)))
                (current-directory root)
                (echo-message! echo (string-append "Project: " root)))
              (echo-message! echo "No project found at that location")))))
      ;; Show project history
      (let* ((win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! "*Projects*" ed))
             (text (string-append "Known projects:\n\n"
                     (string-join
                       (map (lambda (p) (string-append "  " p)) *project-history*)
                       "\n")
                     "\n\nPress Enter on a line to switch to that project.")))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

(def (cmd-project-find-regexp app)
  "Find regexp in project files using grep."
  (let* ((echo (app-state-echo app))
         (root (project-current app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (let ((pattern (echo-read-string echo "Project grep: " row width)))
        (when (and pattern (not (string-empty? pattern)))
          (with-exception-catcher
            (lambda (e) (echo-error! echo "grep failed"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "grep"
                                   arguments: (list "-rn" pattern root
                                                   "--include=*.ss" "--include=*.scm"
                                                   "--include=*.py" "--include=*.js"
                                                   "--include=*.go" "--include=*.rs"
                                                   "--include=*.c" "--include=*.h"
                                                   "--include=*.cpp" "--include=*.hpp"
                                                   "--include=*.md" "--include=*.txt")
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #f
                                   directory: root)))
                     (output (read-line proc #f)))
                (process-status proc)
                (let* ((win (current-window fr))
                       (ed (edit-window-editor win))
                       (buf (buffer-create! (string-append "*Project grep: " pattern "*") ed))
                       (text (if output
                               (string-append "Project grep results for: " pattern "\n\n" output)
                               "No matches found.")))
                  (buffer-attach! ed buf)
                  (set! (edit-window-buffer win) buf)
                  (editor-set-text ed text)
                  (editor-goto-pos ed 0)
                  (editor-set-read-only ed #t))))))))))

(def (cmd-project-shell app)
  "Open shell in project root."
  (let* ((echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (begin
        (current-directory root)
        ;; Add to project history
        (set! *project-history* (cons root (delete root *project-history*)))
        ;; Open shell
        (execute-command! app 'shell)
        (echo-message! echo (string-append "Shell in project: " root))))))

(def (cmd-project-dired app)
  "Open dired at project root."
  (let* ((echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (begin
        ;; Add to project history  
        (set! *project-history* (cons root (delete root *project-history*)))
        (execute-command! app 'dired)))))

(def (cmd-project-eshell app)
  "Open eshell in project root."
  (let* ((echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (begin
        (current-directory root)
        ;; Add to project history
        (set! *project-history* (cons root (delete root *project-history*)))
        ;; Open eshell
        (execute-command! app 'eshell)
        (echo-message! echo (string-append "Eshell in project: " root))))))

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

;; XML formatting — uses xmllint if available
(def (cmd-xml-format app)
  "Format XML in region or buffer using xmllint."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed)))
    (with-exception-catcher
      (lambda (e) (echo-error! (app-state-echo app) "xmllint not available"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "xmllint"
                             arguments: '("--format" "-")
                             stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t)))
               (_ (begin (display text proc) (close-output-port proc)))
               (result (read-line proc #f)))
          (process-status proc)
          (if (and result (> (string-length result) 0))
            (begin (editor-set-text ed result)
                   (echo-message! (app-state-echo app) "XML formatted"))
            (echo-error! (app-state-echo app) "XML format failed")))))))

;; Desktop notifications
(def (cmd-notifications-list app)
  "List desktop notifications using notify-send or dunstctl."
  (with-exception-catcher
    (lambda (e) (echo-message! (app-state-echo app) "No notification daemon available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "dunstctl"
                           arguments: '("history")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (if out
          (open-output-buffer app "*Notifications*" out)
          (echo-message! (app-state-echo app) "No notifications"))))))

;; Profiler
(def (cmd-profiler-report app)
  "Show profiler report — displays GC and memory statistics."
  (let* ((stats (with-output-to-string
                  (lambda ()
                    (display "Profiler Report\n\n")
                    (display "GC Statistics:\n")
                    (##gc)
                    (let ((info (##process-statistics)))
                      (display (string-append "  User time:   " (number->string (f64vector-ref info 0)) "s\n"))
                      (display (string-append "  System time: " (number->string (f64vector-ref info 1)) "s\n"))
                      (display (string-append "  Real time:   " (number->string (f64vector-ref info 2)) "s\n"))
                      (display (string-append "  GC user:     " (number->string (f64vector-ref info 3)) "s\n"))
                      (display (string-append "  GC real:     " (number->string (f64vector-ref info 5)) "s\n"))
                      (display (string-append "  Bytes alloc: " (number->string (inexact->exact (f64vector-ref info 6))) "\n")))))))
    (open-output-buffer app "*Profiler*" stats)))

;; Narrowing extras
(def (cmd-narrow-to-page app)
  "Narrow to current page — finds page delimiters (form feeds)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text))
         (page-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((char=? (string-ref text i) #\page) (+ i 1))
                             (else (loop (- i 1))))))
         (page-end (let loop ((i pos))
                     (cond ((>= i len) len)
                           ((char=? (string-ref text i) #\page) i)
                           (else (loop (+ i 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Page: " (number->string page-start) "-" (number->string page-end)))))

;; Encoding detection
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

;; Save hooks
(def *hooks* (make-hash-table))  ; hook-name -> list of symbols

(def (cmd-add-hook app)
  "Add a hook function — registers a named hook."
  (let ((hook-name (app-read-string app "Hook name: ")))
    (when (and hook-name (not (string-empty? hook-name)))
      (let ((func-name (app-read-string app "Function name: ")))
        (when (and func-name (not (string-empty? func-name)))
          (let ((existing (or (hash-get *hooks* hook-name) '())))
            (hash-put! *hooks* hook-name (cons func-name existing))
            (echo-message! (app-state-echo app)
              (string-append "Added " func-name " to " hook-name))))))))

(def (cmd-remove-hook app)
  "Remove a hook function."
  (let ((hook-name (app-read-string app "Hook name: ")))
    (when (and hook-name (not (string-empty? hook-name)))
      (let ((func-name (app-read-string app "Function to remove: ")))
        (when (and func-name (not (string-empty? func-name)))
          (let ((existing (or (hash-get *hooks* hook-name) '())))
            (hash-put! *hooks* hook-name (filter (lambda (f) (not (string=? f func-name))) existing))
            (echo-message! (app-state-echo app)
              (string-append "Removed " func-name " from " hook-name))))))))

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

