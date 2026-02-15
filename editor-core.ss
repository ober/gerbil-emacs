;;; -*- Gerbil -*-
;;; Core editor commands: accessors, self-insert, navigation,
;;; editing, kill/yank, mark/region, files, windows, search, shell

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/text/hex
        :std/crypto/digest
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/highlight
        :gerbil-emacs/persist)

;;;============================================================================
;;; Shared state (used across editor sub-modules)
;;;============================================================================
(def *auto-pair-mode* #t)

;;;============================================================================
;;; Pulse/flash highlight on jump (beacon-like)
;;;============================================================================
;; Uses indicator #1 (indicator #0 is for search highlights)
(def *pulse-indicator* 1)
(def *pulse-countdown* 0)   ; ticks remaining before clearing indicator
(def *pulse-editor* #f)     ; editor that has the active pulse

(def (pulse-line! ed line-num)
  "Flash-highlight the given line number temporarily.
   The highlight is cleared after ~500ms (10 ticks at 50ms poll)."
  (let* ((start (editor-position-from-line ed line-num))
         (end (editor-get-line-end-position ed line-num))
         (len (- end start)))
    (when (> len 0)
      ;; Clear any previous pulse
      (when *pulse-editor*
        (pulse-clear! *pulse-editor*))
      ;; Set up indicator style: INDIC_FULLBOX with yellow/gold color
      (send-message ed SCI_INDICSETSTYLE *pulse-indicator* INDIC_FULLBOX)
      (send-message ed SCI_INDICSETFORE *pulse-indicator* #x00A5FF) ; golden/orange
      (send-message ed SCI_SETINDICATORCURRENT *pulse-indicator* 0)
      (send-message ed SCI_INDICATORFILLRANGE start len)
      (set! *pulse-editor* ed)
      (set! *pulse-countdown* 10))))  ; 10 * 50ms = 500ms

(def (pulse-tick!)
  "Called each main loop iteration. Decrements pulse countdown and clears when done."
  (when (> *pulse-countdown* 0)
    (set! *pulse-countdown* (- *pulse-countdown* 1))
    (when (= *pulse-countdown* 0)
      (when *pulse-editor*
        (pulse-clear! *pulse-editor*)))))

(def (pulse-clear! ed)
  "Remove the pulse indicator from the editor."
  (let ((len (editor-get-text-length ed)))
    (send-message ed SCI_SETINDICATORCURRENT *pulse-indicator* 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len))
  (when (eq? *pulse-editor* ed)
    (set! *pulse-editor* #f)
    (set! *pulse-countdown* 0)))

;;;============================================================================
;;; System clipboard integration (xclip/xsel/wl-copy)
;;;============================================================================

(def *clipboard-command* #f)  ; cached clipboard command, or 'none

(def (find-clipboard-command!)
  "Detect available clipboard command. Caches result."
  (unless *clipboard-command*
    (set! *clipboard-command*
      (cond
        ((file-exists? "/usr/bin/wl-copy") 'wl-copy)     ; Wayland
        ((file-exists? "/usr/bin/xclip") 'xclip)         ; X11
        ((file-exists? "/usr/bin/xsel") 'xsel)            ; X11 alt
        (else 'none))))
  *clipboard-command*)

(def (clipboard-set! text)
  "Copy text to system clipboard if a clipboard tool is available."
  (let ((cmd (find-clipboard-command!)))
    (unless (eq? cmd 'none)
      (with-catch
        (lambda (e) #f)  ; silently ignore clipboard errors
        (lambda ()
          (let ((args (case cmd
                        ((wl-copy) '("wl-copy"))
                        ((xclip)  '("xclip" "-selection" "clipboard"))
                        ((xsel)   '("xsel" "--clipboard" "--input")))))
            (let ((proc (open-process
                          (list path: (car args)
                                arguments: (cdr args)
                                stdin-redirection: #t
                                stdout-redirection: #f
                                stderr-redirection: #f))))
              (display text proc)
              (close-output-port proc)
              (process-status proc))))))))

(def (clipboard-get)
  "Get text from system clipboard. Returns string or #f."
  (let ((cmd (find-clipboard-command!)))
    (if (eq? cmd 'none)
      #f
      (with-catch
        (lambda (e) #f)
        (lambda ()
          (let ((args (case cmd
                        ((wl-copy) '("wl-paste" "--no-newline"))
                        ((xclip)  '("xclip" "-selection" "clipboard" "-o"))
                        ((xsel)   '("xsel" "--clipboard" "--output")))))
            (let ((proc (open-process
                          (list path: (car args)
                                arguments: (cdr args)
                                stdin-redirection: #f
                                stdout-redirection: #t
                                stderr-redirection: #f))))
              (let ((text (read-line proc #f)))
                (close-input-port proc)
                (process-status proc)
                text))))))))

;;;============================================================================
;;; Uniquify buffer name helper
;;;============================================================================

(def (uniquify-buffer-name path)
  "Generate a unique buffer name for a file path by adding parent dir when needed."
  (let* ((basename (path-strip-directory path))
         (existing (filter (lambda (b)
                             (and (buffer-file-path b)
                                  (not (string=? (buffer-file-path b) path))
                                  (string=? (path-strip-directory (buffer-file-path b))
                                            basename)))
                           (buffer-list))))
    (if (null? existing)
      basename
      (let ((parent (path-strip-directory
                      (path-strip-trailing-directory-separator
                        (path-directory path)))))
        (string-append basename "<" parent ">")))))

;;;============================================================================
;;; Line ending detection
;;;============================================================================

(def (detect-eol-mode text)
  "Detect line ending mode from text content. Returns SC_EOL_* constant."
  (let loop ((i 0))
    (if (>= i (string-length text))
      SC_EOL_LF  ;; default
      (let ((ch (string-ref text i)))
        (cond
          ((char=? ch #\return)
           (if (and (< (+ i 1) (string-length text))
                    (char=? (string-ref text (+ i 1)) #\newline))
             SC_EOL_CRLF
             SC_EOL_CR))
          ((char=? ch #\newline) SC_EOL_LF)
          (else (loop (+ i 1))))))))

;;;============================================================================
;;; File modification tracking and auto-save
;;;============================================================================

;; Maps buffer → last-known file modification time (seconds since epoch)
(def *buffer-mod-times* (make-hash-table))

;; Auto-save enabled flag and interval counter
(def *auto-save-enabled* #t)
(def *auto-save-counter* 0)
(def *auto-save-interval* 600) ;; ~30 seconds at 50ms poll rate

(def (file-mod-time path)
  "Get file modification time as seconds, or #f if file doesn't exist."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (time->seconds (file-info-last-modification-time (file-info path))))))

(def (update-buffer-mod-time! buf)
  "Record the current file modification time for a buffer."
  (let ((path (buffer-file-path buf)))
    (when path
      (let ((mt (file-mod-time path)))
        (when mt
          (hash-put! *buffer-mod-times* buf mt))))))

(def (auto-save-buffers! app)
  "Write auto-save files (#name#) for modified file-visiting buffers."
  (when *auto-save-enabled*
    (for-each
      (lambda (buf)
        (let ((path (buffer-file-path buf)))
          (when path
            ;; Find a window showing this buffer to check if modified
            (let loop ((wins (frame-windows (app-state-frame app))))
              (when (pair? wins)
                (if (eq? (edit-window-buffer (car wins)) buf)
                  (let ((ed (edit-window-editor (car wins))))
                    (when (editor-get-modify? ed)
                      (let ((auto-path (make-auto-save-path path)))
                        (with-catch
                          (lambda (e) #f)
                          (lambda ()
                            (let ((text (editor-get-text ed)))
                              (write-string-to-file auto-path text)))))))
                  (loop (cdr wins))))))))
      (buffer-list))))

(def (check-file-modifications! app)
  "Check if any file-visiting buffers have been modified externally.
   Warns in the echo area if a file changed on disk."
  (for-each
    (lambda (buf)
      (let ((path (buffer-file-path buf)))
        (when path
          (let ((saved-mt (hash-get *buffer-mod-times* buf))
                (current-mt (file-mod-time path)))
            (when (and saved-mt current-mt (> current-mt saved-mt))
              ;; File changed on disk — update recorded time and warn
              (hash-put! *buffer-mod-times* buf current-mt)
              (echo-message! (app-state-echo app)
                (string-append (buffer-name buf) " changed on disk; revert with C-x C-r")))))))
    (buffer-list)))

;;;============================================================================
;;; Accessors
;;;============================================================================

(def (current-editor app)
  (edit-window-editor (current-window (app-state-frame app))))

(def (current-buffer-from-app app)
  (edit-window-buffer (current-window (app-state-frame app))))

(def (app-read-string app prompt)
  "Convenience wrapper: read a string from the echo area."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (echo-read-string echo prompt row width)))

(def (editor-replace-selection ed text)
  "Replace the current selection with text. SCI_REPLACESEL=2170."
  (send-message/string ed 2170 text))

;; Auto-save path: #filename# (Emacs convention)
(def (make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

;;;============================================================================
;;; Self-insert command
;;;============================================================================

;; Auto-pair matching characters
(def (auto-pair-char ch)
  "Return the closing character for auto-pairing, or #f."
  (cond
    ((= ch 40) 41)   ; ( -> )
    ((= ch 91) 93)   ; [ -> ]
    ((= ch 34) 34)   ; " -> "
    (else #f)))

(def (cmd-self-insert! app ch)
  ;; Clear search highlights on any text insertion
  (clear-search-highlights! (current-editor app))
  (let ((buf (current-buffer-from-app app)))
    (cond
      ;; Suppress self-insert in dired buffers
      ((dired-buffer? buf) (void))
      ;; In REPL buffers, only allow typing after the prompt
      ((repl-buffer? buf)
       (let* ((ed (current-editor app))
              (pos (editor-get-current-pos ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (editor-send-key ed ch))))
      ;; Eshell: allow typing after the last prompt
      ((eshell-buffer? buf)
       (editor-send-key (current-editor app) ch))
      ;; Shell: allow typing after the prompt position
      ((shell-buffer? buf)
       (let* ((ed (current-editor app))
              (pos (editor-get-current-pos ed))
              (ss (hash-get *shell-state* buf)))
         (when (and ss (>= pos (shell-state-prompt-pos ss)))
           (editor-send-key ed ch))))
      ;; Terminal: forward character directly to PTY
      ((terminal-buffer? buf)
       (let ((ts (hash-get *terminal-state* buf)))
         (when ts
           (terminal-send-raw! ts (string (integer->char ch))))))
      (else
       (let* ((ed (current-editor app))
              (close-ch (and *auto-pair-mode* (auto-pair-char ch)))
              (n (get-prefix-arg app))) ; Get prefix arg

         (if (and close-ch (= n 1)) ; Only auto-pair if n=1
           ;; Auto-pair: insert both chars and place cursor between
           (let ((pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos
               (string (integer->char ch) (integer->char close-ch)))
             (editor-goto-pos ed (+ pos 1)))
           ;; Insert character n times
           (let* ((pos (editor-get-current-pos ed))
                  (str (make-string n (integer->char ch))))
             (editor-insert-text ed pos str)
             (editor-goto-pos ed (+ pos n)))))))))

;;;============================================================================
;;; Navigation commands
;;;============================================================================

(def (cmd-forward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_RIGHT) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_LEFT) (loop (+ i 1)))))))

(def (cmd-backward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_LEFT) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_RIGHT) (loop (+ i 1)))))))

(def (cmd-next-line app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_DOWN) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_UP) (loop (+ i 1)))))))

(def (cmd-previous-line app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_UP) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_DOWN) (loop (+ i 1)))))))

(def (cmd-beginning-of-line app)
  "Smart beginning of line: toggle between first non-whitespace and column 0."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         ;; Find first non-whitespace character on the line
         (indent-pos
           (let loop ((p line-start))
             (if (>= p line-end)
               line-start  ; all whitespace line -> go to start
               (let ((ch (send-message ed SCI_GETCHARAT p 0)))
                 (if (or (= ch 32) (= ch 9))  ; space or tab
                   (loop (+ p 1))
                   p))))))
    ;; If already at indentation, go to column 0; otherwise go to indentation
    (if (= pos indent-pos)
      (editor-goto-pos ed line-start)
      (editor-goto-pos ed indent-pos))))

(def (cmd-end-of-line app)
  (editor-send-key (current-editor app) SCK_END))

(def (cmd-forward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_RIGHT ctrl: #t) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_LEFT ctrl: #t) (loop (+ i 1)))))))

(def (cmd-backward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-editor app)))
    (if (>= n 0)
      (let loop ((i 0)) (when (< i n) (editor-send-key ed SCK_LEFT ctrl: #t) (loop (+ i 1))))
      (let loop ((i 0)) (when (< i (- n)) (editor-send-key ed SCK_RIGHT ctrl: #t) (loop (+ i 1)))))))

(def (cmd-beginning-of-buffer app)
  (editor-send-key (current-editor app) SCK_HOME ctrl: #t))

(def (cmd-end-of-buffer app)
  (editor-send-key (current-editor app) SCK_END ctrl: #t))

(def (cmd-scroll-down app)
  (editor-send-key (current-editor app) SCK_NEXT))

(def (cmd-scroll-up app)
  (editor-send-key (current-editor app) SCK_PRIOR))

(def (cmd-recenter app)
  "Center the current line on screen (C-l behavior)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (fr (app-state-frame app))
         (win (current-window fr))
         ;; Window height minus modeline = visible lines
         (visible-lines (max 1 (- (edit-window-h win) 1)))
         ;; Target: place current line at center of screen
         (target-first (max 0 (- cur-line (quotient visible-lines 2)))))
    (send-message ed SCI_SETFIRSTVISIBLELINE target-first 0)))

;;;============================================================================
;;; Editing commands
;;;============================================================================

(def (cmd-delete-char app)
  (editor-send-key (current-editor app) SCK_DELETE))

(def (cmd-backward-delete-char app)
  (let ((buf (current-buffer-from-app app)))
    (cond
      ;; In REPL buffers, don't delete past the prompt
      ((repl-buffer? buf)
       (let* ((ed (current-editor app))
              (pos (editor-get-current-pos ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (> pos (repl-state-prompt-pos rs)))
           (editor-send-key ed SCK_BACK))))
      ;; Terminal: send backspace to PTY
      ((terminal-buffer? buf)
       (let ((ts (hash-get *terminal-state* buf)))
         (when ts
           (terminal-send-raw! ts "\x7f;"))))  ; DEL character
      (else
       (editor-send-key (current-editor app) SCK_BACK)))))

(def (cmd-backward-delete-char-untabify app)
  "Delete backward, converting tabs to spaces if in leading whitespace."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (when (> pos 0)
      (let* ((line (editor-line-from-position ed pos))
             (line-start (editor-position-from-line ed line))
             (col (- pos line-start))
             (ch-before (send-message ed SCI_GETCHARAT (- pos 1) 0)))
        ;; If char before cursor is a tab and we're in leading whitespace
        (if (and (= ch-before 9) ;; tab
                 (let loop ((p line-start))
                   (or (>= p pos)
                       (let ((c (send-message ed SCI_GETCHARAT p 0)))
                         (and (or (= c 32) (= c 9))
                              (loop (+ p 1)))))))
          ;; Delete the tab character
          (begin
            (editor-send-key ed SCK_BACK))
          ;; Normal backspace
          (editor-send-key ed SCK_BACK))))))

(def (get-line-indent text line-start)
  "Count leading whitespace chars starting at line-start in text."
  (let ((len (string-length text)))
    (let loop ((i line-start) (count 0))
      (if (>= i len) count
        (let ((ch (string-ref text i)))
          (cond
            ((char=? ch #\space) (loop (+ i 1) (+ count 1)))
            ((char=? ch #\tab) (loop (+ i 1) (+ count 2)))
            (else count)))))))

(def (buffer-list-buffer? buf)
  "Check if this buffer is a *Buffer List* buffer."
  (eq? (buffer-lexer-lang buf) 'buffer-list))

(def (cmd-buffer-list-select app)
  "Switch to the buffer named on the current line in *Buffer List*."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-text (editor-get-line ed line))
         ;; Lines are "  BufferName\t\tPath" — strip leading spaces, take up to first tab
         (trimmed (string-trim line-text))
         (tab-pos (string-index trimmed #\tab))
         (name (if tab-pos (substring trimmed 0 tab-pos) trimmed)))
    (if (and (> (string-length name) 0)
             (not (string=? name "Buffer"))   ;; skip header line
             (not (string=? name "------")))  ;; skip separator line
      (let ((buf (buffer-by-name name)))
        (if buf
          (let ((fr (app-state-frame app)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf))
          (echo-error! (app-state-echo app) (string-append "No buffer: " name))))
      (echo-message! (app-state-echo app) "No buffer on this line"))))

(def (cmd-newline app)
  (let ((buf (current-buffer-from-app app)))
    (cond
      ((dired-buffer? buf)       (cmd-dired-find-file app))
      ((buffer-list-buffer? buf) (cmd-buffer-list-select app))
      ((repl-buffer? buf)        (cmd-repl-send app))
      ((eshell-buffer? buf)      (cmd-eshell-send app))
      ((shell-buffer? buf)       (cmd-shell-send app))
      ((terminal-buffer? buf)    (cmd-terminal-send app))
      (else
       ;; Electric indent: match previous line's indentation
       (let* ((ed (current-editor app))
              (pos (editor-get-current-pos ed))
              (text (editor-get-text ed))
              (line (editor-line-from-position ed pos))
              (line-start (editor-position-from-line ed line))
              (indent (get-line-indent text line-start))
              (indent-str (make-string indent #\space)))
         (editor-insert-text ed pos (string-append "\n" indent-str))
         (editor-goto-pos ed (+ pos 1 indent)))))))

(def (cmd-open-line app)
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\n")))

(def (cmd-undo app)
  (let ((ed (current-editor app)))
    (if (editor-can-undo? ed)
      (editor-undo ed)
      (echo-message! (app-state-echo app) "No further undo information"))))

(def (cmd-redo app)
  (let ((ed (current-editor app)))
    (if (editor-can-redo? ed)
      (editor-redo ed)
      (echo-message! (app-state-echo app) "No further redo information"))))

;;;============================================================================
;;; Kill / Yank
;;;============================================================================

(def (cmd-kill-line app)
  "Kill from point to end of line, or kill newline if at end."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-end (editor-get-line-end-position ed line)))
    (if (= pos line-end)
      ;; At end of line: delete the newline
      (editor-delete-range ed pos 1)
      ;; Kill to end of line: select and cut
      (begin
        (editor-set-selection ed pos line-end)
        (editor-cut ed)
        ;; Store in kill ring and sync to system clipboard
        (let ((clip (editor-get-clipboard ed)))
          (when (> (string-length clip) 0)
            (set! (app-state-kill-ring app)
                  (cons clip (app-state-kill-ring app)))
            (clipboard-set! clip)))))))

(def (cmd-yank app)
  "Yank (paste) and track position for yank-pop."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-paste ed)
    ;; Track where we yanked so yank-pop can replace it
    (let ((new-pos (editor-get-current-pos ed)))
      (set! (app-state-last-yank-pos app) pos)
      (set! (app-state-last-yank-len app) (- new-pos pos))
      (set! (app-state-kill-ring-idx app) 0))))

;;;============================================================================
;;; Mark and region
;;;============================================================================

(def (cmd-set-mark app)
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (buf (current-buffer-from-app app)))
    ;; Push old mark to mark ring before overwriting
    (when (buffer-mark buf)
      (push-mark-ring! app buf (buffer-mark buf)))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-kill-region app)
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (editor-get-current-pos ed)))
        (editor-set-selection ed (min mark pos) (max mark pos))
        (editor-cut ed)
        ;; Sync to system clipboard
        (let ((clip (editor-get-clipboard ed)))
          (when (> (string-length clip) 0)
            (clipboard-set! clip)))
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-copy-region app)
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (editor-get-current-pos ed)))
        (editor-set-selection ed (min mark pos) (max mark pos))
        (editor-copy ed)
        ;; Sync to system clipboard
        (let ((clip (editor-get-clipboard ed)))
          (when (> (string-length clip) 0)
            (clipboard-set! clip)))
        ;; Deselect
        (editor-set-selection ed pos pos)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region copied"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (expand-filename path)
  "Expand ~ and environment variables in a file path.
   ~/foo -> /home/user/foo, $HOME/foo -> /home/user/foo"
  (cond
    ;; ~/path -> home directory
    ((and (> (string-length path) 0)
          (char=? (string-ref path 0) #\~))
     (let ((home (or (getenv "HOME")
                     (user-info-home (user-info (user-name))))))
       (if (= (string-length path) 1)
         home
         (if (char=? (string-ref path 1) #\/)
           (string-append home (substring path 1 (string-length path)))
           path))))  ; ~user not supported
    ;; $VAR/path -> environment variable expansion
    ((and (> (string-length path) 1)
          (char=? (string-ref path 0) #\$))
     (let* ((slash-pos (string-index path #\/))
            (var-name (if slash-pos
                        (substring path 1 slash-pos)
                        (substring path 1 (string-length path))))
            (rest (if slash-pos
                     (substring path slash-pos (string-length path))
                     ""))
            (value (getenv var-name)))
       (if value
         (string-append value rest)
         path)))
    (else path)))

(def (list-directory-files dir)
  "List files in a directory for completion. Returns sorted list of basenames."
  (with-catch (lambda (e) [])
    (lambda ()
      (let ((entries (directory-files dir)))
        (sort entries string<?)))))

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         ;; Get files in current directory for completion
         (cwd (current-directory))
         (files (list-directory-files cwd))
         (filename (echo-read-string-with-completion echo "Find file: "
                      files row width)))
    (when filename
      (when (> (string-length filename) 0)
        (let ((filename (expand-filename filename)))
        ;; Check if it's a directory
        (if (and (file-exists? filename)
                 (eq? 'directory (file-info-type (file-info filename))))
          (dired-open-directory! app filename)
          ;; Regular file
          (let* ((name (uniquify-buffer-name filename))
                 (ed (current-editor app))
                 (buf (buffer-create! name ed filename)))
            ;; Track in recent files
            (recent-files-add! filename)
            ;; Set major mode from auto-mode-alist and activate it
            (let ((mode (detect-major-mode filename)))
              (when mode
                (buffer-local-set! buf 'major-mode mode)
                ;; Try to execute the mode command (e.g., 'markdown-mode -> cmd-markdown-mode)
                (let ((mode-cmd (find-command mode)))
                  (when mode-cmd (mode-cmd app)))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (when (file-exists? filename)
              (let ((text (read-file-as-string filename)))
                (when text
                  (editor-set-text ed text)
                  (editor-set-save-point ed)
                  ;; Restore cursor position from save-place
                  (let ((saved-pos (save-place-restore filename)))
                    (if (and saved-pos (< saved-pos (string-length text)))
                      (begin
                        (editor-goto-pos ed saved-pos)
                        (editor-scroll-caret ed))
                      (editor-goto-pos ed 0))))))
            ;; Apply syntax highlighting for all recognized file types
            ;; Try extension-based detection first, fall back to shebang
            (let ((lang (detect-file-language filename)))
              (if lang
                (setup-highlighting-for-file! ed filename)
                ;; No extension match: try shebang from file content
                (when (file-exists? filename)
                  (let ((text (editor-get-text ed)))
                    (when (and text (> (string-length text) 2))
                      (let ((shebang-lang (detect-language-from-shebang text)))
                        (when shebang-lang
                          (setup-highlighting-for-file! ed
                            (string-append "shebang." (symbol->string shebang-lang))))))))))
            ;; Auto-detect and set line ending mode from file content
            (when (file-exists? filename)
              (let ((text (editor-get-text ed)))
                (when (and text (> (string-length text) 0))
                  (let ((eol-mode (detect-eol-mode text)))
                    (send-message ed SCI_SETEOLMODE eol-mode 0)))))
            ;; Enable line numbers for code files with adaptive width
            (let ((lang (or (detect-file-language filename)
                            (and (file-exists? filename)
                                 (detect-language-from-shebang (editor-get-text ed))))))
              (when lang
                (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
                (let* ((lines (send-message ed SCI_GETLINECOUNT 0 0))
                       (width (cond ((> lines 9999) 6)
                                    ((> lines 999) 5)
                                    (else 4))))
                  (send-message ed SCI_SETMARGINWIDTHN 0 width))))
            (echo-message! echo (string-append "Opened: " filename)))))))))

(def (cmd-save-buffer app)
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      ;; Save to existing path
      (begin
        ;; Create backup file if original exists and hasn't been backed up yet
        (when (and (file-exists? path) (not (buffer-backup-done? buf)))
          (let ((backup-path (string-append path "~")))
            (with-catch
              (lambda (e) #f)  ; Ignore backup errors
              (lambda ()
                (copy-file path backup-path)
                (set! (buffer-backup-done? buf) #t)))))
        ;; Remember cursor position for save-place
        (save-place-remember! path (editor-get-current-pos ed))
        ;; Delete trailing whitespace if enabled
        (when *delete-trailing-whitespace-on-save*
          (let* ((text (editor-get-text ed))
                 (lines (string-split text #\newline))
                 (cleaned (map (lambda (line) (string-trim-right line)) lines))
                 (result (string-join cleaned "\n")))
            (unless (string=? text result)
              (with-undo-action ed
                (editor-delete-range ed 0 (string-length text))
                (editor-insert-text ed 0 result)))))
        (let ((text (editor-get-text ed)))
          ;; Ensure final newline if required
          (when (and *require-final-newline*
                     (> (string-length text) 0)
                     (not (char=? (string-ref text (- (string-length text) 1)) #\newline)))
            (editor-append-text ed "\n")
            (set! text (editor-get-text ed)))
          (write-string-to-file path text)
          (editor-set-save-point ed)
          ;; Update recorded modification time
          (update-buffer-mod-time! buf)
          ;; Remove auto-save file if it exists
          (let ((auto-save-path (make-auto-save-path path)))
            (when (file-exists? auto-save-path)
              (delete-file auto-save-path)))
          (echo-message! echo (string-append "Wrote " path))))
      ;; No path: prompt for one
      (let* ((fr (app-state-frame app))
             (row (- (frame-height fr) 1))
             (width (frame-width fr))
             (filename (echo-read-string echo "Write file: " row width)))
        (when (and filename (> (string-length filename) 0))
          (set! (buffer-file-path buf) filename)
          (set! (buffer-name buf) (path-strip-directory filename))
          (let ((text (editor-get-text ed)))
            (write-string-to-file filename text)
            (editor-set-save-point ed)
            (echo-message! echo (string-append "Wrote " filename))))))))

;;;============================================================================
;;; Write file (Save As)
;;;============================================================================

(def (cmd-write-file app)
  "Write buffer to a new file (save as)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Write file: " row width)))
    (when (and filename (> (string-length filename) 0))
      (let* ((buf (current-buffer-from-app app))
             (ed (current-editor app))
             (text (editor-get-text ed)))
        (set! (buffer-file-path buf) filename)
        (set! (buffer-name buf) (path-strip-directory filename))
        (write-string-to-file filename text)
        (editor-set-save-point ed)
        (echo-message! echo (string-append "Wrote " filename))))))

;;;============================================================================
;;; Revert buffer
;;;============================================================================

(def (cmd-revert-buffer app)
  "Reload the current buffer from disk."
  (let* ((buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-editor app))
             (text (read-file-as-string path)))
        (when text
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (update-buffer-mod-time! buf)
          (echo-message! echo (string-append "Reverted " path))))
      (echo-error! echo "Buffer is not visiting a file"))))

;;;============================================================================
;;; Buffer commands
;;;============================================================================

(def (cmd-switch-buffer app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         ;; Build completion list from buffer names (current buffer last)
         (cur-name (buffer-name (current-buffer-from-app app)))
         (names (map buffer-name (buffer-list)))
         (other-names (filter (lambda (n) (not (string=? n cur-name))) names))
         (completions (append other-names (list cur-name)))
         (name (echo-read-string-with-completion echo "Switch to buffer: "
                  completions row width)))
    (when (and name (> (string-length name) 0))
      (let ((buf (buffer-by-name name)))
        (if buf
          (let ((ed (current-editor app)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf))
          (echo-error! echo (string-append "No buffer: " name)))))))

(def (cmd-kill-buffer-cmd app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (cur-buf (current-buffer-from-app app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (names (map buffer-name (buffer-list)))
         (name (echo-read-string-with-completion echo
                  (string-append "Kill buffer (" (buffer-name cur-buf) "): ")
                  names row width)))
    (when name
      (let* ((target-name (if (string=? name "") (buffer-name cur-buf) name))
             (buf (buffer-by-name target-name)))
        (if buf
          (let ((ed (current-editor app)))
            (if (<= (length (buffer-list)) 1)
              (echo-error! echo "Can't kill last buffer")
              ;; Check if buffer is modified and needs confirmation
              (if (and (buffer-file-path buf)
                       (editor-get-modify? ed)
                       (eq? buf (current-buffer-from-app app))
                       (let ((answer (echo-read-string echo
                                       (string-append "Buffer " target-name
                                         " modified; kill anyway? (yes/no) ")
                                       row width)))
                         (not (and answer (or (string=? answer "yes")
                                             (string=? answer "y"))))))
                (echo-message! echo "Cancelled")
                (begin
                  ;; Switch to another buffer if killing current
                  (when (eq? buf (current-buffer-from-app app))
                    (let ((other (let loop ((bs (buffer-list)))
                                   (cond ((null? bs) #f)
                                         ((eq? (car bs) buf) (loop (cdr bs)))
                                         (else (car bs))))))
                      (when other
                        (buffer-attach! ed other)
                        (set! (edit-window-buffer (current-window fr)) other))))
                  ;; Clean up dired entries if applicable
                  (hash-remove! *dired-entries* buf)
                  ;; Clean up REPL state if applicable
                  (let ((rs (hash-get *repl-state* buf)))
                    (when rs
                      (repl-stop! rs)
                      (hash-remove! *repl-state* buf)))
                  ;; Clean up eshell state if applicable
                  (hash-remove! *eshell-state* buf)
                  ;; Clean up shell state if applicable
                  (let ((ss (hash-get *shell-state* buf)))
                    (when ss
                      (shell-stop! ss)
                      (hash-remove! *shell-state* buf)))
                  (buffer-kill! ed buf)
                  (echo-message! echo (string-append "Killed " target-name))))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

;;;============================================================================
;;; Window commands
;;;============================================================================

(def (setup-new-editor-defaults! ed)
  "Apply dark theme, line numbers, and defaults to a new Scintilla editor."
  (editor-style-set-foreground ed STYLE_DEFAULT #xd8d8d8)
  (editor-style-set-background ed STYLE_DEFAULT #x181818)
  (send-message ed SCI_STYLECLEARALL)
  (editor-set-caret-foreground ed #xFFFFFF)
  ;; Line numbers
  (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
  (send-message ed SCI_SETMARGINWIDTHN 0 5)
  (editor-style-set-foreground ed STYLE_LINENUMBER #x808080)
  (editor-style-set-background ed STYLE_LINENUMBER #x181818))

(def (cmd-split-window app)
  (let ((new-ed (frame-split! (app-state-frame app))))
    (setup-new-editor-defaults! new-ed)))

(def (cmd-split-window-right app)
  (let ((new-ed (frame-split-right! (app-state-frame app))))
    (setup-new-editor-defaults! new-ed)))

(def (cmd-other-window app)
  (frame-other-window! (app-state-frame app)))

(def (cmd-delete-window app)
  (if (> (length (frame-windows (app-state-frame app))) 1)
    (frame-delete-window! (app-state-frame app))
    (echo-error! (app-state-echo app) "Can't delete sole window")))

(def (cmd-delete-other-windows app)
  (frame-delete-other-windows! (app-state-frame app)))

;;;============================================================================
;;; Search highlighting (highlight all matches)
;;;============================================================================

;; Use indicator 8 for search highlights (0-7 may be used by lexers)
(def *search-indicator* 8)

(def SCI_INDICSETALPHA 2523)  ; not in constants.ss yet

(def (setup-search-indicator! ed)
  "Configure the search highlight indicator."
  (send-message ed SCI_INDICSETSTYLE *search-indicator* INDIC_ROUNDBOX)
  (send-message ed SCI_INDICSETFORE *search-indicator* #xFFCC00)  ; yellow
  (send-message ed SCI_INDICSETUNDER *search-indicator* 1)        ; draw under text
  (send-message ed SCI_INDICSETALPHA *search-indicator* 80))      ; semi-transparent

(def (highlight-all-matches! ed query (flags 0))
  "Highlight all occurrences of query in the editor using indicators.
   flags: 0 for literal, SCFIND_REGEXP for regex."
  (setup-search-indicator! ed)
  ;; Clear existing search highlights
  (clear-search-highlights! ed)
  (when (> (string-length query) 0)
    (let ((len (editor-get-text-length ed)))
      ;; Set current indicator
      (send-message ed SCI_SETINDICATORCURRENT *search-indicator*)
      ;; Find all matches
      (send-message ed SCI_SETSEARCHFLAGS flags)
      (let loop ((start 0))
        (when (< start len)
          (send-message ed SCI_SETTARGETSTART start)
          (send-message ed SCI_SETTARGETEND len)
          (let ((found (send-message/string ed SCI_SEARCHINTARGET query)))
            (when (>= found 0)
              (let ((match-end (send-message ed SCI_GETTARGETEND)))
                (when (> match-end found)  ;; guard against zero-length regex matches
                  (send-message ed SCI_INDICATORFILLRANGE found (- match-end found)))
                (loop (+ (max found match-end) 1))))))))))

(def (clear-search-highlights! ed)
  "Remove all search highlight indicators."
  (let ((len (editor-get-text-length ed)))
    (when (> len 0)
      (send-message ed SCI_SETINDICATORCURRENT *search-indicator*)
      (send-message ed SCI_INDICATORCLEARRANGE 0 len))))

;;;============================================================================
;;; Search
;;;============================================================================

(def (search-forward-impl! app query)
  "Execute a forward search for query. Used by cmd-search-forward."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! (app-state-last-search app) query)
    ;; Highlight all matches
    (highlight-all-matches! ed query)
    (let ((pos (editor-get-current-pos ed))
          (len (editor-get-text-length ed)))
      ;; Search forward from current position
      (send-message ed SCI_SETTARGETSTART pos)
      (send-message ed SCI_SETTARGETEND len)
      (send-message ed SCI_SETSEARCHFLAGS 0)
      (let ((found (send-message/string ed SCI_SEARCHINTARGET query)))
        (if (>= found 0)
          (begin
            (editor-goto-pos ed found)
            (editor-set-selection ed found
                                  (+ found (string-length query)))
            (pulse-line! ed (editor-line-from-position ed found)))
          ;; Wrap around from beginning
          (begin
            (send-message ed SCI_SETTARGETSTART 0)
            (send-message ed SCI_SETTARGETEND len)
            (let ((found2 (send-message/string ed SCI_SEARCHINTARGET query)))
              (if (>= found2 0)
                (begin
                  (editor-goto-pos ed found2)
                  (editor-set-selection ed found2
                                        (+ found2 (string-length query)))
                  (pulse-line! ed (editor-line-from-position ed found2))
                  (echo-message! echo "Wrapped"))
                (echo-error! echo
                             (string-append "Not found: " query))))))))))

(def (search-forward-regexp-impl! app pattern)
  "Execute a forward regex search for pattern using Scintilla SCFIND_REGEXP."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! (app-state-last-search app) pattern)
    ;; Highlight all regex matches
    (highlight-all-matches! ed pattern SCFIND_REGEXP)
    (let ((pos (editor-get-current-pos ed))
          (len (editor-get-text-length ed)))
      ;; Search forward from current position
      (send-message ed SCI_SETTARGETSTART pos)
      (send-message ed SCI_SETTARGETEND len)
      (send-message ed SCI_SETSEARCHFLAGS SCFIND_REGEXP)
      (let ((found (send-message/string ed SCI_SEARCHINTARGET pattern)))
        (if (>= found 0)
          (let ((match-end (send-message ed SCI_GETTARGETEND)))
            (editor-goto-pos ed found)
            (editor-set-selection ed found match-end)
            (pulse-line! ed (editor-line-from-position ed found)))
          ;; Wrap around from beginning
          (begin
            (send-message ed SCI_SETTARGETSTART 0)
            (send-message ed SCI_SETTARGETEND len)
            (let ((found2 (send-message/string ed SCI_SEARCHINTARGET pattern)))
              (if (>= found2 0)
                (let ((match-end2 (send-message ed SCI_GETTARGETEND)))
                  (editor-goto-pos ed found2)
                  (editor-set-selection ed found2 match-end2)
                  (pulse-line! ed (editor-line-from-position ed found2))
                  (echo-message! echo "Wrapped"))
                (echo-error! echo
                  (string-append "No regexp match: " pattern))))))))))

(def (cmd-search-forward app)
  ;; If repeating C-s with an existing search query, skip the prompt
  (let ((default (or (app-state-last-search app) "")))
    (if (and (eq? (app-state-last-command app) 'search-forward)
             (> (string-length default) 0))
      ;; Repeat: move past current match, then search again
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (editor-goto-pos ed (+ pos 1))
        (search-forward-impl! app default))
      ;; First C-s: prompt for query
      (let* ((echo (app-state-echo app))
             (fr (app-state-frame app))
             (row (- (frame-height fr) 1))
             (width (frame-width fr))
             (prompt (if (string=? default "")
                       "Search: "
                       (string-append "Search [" default "]: ")))
             (input (echo-read-string echo prompt row width)))
        (when input
          (let ((query (if (string=? input "") default input)))
            (when (> (string-length query) 0)
              (search-forward-impl! app query))))))))

(def (cmd-search-backward app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (default (or (app-state-last-search app) ""))
         (prompt (if (string=? default "")
                   "Search backward: "
                   (string-append "Search backward [" default "]: ")))
         (input (echo-read-string echo prompt row width)))
    (when input
      (let* ((query (if (string=? input "") default input))
             (ed (current-editor app)))
        (when (> (string-length query) 0)
          (set! (app-state-last-search app) query)
          ;; Highlight all matches
          (highlight-all-matches! ed query)
          (let ((pos (editor-get-current-pos ed)))
            ;; Search backward: set target end before start
            (send-message ed SCI_SETTARGETSTART pos)
            (send-message ed SCI_SETTARGETEND 0)
            (send-message ed SCI_SETSEARCHFLAGS 0)
            (let ((found (send-message/string ed SCI_SEARCHINTARGET query)))
              (if (>= found 0)
                (begin
                  (editor-goto-pos ed found)
                  (editor-set-selection ed found
                                        (+ found (string-length query)))
                  (pulse-line! ed (editor-line-from-position ed found)))
                (echo-error! echo
                             (string-append "Not found: " query))))))))))

;;;============================================================================
;;; Eshell commands
;;;============================================================================

(def eshell-buffer-name "*eshell*")

(def (cmd-eshell app)
  "Open or switch to the *eshell* buffer."
  (let ((existing (buffer-by-name eshell-buffer-name)))
    (if existing
      ;; Switch to existing eshell buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app)))
        (buffer-attach! ed existing)
        (set! (edit-window-buffer (current-window fr)) existing)
        (echo-message! (app-state-echo app) eshell-buffer-name))
      ;; Create new eshell buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app))
             (buf (buffer-create! eshell-buffer-name ed #f)))
        ;; Mark as eshell buffer
        (set! (buffer-lexer-lang buf) 'eshell)
        ;; Attach buffer to editor
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        ;; Store eshell state (just current directory for now)
        (hash-put! *eshell-state* buf (current-directory))
        ;; Insert welcome message and prompt
        (let ((welcome (string-append "Gerbil Eshell\n"
                                       "Type commands, Gerbil expressions, or 'exit' to close.\n\n"
                                       eshell-prompt)))
          (editor-set-text ed welcome)
          (let ((len (editor-get-text-length ed)))
            (editor-goto-pos ed len)))
        (echo-message! (app-state-echo app) "Eshell started")))))

(def (cmd-eshell-send app)
  "Process eshell input."
  (let* ((buf (current-buffer-from-app app))
         (cwd (hash-get *eshell-state* buf)))
    (when cwd
      (let* ((ed (current-editor app))
             (all-text (editor-get-text ed))
             ;; Find the last prompt position
             (prompt-pos (eshell-find-last-prompt all-text))
             (end-pos (string-length all-text))
             (input (if (and prompt-pos (> end-pos (+ prompt-pos (string-length eshell-prompt))))
                      (substring all-text (+ prompt-pos (string-length eshell-prompt)) end-pos)
                      "")))
        ;; Append newline
        (editor-append-text ed "\n")
        ;; Process the input
        (let-values (((output new-cwd) (eshell-process-input input cwd)))
          ;; Update cwd
          (hash-put! *eshell-state* buf new-cwd)
          (cond
            ((eq? output 'clear)
             ;; Clear buffer, re-insert prompt
             (editor-set-text ed eshell-prompt)
             (editor-goto-pos ed (editor-get-text-length ed)))
            ((eq? output 'exit)
             ;; Kill eshell buffer
             (cmd-kill-buffer-cmd app))
            (else
             ;; Insert output + new prompt
             (when (and (string? output) (> (string-length output) 0))
               (editor-append-text ed output))
             (editor-append-text ed eshell-prompt)
             (editor-goto-pos ed (editor-get-text-length ed))
             (editor-scroll-caret ed))))))))

(def (eshell-find-last-prompt text)
  "Find the position of the last eshell prompt in text."
  (let ((prompt eshell-prompt)
        (prompt-len (string-length eshell-prompt)))
    (let loop ((pos (- (string-length text) prompt-len)))
      (cond
        ((< pos 0) #f)
        ((string=? (substring text pos (+ pos prompt-len)) prompt) pos)
        (else (loop (- pos 1)))))))

;;;============================================================================
;;; Shell commands
;;;============================================================================

(def shell-buffer-name "*shell*")

(def (cmd-shell app)
  "Open or switch to the *shell* buffer."
  (let ((existing (buffer-by-name shell-buffer-name)))
    (if existing
      ;; Switch to existing shell buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app)))
        (buffer-attach! ed existing)
        (set! (edit-window-buffer (current-window fr)) existing)
        (echo-message! (app-state-echo app) shell-buffer-name))
      ;; Create new shell buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app))
             (buf (buffer-create! shell-buffer-name ed #f)))
        ;; Mark as shell buffer
        (set! (buffer-lexer-lang buf) 'shell)
        ;; Attach buffer to editor
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        ;; Spawn shell subprocess
        (let ((ss (shell-start!)))
          (hash-put! *shell-state* buf ss)
          ;; Start with empty buffer; shell output will appear via polling
          (editor-set-text ed "")
          (set! (shell-state-prompt-pos ss) 0))
        (echo-message! (app-state-echo app) "Shell started")))))

(def (cmd-shell-send app)
  "Send the current input line to the shell subprocess."
  (let* ((buf (current-buffer-from-app app))
         (ss (hash-get *shell-state* buf)))
    (when ss
      (let* ((ed (current-editor app))
             (prompt-pos (shell-state-prompt-pos ss))
             (all-text (editor-get-text ed))
             (end-pos (string-length all-text))
             (input (if (> end-pos prompt-pos)
                      (substring all-text prompt-pos end-pos)
                      "")))
        ;; Append newline to buffer
        (editor-append-text ed "\n")
        ;; Send to shell
        (shell-send! ss input)
        ;; Update prompt-pos to after the newline
        (set! (shell-state-prompt-pos ss) (editor-get-text-length ed))))))

;;;============================================================================
;;; Terminal commands (PTY-backed vterm-like terminal)
;;;============================================================================

(def terminal-buffer-counter 0)

(def (cmd-term app)
  "Open a new PTY-backed terminal buffer (vterm-like)."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (name (begin
                 (set! terminal-buffer-counter (+ terminal-buffer-counter 1))
                 (if (= terminal-buffer-counter 1)
                   "*terminal*"
                   (string-append "*terminal-"
                                  (number->string terminal-buffer-counter) "*"))))
         (buf (buffer-create! name ed #f)))
    ;; Mark as terminal buffer
    (set! (buffer-lexer-lang buf) 'terminal)
    ;; Attach buffer to editor
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    ;; Set up terminal ANSI color styles
    (setup-terminal-styles! ed)
    ;; Spawn PTY-backed shell
    (let ((ts (terminal-start!)))
      (hash-put! *terminal-state* buf ts)
      (editor-set-text ed "")
      (set! (terminal-state-prompt-pos ts) 0))
    (echo-message! (app-state-echo app) (string-append name " started"))))

(def (cmd-terminal-send app)
  "Send Enter (newline) to the terminal PTY."
  (let* ((buf (current-buffer-from-app app))
         (ts (hash-get *terminal-state* buf)))
    (when ts
      ;; Send newline to PTY — output will come back via polling
      (terminal-send-raw! ts "\n"))))

(def (cmd-term-interrupt app)
  "Send Ctrl-C (interrupt) to the terminal PTY."
  (let* ((buf (current-buffer-from-app app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x03;")  ; Ctrl-C
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

(def (cmd-term-send-eof app)
  "Send Ctrl-D (EOF) to the terminal PTY."
  (let* ((buf (current-buffer-from-app app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x04;")  ; Ctrl-D
      ;; Fall back to normal Ctrl-D behavior (delete char)
      (editor-send-key (current-editor app) SCK_DELETE))))

(def (cmd-term-send-tab app)
  "Send Tab to the terminal PTY (for tab completion)."
  (let* ((buf (current-buffer-from-app app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\t")
      ;; Fall back to normal Tab behavior
      (editor-send-key (current-editor app) (char->integer #\tab)))))

;;;============================================================================
;;; Dired support (needed by cmd-find-file and cmd-newline)
;;;============================================================================

(def (dired-open-directory! app dir-path)
  "Open a directory listing in a new dired buffer."
  (let* ((dir (strip-trailing-slash dir-path))
         (name (string-append dir "/"))
         (fr (app-state-frame app))
         (ed (current-editor app))
         (buf (buffer-create! name ed dir)))
    ;; Mark as dired buffer
    (set! (buffer-lexer-lang buf) 'dired)
    ;; Attach buffer to editor
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    ;; Generate and set listing
    (let-values (((text entries) (dired-format-listing dir)))
      (editor-set-text ed text)
      (editor-set-save-point ed)
      ;; Position cursor at first entry (line 3, after header + count + blank)
      (editor-goto-pos ed 0)
      (editor-send-key ed SCK_DOWN)
      (editor-send-key ed SCK_DOWN)
      (editor-send-key ed SCK_DOWN)
      (editor-send-key ed SCK_HOME)
      ;; Store entries for navigation
      (hash-put! *dired-entries* buf entries))
    (echo-message! (app-state-echo app) (string-append "Directory: " dir))))

(def (cmd-dired-find-file app)
  "In a dired buffer, open the file or directory under cursor."
  (let* ((buf (current-buffer-from-app app))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let ((idx (- line 3)))
        (if (or (< idx 0) (>= idx (vector-length entries)))
          (echo-message! (app-state-echo app) "No file on this line")
          (let ((full-path (vector-ref entries idx)))
            (with-catch
              (lambda (e)
                (echo-error! (app-state-echo app)
                             (string-append "Error: "
                               (with-output-to-string
                                 (lambda () (display-exception e))))))
              (lambda ()
                (let ((info (file-info full-path)))
                  (if (eq? 'directory (file-info-type info))
                    (dired-open-directory! app full-path)
                    ;; Open as regular file
                    (let* ((fname (path-strip-directory full-path))
                           (fr (app-state-frame app))
                           (new-buf (buffer-create! fname ed full-path)))
                      (buffer-attach! ed new-buf)
                      (set! (edit-window-buffer (current-window fr)) new-buf)
                      (let ((text (read-file-as-string full-path)))
                        (when text
                          (editor-set-text ed text)
                          (editor-set-save-point ed)
                          (editor-goto-pos ed 0)))
                      ;; Apply syntax highlighting for Gerbil files
                      (when (gerbil-file-extension? full-path)
                        (setup-gerbil-highlighting! ed)
                        (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
                        (send-message ed SCI_SETMARGINWIDTHN 0 4))
                      (echo-message! (app-state-echo app)
                                     (string-append "Opened: " full-path)))))))))))))

;;;============================================================================
;;; REPL commands (needed by cmd-newline)
;;;============================================================================

(def repl-buffer-name "*REPL*")

(def (cmd-repl app)
  "Open or switch to the *REPL* buffer."
  (let ((existing (buffer-by-name repl-buffer-name)))
    (if existing
      ;; Switch to existing REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app)))
        (buffer-attach! ed existing)
        (set! (edit-window-buffer (current-window fr)) existing)
        (echo-message! (app-state-echo app) repl-buffer-name))
      ;; Create new REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-editor app))
             (buf (buffer-create! repl-buffer-name ed #f)))
        ;; Mark as REPL buffer
        (set! (buffer-lexer-lang buf) 'repl)
        ;; Attach buffer to editor
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        ;; Spawn gxi subprocess
        (let ((rs (repl-start!)))
          (hash-put! *repl-state* buf rs)
          ;; Insert initial prompt
          (editor-set-text ed repl-prompt)
          (let ((len (editor-get-text-length ed)))
            (set! (repl-state-prompt-pos rs) len)
            (editor-goto-pos ed len)))
        (echo-message! (app-state-echo app) "REPL started")))))

(def (cmd-repl-send app)
  "Send the current input line to the gxi subprocess."
  (let* ((buf (current-buffer-from-app app))
         (rs (hash-get *repl-state* buf)))
    (when rs
      (let* ((ed (current-editor app))
             (prompt-pos (repl-state-prompt-pos rs))
             (all-text (editor-get-text ed))
             (end-pos (string-length all-text))
             ;; Extract user input after the prompt
             (input (if (> end-pos prompt-pos)
                      (substring all-text prompt-pos end-pos)
                      "")))
        ;; Append newline to the buffer
        (editor-append-text ed "\n")
        ;; Send to gxi
        (repl-send! rs input)
        ;; Update prompt-pos to after the newline (output will appear here)
        (set! (repl-state-prompt-pos rs) (editor-get-text-length ed))))))

;;;============================================================================
;;; Mark ring (needed by cmd-set-mark)
;;;============================================================================

(def max-mark-ring-size 16)

(def (push-mark-ring! app buf pos)
  "Push a mark position onto the mark ring."
  (let* ((entry (cons (buffer-name buf) pos))
         (ring (app-state-mark-ring app))
         (new-ring (cons entry
                     (if (>= (length ring) max-mark-ring-size)
                       (let trim ((r ring) (n (- max-mark-ring-size 1)))
                         (if (or (null? r) (= n 0)) '()
                           (cons (car r) (trim (cdr r) (- n 1)))))
                       ring))))
    (set! (app-state-mark-ring app) new-ring)))

;;;============================================================================
;;; Tab insertion command
;;;============================================================================

(def (cmd-tab-to-tab-stop app)
  "Insert spaces (or tab) to the next tab stop."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (col (editor-get-column ed pos))
         (tw (send-message ed SCI_GETTABWIDTH 0 0))
         (tw (if (> tw 0) tw 4)) ;; default 4 if unset
         (use-tabs (= 1 (send-message ed SCI_GETUSETABS 0 0))))
    (if use-tabs
      (begin
        (editor-insert-text ed pos "\t")
        (editor-goto-pos ed (+ pos 1)))
      (let* ((next-stop (* (+ 1 (quotient col tw)) tw))
             (spaces (- next-stop col))
             (str (make-string spaces #\space)))
        (editor-insert-text ed pos str)
        (editor-goto-pos ed (+ pos spaces))))))


