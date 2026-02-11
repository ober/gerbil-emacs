;;; -*- Gerbil -*-
;;; TUI editor commands and app state for gerbil-emacs
;;;
;;; All commands take an app-state parameter.
;;; App state, command registry, and file I/O helpers are in core.ss.

(export
  (struct-out app-state)
  new-app-state
  current-editor
  current-buffer-from-app
  execute-command!
  cmd-self-insert!
  register-all-commands!
  read-file-as-string
  position-cursor-for-replace!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/highlight)

;;;============================================================================
;;; Accessors
;;;============================================================================

(def (current-editor app)
  (edit-window-editor (current-window (app-state-frame app))))

(def (current-buffer-from-app app)
  (edit-window-buffer (current-window (app-state-frame app))))

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
      (else
       (let* ((ed (current-editor app))
              (close-ch (auto-pair-char ch)))
         (if close-ch
           ;; Auto-pair: insert both chars and place cursor between
           (let ((pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos
               (string (integer->char ch) (integer->char close-ch)))
             (editor-goto-pos ed (+ pos 1)))
           (editor-send-key ed ch)))))))

;;;============================================================================
;;; Navigation commands
;;;============================================================================

(def (cmd-forward-char app)
  (editor-send-key (current-editor app) SCK_RIGHT))

(def (cmd-backward-char app)
  (editor-send-key (current-editor app) SCK_LEFT))

(def (cmd-next-line app)
  (editor-send-key (current-editor app) SCK_DOWN))

(def (cmd-previous-line app)
  (editor-send-key (current-editor app) SCK_UP))

(def (cmd-beginning-of-line app)
  (editor-send-key (current-editor app) SCK_HOME))

(def (cmd-end-of-line app)
  (editor-send-key (current-editor app) SCK_END))

(def (cmd-forward-word app)
  (editor-send-key (current-editor app) SCK_RIGHT ctrl: #t))

(def (cmd-backward-word app)
  (editor-send-key (current-editor app) SCK_LEFT ctrl: #t))

(def (cmd-beginning-of-buffer app)
  (editor-send-key (current-editor app) SCK_HOME ctrl: #t))

(def (cmd-end-of-buffer app)
  (editor-send-key (current-editor app) SCK_END ctrl: #t))

(def (cmd-scroll-down app)
  (editor-send-key (current-editor app) SCK_NEXT))

(def (cmd-scroll-up app)
  (editor-send-key (current-editor app) SCK_PRIOR))

(def (cmd-recenter app)
  (editor-scroll-caret (current-editor app)))

;;;============================================================================
;;; Editing commands
;;;============================================================================

(def (cmd-delete-char app)
  (editor-send-key (current-editor app) SCK_DELETE))

(def (cmd-backward-delete-char app)
  (let ((buf (current-buffer-from-app app)))
    (if (repl-buffer? buf)
      ;; In REPL buffers, don't delete past the prompt
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (rs (hash-get *repl-state* buf)))
        (when (and rs (> pos (repl-state-prompt-pos rs)))
          (editor-send-key ed SCK_BACK)))
      (editor-send-key (current-editor app) SCK_BACK))))

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

(def (cmd-newline app)
  (let ((buf (current-buffer-from-app app)))
    (cond
      ((dired-buffer? buf)  (cmd-dired-find-file app))
      ((repl-buffer? buf)   (cmd-repl-send app))
      ((eshell-buffer? buf) (cmd-eshell-send app))
      ((shell-buffer? buf)  (cmd-shell-send app))
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
        ;; Store in kill ring
        (let ((clip (editor-get-clipboard ed)))
          (when (> (string-length clip) 0)
            (set! (app-state-kill-ring app)
                  (cons clip (app-state-kill-ring app)))))))))

(def (cmd-yank app)
  (editor-paste (current-editor app)))

;;;============================================================================
;;; Mark and region
;;;============================================================================

(def (cmd-set-mark app)
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (buf (current-buffer-from-app app)))
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
        ;; Deselect
        (editor-set-selection ed pos pos)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region copied"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Find file: " row width)))
    (when filename
      (when (> (string-length filename) 0)
        ;; Check if it's a directory
        (if (and (file-exists? filename)
                 (eq? 'directory (file-info-type (file-info filename))))
          (dired-open-directory! app filename)
          ;; Regular file
          (let* ((name (path-strip-directory filename))
                 (ed (current-editor app))
                 (buf (buffer-create! name ed filename)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (when (file-exists? filename)
              (let ((text (read-file-as-string filename)))
                (when text
                  (editor-set-text ed text)
                  (editor-set-save-point ed)
                  (editor-goto-pos ed 0))))
            ;; Apply syntax highlighting for Gerbil files
            (when (gerbil-file-extension? filename)
              (setup-gerbil-highlighting! ed))
            (echo-message! echo (string-append "Opened: " filename))))))))

(def (cmd-save-buffer app)
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      ;; Save to existing path
      (begin
        (let ((text (editor-get-text ed)))
          (write-string-to-file path text)
          (editor-set-save-point ed)
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
         (name (echo-read-string echo "Switch to buffer: " row width)))
    (when name
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
         (name (echo-read-string echo
                  (string-append "Kill buffer (" (buffer-name cur-buf) "): ")
                  row width)))
    (when name
      (let* ((target-name (if (string=? name "") (buffer-name cur-buf) name))
             (buf (buffer-by-name target-name)))
        (if buf
          (let ((ed (current-editor app)))
            (if (<= (length (buffer-list)) 1)
              (echo-error! echo "Can't kill last buffer")
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
                (echo-message! echo (string-append "Killed " target-name)))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

;;;============================================================================
;;; Window commands
;;;============================================================================

(def (cmd-split-window app)
  (let ((new-ed (frame-split! (app-state-frame app))))
    ;; Apply dark theme to the new editor
    (editor-style-set-foreground new-ed STYLE_DEFAULT #xd8d8d8)
    (editor-style-set-background new-ed STYLE_DEFAULT #x181818)
    (send-message new-ed SCI_STYLECLEARALL)
    (editor-set-caret-foreground new-ed #xFFFFFF)))

(def (cmd-split-window-right app)
  (let ((new-ed (frame-split-right! (app-state-frame app))))
    (editor-style-set-foreground new-ed STYLE_DEFAULT #xd8d8d8)
    (editor-style-set-background new-ed STYLE_DEFAULT #x181818)
    (send-message new-ed SCI_STYLECLEARALL)
    (editor-set-caret-foreground new-ed #xFFFFFF)))

(def (cmd-other-window app)
  (frame-other-window! (app-state-frame app)))

(def (cmd-delete-window app)
  (if (> (length (frame-windows (app-state-frame app))) 1)
    (frame-delete-window! (app-state-frame app))
    (echo-error! (app-state-echo app) "Can't delete sole window")))

(def (cmd-delete-other-windows app)
  (frame-delete-other-windows! (app-state-frame app)))

;;;============================================================================
;;; Search
;;;============================================================================

(def (cmd-search-forward app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (default (or (app-state-last-search app) ""))
         (prompt (if (string=? default "")
                   "Search: "
                   (string-append "Search [" default "]: ")))
         (input (echo-read-string echo prompt row width)))
    (when input
      (let* ((query (if (string=? input "") default input))
             (ed (current-editor app)))
        (when (> (string-length query) 0)
          (set! (app-state-last-search app) query)
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
                                        (+ found (string-length query))))
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
                        (echo-message! echo "Wrapped"))
                      (echo-error! echo
                                   (string-append "Not found: " query)))))))))))))

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
                                        (+ found (string-length query))))
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
;;; Goto line
;;;============================================================================

(def (cmd-goto-line app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto line: " row width)))
    (when (and input (> (string-length input) 0))
      (let ((line-num (string->number input)))
        (if (and line-num (> line-num 0))
          (let ((ed (current-editor app)))
            (editor-goto-line ed (- line-num 1))  ; 0-based
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Line " input)))
          (echo-error! echo "Invalid line number"))))))

;;;============================================================================
;;; M-x (execute extended command)
;;;============================================================================

(def (cmd-execute-extended-command app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "M-x " row width)))
    (when (and input (> (string-length input) 0))
      (let ((cmd-name (string->symbol input)))
        (execute-command! app cmd-name)))))

;;;============================================================================
;;; Help commands
;;;============================================================================

(def (cmd-describe-key app)
  "Prompt for a key, display its binding."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    ;; Draw prompt
    (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
    (tui-print! 0 row #xd8d8d8 #x181818 "Describe key: ")
    (tui-present!)
    ;; Wait for a key event
    (let ((ev (tui-poll-event)))
      (when (and ev (tui-event-key? ev))
        (let* ((key-str (key-event->string ev))
               (binding (keymap-lookup *global-keymap* key-str)))
          (cond
            ((hash-table? binding)
             (echo-message! echo (string-append key-str " is a prefix key")))
            ((symbol? binding)
             (echo-message! echo
               (string-append key-str " runs " (symbol->string binding))))
            (else
             (echo-message! echo
               (string-append key-str " is not bound")))))))))

(def (cmd-describe-command app)
  "Prompt for a command name, show if it exists."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Describe command: " row width)))
    (when (and input (> (string-length input) 0))
      (let ((cmd (find-command (string->symbol input))))
        (if cmd
          (echo-message! echo (string-append input " is a command"))
          (echo-error! echo (string-append input " is not a command")))))))

(def (cmd-list-bindings app)
  "Display all keybindings in a *Help* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (lines '()))
    ;; Collect global keymap bindings
    (for-each
      (lambda (entry)
        (let ((key (car entry))
              (val (cdr entry)))
          (cond
            ((symbol? val)
             (set! lines (cons (string-append "  " key "\t" (symbol->string val))
                               lines)))
            ((hash-table? val)
             ;; Prefix map: list its sub-bindings
             (for-each
               (lambda (sub-entry)
                 (let ((sub-key (car sub-entry))
                       (sub-val (cdr sub-entry)))
                   (when (symbol? sub-val)
                     (set! lines
                       (cons (string-append "  " key " " sub-key "\t"
                                            (symbol->string sub-val))
                             lines)))))
               (keymap-entries val))))))
      (keymap-entries *global-keymap*))
    ;; Sort and format
    (let* ((sorted (sort lines string<?))
           (text (string-append "Key Bindings:\n\n"
                                (string-join sorted "\n")
                                "\n")))
      ;; Create or reuse *Help* buffer
      (let ((buf (or (buffer-by-name "*Help*")
                     (buffer-create! "*Help*" ed #f))))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed text)
        (editor-set-save-point ed)
        (editor-goto-pos ed 0)
        (echo-message! (app-state-echo app) "*Help*")))))

;;;============================================================================
;;; Buffer list
;;;============================================================================

(def (cmd-list-buffers app)
  "Display all buffers in a *Buffer List* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (bufs (buffer-list))
         (header "  Buffer\t\tFile\n  ------\t\t----\n")
         (lines (map (lambda (buf)
                       (let ((name (buffer-name buf))
                             (path (or (buffer-file-path buf) "")))
                         (string-append "  " name "\t\t" path)))
                     bufs))
         (text (string-append header (string-join lines "\n") "\n")))
    (let ((buf (or (buffer-by-name "*Buffer List*")
                   (buffer-create! "*Buffer List*" ed #f))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! (app-state-echo app) "*Buffer List*"))))

;;;============================================================================
;;; Query replace
;;;============================================================================

(def (cmd-query-replace app)
  "Interactive search and replace (M-%)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-str (echo-read-string echo "Query replace: " row width)))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (echo-read-string echo
                      (string-append "Replace \"" from-str "\" with: ")
                      row width)))
        (when to-str
          (let ((ed (current-editor app)))
            (query-replace-loop! app ed from-str to-str)))))))

(def (query-replace-loop! app ed from-str to-str)
  "Drive the query-replace interaction."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-len (string-length from-str))
         (to-len (string-length to-str))
         (replaced 0))
    ;; Start searching from beginning
    (editor-goto-pos ed 0)
    (let loop ()
      (let ((text-len (editor-get-text-length ed))
            (pos (editor-get-current-pos ed)))
        ;; Search forward
        (send-message ed SCI_SETTARGETSTART pos)
        (send-message ed SCI_SETTARGETEND text-len)
        (send-message ed SCI_SETSEARCHFLAGS 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET from-str)))
          (if (< found 0)
            ;; No more matches
            (echo-message! echo
              (string-append "Replaced " (number->string replaced) " occurrences"))
            ;; Found a match, highlight it
            (begin
              (editor-set-selection ed found (+ found from-len))
              (editor-scroll-caret ed)
              ;; Redraw so user can see the match
              (frame-refresh! fr)
              (position-cursor-for-replace! app)
              ;; Prompt: y/n/!/q
              (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
              (tui-print! 0 row #xd8d8d8 #x181818
                "Replace? (y)es (n)o (!)all (q)uit")
              (tui-present!)
              (let ((ev (tui-poll-event)))
                (when (and ev (tui-event-key? ev))
                  (let ((ch (tui-event-ch ev)))
                    (cond
                      ;; Yes: replace and continue
                      ((= ch (char->integer #\y))
                       (send-message ed SCI_SETTARGETSTART found)
                       (send-message ed SCI_SETTARGETEND (+ found from-len))
                       (send-message/string ed SCI_REPLACETARGET to-str)
                       (editor-goto-pos ed (+ found to-len))
                       (set! replaced (+ replaced 1))
                       (loop))
                      ;; No: skip
                      ((= ch (char->integer #\n))
                       (editor-goto-pos ed (+ found from-len))
                       (loop))
                      ;; All: replace all remaining
                      ((= ch (char->integer #\!))
                       (let all-loop ()
                         (let ((text-len2 (editor-get-text-length ed))
                               (pos2 (editor-get-current-pos ed)))
                           (send-message ed SCI_SETTARGETSTART pos2)
                           (send-message ed SCI_SETTARGETEND text-len2)
                           (let ((found2 (send-message/string ed SCI_SEARCHINTARGET from-str)))
                             (when (>= found2 0)
                               (send-message ed SCI_SETTARGETSTART found2)
                               (send-message ed SCI_SETTARGETEND (+ found2 from-len))
                               (send-message/string ed SCI_REPLACETARGET to-str)
                               (editor-goto-pos ed (+ found2 to-len))
                               (set! replaced (+ replaced 1))
                               (all-loop)))))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Quit
                      ((= ch (char->integer #\q))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Unknown key: skip
                      (else (loop)))))))))))))

(def (position-cursor-for-replace! app)
  "Helper to show cursor during query-replace."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (screen-x (send-message ed SCI_POINTXFROMPOSITION 0 pos))
         (screen-y (send-message ed SCI_POINTYFROMPOSITION 0 pos))
         (win-x (edit-window-x win))
         (win-y (edit-window-y win)))
    (tui-set-cursor! (+ win-x screen-x) (+ win-y screen-y))
    (tui-present!)))

;;;============================================================================
;;; Tab / indent
;;;============================================================================

(def (cmd-indent-or-complete app)
  "Insert appropriate indentation."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app)))
    (cond
      ((dired-buffer? buf) (void))
      ((repl-buffer? buf)
       ;; In REPL, insert 2 spaces
       (let ((pos (editor-get-current-pos ed))
             (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (editor-insert-text ed pos "  "))))
      (else
       ;; Insert 2-space indent (Scheme convention)
       (let ((pos (editor-get-current-pos ed)))
         (editor-insert-text ed pos "  "))))))

;;;============================================================================
;;; Beginning/end of defun
;;;============================================================================

(def (cmd-beginning-of-defun app)
  "Move to the beginning of the current/previous top-level form."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Search backward for '(' at column 0
    (let loop ((i (- pos 1)))
      (cond
        ((< i 0)
         (editor-goto-pos ed 0)
         (echo-message! (app-state-echo app) "Beginning of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (editor-goto-pos ed i)
         (editor-scroll-caret ed))
        (else (loop (- i 1)))))))

(def (cmd-end-of-defun app)
  "Move to the end of the current top-level form."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; First find the start of the current/next defun
    (let find-start ((i pos))
      (cond
        ((>= i len)
         (editor-goto-pos ed len)
         (echo-message! (app-state-echo app) "End of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         ;; Found start of defun, now find matching close paren
         (let match ((j (+ i 1)) (depth 1))
           (cond
             ((>= j len) (editor-goto-pos ed len))
             ((= depth 0)
              (editor-goto-pos ed j)
              (editor-scroll-caret ed))
             ((char=? (string-ref text j) #\() (match (+ j 1) (+ depth 1)))
             ((char=? (string-ref text j) #\)) (match (+ j 1) (- depth 1)))
             (else (match (+ j 1) depth)))))
        (else (find-start (+ i 1)))))))

;;;============================================================================
;;; Toggle line numbers
;;;============================================================================

(def (cmd-toggle-line-numbers app)
  "Toggle line number margin on/off."
  (let ((ed (current-editor app)))
    (let ((cur-width (send-message ed SCI_GETMARGINWIDTHN 0 0)))
      (if (> cur-width 0)
        (begin
          (send-message ed SCI_SETMARGINWIDTHN 0 0)
          (echo-message! (app-state-echo app) "Line numbers off"))
        (begin
          ;; Set margin 0 to line numbers type
          (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
          ;; Width of ~4 chars for line numbers
          (send-message ed SCI_SETMARGINWIDTHN 0 4)
          (echo-message! (app-state-echo app) "Line numbers on"))))))

;;;============================================================================
;;; Toggle word wrap
;;;============================================================================

(def (cmd-toggle-word-wrap app)
  "Toggle word wrap on/off."
  (let ((ed (current-editor app)))
    (let ((cur (editor-get-wrap-mode ed)))
      (if (= cur SC_WRAP_NONE)
        (begin
          (editor-set-wrap-mode ed SC_WRAP_WORD)
          (echo-message! (app-state-echo app) "Word wrap on"))
        (begin
          (editor-set-wrap-mode ed SC_WRAP_NONE)
          (echo-message! (app-state-echo app) "Word wrap off"))))))

;;;============================================================================
;;; Toggle whitespace visibility
;;;============================================================================

(def (cmd-toggle-whitespace app)
  "Toggle whitespace visibility."
  (let ((ed (current-editor app)))
    (let ((cur (editor-get-view-whitespace ed)))
      (if (= cur SCWS_INVISIBLE)
        (begin
          (editor-set-view-whitespace ed SCWS_VISIBLEALWAYS)
          (echo-message! (app-state-echo app) "Whitespace visible"))
        (begin
          (editor-set-view-whitespace ed SCWS_INVISIBLE)
          (echo-message! (app-state-echo app) "Whitespace hidden"))))))

;;;============================================================================
;;; Zoom
;;;============================================================================

(def (cmd-zoom-in app)
  (let ((ed (current-editor app)))
    (editor-zoom-in ed)
    (echo-message! (app-state-echo app)
                   (string-append "Zoom: " (number->string (editor-get-zoom ed))))))

(def (cmd-zoom-out app)
  (let ((ed (current-editor app)))
    (editor-zoom-out ed)
    (echo-message! (app-state-echo app)
                   (string-append "Zoom: " (number->string (editor-get-zoom ed))))))

(def (cmd-zoom-reset app)
  (let ((ed (current-editor app)))
    (editor-set-zoom ed 0)
    (echo-message! (app-state-echo app) "Zoom reset")))

;;;============================================================================
;;; Select all
;;;============================================================================

(def (cmd-select-all app)
  (let ((ed (current-editor app)))
    (editor-select-all ed)
    (echo-message! (app-state-echo app) "Mark set (whole buffer)")))

;;;============================================================================
;;; Duplicate line
;;;============================================================================

(def (cmd-duplicate-line app)
  "Duplicate the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (line-text (editor-get-line ed line)))
    ;; Insert a copy after the current line
    (editor-goto-pos ed line-end)
    (editor-insert-text ed line-end (string-append "\n" (string-trim-right line-text #\newline)))))

;;;============================================================================
;;; Comment toggle (Scheme: ;; prefix)
;;;============================================================================

(def (cmd-toggle-comment app)
  "Toggle ;; comment on the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (line-text (editor-get-line ed line))
         (trimmed (string-trim line-text)))
    (cond
      ;; Line starts with ";; " — remove it
      ((and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";; "))
       ;; Find position of ";; " in the original line
       (let ((comment-pos (string-contains line-text ";; ")))
         (when comment-pos
           (editor-delete-range ed (+ line-start comment-pos) 3))))
      ;; Line starts with ";;" — remove it
      ((and (>= (string-length trimmed) 2)
            (string=? (substring trimmed 0 2) ";;"))
       (let ((comment-pos (string-contains line-text ";;")))
         (when comment-pos
           (editor-delete-range ed (+ line-start comment-pos) 2))))
      ;; Add ";; " at start of line
      (else
       (editor-insert-text ed line-start ";; ")))))

;;;============================================================================
;;; Transpose chars (C-t)
;;;============================================================================

(def (cmd-transpose-chars app)
  "Swap the two characters before point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (when (>= pos 2)
      (let* ((text (editor-get-text ed))
             (c1 (string-ref text (- pos 2)))
             (c2 (string-ref text (- pos 1))))
        (with-undo-action ed
          (editor-delete-range ed (- pos 2) 2)
          (editor-insert-text ed (- pos 2)
            (string c2 c1)))
        (editor-goto-pos ed pos)))))

;;;============================================================================
;;; Word case commands
;;;============================================================================

(def (word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

(def (word-at-point ed)
  "Get the word boundaries at/after current position.
   Returns (values start end) or (values #f #f) if no word."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Skip non-word chars
    (let skip ((i pos))
      (if (>= i len)
        (values #f #f)
        (let ((ch (string-ref text i)))
          (if (word-char? ch)
            ;; Found start of word, find end
            (let find-end ((j (+ i 1)))
              (if (>= j len)
                (values i j)
                (if (word-char? (string-ref text j))
                  (find-end (+ j 1))
                  (values i j))))
            (skip (+ i 1))))))))

(def (cmd-upcase-word app)
  "Convert the next word to uppercase."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (upper (string-upcase word)))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start upper))
          (editor-goto-pos ed end))))))

(def (cmd-downcase-word app)
  "Convert the next word to lowercase."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (lower (string-downcase word)))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start lower))
          (editor-goto-pos ed end))))))

(def (cmd-capitalize-word app)
  "Capitalize the next word."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when (and start (< start end))
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (cap (string-append
                      (string-upcase (substring word 0 1))
                      (string-downcase (substring word 1 (string-length word))))))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start cap))
          (editor-goto-pos ed end))))))

;;;============================================================================
;;; Kill word (M-d)
;;;============================================================================

(def (cmd-kill-word app)
  "Kill from point to end of word."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((pos (editor-get-current-pos ed))
               (kill-start (min pos start))
               (text (editor-get-text ed))
               (killed (substring text kill-start end)))
          ;; Add to kill ring
          (set! (app-state-kill-ring app)
                (cons killed (app-state-kill-ring app)))
          (editor-delete-range ed kill-start (- end kill-start)))))))

;;;============================================================================
;;; What line (M-g l)
;;;============================================================================

(def (cmd-what-line app)
  "Display current line number in echo area."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (col (+ 1 (editor-get-column ed pos)))
         (total (editor-get-line-count ed)))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     " of " (number->string total)
                     ", Column " (number->string col)))))

;;;============================================================================
;;; Delete trailing whitespace
;;;============================================================================

(def (cmd-delete-trailing-whitespace app)
  "Remove trailing whitespace from all lines."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line))
                       lines))
         (new-text (string-join cleaned "\n")))
    (when (not (string=? text new-text))
      (editor-set-text ed new-text)
      (editor-goto-pos ed (min pos (editor-get-text-length ed)))
      (echo-message! (app-state-echo app) "Trailing whitespace deleted"))))

;;;============================================================================
;;; Count words/lines
;;;============================================================================

(def (cmd-count-words app)
  "Display word, line, and character counts for the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (chars (string-length text))
         (lines (editor-get-line-count ed))
         ;; Simple word count: count transitions from non-word to word chars
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars) count
                    (let ((ch (string-ref text i)))
                      (if (or (char-alphabetic? ch) (char-numeric? ch))
                        (loop (+ i 1) #t (if in-word count (+ count 1)))
                        (loop (+ i 1) #f count)))))))
    (echo-message! (app-state-echo app)
      (string-append "Lines: " (number->string lines)
                     "  Words: " (number->string words)
                     "  Chars: " (number->string chars)))))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state)))

(def (cmd-quit app)
  (set! (app-state-running app) #f))

;;;============================================================================
;;; Dired (directory listing) support
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
                        (setup-gerbil-highlighting! ed))
                      (echo-message! (app-state-echo app)
                                     (string-append "Opened: " full-path)))))))))))))

;;;============================================================================
;;; REPL commands
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

(def (cmd-eval-expression app)
  "Prompt for an expression in the echo area, eval it in-process."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Eval: " row width)))
    (when (and input (> (string-length input) 0))
      (let-values (((result error?) (eval-expression-string input)))
        (if error?
          (echo-error! echo result)
          (echo-message! echo result))))))

;;;============================================================================
;;; Register all commands
;;;============================================================================

(def (register-all-commands!)
  ;; Navigation
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'backward-word cmd-backward-word)
  (register-command! 'beginning-of-buffer cmd-beginning-of-buffer)
  (register-command! 'end-of-buffer cmd-end-of-buffer)
  (register-command! 'scroll-down cmd-scroll-down)
  (register-command! 'scroll-up cmd-scroll-up)
  (register-command! 'recenter cmd-recenter)
  ;; Editing
  (register-command! 'delete-char cmd-delete-char)
  (register-command! 'backward-delete-char cmd-backward-delete-char)
  (register-command! 'newline cmd-newline)
  (register-command! 'open-line cmd-open-line)
  (register-command! 'undo cmd-undo)
  ;; (indent was here — now replaced by indent-or-complete)
  ;; Kill/Yank
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'yank cmd-yank)
  ;; Mark/Region
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'copy-region cmd-copy-region)
  ;; File
  (register-command! 'find-file cmd-find-file)
  (register-command! 'save-buffer cmd-save-buffer)
  ;; Buffer
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  ;; Window
  (register-command! 'split-window cmd-split-window)
  (register-command! 'split-window-right cmd-split-window-right)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  ;; Search
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'search-backward cmd-search-backward)
  ;; REPL
  (register-command! 'repl cmd-repl)
  (register-command! 'eval-expression cmd-eval-expression)
  ;; Eshell
  (register-command! 'eshell cmd-eshell)
  ;; Shell
  (register-command! 'shell cmd-shell)
  ;; Goto line
  (register-command! 'goto-line cmd-goto-line)
  ;; M-x
  (register-command! 'execute-extended-command cmd-execute-extended-command)
  ;; Help
  (register-command! 'describe-key cmd-describe-key)
  (register-command! 'describe-command cmd-describe-command)
  (register-command! 'list-bindings cmd-list-bindings)
  ;; Buffer list
  (register-command! 'list-buffers cmd-list-buffers)
  ;; Query replace
  (register-command! 'query-replace cmd-query-replace)
  ;; Tab/indent
  (register-command! 'indent-or-complete cmd-indent-or-complete)
  ;; Redo
  (register-command! 'redo cmd-redo)
  ;; Toggles
  (register-command! 'toggle-line-numbers cmd-toggle-line-numbers)
  (register-command! 'toggle-word-wrap cmd-toggle-word-wrap)
  (register-command! 'toggle-whitespace cmd-toggle-whitespace)
  ;; Zoom
  (register-command! 'zoom-in cmd-zoom-in)
  (register-command! 'zoom-out cmd-zoom-out)
  (register-command! 'zoom-reset cmd-zoom-reset)
  ;; Select all
  (register-command! 'select-all cmd-select-all)
  ;; Duplicate line
  (register-command! 'duplicate-line cmd-duplicate-line)
  ;; Comment toggle
  (register-command! 'toggle-comment cmd-toggle-comment)
  ;; Transpose
  (register-command! 'transpose-chars cmd-transpose-chars)
  ;; Word case
  (register-command! 'upcase-word cmd-upcase-word)
  (register-command! 'downcase-word cmd-downcase-word)
  (register-command! 'capitalize-word cmd-capitalize-word)
  ;; Kill word
  (register-command! 'kill-word cmd-kill-word)
  ;; What line
  (register-command! 'what-line cmd-what-line)
  ;; Write file / revert
  (register-command! 'write-file cmd-write-file)
  (register-command! 'revert-buffer cmd-revert-buffer)
  ;; Defun navigation
  (register-command! 'beginning-of-defun cmd-beginning-of-defun)
  (register-command! 'end-of-defun cmd-end-of-defun)
  ;; Delete trailing whitespace
  (register-command! 'delete-trailing-whitespace cmd-delete-trailing-whitespace)
  ;; Count words
  (register-command! 'count-words cmd-count-words)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit))
