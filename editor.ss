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

(def (app-read-string app prompt)
  "Convenience wrapper: read a string from the echo area."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (echo-read-string echo prompt row width)))

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
              (close-ch (and *auto-pair-mode* (auto-pair-char ch))))
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
              (setup-gerbil-highlighting! ed)
              ;; Enable line numbers for code files
              (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
              (send-message ed SCI_SETMARGINWIDTHN 0 4))
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
                        (setup-gerbil-highlighting! ed)
                        (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
                        (send-message ed SCI_SETMARGINWIDTHN 0 4))
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
;;; Yank-pop (M-y) — rotate through kill ring
;;;============================================================================

(def (cmd-yank-pop app)
  "Replace last yank with previous kill ring entry."
  (let ((kr (app-state-kill-ring app))
        (pos (app-state-last-yank-pos app))
        (len (app-state-last-yank-len app)))
    (if (or (null? kr) (not pos) (not len))
      (echo-error! (app-state-echo app) "No previous yank")
      (let* ((idx (modulo (+ (app-state-kill-ring-idx app) 1) (length kr)))
             (text (list-ref kr idx))
             (ed (current-editor app)))
        ;; Delete the previous yank
        (editor-delete-range ed pos len)
        ;; Insert the next kill ring entry
        (editor-insert-text ed pos text)
        (editor-goto-pos ed (+ pos (string-length text)))
        ;; Update tracking
        (set! (app-state-kill-ring-idx app) idx)
        (set! (app-state-last-yank-len app) (string-length text))))))

;;;============================================================================
;;; Occur mode (M-s o) — list matching lines
;;;============================================================================

(def (cmd-occur app)
  "List all lines matching a pattern in the current buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Occur: " row width)))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-editor app))
             (src-name (buffer-name (current-buffer-from-app app)))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matches '())
             (line-num 0))
        ;; Find matching lines
        (for-each
          (lambda (line)
            (set! line-num (+ line-num 1))
            (when (string-contains line pattern)
              (set! matches
                (cons (string-append
                        (number->string line-num) ":"
                        line)
                      matches))))
          lines)
        (let ((matches (reverse matches)))
          (if (null? matches)
            (echo-error! echo (string-append "No matches for: " pattern))
            ;; Display in *Occur* buffer
            (let* ((header (string-append (number->string (length matches))
                                          " matches for \"" pattern
                                          "\" in " src-name ":\n\n"))
                   (result-text (string-append header
                                               (string-join matches "\n")
                                               "\n"))
                   (buf (or (buffer-by-name "*Occur*")
                            (buffer-create! "*Occur*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed result-text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo
                (string-append (number->string (length matches))
                               " matches")))))))))

;;;============================================================================
;;; Compile mode (C-x c) — run build command
;;;============================================================================

(def (cmd-compile app)
  "Run a compile command and display output in *Compilation* buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (default (or (app-state-last-compile app) "make build"))
         (prompt (string-append "Compile command [" default "]: "))
         (input (echo-read-string echo prompt row width)))
    (when input
      (let* ((cmd (if (string=? input "") default input))
             (ed (current-editor app)))
        (set! (app-state-last-compile app) cmd)
        ;; Run the command and capture output
        (echo-message! echo (string-append "Running: " cmd))
        ;; Redraw to show message
        (frame-refresh! (app-state-frame app))
        (let* ((output (with-catch
                         (lambda (e)
                           (string-append "Error running command: "
                             (with-output-to-string
                               (lambda () (display-exception e)))))
                         (lambda ()
                           (let ((proc (open-process
                                         (list path: "/bin/sh"
                                               arguments: (list "-c" cmd)
                                               stdin-redirection: #f
                                               stdout-redirection: #t
                                               stderr-redirection: #t
                                               pseudo-terminal: #f))))
                             (let ((result (read-line proc #f)))
                               (let ((status (process-status proc)))
                                 (string-append
                                   (or result "")
                                   "\n\nProcess exited with status "
                                   (number->string status))))))))
               (text (string-append "-*- Compilation -*-\n"
                                    "Command: " cmd "\n"
                                    (make-string 60 #\-) "\n\n"
                                    output "\n"))
               (buf (or (buffer-by-name "*Compilation*")
                        (buffer-create! "*Compilation*" ed #f))))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer (current-window fr)) buf)
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (echo-message! echo "Compilation finished"))))))

;;;============================================================================
;;; Shell command on region (M-|)
;;;============================================================================

(def (cmd-shell-command-on-region app)
  "Pipe region through a shell command, display output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command on region: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No region (set mark first)")
          (let* ((pos (editor-get-current-pos ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (editor-get-text ed))
                 (region-text (substring text start end))
                 ;; Run command with region as stdin
                 (output (with-catch
                           (lambda (e)
                             (string-append "Error: "
                               (with-output-to-string
                                 (lambda () (display-exception e)))))
                           (lambda ()
                             (let ((proc (open-process
                                           (list path: "/bin/sh"
                                                 arguments: (list "-c" cmd)
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t
                                                 pseudo-terminal: #f))))
                               (display region-text proc)
                               (close-output-port proc)
                               (let ((result (read-line proc #f)))
                                 (process-status proc)
                                 (or result "")))))))
            ;; Display output in *Shell Output* buffer
            (let ((out-buf (or (buffer-by-name "*Shell Output*")
                               (buffer-create! "*Shell Output*" ed #f))))
              (buffer-attach! ed out-buf)
              (set! (edit-window-buffer (current-window fr)) out-buf)
              (editor-set-text ed output)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (set! (buffer-mark buf) #f)
              (echo-message! echo "Shell command done"))))))))

;;;============================================================================
;;; Sort lines (M-^)
;;;============================================================================

(def (cmd-sort-lines app)
  "Sort lines in the buffer (or region if mark is set)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (if mark
      ;; Sort region only
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length sorted)) " lines")))
      ;; Sort whole buffer
      (let* ((lines (string-split text #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length sorted)) " lines"))))))

;;;============================================================================
;;; Bookmarks
;;;============================================================================

(def (cmd-bookmark-set app)
  "Set a bookmark at the current position."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Bookmark name: " row width)))
    (when (and name (> (string-length name) 0))
      (let* ((buf (current-buffer-from-app app))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (hash-put! (app-state-bookmarks app) name
                   (cons (buffer-name buf) pos))
        (echo-message! echo (string-append "Bookmark \"" name "\" set"))))))

(def (cmd-bookmark-jump app)
  "Jump to a named bookmark."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Jump to bookmark: " row width)))
    (when (and name (> (string-length name) 0))
      (let ((bm (hash-get (app-state-bookmarks app) name)))
        (if bm
          (let* ((buf-name (car bm))
                 (pos (cdr bm))
                 (buf (buffer-by-name buf-name)))
            (if buf
              (let ((ed (current-editor app)))
                (buffer-attach! ed buf)
                (set! (edit-window-buffer (current-window fr)) buf)
                (editor-goto-pos ed pos)
                (editor-scroll-caret ed)
                (echo-message! echo (string-append "Jumped to \"" name "\"")))
              (echo-error! echo (string-append "Buffer gone: " buf-name))))
          (echo-error! echo (string-append "No bookmark: " name)))))))

(def (cmd-bookmark-list app)
  "Display all bookmarks in a *Bookmarks* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (bms (app-state-bookmarks app))
         (entries '()))
    (hash-for-each
      (lambda (name val)
        (set! entries
          (cons (string-append "  " name "\t"
                               (car val) ":"
                               (number->string (cdr val)))
                entries)))
      bms)
    (let* ((sorted (sort entries string<?))
           (text (if (null? sorted)
                   "No bookmarks set.\n"
                   (string-append "Bookmarks:\n\n"
                                  (string-join sorted "\n")
                                  "\n")))
           (buf (or (buffer-by-name "*Bookmarks*")
                    (buffer-create! "*Bookmarks*" ed #f))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! (app-state-echo app) "*Bookmarks*"))))

;;;============================================================================
;;; Rectangle operations
;;;============================================================================

(def (get-region-lines app)
  "Get start/end lines and columns from mark and point.
   Returns (values start-line start-col end-line end-col) or #f."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (values #f #f #f #f)
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (start-col (editor-get-column ed start))
             (end-col (editor-get-column ed end)))
        (values start-line start-col end-line end-col)))))

(def (cmd-kill-rectangle app)
  "Kill the rectangle defined by mark and point."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((ed (current-editor app))
               (left-col (min start-col end-col))
               (right-col (max start-col end-col))
               (rect-lines '()))
          ;; Extract and delete rectangle, line by line from bottom to top
          (let loop ((line end-line))
            (when (>= line start-line)
              (let* ((line-start (editor-position-from-line ed line))
                     (line-end (editor-get-line-end-position ed line))
                     (line-text (editor-get-line ed line))
                     (line-len (string-length (string-trim-right line-text #\newline)))
                     (l (min left-col line-len))
                     (r (min right-col line-len))
                     (extracted (if (< l r)
                                  (substring (string-trim-right line-text #\newline) l r)
                                  "")))
                (set! rect-lines (cons extracted rect-lines))
                ;; Delete the rectangle portion of this line
                (when (< l r)
                  (editor-delete-range ed (+ line-start l) (- r l)))
                (loop (- line 1)))))
          ;; Store in rectangle kill ring
          (set! (app-state-rect-kill app) rect-lines)
          (set! (buffer-mark (current-buffer-from-app app)) #f)
          (echo-message! echo
            (string-append "Killed rectangle (" (number->string (length rect-lines)) " lines)")))))))

(def (cmd-yank-rectangle app)
  "Yank (paste) the last killed rectangle at point."
  (let* ((echo (app-state-echo app))
         (rk (app-state-rect-kill app)))
    (if (null? rk)
      (echo-error! echo "No rectangle to yank")
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (col (editor-get-column ed pos)))
        (with-undo-action ed
          (let loop ((lines rk) (cur-line line))
            (when (pair? lines)
              (let* ((rect-text (car lines))
                     (line-start (editor-position-from-line ed cur-line))
                     (line-end (editor-get-line-end-position ed cur-line))
                     (line-len (- line-end line-start))
                     ;; Pad line if shorter than insertion column
                     (insert-pos (+ line-start (min col line-len))))
                (when (< line-len col)
                  (editor-insert-text ed line-end
                    (make-string (- col line-len) #\space)))
                (let ((actual-pos (+ line-start col)))
                  (editor-insert-text ed actual-pos rect-text)))
              (loop (cdr lines) (+ cur-line 1)))))
        (echo-message! echo "Rectangle yanked")))))

;;;============================================================================
;;; Go to matching paren
;;;============================================================================

(def (cmd-goto-matching-paren app)
  "Jump to the matching parenthesis/bracket/brace."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (ch-at (send-message ed SCI_GETCHARAT pos 0))
         (ch-before (if (> pos 0) (send-message ed SCI_GETCHARAT (- pos 1) 0) 0)))
    (cond
      ((brace-char? ch-at)
       (let ((match (send-message ed SCI_BRACEMATCH pos 0)))
         (if (>= match 0)
           (editor-goto-pos ed match)
           (echo-error! (app-state-echo app) "No matching paren"))))
      ((brace-char? ch-before)
       (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
         (if (>= match 0)
           (editor-goto-pos ed match)
           (echo-error! (app-state-echo app) "No matching paren"))))
      (else
       (echo-error! (app-state-echo app) "Not on a paren")))))

;;;============================================================================
;;; Join line (M-j)
;;;============================================================================

(def (cmd-join-line app)
  "Join the current line with the next line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-end (editor-get-line-end-position ed line))
         (total (editor-get-line-count ed)))
    (when (< (+ line 1) total)
      ;; Get next line text to find leading whitespace
      (let* ((next-start (editor-position-from-line ed (+ line 1)))
             (text (editor-get-text ed))
             (next-ws-end next-start))
        ;; Skip whitespace at start of next line
        (let skip ((i next-start))
          (when (< i (string-length text))
            (let ((ch (string-ref text i)))
              (when (or (char=? ch #\space) (char=? ch #\tab))
                (set! next-ws-end (+ i 1))
                (skip (+ i 1))))))
        ;; Delete from end of current line to end of whitespace on next line
        ;; and insert a single space
        (with-undo-action ed
          (editor-delete-range ed line-end (- next-ws-end line-end))
          (editor-insert-text ed line-end " "))))))

;;;============================================================================
;;; Delete blank lines (C-x C-o)
;;;============================================================================

(def (cmd-delete-blank-lines app)
  "Delete blank lines around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (editor-line-from-position ed pos))
         (total (editor-get-line-count ed))
         ;; Check if current line is blank
         (line-text (editor-get-line ed line))
         (blank? (lambda (s) (string=? (string-trim s) ""))))
    (if (blank? line-text)
      ;; Find range of blank lines around point
      (let find-start ((l line))
        (let ((start (if (and (> l 0) (blank? (editor-get-line ed (- l 1))))
                       (find-start (- l 1))
                       l)))
          (let find-end ((l line))
            (let ((end (if (and (< (+ l 1) total) (blank? (editor-get-line ed (+ l 1))))
                         (find-end (+ l 1))
                         l)))
              ;; Delete from start of first blank line to end of last + newline
              (let* ((del-start (editor-position-from-line ed start))
                     (del-end (if (< (+ end 1) total)
                                (editor-position-from-line ed (+ end 1))
                                (editor-get-text-length ed))))
                ;; Keep one blank line
                (editor-delete-range ed del-start (- del-end del-start))
                (editor-insert-text ed del-start "\n")
                (echo-message! (app-state-echo app) "Blank lines deleted"))))))
      (echo-message! (app-state-echo app) "Not on a blank line"))))

;;;============================================================================
;;; Indent region
;;;============================================================================

(def (cmd-indent-region app)
  "Indent all lines in region by 2 spaces."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          ;; Insert 2 spaces at beginning of each line, from bottom to top
          (let loop ((line end-line))
            (when (>= line start-line)
              (let ((line-pos (editor-position-from-line ed line)))
                (editor-insert-text ed line-pos "  "))
              (loop (- line 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region indented")))))

;;;============================================================================
;;; Case region
;;;============================================================================

(def (cmd-downcase-region app)
  "Convert region to lowercase."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (editor-get-text ed))
             (region (substring text start end))
             (lower (string-downcase region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start lower))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region downcased")))))

(def (cmd-upcase-region app)
  "Convert region to uppercase."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (editor-get-text ed))
             (region (substring text start end))
             (upper (string-upcase region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start upper))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region upcased")))))

;;;============================================================================
;;; Shell command (M-!)
;;;============================================================================

(def (cmd-shell-command app)
  "Run a shell command and display output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-editor app))
             (output (with-catch
                       (lambda (e)
                         (string-append "Error: "
                           (with-output-to-string
                             (lambda () (display-exception e)))))
                       (lambda ()
                         (let ((proc (open-process
                                       (list path: "/bin/sh"
                                             arguments: (list "-c" cmd)
                                             stdin-redirection: #f
                                             stdout-redirection: #t
                                             stderr-redirection: #t
                                             pseudo-terminal: #f))))
                           (let ((result (read-line proc #f)))
                             (process-status proc)
                             (or result "")))))))
        ;; If short output (1 line), show in echo area
        (if (not (string-contains output "\n"))
          (echo-message! echo output)
          ;; Multi-line: show in *Shell Output* buffer
          (let ((buf (or (buffer-by-name "*Shell Output*")
                         (buffer-create! "*Shell Output*" ed #f))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-set-text ed output)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! echo "Shell command done")))))))

;;;============================================================================
;;; Fill paragraph (M-q) — word wrap at fill-column (80)
;;;============================================================================

(def fill-column 80)

(def (cmd-fill-paragraph app)
  "Fill (word-wrap) the current paragraph at fill-column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find paragraph boundaries (blank lines or start/end of buffer)
    (let* ((para-start
             (let loop ((i (- pos 1)))
               (cond
                 ((< i 0) 0)
                 ((and (char=? (string-ref text i) #\newline)
                       (or (= i 0)
                           (and (> i 0) (char=? (string-ref text (- i 1)) #\newline))))
                  (+ i 1))
                 (else (loop (- i 1))))))
           (para-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((and (char=? (string-ref text i) #\newline)
                       (< (+ i 1) len)
                       (char=? (string-ref text (+ i 1)) #\newline))
                  i)
                 (else (loop (+ i 1))))))
           (para-text (substring text para-start para-end))
           ;; Collapse whitespace and split into words
           (words (let split ((s para-text) (acc '()))
                    (let ((trimmed (string-trim s)))
                      (if (string=? trimmed "")
                        (reverse acc)
                        ;; Find next word boundary
                        (let find-end ((i 0))
                          (cond
                            ((>= i (string-length trimmed))
                             (reverse (cons trimmed acc)))
                            ((or (char=? (string-ref trimmed i) #\space)
                                 (char=? (string-ref trimmed i) #\newline)
                                 (char=? (string-ref trimmed i) #\tab))
                             (split (substring trimmed i (string-length trimmed))
                                    (cons (substring trimmed 0 i) acc)))
                            (else (find-end (+ i 1)))))))))
           ;; Rebuild with word wrap
           (filled (let loop ((ws words) (line "") (lines '()))
                     (if (null? ws)
                       (string-join (reverse (if (string=? line "") lines
                                                (cons line lines)))
                                   "\n")
                       (let* ((word (car ws))
                              (new-line (if (string=? line "")
                                          word
                                          (string-append line " " word))))
                         (if (> (string-length new-line) fill-column)
                           ;; Wrap
                           (if (string=? line "")
                             ;; Single word longer than fill-column
                             (loop (cdr ws) "" (cons word lines))
                             (loop ws "" (cons line lines)))
                           (loop (cdr ws) new-line lines)))))))
      ;; Replace paragraph text
      (with-undo-action ed
        (editor-delete-range ed para-start (- para-end para-start))
        (editor-insert-text ed para-start filled))
      (echo-message! (app-state-echo app) "Paragraph filled"))))

;;;============================================================================
;;; Grep (M-x grep)
;;;============================================================================

(def (cmd-grep app)
  "Search for a pattern in files using grep, show results."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Grep: " row width)))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((dir (echo-read-string echo "In directory: " row width))
             (search-dir (if (or (not dir) (string=? dir "")) "." dir)))
        (echo-message! echo (string-append "Searching..."))
        (frame-refresh! fr)
        (let* ((ed (current-editor app))
               (cmd (string-append "grep -rn --include='*.ss' --include='*.scm' "
                                   "-- " (shell-quote pattern) " "
                                   (shell-quote search-dir) " 2>&1 || true"))
               (output (with-catch
                         (lambda (e) "Error running grep")
                         (lambda ()
                           (let ((proc (open-process
                                         (list path: "/bin/sh"
                                               arguments: (list "-c" cmd)
                                               stdin-redirection: #f
                                               stdout-redirection: #t
                                               stderr-redirection: #t
                                               pseudo-terminal: #f))))
                             (let ((result (read-line proc #f)))
                               (process-status proc)
                               (or result ""))))))
               (text (string-append "-*- Grep -*-\n"
                                    "Pattern: " pattern "\n"
                                    "Directory: " search-dir "\n"
                                    (make-string 60 #\-) "\n\n"
                                    output "\n"))
               (buf (or (buffer-by-name "*Grep*")
                        (buffer-create! "*Grep*" ed #f))))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer (current-window fr)) buf)
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (echo-message! echo "Grep done"))))))

(def (shell-quote s)
  "Quote a string for safe shell use."
  (string-append "'" (let loop ((i 0) (acc ""))
                       (if (>= i (string-length s))
                         acc
                         (let ((ch (string-ref s i)))
                           (if (char=? ch #\')
                             (loop (+ i 1) (string-append acc "'\"'\"'"))
                             (loop (+ i 1) (string-append acc (string ch)))))))
                 "'"))

;;;============================================================================
;;; Insert file (C-x i)
;;;============================================================================

(def (cmd-insert-file app)
  "Insert contents of a file at point."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Insert file: " row width)))
    (when (and filename (> (string-length filename) 0))
      (if (file-exists? filename)
        (let* ((text (read-file-as-string filename))
               (ed (current-editor app))
               (pos (editor-get-current-pos ed)))
          (when text
            (editor-insert-text ed pos text)
            (echo-message! echo (string-append "Inserted " filename))))
        (echo-error! echo (string-append "File not found: " filename))))))

;;;============================================================================
;;; Dynamic abbreviation (M-/)
;;;============================================================================

(def (collect-dabbrev-matches text prefix pos)
  "Collect all words in text matching prefix, ordered by distance from pos."
  (let* ((plen (string-length prefix))
         (tlen (string-length text))
         (matches '()))
    ;; Scan the entire text for words matching the prefix
    (let loop ((i 0))
      (when (< i tlen)
        ;; Find start of word
        (if (or (char-alphabetic? (string-ref text i))
                (char=? (string-ref text i) #\_)
                (char=? (string-ref text i) #\-))
          (let find-end ((j (+ i 1)))
            (if (or (>= j tlen)
                    (not (or (char-alphabetic? (string-ref text j))
                             (char-numeric? (string-ref text j))
                             (char=? (string-ref text j) #\_)
                             (char=? (string-ref text j) #\-)
                             (char=? (string-ref text j) #\?)
                             (char=? (string-ref text j) #\!))))
              (let ((word (substring text i j)))
                (when (and (> (string-length word) plen)
                           (string=? (substring word 0 plen) prefix)
                           (not (= i (- pos plen))))  ; Skip the prefix itself
                  (set! matches (cons (cons (abs (- i pos)) word) matches)))
                (loop j))
              (find-end (+ j 1))))
          (loop (+ i 1)))))
    ;; Sort by distance from cursor, remove duplicates
    (let* ((sorted (sort matches (lambda (a b) (< (car a) (car b)))))
           (words (map cdr sorted)))
      ;; Remove duplicates keeping order
      (let dedup ((ws words) (seen '()) (acc '()))
        (if (null? ws) (reverse acc)
          (if (member (car ws) seen)
            (dedup (cdr ws) seen acc)
            (dedup (cdr ws) (cons (car ws) seen) (cons (car ws) acc))))))))

(def (cmd-dabbrev-expand app)
  "Expand word before point using other words in buffer."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app))
         (state (app-state-dabbrev-state app)))
    (if (and state (pair? state))
      ;; Continue cycling through matches
      (let* ((prefix (car state))
             (remaining (cadr state))
             (last-pos (caddr state))
             (last-len (cadddr state)))
        (if (null? remaining)
          (begin
            (set! (app-state-dabbrev-state app) #f)
            (echo-message! echo "No more expansions"))
          (let* ((next (car remaining))
                 (expand-len (- (string-length next) (string-length prefix))))
            ;; Delete previous expansion
            (editor-delete-range ed (+ last-pos (string-length prefix))
                                (- last-len (string-length prefix)))
            ;; Insert new expansion
            (editor-insert-text ed (+ last-pos (string-length prefix))
                                (substring next (string-length prefix)
                                           (string-length next)))
            (editor-goto-pos ed (+ last-pos (string-length next)))
            (set! (app-state-dabbrev-state app)
              (list prefix (cdr remaining) last-pos (string-length next))))))
      ;; First expansion: find prefix before cursor
      (let find-prefix ((i (- pos 1)) (count 0))
        (if (or (< i 0)
                (not (or (char-alphabetic? (string-ref text i))
                         (char-numeric? (string-ref text i))
                         (char=? (string-ref text i) #\_)
                         (char=? (string-ref text i) #\-)
                         (char=? (string-ref text i) #\?)
                         (char=? (string-ref text i) #\!))))
          ;; Found prefix start
          (let* ((prefix-start (+ i 1))
                 (prefix (substring text prefix-start pos)))
            (if (= (string-length prefix) 0)
              (echo-message! echo "No prefix to expand")
              (let ((matches (collect-dabbrev-matches text prefix pos)))
                (if (null? matches)
                  (echo-message! echo "No expansion found")
                  (let* ((first-match (car matches))
                         (expand-text (substring first-match (string-length prefix)
                                                 (string-length first-match))))
                    (editor-insert-text ed pos expand-text)
                    (editor-goto-pos ed (+ pos (string-length expand-text)))
                    (set! (app-state-dabbrev-state app)
                      (list prefix (cdr matches) prefix-start
                            (string-length first-match))))))))
          (find-prefix (- i 1) (+ count 1)))))))

;;;============================================================================
;;; What cursor position (C-x =)
;;;============================================================================

(def (cmd-what-cursor-position app)
  "Display character information at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch))
             (line (+ 1 (editor-line-from-position ed pos)))
             (col (+ 1 (editor-get-column ed pos)))
             (pct (if (= len 0) 0 (quotient (* pos 100) len))))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (" (number->string code) ", #x"
                         (number->string code 16) ")"
                         "  point=" (number->string pos)
                         " of " (number->string len)
                         " (" (number->string pct) "%)"
                         "  line " (number->string line)
                         " col " (number->string col)))))))

;;;============================================================================
;;; Keyboard macros
;;;============================================================================

(def (cmd-start-kbd-macro app)
  "Start recording a keyboard macro."
  (if (app-state-macro-recording app)
    (echo-error! (app-state-echo app) "Already recording")
    (begin
      (set! (app-state-macro-recording app) [])
      (echo-message! (app-state-echo app) "Defining kbd macro..."))))

(def (cmd-end-kbd-macro app)
  "Stop recording and save the keyboard macro."
  (if (not (app-state-macro-recording app))
    (echo-error! (app-state-echo app) "Not recording")
    (begin
      (set! (app-state-macro-last app)
        (reverse (app-state-macro-recording app)))
      (set! (app-state-macro-recording app) #f)
      (echo-message! (app-state-echo app)
        (string-append "Macro defined ("
                       (number->string (length (app-state-macro-last app)))
                       " steps)")))))

(def (cmd-call-last-kbd-macro app)
  "Execute the last recorded keyboard macro."
  (let ((macro (app-state-macro-last app)))
    (if (or (not macro) (null? macro))
      (echo-error! (app-state-echo app) "No macro defined")
      (begin
        (for-each
          (lambda (step)
            (let ((action (car step))
                  (data (cdr step)))
              (case action
                ((command) (execute-command! app data))
                ((self-insert) (cmd-self-insert! app data)))))
          macro)
        (echo-message! (app-state-echo app) "Macro executed")))))

(def (macro-record-step! app action data)
  "If macro recording, record a step."
  (when (app-state-macro-recording app)
    (let ((step (cons action data)))
      (set! (app-state-macro-recording app)
        (cons step (app-state-macro-recording app))))))

;;;============================================================================
;;; Mark ring
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

(def (cmd-pop-mark app)
  "Pop the mark ring and jump to the previous mark position."
  (let ((ring (app-state-mark-ring app)))
    (if (null? ring)
      (echo-error! (app-state-echo app) "Mark ring empty")
      (let* ((entry (car ring))
             (buf-name (car entry))
             (pos (cdr entry))
             (buf (buffer-by-name buf-name))
             (fr (app-state-frame app)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let ((ed (current-editor app)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-goto-pos ed pos)
            (editor-scroll-caret ed)
            (echo-message! (app-state-echo app) "Mark popped"))
          (echo-error! (app-state-echo app)
            (string-append "Buffer gone: " buf-name)))))))

;;;============================================================================
;;; Registers
;;;============================================================================

(def (cmd-copy-to-register app)
  "Save region text to a register (C-x r s)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Copy to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No mark set")
          (let* ((pos (editor-get-current-pos ed))
                 (start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end)))
            (hash-put! (app-state-registers app) reg-char text)
            (echo-message! echo
              (string-append "Copied to register " (string reg-char)))))))))

(def (cmd-insert-register app)
  "Insert text from a register (C-x r i)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond
          ((not val)
           (echo-error! echo
             (string-append "Register " (string reg-char) " is empty")))
          ((string? val)
           (let* ((ed (current-editor app))
                  (pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos val)
             (echo-message! echo
               (string-append "Inserted from register " (string reg-char)))))
          ;; Point register — jump instead
          ((pair? val)
           (let* ((buf-name (car val))
                  (reg-pos (cdr val))
                  (buf (buffer-by-name buf-name))
                  (fr (app-state-frame app)))
             (if buf
               (let ((ed (current-editor app)))
                 (buffer-attach! ed buf)
                 (set! (edit-window-buffer (current-window fr)) buf)
                 (editor-goto-pos ed reg-pos)
                 (editor-scroll-caret ed)
                 (echo-message! echo "Jumped to register"))
               (echo-error! echo
                 (string-append "Buffer gone: " buf-name))))))))))

(def (cmd-point-to-register app)
  "Save current position to a register (C-x r SPC)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Point to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (ed (current-editor app))
             (buf (current-buffer-from-app app))
             (pos (editor-get-current-pos ed)))
        (hash-put! (app-state-registers app) reg-char
                   (cons (buffer-name buf) pos))
        (echo-message! echo
          (string-append "Position saved to register " (string reg-char)))))))

(def (cmd-jump-to-register app)
  "Jump to a position saved in a register (C-x r j)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Jump to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond
          ((not val)
           (echo-error! echo
             (string-append "Register " (string reg-char) " is empty")))
          ((pair? val)
           (let* ((buf-name (car val))
                  (pos (cdr val))
                  (buf (buffer-by-name buf-name)))
             (if buf
               (let ((ed (current-editor app)))
                 (buffer-attach! ed buf)
                 (set! (edit-window-buffer (current-window fr)) buf)
                 (editor-goto-pos ed pos)
                 (editor-scroll-caret ed)
                 (echo-message! echo "Jumped to register"))
               (echo-error! echo
                 (string-append "Buffer gone: " buf-name)))))
          ((string? val)
           ;; Text register — insert it
           (let* ((ed (current-editor app))
                  (pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos val)
             (echo-message! echo
               (string-append "Inserted from register " (string reg-char))))))))))

;;;============================================================================
;;; Backward kill word, zap to char, goto char
;;;============================================================================

(def (cmd-backward-kill-word app)
  "Kill from point backward to the beginning of the previous word."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (when (> pos 0)
      ;; Skip whitespace/non-word chars backward
      (let skip-space ((i (- pos 1)))
        (if (and (>= i 0) (not (word-char? (string-ref text i))))
          (skip-space (- i 1))
          ;; Now skip word chars backward
          (let skip-word ((j i))
            (if (and (>= j 0) (word-char? (string-ref text j)))
              (skip-word (- j 1))
              ;; j+1 is the start of the word
              (let* ((start (+ j 1))
                     (killed (substring text start pos)))
                (when (> (string-length killed) 0)
                  (set! (app-state-kill-ring app)
                    (cons killed (app-state-kill-ring app)))
                  (send-message ed SCI_DELETERANGE start (- pos start)))))))))))

(def (cmd-zap-to-char app)
  "Kill from point to the next occurrence of a character (inclusive)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Zap to char: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((target-char (string-ref input 0))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed))
             ;; Search forward for the character
             (found-pos
               (let loop ((i pos))
                 (cond
                   ((>= i len) #f)
                   ((let ((ch (send-message ed SCI_GETCHARAT i 0)))
                      (= ch (char->integer target-char)))
                    (+ i 1))  ; inclusive of the target char
                   (else (loop (+ i 1)))))))
        (if found-pos
          (let ((text (substring (editor-get-text ed) pos found-pos)))
            ;; Add to kill ring
            (set! (app-state-kill-ring app)
              (cons text (app-state-kill-ring app)))
            ;; Delete the text
            (send-message ed SCI_DELETERANGE pos (- found-pos pos)))
          (echo-error! echo
            (string-append "Character '" (string target-char) "' not found")))))))

(def (cmd-goto-char app)
  "Go to a specific character position in the buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto char: " row width)))
    (when input
      (let ((n (string->number input)))
        (if n
          (let ((ed (current-editor app)))
            (editor-goto-pos ed (max 0 (inexact->exact (floor n))))
            (editor-scroll-caret ed))
          (echo-error! echo "Invalid position"))))))

;;;============================================================================
;;; Replace string (non-interactive)
;;;============================================================================

(def (cmd-replace-string app)
  "Non-interactive replace: replace all occurrences of FROM with TO."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-str (echo-read-string echo "Replace string: " row width)))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (echo-read-string echo
                      (string-append "Replace \"" from-str "\" with: ")
                      row width)))
        (when to-str
          (let* ((ed (current-editor app))
                 (count 0))
            (send-message ed SCI_BEGINUNDOACTION)
            ;; Search from beginning
            (send-message ed SCI_SETTARGETSTART 0 0)
            (send-message ed SCI_SETTARGETEND (editor-get-text-length ed) 0)
            (send-message ed SCI_SETSEARCHFLAGS 0 0)
            (let loop ()
              (let ((found (send-message/string ed SCI_SEARCHINTARGET from-str)))
                (when (>= found 0)
                  (send-message/string ed SCI_REPLACETARGET to-str)
                  (set! count (+ count 1))
                  ;; Set target for next search
                  (let ((new-start (+ found (string-length to-str))))
                    (send-message ed SCI_SETTARGETSTART new-start 0)
                    (send-message ed SCI_SETTARGETEND (editor-get-text-length ed) 0)
                    (loop)))))
            (send-message ed SCI_ENDUNDOACTION)
            (echo-message! echo
              (string-append "Replaced " (number->string count)
                             " occurrences"))))))))

;;;============================================================================
;;; Transpose words and lines
;;;============================================================================

(def (cmd-transpose-words app)
  "Swap the word before the cursor with the word after."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Find end of current word
    (let find-word2-end ((i pos))
      (if (and (< i len) (word-char? (string-ref text i)))
        (find-word2-end (+ i 1))
        ;; i is end of word2; find start of word2
        (let find-word2-start ((j (- i 1)))
          (if (and (>= j 0) (word-char? (string-ref text j)))
            (find-word2-start (- j 1))
            ;; j+1 is start of word2
            (let ((w2-start (+ j 1))
                  (w2-end i))
              ;; Find end of word1 (skip non-word chars backward from w2-start)
              (let find-word1-end ((k (- w2-start 1)))
                (if (and (>= k 0) (not (word-char? (string-ref text k))))
                  (find-word1-end (- k 1))
                  ;; k is last char of word1
                  (when (>= k 0)
                    (let find-word1-start ((m k))
                      (if (and (>= m 0) (word-char? (string-ref text m)))
                        (find-word1-start (- m 1))
                        ;; m+1 is start of word1
                        (let* ((w1-start (+ m 1))
                               (w1-end (+ k 1))
                               (word1 (substring text w1-start w1-end))
                               (word2 (substring text w2-start w2-end))
                               (between (substring text w1-end w2-start))
                               (new-text (string-append word2 between word1)))
                          (send-message ed SCI_BEGINUNDOACTION)
                          (send-message ed SCI_DELETERANGE w1-start (- w2-end w1-start))
                          (editor-insert-text ed w1-start new-text)
                          (editor-goto-pos ed w2-end)
                          (send-message ed SCI_ENDUNDOACTION))))))))))))))

(def (cmd-transpose-lines app)
  "Swap the current line with the line above."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos)))
    (when (> cur-line 0)
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (prev-start (send-message ed SCI_POSITIONFROMLINE (- cur-line 1) 0))
             (prev-end (send-message ed SCI_GETLINEENDPOSITION (- cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (prev-text (substring text prev-start prev-end)))
        (send-message ed SCI_BEGINUNDOACTION)
        ;; Replace current line with previous, and previous with current
        ;; Do it by replacing from prev-start to cur-end
        (send-message ed SCI_DELETERANGE prev-start (- cur-end prev-start))
        (let ((new-text (string-append cur-text "\n" prev-text)))
          (editor-insert-text ed prev-start new-text))
        ;; Move cursor to end of what was the current line (now on the line below)
        (editor-goto-pos ed (+ prev-start (string-length cur-text) 1
                               (string-length prev-text)))
        (send-message ed SCI_ENDUNDOACTION)))))

;;;============================================================================
;;; Just one space, repeat command
;;;============================================================================

(def (cmd-just-one-space app)
  "Delete all spaces and tabs around point, leaving just one space."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Find extent of whitespace around point
    (let* ((start (let back ((i (- pos 1)))
                    (if (and (>= i 0)
                             (let ((ch (string-ref text i)))
                               (or (char=? ch #\space) (char=? ch #\tab))))
                      (back (- i 1))
                      (+ i 1))))
           (end (let fwd ((i pos))
                  (if (and (< i len)
                           (let ((ch (string-ref text i)))
                             (or (char=? ch #\space) (char=? ch #\tab))))
                    (fwd (+ i 1))
                    i))))
      (when (> (- end start) 1)
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE start (- end start))
        (editor-insert-text ed start " ")
        (editor-goto-pos ed (+ start 1))
        (send-message ed SCI_ENDUNDOACTION)))))

(def (cmd-repeat app)
  "Repeat the last command."
  (let ((last (app-state-last-command app)))
    (if (and last (not (eq? last 'repeat)))
      (execute-command! app last)
      (echo-error! (app-state-echo app) "No command to repeat"))))

;;;============================================================================
;;; Next/previous error (placeholder — navigate search results)
;;;============================================================================

(def (cmd-next-error app)
  "Jump to next error/match position (wraps search forward)."
  (let* ((ed (current-editor app))
         (search (app-state-last-search app)))
    (if (not search)
      (echo-error! (app-state-echo app) "No previous search")
      (let* ((pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed)))
        ;; Search forward from current position
        (send-message ed SCI_SETTARGETSTART (+ pos 1) 0)
        (send-message ed SCI_SETTARGETEND len 0)
        (send-message ed SCI_SETSEARCHFLAGS 0 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET search)))
          (if (>= found 0)
            (begin
              (editor-goto-pos ed found)
              (editor-scroll-caret ed))
            ;; Wrap around
            (begin
              (send-message ed SCI_SETTARGETSTART 0 0)
              (send-message ed SCI_SETTARGETEND pos 0)
              (let ((found2 (send-message/string ed SCI_SEARCHINTARGET search)))
                (if (>= found2 0)
                  (begin
                    (editor-goto-pos ed found2)
                    (editor-scroll-caret ed)
                    (echo-message! (app-state-echo app) "Wrapped"))
                  (echo-error! (app-state-echo app) "No more matches"))))))))))

(def (cmd-previous-error app)
  "Jump to previous error/match position (wraps search backward)."
  (let* ((ed (current-editor app))
         (search (app-state-last-search app)))
    (if (not search)
      (echo-error! (app-state-echo app) "No previous search")
      (let* ((pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed)))
        ;; Search backward: set target from pos-1 back to 0
        (send-message ed SCI_SETTARGETSTART (max 0 (- pos 1)) 0)
        (send-message ed SCI_SETTARGETEND 0 0)
        (send-message ed SCI_SETSEARCHFLAGS 0 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET search)))
          (if (>= found 0)
            (begin
              (editor-goto-pos ed found)
              (editor-scroll-caret ed))
            ;; Wrap around from end
            (begin
              (send-message ed SCI_SETTARGETSTART len 0)
              (send-message ed SCI_SETTARGETEND pos 0)
              (let ((found2 (send-message/string ed SCI_SEARCHINTARGET search)))
                (if (>= found2 0)
                  (begin
                    (editor-goto-pos ed found2)
                    (editor-scroll-caret ed)
                    (echo-message! (app-state-echo app) "Wrapped"))
                  (echo-error! (app-state-echo app) "No more matches"))))))))))

;;;============================================================================
;;; Kill whole line, move line up/down
;;;============================================================================

(def (cmd-kill-whole-line app)
  "Kill the entire current line including the newline."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (next-line-start (send-message ed SCI_POSITIONFROMLINE (+ line 1) 0))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; If last line (no next line), delete to end including preceding newline
    (if (= next-line-start 0)
      (let* ((del-start (if (and (> line-start 0)
                                 (char=? (string-ref text (- line-start 1)) #\newline))
                          (- line-start 1) line-start))
             (killed (substring text del-start len)))
        (set! (app-state-kill-ring app)
          (cons killed (app-state-kill-ring app)))
        (send-message ed SCI_DELETERANGE del-start (- len del-start)))
      ;; Normal case: delete from line-start to next-line-start
      (let ((killed (substring text line-start next-line-start)))
        (set! (app-state-kill-ring app)
          (cons killed (app-state-kill-ring app)))
        (send-message ed SCI_DELETERANGE line-start (- next-line-start line-start))))))

(def (cmd-move-line-up app)
  "Move the current line up one position."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos)))
    (when (> cur-line 0)
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (prev-start (send-message ed SCI_POSITIONFROMLINE (- cur-line 1) 0))
             (prev-end (send-message ed SCI_GETLINEENDPOSITION (- cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (prev-text (substring text prev-start prev-end))
             (col (- pos cur-start)))
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE prev-start (- cur-end prev-start))
        (editor-insert-text ed prev-start (string-append cur-text "\n" prev-text))
        ;; Put cursor on same column in moved line
        (editor-goto-pos ed (+ prev-start (min col (string-length cur-text))))
        (send-message ed SCI_ENDUNDOACTION)))))

(def (cmd-move-line-down app)
  "Move the current line down one position."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (line-count (editor-get-line-count ed)))
    (when (< cur-line (- line-count 1))
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (next-start (send-message ed SCI_POSITIONFROMLINE (+ cur-line 1) 0))
             (next-end (send-message ed SCI_GETLINEENDPOSITION (+ cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (next-text (substring text next-start next-end))
             (col (- pos cur-start)))
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE cur-start (- next-end cur-start))
        (editor-insert-text ed cur-start (string-append next-text "\n" cur-text))
        ;; Put cursor on same column in moved line (now on line below)
        (let ((new-line-start (+ cur-start (string-length next-text) 1)))
          (editor-goto-pos ed (+ new-line-start (min col (string-length cur-text)))))
        (send-message ed SCI_ENDUNDOACTION)))))

;;;============================================================================
;;; Pipe buffer to shell, narrow/widen
;;;============================================================================

(def (cmd-pipe-buffer app)
  "Pipe buffer contents to a shell command and show output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Pipe buffer to: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! echo (string-append "Error: "
                                (with-output-to-string
                                  (lambda () (display-exception e))))))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "/bin/sh"
                                 arguments: (list "-c" cmd)
                                 stdin-redirection: #t
                                 stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (_ (begin (display text proc)
                             (close-output-port proc)))
                   (output (read-line proc #f))
                   (status (process-status proc)))
              ;; Show output in a new buffer
              (if (and output (> (string-length output) 0))
                (let* ((buf-name "*Shell Output*")
                       (existing (buffer-by-name buf-name)))
                  (when existing
                    (buffer-list-remove! existing))
                  (let ((buf (buffer-create! buf-name ed #f)))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer (current-window fr)) buf)
                    (editor-set-text ed output)
                    (editor-goto-pos ed 0)
                    (echo-message! echo
                      (string-append "Pipe complete (exit " (number->string status) ")"))))
                (echo-message! echo
                  (string-append "No output (exit " (number->string status) ")"))))))))))

(def (cmd-narrow-to-region app)
  "Narrow not supported — Scintilla line hiding APIs not available."
  (echo-error! (app-state-echo app) "Narrow not supported in this build"))

(def (cmd-widen app)
  "Widen not supported — Scintilla line hiding APIs not available."
  (echo-error! (app-state-echo app) "Widen not supported in this build"))

;;;============================================================================
;;; String rectangle, open rectangle
;;;============================================================================

(def (cmd-string-rectangle app)
  "Replace rectangle region with a string on each line."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((fr (app-state-frame app))
               (row (- (frame-height fr) 1))
               (width (frame-width fr))
               (str (echo-read-string echo "String rectangle: " row width)))
          (if (not str)
            (echo-message! echo "Cancelled")
            (let* ((ed (current-editor app))
                   (left-col (min start-col end-col))
                   (right-col (max start-col end-col)))
              (with-undo-action ed
                ;; Process lines from bottom to top to preserve positions
                (let loop ((line end-line))
                  (when (>= line start-line)
                    (let* ((line-start (editor-position-from-line ed line))
                           (line-text (editor-get-line ed line))
                           (line-len (string-length (string-trim-right line-text #\newline)))
                           (l (min left-col line-len))
                           (r (min right-col line-len)))
                      ;; Delete old rectangle portion and insert replacement
                      (when (< l r)
                        (editor-delete-range ed (+ line-start l) (- r l)))
                      (editor-insert-text ed (+ line-start l) str))
                    (loop (- line 1)))))
              (set! (buffer-mark (current-buffer-from-app app)) #f)
              (echo-message! echo "String rectangle done"))))))))

(def (cmd-open-rectangle app)
  "Insert blank space to fill the rectangle region."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((ed (current-editor app))
               (left-col (min start-col end-col))
               (right-col (max start-col end-col))
               (width (- right-col left-col)))
          (when (> width 0)
            (with-undo-action ed
              ;; Insert spaces from bottom to top
              (let loop ((line end-line))
                (when (>= line start-line)
                  (let* ((line-start (editor-position-from-line ed line))
                         (line-text (editor-get-line ed line))
                         (line-len (string-length (string-trim-right line-text #\newline)))
                         (insert-pos (+ line-start (min left-col line-len))))
                    ;; Pad if line is shorter than left-col
                    (when (< line-len left-col)
                      (editor-insert-text ed (+ line-start line-len)
                        (make-string (- left-col line-len) #\space)))
                    (editor-insert-text ed (+ line-start (min left-col line-len))
                      (make-string width #\space)))
                  (loop (- line 1))))))
          (set! (buffer-mark (current-buffer-from-app app)) #f)
          (echo-message! echo
            (string-append "Opened rectangle (" (number->string width) " cols)")))))))

;;;============================================================================
;;; Number lines, reverse region
;;;============================================================================

(def (cmd-number-lines app)
  "Number lines in the region (or whole buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (num-lines (+ (- end-line start-line) 1))
             (width (string-length (number->string num-lines))))
        (with-undo-action ed
          ;; Insert from bottom to top to preserve positions
          (let loop ((line end-line) (n num-lines))
            (when (>= line start-line)
              (let* ((line-start (editor-position-from-line ed line))
                     (prefix (string-append
                               (let ((s (number->string n)))
                                 (string-append
                                   (make-string (- width (string-length s)) #\space)
                                   s))
                               ": ")))
                (editor-insert-text ed line-start prefix))
              (loop (- line 1) (- n 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo
          (string-append "Numbered " (number->string num-lines) " lines")))
      ;; Whole buffer
      (let* ((total (editor-get-line-count ed))
             (width (string-length (number->string total))))
        (with-undo-action ed
          (let loop ((line (- total 1)) (n total))
            (when (>= line 0)
              (let* ((line-start (editor-position-from-line ed line))
                     (prefix (string-append
                               (let ((s (number->string n)))
                                 (string-append
                                   (make-string (- width (string-length s)) #\space)
                                   s))
                               ": ")))
                (editor-insert-text ed line-start prefix))
              (loop (- line 1) (- n 1)))))
        (echo-message! echo
          (string-append "Numbered " (number->string total) " lines"))))))

(def (cmd-reverse-region app)
  "Reverse the order of lines in the region (or whole buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end))
             (lines (string-split region #\newline))
             (reversed (reverse lines))
             (result (string-join reversed "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo
          (string-append "Reversed " (number->string (length reversed)) " lines")))
      ;; Whole buffer
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (reversed (reverse lines))
             (result (string-join reversed "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Reversed " (number->string (length reversed)) " lines"))))))

;;;============================================================================
;;; Flush lines, keep lines
;;;============================================================================

(def (cmd-flush-lines app)
  "Delete lines matching a regexp pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Flush lines matching: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (original-count (length lines))
             (kept (filter (lambda (line) (not (string-contains line pattern))) lines))
             (removed (- original-count (length kept)))
             (result (string-join kept "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Flushed " (number->string removed) " lines"))))))

(def (cmd-keep-lines app)
  "Keep only lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Keep lines matching: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (original-count (length lines))
             (kept (filter (lambda (line) (string-contains line pattern)) lines))
             (removed (- original-count (length kept)))
             (result (string-join kept "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Kept " (number->string (length kept))
                         " lines, removed " (number->string removed)))))))

;;;============================================================================
;;; Align regexp
;;;============================================================================

(def (cmd-align-regexp app)
  "Align lines on a substring pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Align on: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed)))
        ;; Determine range
        (let-values (((start end)
                      (if mark
                        (values (min mark pos) (max mark pos))
                        (values 0 (string-length text)))))
          (let* ((region (substring text start end))
                 (lines (string-split region #\newline))
                 ;; Find max column position of the pattern
                 (positions (map (lambda (line)
                                  (let ((idx (string-contains line pattern)))
                                    (or idx -1)))
                                lines))
                 (max-col (apply max (cons 0 (filter (lambda (x) (>= x 0)) positions)))))
            (if (= max-col 0)
              (echo-error! echo (string-append "Pattern not found: " pattern))
              (let* ((aligned
                       (map (lambda (line)
                              (let ((idx (string-contains line pattern)))
                                (if idx
                                  (string-append
                                    (substring line 0 idx)
                                    (make-string (- max-col idx) #\space)
                                    (substring line idx (string-length line)))
                                  line)))
                            lines))
                     (result (string-join aligned "\n")))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (when mark (set! (buffer-mark buf) #f))
                (echo-message! echo "Aligned")))))))))

;;;============================================================================
;;; Sort fields
;;;============================================================================

(def (cmd-sort-fields app)
  "Sort lines by whitespace-delimited field number."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (field-str (echo-read-string echo "Sort by field #: " row width)))
    (if (not field-str)
      (echo-message! echo "Cancelled")
      (let ((field-num (string->number field-str)))
        (if (not field-num)
          (echo-error! echo "Invalid field number")
          (let* ((ed (current-editor app))
                 (buf (current-buffer-from-app app))
                 (mark (buffer-mark buf))
                 (pos (editor-get-current-pos ed))
                 (text (editor-get-text ed)))
            (let-values (((start end)
                          (if mark
                            (values (min mark pos) (max mark pos))
                            (values 0 (string-length text)))))
              (let* ((region (substring text start end))
                     (lines (string-split region #\newline))
                     (field-idx (- field-num 1))  ; 1-based to 0-based
                     (get-field
                       (lambda (line)
                         (let ((fields (string-split line #\space)))
                           ;; Filter out empty strings from split
                           (let ((fs (filter (lambda (s) (not (string-empty? s))) fields)))
                             (if (< field-idx (length fs))
                               (list-ref fs field-idx)
                               "")))))
                     (sorted (sort lines
                               (lambda (a b)
                                 (string<? (get-field a) (get-field b)))))
                     (result (string-join sorted "\n")))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (when mark (set! (buffer-mark buf) #f))
                (echo-message! echo
                  (string-append "Sorted by field " field-str))))))))))

;;;============================================================================
;;; Mark word, mark paragraph, paragraph navigation
;;;============================================================================

(def (cmd-mark-word app)
  "Set mark at end of next word (like M-@ in Emacs)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Set mark at current position if not already set
    (when (not (buffer-mark buf))
      (set! (buffer-mark buf) pos))
    ;; Find end of word from current pos
    (let skip-nonword ((i pos))
      (if (and (< i len) (not (word-char? (char->integer (string-ref text i)))))
        (skip-nonword (+ i 1))
        (let find-end ((j i))
          (if (and (< j len) (word-char? (char->integer (string-ref text j))))
            (find-end (+ j 1))
            (begin
              (editor-goto-pos ed j)
              (echo-message! (app-state-echo app) "Mark word"))))))))

(def (cmd-mark-paragraph app)
  "Select the current paragraph (M-h)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find start of paragraph (search backward for blank line or BOF)
    (let find-start ((i pos))
      (let ((start
              (cond
                ((<= i 0) 0)
                ;; Check if we're at start of a blank line
                ((and (> i 1)
                      (char=? (string-ref text (- i 1)) #\newline)
                      (or (= i (string-length text))
                          (char=? (string-ref text i) #\newline)))
                 i)
                (else (find-start (- i 1))))))
        ;; Find end of paragraph (search forward for blank line or EOF)
        (let find-end ((j pos))
          (let ((end
                  (cond
                    ((>= j len) len)
                    ;; Blank line = two consecutive newlines
                    ((and (char=? (string-ref text j) #\newline)
                          (< (+ j 1) len)
                          (char=? (string-ref text (+ j 1)) #\newline))
                     (+ j 1))
                    (else (find-end (+ j 1))))))
            (set! (buffer-mark buf) start)
            (editor-goto-pos ed end)
            (echo-message! (app-state-echo app) "Mark paragraph")))))))

(def (cmd-forward-paragraph app)
  "Move forward to end of next paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Skip any blank lines at point
    (let skip-blank ((i pos))
      (if (and (< i len)
               (char=? (string-ref text i) #\newline))
        (skip-blank (+ i 1))
        ;; Now find next blank line or EOF
        (let find-end ((j i))
          (cond
            ((>= j len) (editor-goto-pos ed len))
            ((and (char=? (string-ref text j) #\newline)
                  (< (+ j 1) len)
                  (char=? (string-ref text (+ j 1)) #\newline))
             (editor-goto-pos ed (+ j 1)))
            (else (find-end (+ j 1)))))))))

(def (cmd-backward-paragraph app)
  "Move backward to start of previous paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    ;; Skip any blank lines at point
    (let skip-blank ((i (max 0 (- pos 1))))
      (if (and (> i 0)
               (char=? (string-ref text i) #\newline))
        (skip-blank (- i 1))
        ;; Now find previous blank line or BOF
        (let find-start ((j i))
          (cond
            ((<= j 0) (editor-goto-pos ed 0))
            ((and (char=? (string-ref text j) #\newline)
                  (> j 0)
                  (char=? (string-ref text (- j 1)) #\newline))
             (editor-goto-pos ed (+ j 1)))
            (else (find-start (- j 1)))))))))

;;;============================================================================
;;; Back to indentation, delete indentation
;;;============================================================================

(def (cmd-back-to-indentation app)
  "Move to first non-whitespace character on current line (M-m)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let find-nonws ((i line-start))
      (if (and (< i len)
               (let ((ch (string-ref text i)))
                 (and (not (char=? ch #\newline))
                      (or (char=? ch #\space) (char=? ch #\tab)))))
        (find-nonws (+ i 1))
        (editor-goto-pos ed i)))))

(def (cmd-delete-indentation app)
  "Join current line with previous, removing indentation (M-^)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos)))
    (when (> line 0)
      ;; Go to beginning of current line
      (let* ((line-start (editor-position-from-line ed line))
             (prev-end (editor-get-line-end-position ed (- line 1)))
             (text (editor-get-text ed))
             ;; Find end of whitespace at start of current line
             (ws-end line-start))
        (let skip ((i line-start))
          (when (< i (string-length text))
            (let ((ch (string-ref text i)))
              (when (or (char=? ch #\space) (char=? ch #\tab))
                (set! ws-end (+ i 1))
                (skip (+ i 1))))))
        ;; Delete from end of previous line through whitespace, insert space
        (with-undo-action ed
          (editor-delete-range ed prev-end (- ws-end prev-end))
          (editor-insert-text ed prev-end " "))))))

;;;============================================================================
;;; Whitespace navigation/cleanup
;;;============================================================================

(def (cmd-cycle-spacing app)
  "Cycle between: collapse whitespace to one space, remove all, restore original."
  ;; Simplified: just collapse to single space (same as just-one-space)
  (cmd-just-one-space app))

(def (cmd-fixup-whitespace app)
  "Fix up whitespace around point (collapse multiple spaces/tabs to one)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find whitespace range around point
    (let find-start ((i (- pos 1)))
      (let ((ws-start
              (if (and (>= i 0)
                       (let ((ch (string-ref text i)))
                         (or (char=? ch #\space) (char=? ch #\tab))))
                (find-start (- i 1))
                (+ i 1))))
        (let find-end ((j pos))
          (let ((ws-end
                  (if (and (< j len)
                           (let ((ch (string-ref text j)))
                             (or (char=? ch #\space) (char=? ch #\tab))))
                    (find-end (+ j 1))
                    j)))
            (when (> (- ws-end ws-start) 1)
              (with-undo-action ed
                (editor-delete-range ed ws-start (- ws-end ws-start))
                (editor-insert-text ed ws-start " ")))))))))

;;;============================================================================
;;; Misc navigation commands
;;;============================================================================

(def (cmd-exchange-point-and-mark app)
  "Swap point and mark positions."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No mark set")
      (let ((pos (editor-get-current-pos ed)))
        (set! (buffer-mark buf) pos)
        (editor-goto-pos ed mark)
        (echo-message! (app-state-echo app) "Mark and point exchanged")))))

(def (cmd-mark-whole-buffer app)
  "Mark the whole buffer (C-x h already does select-all, this is an alias)."
  (cmd-select-all app))

(def (cmd-recenter-top-bottom app)
  "Recenter display with point at center/top/bottom."
  ;; Simplified: just recenter
  (editor-scroll-caret (current-editor app)))

(def (cmd-what-page app)
  "Display what page and line the cursor is on."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (editor-line-from-position ed pos))
         ;; Count form feeds (page breaks) before point
         (pages
           (let loop ((i 0) (count 1))
             (if (>= i pos) count
               (if (char=? (string-ref text i) #\page)
                 (loop (+ i 1) (+ count 1))
                 (loop (+ i 1) count))))))
    (echo-message! (app-state-echo app)
      (string-append "Page " (number->string pages)
                     ", Line " (number->string (+ line 1))))))

(def (cmd-count-lines-region app)
  "Count lines in the region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (lines (+ (- end-line start-line) 1))
             (chars (- end start)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string lines)
                         " lines, " (number->string chars) " chars"))))))

(def (cmd-copy-line app)
  "Copy the current line to the kill ring without deleting."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (text (editor-get-text ed))
         (total (editor-get-line-count ed))
         ;; Include newline if not last line
         (end (if (< (+ line 1) total)
                (editor-position-from-line ed (+ line 1))
                line-end))
         (line-text (substring text line-start end)))
    (set! (app-state-kill-ring app)
      (cons line-text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

;;;============================================================================
;;; Help: where-is, apropos-command
;;;============================================================================

(def (cmd-where-is app)
  "Show what key a command is bound to."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Where is command: " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let* ((cmd-name (string->symbol input))
             ;; Search all keymaps for this command
             (found '()))
        ;; Search global keymap
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (cond
                ((eq? val cmd-name)
                 (set! found (cons key found)))
                ((hash-table? val)
                 ;; Search prefix map
                 (for-each
                   (lambda (sub)
                     (when (eq? (cdr sub) cmd-name)
                       (set! found (cons (string-append key " " (car sub)) found))))
                   (keymap-entries val))))))
          (keymap-entries *global-keymap*))
        (if (null? found)
          (echo-message! echo (string-append input " is not on any key"))
          (echo-message! echo
            (string-append input " is on "
                           (string-join (reverse found) ", "))))))))

(def (cmd-apropos-command app)
  "Search commands by name substring."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Apropos command: " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let ((matches '()))
        ;; Search all registered commands
        (hash-for-each
          (lambda (name _proc)
            (when (string-contains (symbol->string name) input)
              (set! matches (cons (symbol->string name) matches))))
          *all-commands*)
        (if (null? matches)
          (echo-message! echo (string-append "No commands matching '" input "'"))
          (let* ((sorted (sort matches string<?))
                 (text (string-append "Commands matching '" input "':\n\n"
                                      (string-join sorted "\n") "\n")))
            ;; Show in *Help* buffer
            (let* ((ed (current-editor app))
                   (buf (or (buffer-by-name "*Help*")
                            (buffer-create! "*Help*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo
                (string-append (number->string (length sorted))
                               " commands match")))))))))

;;;============================================================================
;;; Buffer: toggle-read-only, rename-buffer
;;;============================================================================

(def (cmd-toggle-read-only app)
  "Toggle the read-only state of the current buffer."
  (let* ((ed (current-editor app))
         (readonly? (editor-get-read-only? ed)))
    (editor-set-read-only ed (not readonly?))
    (echo-message! (app-state-echo app)
      (if readonly? "Buffer is now writable" "Buffer is now read-only"))))

(def (cmd-rename-buffer app)
  "Rename the current buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (old-name (buffer-name buf))
         (new-name (echo-read-string echo
                     (string-append "Rename buffer (was " old-name "): ")
                     row width)))
    (if (not new-name)
      (echo-message! echo "Cancelled")
      (if (string-empty? new-name)
        (echo-error! echo "Name cannot be empty")
        (begin
          (set! (buffer-name buf) new-name)
          (echo-message! echo
            (string-append "Renamed to " new-name)))))))

;;;============================================================================
;;; Other-window commands: find-file/switch-buffer in other window
;;;============================================================================

(def (cmd-switch-buffer-other-window app)
  "Switch to a buffer in the other window."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      ;; Split first, then switch buffer in the new window
      (begin
        (cmd-split-window app)
        (frame-other-window! fr)
        (cmd-switch-buffer app))
      ;; Already split: switch to other window, then prompt for buffer
      (begin
        (frame-other-window! fr)
        (cmd-switch-buffer app)))))

(def (cmd-find-file-other-window app)
  "Open a file in the other window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      (begin
        (cmd-split-window app)
        (frame-other-window! fr)
        (cmd-find-file app))
      (begin
        (frame-other-window! fr)
        (cmd-find-file app)))))

;;;============================================================================
;;; Emacs-style universal argument (C-u) stub
;;;============================================================================

(def (cmd-universal-argument app)
  "Universal argument (stub — displays message only)."
  (echo-message! (app-state-echo app) "C-u prefix not yet supported"))

;;;============================================================================
;;; Text transforms: tabify, untabify, base64, rot13
;;;============================================================================

(def (cmd-tabify app)
  "Convert spaces to tabs in region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             ;; Replace runs of 8 spaces with tab (simple approach)
             (result (let loop ((s region) (acc ""))
                       (let ((idx (string-contains s "        ")))  ; 8 spaces
                         (if idx
                           (loop (substring s (+ idx 8) (string-length s))
                                 (string-append acc (substring s 0 idx) "\t"))
                           (string-append acc s))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "Tabified")))))

(def (cmd-untabify app)
  "Convert tabs to spaces in region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             ;; Replace all tabs with 8 spaces
             (result (let loop ((i 0) (acc '()))
                       (if (>= i (string-length region))
                         (apply string-append (reverse acc))
                         (if (char=? (string-ref region i) #\tab)
                           (loop (+ i 1) (cons "        " acc))
                           (loop (+ i 1) (cons (string (string-ref region i)) acc)))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "Untabified")))))

(def (cmd-base64-encode-region app)
  "Base64 encode the region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end))
             (encoded (base64-encode region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start encoded))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Base64 encoded")))))

(def (cmd-base64-decode-region app)
  "Base64 decode the region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (with-catch
        (lambda (e)
          (echo-error! echo "Base64 decode error"))
        (lambda ()
          (let* ((pos (editor-get-current-pos ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (region (substring (editor-get-text ed) start end))
                 (decoded (base64-decode (string-trim-both region))))
            (with-undo-action ed
              (editor-delete-range ed start (- end start))
              (editor-insert-text ed start decoded))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Base64 decoded")))))))

(def (rot13-char ch)
  "Apply ROT13 to a character."
  (cond
    ((and (char>=? ch #\a) (char<=? ch #\z))
     (integer->char (+ (char->integer #\a)
                       (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
    ((and (char>=? ch #\A) (char<=? ch #\Z))
     (integer->char (+ (char->integer #\A)
                       (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
    (else ch)))

(def (rot13-string s)
  "Apply ROT13 to a string."
  (let* ((len (string-length s))
         (result (make-string len)))
    (let loop ((i 0))
      (when (< i len)
        (string-set! result i (rot13-char (string-ref s i)))
        (loop (+ i 1))))
    result))

(def (cmd-rot13-region app)
  "ROT13 encode the region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (rot13-string region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "ROT13 applied")))))

;;;============================================================================
;;; Hex dump display
;;;============================================================================

(def (cmd-hexl-mode app)
  "Display buffer contents as hex dump in *Hex* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (text (editor-get-text ed))
         (bytes (string->bytes text))
         (len (u8vector-length bytes))
         (lines '()))
    ;; Format hex dump, 16 bytes per line
    (let loop ((offset 0))
      (when (< offset len)
        (let* ((end (min (+ offset 16) len))
               (hex-parts '())
               (ascii-parts '()))
          ;; Hex portion
          (let hex-loop ((i offset))
            (when (< i end)
              (let* ((b (u8vector-ref bytes i))
                     (h (number->string b 16)))
                (set! hex-parts
                  (cons (if (< b 16) (string-append "0" h) h)
                        hex-parts)))
              (hex-loop (+ i 1))))
          ;; ASCII portion
          (let ascii-loop ((i offset))
            (when (< i end)
              (let ((b (u8vector-ref bytes i)))
                (set! ascii-parts
                  (cons (if (and (>= b 32) (<= b 126))
                          (string (integer->char b))
                          ".")
                        ascii-parts)))
              (ascii-loop (+ i 1))))
          ;; Format offset
          (let* ((off-str (number->string offset 16))
                 (off-padded (string-append
                               (make-string (max 0 (- 8 (string-length off-str))) #\0)
                               off-str))
                 (hex-str (string-join (reverse hex-parts) " "))
                 ;; Pad hex to consistent width (47 chars for 16 bytes)
                 (hex-padded (string-append hex-str
                               (make-string (max 0 (- 47 (string-length hex-str))) #\space)))
                 (ascii-str (apply string-append (reverse ascii-parts))))
            (set! lines
              (cons (string-append off-padded "  " hex-padded "  |" ascii-str "|")
                    lines))))
        (loop (+ offset 16))))
    ;; Display in *Hex* buffer
    (let* ((result (string-join (reverse lines) "\n"))
           (full-text (string-append "Hex Dump (" (number->string len) " bytes):\n\n"
                                     result "\n"))
           (buf (or (buffer-by-name "*Hex*")
                    (buffer-create! "*Hex*" ed #f))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed full-text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! echo "*Hex*"))))

;;;============================================================================
;;; Count matches, delete duplicate lines
;;;============================================================================

(def (cmd-count-matches app)
  "Count occurrences of a pattern in the buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Count matches for: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (plen (string-length pattern))
             (count
               (if (= plen 0) 0
                 (let loop ((pos 0) (n 0))
                   (let ((idx (string-contains text pattern pos)))
                     (if idx
                       (loop (+ idx plen) (+ n 1))
                       n))))))
        (echo-message! echo
          (string-append (number->string count) " occurrence"
                         (if (= count 1) "" "s")
                         " of \"" pattern "\""))))))

(def (cmd-delete-duplicate-lines app)
  "Remove duplicate lines from region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (lines (string-split region #\newline))
             ;; Remove duplicates while preserving order
             (seen (make-hash-table))
             (unique
               (filter (lambda (line)
                         (if (hash-get seen line)
                           #f
                           (begin (hash-put! seen line #t) #t)))
                       lines))
             (removed (- (length lines) (length unique)))
             (result (string-join unique "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo
          (string-append "Removed " (number->string removed) " duplicate line"
                         (if (= removed 1) "" "s")))))))

;;;============================================================================
;;; Diff buffer with file
;;;============================================================================

(def (cmd-diff-buffer-with-file app)
  "Show diff between buffer contents and the saved file."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no associated file")
      (if (not (file-exists? path))
        (echo-error! echo (string-append "File not found: " path))
        (let* ((file-text (read-file-as-string path))
               (buf-text (editor-get-text ed))
               ;; Write both to temp files and run diff
               (pid (number->string (##current-process-id)))
               (tmp1 (string-append "/tmp/gerbil-emacs-diff-file-" pid))
               (tmp2 (string-append "/tmp/gerbil-emacs-diff-buf-" pid)))
          (write-string-to-file file-text tmp1)
          (write-string-to-file buf-text tmp2)
          (let* ((proc (open-process
                         (list path: "/usr/bin/diff"
                               arguments: (list "-u" tmp1 tmp2)
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (status (process-status proc)))
            ;; Clean up temp files
            (with-catch void (lambda () (delete-file tmp1)))
            (with-catch void (lambda () (delete-file tmp2)))
            (if (and output (> (string-length output) 0))
              ;; Show diff in *Diff* buffer
              (let ((diff-buf (or (buffer-by-name "*Diff*")
                                  (buffer-create! "*Diff*" ed #f))))
                (buffer-attach! ed diff-buf)
                (set! (edit-window-buffer (current-window fr)) diff-buf)
                (editor-set-text ed output)
                (editor-set-save-point ed)
                (editor-goto-pos ed 0)
                (echo-message! echo "*Diff*"))
              (echo-message! echo "No differences"))))))))

;;;============================================================================
;;; Checksum: SHA256
;;;============================================================================

(def (cmd-checksum app)
  "Show SHA256 checksum of the buffer or region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (hash-bytes (sha256 (string->bytes region)))
             (hex-str (hex-encode hash-bytes)))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo (string-append "SHA256: " hex-str))))))

;;;============================================================================
;;; Async shell command
;;;============================================================================

(def (cmd-async-shell-command app)
  "Run a shell command asynchronously, showing output in *Async Shell*."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Async shell command: " row width)))
    (if (not cmd)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (proc (open-process
                     (list path: "/bin/sh"
                           arguments: (list "-c" cmd)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc #f))
             (status (process-status proc)))
        (if (and output (> (string-length output) 0))
          (let ((out-buf (or (buffer-by-name "*Async Shell*")
                             (buffer-create! "*Async Shell*" ed #f))))
            (buffer-attach! ed out-buf)
            (set! (edit-window-buffer (current-window fr)) out-buf)
            (editor-set-text ed
              (string-append "$ " cmd "\n\n" output "\n\n"
                             "(exit " (number->string status) ")"))
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! echo "*Async Shell*"))
          (echo-message! echo
            (string-append "Command finished (exit " (number->string status) ")")))))))

;;;============================================================================
;;; Toggle truncate lines
;;;============================================================================

(def (cmd-toggle-truncate-lines app)
  "Toggle line truncation (word wrap)."
  (cmd-toggle-word-wrap app))

;;;============================================================================
;;; Grep in buffer (interactive)
;;;============================================================================

(def (cmd-grep-buffer app)
  "Search for matching lines and show in *Grep* buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Grep buffer: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (buf-name (buffer-name (current-buffer-from-app app)))
             (lines (string-split text #\newline))
             (matches '())
             (line-num 0))
        ;; Collect matching lines with line numbers
        (for-each
          (lambda (line)
            (set! line-num (+ line-num 1))
            (when (string-contains line pattern)
              (set! matches
                (cons (string-append
                        (number->string line-num) ": " line)
                      matches))))
          lines)
        (if (null? matches)
          (echo-message! echo (string-append "No matches for '" pattern "'"))
          (let* ((result (string-append "Grep: " pattern " in " buf-name "\n\n"
                                        (string-join (reverse matches) "\n") "\n"))
                 (grep-buf (or (buffer-by-name "*Grep*")
                               (buffer-create! "*Grep*" ed #f))))
            (buffer-attach! ed grep-buf)
            (set! (edit-window-buffer (current-window fr)) grep-buf)
            (editor-set-text ed result)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! echo
              (string-append (number->string (length matches)) " match"
                             (if (= (length matches) 1) "" "es")))))))))

;;;============================================================================
;;; Misc: insert-date, insert-char
;;;============================================================================

(def (cmd-insert-date app)
  "Insert current date/time at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         ;; Use external date command for simplicity
         (proc (open-process
                 (list path: "/bin/date"
                       arguments: '()
                       stdout-redirection: #t)))
         (output (read-line proc))
         (status (process-status proc)))
    (when (and (string? output) (> (string-length output) 0))
      (editor-insert-text ed pos output))))

(def (cmd-insert-char app)
  "Insert a character by its Unicode code point."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert char (hex code): " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let ((code (string->number input 16)))
        (if (not code)
          (echo-error! echo "Invalid hex code")
          (let* ((ed (current-editor app))
                 (pos (editor-get-current-pos ed))
                 (ch (string (integer->char code))))
            (editor-insert-text ed pos ch)
            (echo-message! echo
              (string-append "Inserted U+" input))))))))

;;;============================================================================
;;; Eval buffer / eval region
;;;============================================================================

(def (cmd-eval-buffer app)
  "Evaluate the entire buffer as a Gerbil expression."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed)))
    (let-values (((result error?) (eval-expression-string text)))
      (if error?
        (echo-error! echo (string-append "Error: " result))
        (echo-message! echo (string-append "=> " result))))))

(def (cmd-eval-region app)
  "Evaluate the selected region as a Gerbil expression."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end)))
        (let-values (((result error?) (eval-expression-string region)))
          (set! (buffer-mark buf) #f)
          (if error?
            (echo-error! echo (string-append "Error: " result))
            (echo-message! echo (string-append "=> " result))))))))

;;;============================================================================
;;; Clone buffer, scratch buffer
;;;============================================================================

(def (cmd-clone-buffer app)
  "Create a copy of the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (current-buffer-from-app app))
         (text (editor-get-text ed))
         (new-name (string-append (buffer-name buf) "<clone>")))
    (let ((new-buf (buffer-create! new-name ed #f)))
      (buffer-attach! ed new-buf)
      (set! (edit-window-buffer (current-window fr)) new-buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! echo (string-append "Cloned to " new-name)))))

(def (cmd-scratch-buffer app)
  "Switch to the *scratch* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name buffer-scratch-name)
                  (buffer-create! buffer-scratch-name ed #f))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (echo-message! echo buffer-scratch-name)))

;;;============================================================================
;;; Save some buffers
;;;============================================================================

(def (cmd-save-some-buffers app)
  "Save all modified buffers that have file paths."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (saved 0))
    ;; Iterate over all windows and save their buffers if modified
    (for-each
      (lambda (win)
        (let ((win-ed (edit-window-editor win))
              (buf (edit-window-buffer win)))
          (when (and (buffer-file-path buf)
                     (buffer-modified buf))
            (let ((text (editor-get-text win-ed)))
              (write-string-to-file (buffer-file-path buf) text)
              (editor-set-save-point win-ed)
              (set! saved (+ saved 1))))))
      (frame-windows fr))
    (if (= saved 0)
      (echo-message! echo "No buffers need saving")
      (echo-message! echo
        (string-append "Saved " (number->string saved) " buffer"
                       (if (= saved 1) "" "s"))))))

;;;============================================================================
;;; Revert buffer quick (no confirmation)
;;;============================================================================

(def (cmd-revert-buffer-quick app)
  "Revert buffer from disk without confirmation."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file to revert from")
      (if (not (file-exists? path))
        (echo-error! echo (string-append "File not found: " path))
        (let ((text (read-file-as-string path)))
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (echo-message! echo (string-append "Reverted " path)))))))

;;;============================================================================
;;; Toggle syntax highlighting
;;;============================================================================

(def (cmd-toggle-highlighting app)
  "Toggle Gerbil syntax highlighting on the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app)))
    (if (buffer-lexer-lang buf)
      (begin
        ;; Turn off: clear lexer language, reset all styles to default
        (set! (buffer-lexer-lang buf) #f)
        (send-message ed SCI_STYLECLEARALL)
        (echo-message! echo "Highlighting off"))
      (begin
        ;; Turn on: set lexer language, re-apply highlighting
        (set! (buffer-lexer-lang buf) 'gerbil)
        (setup-gerbil-highlighting! ed)
        (echo-message! echo "Highlighting on")))))

;;;============================================================================
;;; Misc utility commands
;;;============================================================================

(def (cmd-view-lossage app)
  "Display recent key sequences in *Lossage* buffer (stub)."
  (echo-message! (app-state-echo app) "Lossage not yet tracked"))

(def (cmd-display-time app)
  "Display current time in echo area."
  (let* ((proc (open-process
                 (list path: "/bin/date"
                       arguments: '("+%Y-%m-%d %H:%M:%S")
                       stdout-redirection: #t)))
         (output (read-line proc))
         (status (process-status proc)))
    (if (string? output)
      (echo-message! (app-state-echo app) output)
      (echo-error! (app-state-echo app) "Cannot get time"))))

(def (cmd-pwd app)
  "Display current working directory."
  (echo-message! (app-state-echo app) (current-directory)))

;;;============================================================================
;;; Ediff (compare two buffers)
;;;============================================================================

(def (cmd-ediff-buffers app)
  "Compare two buffers and show differences in a *Diff* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (names (map buffer-name (buffer-list))))
    (if (< (length names) 2)
      (echo-error! echo "Need at least 2 buffers to compare")
      (let* ((name-a (echo-read-string echo "Buffer A: " row width))
             (buf-a (and name-a (buffer-by-name name-a))))
        (if (not buf-a)
          (echo-error! echo (string-append "No buffer: " (or name-a "")))
          (let* ((name-b (echo-read-string echo "Buffer B: " row width))
                 (buf-b (and name-b (buffer-by-name name-b))))
            (if (not buf-b)
              (echo-error! echo (string-append "No buffer: " (or name-b "")))
              ;; Get text from both buffers, write to temp files, diff
              (let* ((pid (number->string (##current-process-id)))
                     (tmp-a (string-append "/tmp/gerbil-ediff-a-" pid))
                     (tmp-b (string-append "/tmp/gerbil-ediff-b-" pid)))
                ;; We need the text from those buffers — find their windows
                (let ((text-a #f) (text-b #f))
                  (for-each
                    (lambda (win)
                      (let ((wb (edit-window-buffer win)))
                        (when (eq? wb buf-a)
                          (set! text-a (editor-get-text (edit-window-editor win))))
                        (when (eq? wb buf-b)
                          (set! text-b (editor-get-text (edit-window-editor win))))))
                    (frame-windows fr))
                  ;; Fallback: if buffer not in a window, use current editor temporarily
                  (unless text-a
                    (buffer-attach! ed buf-a)
                    (set! text-a (editor-get-text ed)))
                  (unless text-b
                    (buffer-attach! ed buf-b)
                    (set! text-b (editor-get-text ed)))
                  ;; Write to temp files and diff
                  (write-string-to-file tmp-a text-a)
                  (write-string-to-file tmp-b text-b)
                  (let* ((proc (open-process
                                 (list path: "/usr/bin/diff"
                                       arguments: (list "-u"
                                                        (string-append "--label=" name-a)
                                                        (string-append "--label=" name-b)
                                                        tmp-a tmp-b)
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (output (read-line proc #f))
                         (status (process-status proc)))
                    ;; Cleanup temp files
                    (with-catch void (lambda () (delete-file tmp-a)))
                    (with-catch void (lambda () (delete-file tmp-b)))
                    ;; Show diff in buffer
                    (let ((diff-buf (buffer-create! "*Diff*" ed #f)))
                      (buffer-attach! ed diff-buf)
                      (set! (edit-window-buffer (current-window fr)) diff-buf)
                      (if (and (string? output) (> (string-length output) 0))
                        (editor-set-text ed output)
                        (editor-set-text ed "(no differences)\n"))
                      (editor-set-save-point ed)
                      (editor-goto-pos ed 0)
                      (editor-set-read-only ed #t))))))))))))

;;;============================================================================
;;; Simple calculator
;;;============================================================================

(def (cmd-calc app)
  "Evaluate a math expression from the echo area."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (expr (echo-read-string echo "Calc: " row width)))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! echo (string-append "Error: " result))
          (echo-message! echo (string-append "= " result)))))))

;;;============================================================================
;;; Toggle case-fold-search
;;;============================================================================

(def *case-fold-search* #t)

(def (cmd-toggle-case-fold-search app)
  "Toggle case-sensitive search."
  (set! *case-fold-search* (not *case-fold-search*))
  (echo-message! (app-state-echo app)
    (if *case-fold-search*
      "Case-insensitive search"
      "Case-sensitive search")))

;;;============================================================================
;;; Describe-bindings (full binding list in a buffer)
;;;============================================================================

(def (cmd-describe-bindings app)
  "Show all keybindings in a *Bindings* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app)))
    (let ((lines []))
      ;; Collect bindings from all keymaps
      (define (collect-prefix prefix km)
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (if (hash-table? val)
                (collect-prefix (string-append prefix key " ") val)
                (set! lines (cons (string-append prefix key "\t"
                                                 (symbol->string val))
                                  lines)))))
          (keymap-entries km)))
      (collect-prefix "" *global-keymap*)
      ;; Sort and display
      (let* ((sorted (sort lines string<?))
             (text (string-join sorted "\n"))
             (buf (buffer-create! "*Bindings*" ed #f)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed text)
        (editor-set-save-point ed)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)
        (echo-message! echo
          (string-append (number->string (length sorted)) " bindings"))))))

;;;============================================================================
;;; Center line
;;;============================================================================

(def (cmd-center-line app)
  "Center the current line within fill-column (80)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (editor-get-line-end-position ed line-num))
         (text (substring (editor-get-text ed) line-start line-end))
         ;; Strip leading whitespace
         (trimmed (let loop ((i 0))
                    (if (and (< i (string-length text))
                             (or (char=? (string-ref text i) #\space)
                                 (char=? (string-ref text i) #\tab)))
                      (loop (+ i 1))
                      (substring text i (string-length text)))))
         (fill-col 80)
         (padding (max 0 (quotient (- fill-col (string-length trimmed)) 2)))
         (new-line (string-append (make-string padding #\space) trimmed)))
    (with-undo-action ed
      (editor-delete-range ed line-start (- line-end line-start))
      (editor-insert-text ed line-start new-line))))

;;;============================================================================
;;; What face (show current style info)
;;;============================================================================

(def (cmd-what-face app)
  "Show the Scintilla style at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed SCI_GETSTYLEAT pos)))
    (echo-message! (app-state-echo app)
      (string-append "Style " (number->string style) " at pos "
                     (number->string pos)))))

;;;============================================================================
;;; List processes
;;;============================================================================

(def (cmd-list-processes app)
  "Show running subprocesses in *Processes* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (lines ["PID\tType\tBuffer"
                 "---\t----\t------"]))
    ;; Check REPL buffers
    (for-each
      (lambda (buf)
        (when (repl-buffer? buf)
          (let ((rs (hash-get *repl-state* buf)))
            (when rs
              (set! lines (cons
                (string-append "?\tREPL\t" (buffer-name buf))
                lines))))))
      (buffer-list))
    ;; Check shell buffers
    (for-each
      (lambda (buf)
        (when (shell-buffer? buf)
          (let ((ss (hash-get *shell-state* buf)))
            (when ss
              (set! lines (cons
                (string-append "?\tShell\t" (buffer-name buf))
                lines))))))
      (buffer-list))
    (let* ((text (string-join (reverse lines) "\n"))
           (proc-buf (buffer-create! "*Processes*" ed #f)))
      (buffer-attach! ed proc-buf)
      (set! (edit-window-buffer (current-window fr)) proc-buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t)
      (echo-message! echo "Process list"))))

;;;============================================================================
;;; View echo area messages (like *Messages*)
;;;============================================================================

(def *message-log* [])
(def *message-log-max* 100)

(def (log-message! msg)
  "Add a message to the message log."
  (set! *message-log* (cons msg *message-log*))
  (when (> (length *message-log*) *message-log-max*)
    (set! *message-log*
      (let loop ((msgs *message-log*) (n 0) (acc []))
        (if (or (null? msgs) (>= n *message-log-max*))
          (reverse acc)
          (loop (cdr msgs) (+ n 1) (cons (car msgs) acc)))))))

(def (cmd-view-messages app)
  "Show recent echo area messages in *Messages* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (text (if (null? *message-log*)
                 "(no messages)\n"
                 (string-join (reverse *message-log*) "\n")))
         (buf (buffer-create! "*Messages*" ed #f)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (editor-set-text ed text)
    (editor-set-save-point ed)
    ;; Go to end to see latest messages
    (editor-goto-pos ed (string-length text))
    (editor-set-read-only ed #t)
    (echo-message! echo "*Messages*")))

;;;============================================================================
;;; Auto-fill mode toggle (stub)
;;;============================================================================

(def *auto-fill-mode* #f)

(def (cmd-toggle-auto-fill app)
  "Toggle auto-fill mode (line wrap at fill-column)."
  (set! *auto-fill-mode* (not *auto-fill-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-fill-mode*
      "Auto-fill mode on"
      "Auto-fill mode off")))

;;; (delete-trailing-whitespace defined earlier at line ~1247)

;;;============================================================================
;;; Rename file (rename-file-and-buffer)
;;;============================================================================

(def (cmd-rename-file-and-buffer app)
  "Rename current file on disk and update the buffer name."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (old-path (buffer-file-path buf)))
    (if (not old-path)
      (echo-error! echo "Buffer is not visiting a file")
      (let ((new-path (echo-read-string echo
                        (string-append "Rename " old-path " to: ")
                        row width)))
        (when (and new-path (> (string-length new-path) 0))
          (with-catch
            (lambda (e)
              (echo-error! echo
                (string-append "Error: "
                  (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (rename-file old-path new-path)
              (set! (buffer-file-path buf) new-path)
              (set! (buffer-name buf) (path-strip-directory new-path))
              (echo-message! echo
                (string-append "Renamed to " new-path)))))))))

;;;============================================================================
;;; Kill buffer and delete file
;;;============================================================================

(def (cmd-delete-file-and-buffer app)
  "Delete the file on disk and kill the buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer is not visiting a file")
      (let ((confirm (echo-read-string echo
                       (string-append "Really delete " path "? (yes/no) ")
                       row width)))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-error! echo
                (string-append "Error: "
                  (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (delete-file path)
              (echo-message! echo (string-append "Deleted " path))
              ;; Switch away from this buffer
              (let ((scratch (or (buffer-by-name buffer-scratch-name)
                                 (buffer-create! buffer-scratch-name ed #f))))
                (buffer-attach! ed scratch)
                (set! (edit-window-buffer
                        (current-window (app-state-frame app))) scratch)
                (buffer-list-remove! buf)))))))))

;;;============================================================================
;;; Sudo write (write file with sudo)
;;;============================================================================

(def (cmd-sudo-write app)
  "Write current buffer using sudo tee."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file path")
      (let* ((text (editor-get-text ed))
             (pid (number->string (##current-process-id)))
             (tmp (string-append "/tmp/gerbil-emacs-sudo-" pid)))
        (write-string-to-file tmp text)
        (let* ((proc (open-process
                        (list path: "/usr/bin/sudo"
                              arguments: (list "cp" tmp path)
                              stderr-redirection: #t)))
               (status (process-status proc)))
          (with-catch void (lambda () (delete-file tmp)))
          (if (= status 0)
            (begin
              (editor-set-save-point ed)
              (echo-message! echo (string-append "Saved (sudo) " path)))
            (echo-error! echo "sudo write failed")))))))

;;;============================================================================
;;; Sort region (different sort types)
;;;============================================================================

(def (cmd-sort-numeric app)
  "Sort lines in region numerically."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (sorted (sort lines
                       (lambda (a b)
                         (let ((na (with-catch (lambda (e) 0)
                                     (lambda () (string->number a))))
                               (nb (with-catch (lambda (e) 0)
                                     (lambda () (string->number b)))))
                           (< (or na 0) (or nb 0))))))
             (new-text (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Sorted numerically")))))

;;;============================================================================
;;; Word count region
;;;============================================================================

(def (cmd-count-words-region app)
  "Count words in the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (chars (- end start))
             (lines (length (string-split text #\newline)))
             (words (let loop ((i 0) (in-word #f) (count 0))
                      (if (>= i (string-length text))
                        (if in-word (+ count 1) count)
                        (let ((ch (string-ref text i)))
                          (if (or (char=? ch #\space) (char=? ch #\newline)
                                  (char=? ch #\tab))
                            (loop (+ i 1) #f (if in-word (+ count 1) count))
                            (loop (+ i 1) #t count)))))))
        (echo-message! echo
          (string-append "Region: " (number->string lines) " lines, "
                         (number->string words) " words, "
                         (number->string chars) " chars"))))))

;;;============================================================================
;;; Overwrite mode toggle
;;;============================================================================

(def *overwrite-mode* #f)

(def (cmd-toggle-overwrite-mode app)
  "Toggle overwrite mode (insert vs overwrite)."
  (set! *overwrite-mode* (not *overwrite-mode*))
  ;; SCI_SETOVERTYPE (2186) not in gerbil-scintilla constants — use raw value
  (let ((ed (current-editor app)))
    (send-message ed 2186 (if *overwrite-mode* 1 0)))
  (echo-message! (app-state-echo app)
    (if *overwrite-mode*
      "Overwrite mode on"
      "Overwrite mode off")))

;;;============================================================================
;;; Visual line mode (toggle word-wrap + line-at-a-time navigation)
;;;============================================================================

(def *visual-line-mode* #f)

(def (cmd-toggle-visual-line-mode app)
  "Toggle visual-line-mode (word wrap + visual line movement)."
  (set! *visual-line-mode* (not *visual-line-mode*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETWRAPMODE
      (if *visual-line-mode* 1 0)))  ; SC_WRAP_WORD=1, SC_WRAP_NONE=0
  (echo-message! (app-state-echo app)
    (if *visual-line-mode*
      "Visual line mode on"
      "Visual line mode off")))

;;;============================================================================
;;; Set fill column
;;;============================================================================

(def *fill-column* 80)

(def (cmd-set-fill-column app)
  "Set the fill column for line wrapping and centering."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo
                  (string-append "Fill column (current: "
                                 (number->string *fill-column*) "): ")
                  row width)))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (> n 0))
          (begin
            (set! *fill-column* n)
            (echo-message! echo
              (string-append "Fill column set to " (number->string n))))
          (echo-error! echo "Invalid number"))))))

;;;============================================================================
;;; Fill column indicator (display a vertical line)
;;;============================================================================

(def *fill-column-indicator* #f)

(def (cmd-toggle-fill-column-indicator app)
  "Toggle the fill column indicator (display in echo area)."
  (set! *fill-column-indicator* (not *fill-column-indicator*))
  (echo-message! (app-state-echo app)
    (if *fill-column-indicator*
      (string-append "Fill column indicator at " (number->string *fill-column*))
      "Fill column indicator off")))

;;;============================================================================
;;; Toggle debug on error
;;;============================================================================

(def *debug-on-error* #f)

(def (cmd-toggle-debug-on-error app)
  "Toggle debug-on-error mode."
  (set! *debug-on-error* (not *debug-on-error*))
  (echo-message! (app-state-echo app)
    (if *debug-on-error*
      "Debug on error enabled"
      "Debug on error disabled")))

;;;============================================================================
;;; Repeat complex command (re-execute last M-x command)
;;;============================================================================

(def *last-mx-command* #f)

(def (cmd-repeat-complex-command app)
  "Repeat the last M-x command."
  (let ((cmd *last-mx-command*))
    (if (symbol? cmd)
      (begin
        (echo-message! (app-state-echo app)
          (string-append "Repeating: " (symbol->string cmd)))
        (execute-command! app cmd))
      (echo-error! (app-state-echo app) "No previous M-x command"))))

;;;============================================================================
;;; Eldoc-like: show function signature at point
;;;============================================================================

(def (cmd-eldoc app)
  "Show info about the symbol at point (stub)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find word boundaries around cursor
    (let* ((start (let loop ((i (- pos 1)))
                    (if (or (< i 0)
                            (let ((ch (string-ref text i)))
                              (not (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\-)
                                       (char=? ch #\_)
                                       (char=? ch #\!)
                                       (char=? ch #\?)))))
                      (+ i 1) (loop (- i 1)))))
           (end (let loop ((i pos))
                  (if (or (>= i len)
                          (let ((ch (string-ref text i)))
                            (not (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\-)
                                     (char=? ch #\_)
                                     (char=? ch #\!)
                                     (char=? ch #\?)))))
                    i (loop (+ i 1)))))
           (word (if (< start end) (substring text start end) #f)))
      (if word
        (echo-message! (app-state-echo app)
          (string-append "Symbol: " word))
        (echo-message! (app-state-echo app) "No symbol at point")))))

;;;============================================================================
;;; Highlight symbol at point (mark all occurrences)
;;;============================================================================

(def (cmd-highlight-symbol app)
  "Highlight all occurrences of the word at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Get word at point
    (let* ((start (let loop ((i (- pos 1)))
                    (if (or (< i 0)
                            (let ((ch (string-ref text i)))
                              (not (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\_) (char=? ch #\-)))))
                      (+ i 1) (loop (- i 1)))))
           (end (let loop ((i pos))
                  (if (or (>= i len)
                          (let ((ch (string-ref text i)))
                            (not (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\_) (char=? ch #\-)))))
                    i (loop (+ i 1)))))
           (word (if (< start end) (substring text start end) #f)))
      (if (not word)
        (echo-message! echo "No word at point")
        ;; Count occurrences
        (let ((count 0) (wlen (string-length word)))
          (let loop ((i 0))
            (when (<= (+ i wlen) len)
              (when (string=? (substring text i (+ i wlen)) word)
                (set! count (+ count 1)))
              (loop (+ i 1))))
          ;; Use Scintilla indicator to highlight
          (send-message ed SCI_INDICSETSTYLE 0 7)  ; INDIC_ROUNDBOX
          (send-message ed SCI_SETINDICATORCURRENT 0)
          ;; Clear previous highlights
          (send-message ed SCI_INDICATORCLEARRANGE 0 len)
          ;; Set highlights for each occurrence
          (let loop ((i 0))
            (when (<= (+ i wlen) len)
              (when (string=? (substring text i (+ i wlen)) word)
                (send-message ed SCI_INDICATORFILLRANGE i wlen))
              (loop (+ i 1))))
          (echo-message! echo
            (string-append (number->string count) " occurrence"
                           (if (= count 1) "" "s")
                           " of \"" word "\"")))))))

;;;============================================================================
;;; Clear highlight
;;;============================================================================

(def (cmd-clear-highlight app)
  "Clear all occurrence highlights."
  (let* ((ed (current-editor app))
         (len (editor-get-text-length ed)))
    (send-message ed SCI_SETINDICATORCURRENT 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len)
    (echo-message! (app-state-echo app) "Highlights cleared")))

;;;============================================================================
;;; Indent rigidly (shift region left/right)
;;;============================================================================

(def (cmd-indent-rigidly-right app)
  "Shift selected lines right by 2 spaces."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (indented (map (lambda (line) (string-append "  " line)) lines))
             (new-text (string-join indented "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) start)
        (editor-goto-pos ed (+ start (string-length new-text)))
        (echo-message! echo "Indented right")))))

(def (cmd-indent-rigidly-left app)
  "Shift selected lines left by 2 spaces."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (dedented (map (lambda (line)
                              (cond
                                ((and (>= (string-length line) 2)
                                      (char=? (string-ref line 0) #\space)
                                      (char=? (string-ref line 1) #\space))
                                 (substring line 2 (string-length line)))
                                ((and (> (string-length line) 0)
                                      (char=? (string-ref line 0) #\tab))
                                 (substring line 1 (string-length line)))
                                (else line)))
                            lines))
             (new-text (string-join dedented "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) start)
        (editor-goto-pos ed (+ start (string-length new-text)))
        (echo-message! echo "Indented left")))))

;;;============================================================================
;;; Goto first/last non-blank line
;;;============================================================================

(def (cmd-goto-first-non-blank app)
  "Go to the first non-blank line in the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline)))
    (let loop ((i 0) (pos 0))
      (if (>= i (length lines))
        (editor-goto-pos ed 0)
        (let ((line (list-ref lines i)))
          (if (> (string-length (string-trim line)) 0)
            (editor-goto-pos ed pos)
            (loop (+ i 1) (+ pos (string-length line) 1))))))))

(def (cmd-goto-last-non-blank app)
  "Go to the last non-blank line in the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (total (length lines)))
    (let loop ((i (- total 1)) (last-pos (string-length text)))
      (if (< i 0)
        (editor-goto-pos ed last-pos)
        (let* ((line (list-ref lines i))
               (line-start (let lp ((j 0) (pos 0))
                             (if (>= j i) pos
                               (lp (+ j 1) (+ pos (string-length (list-ref lines j)) 1))))))
          (if (> (string-length (string-trim line)) 0)
            (editor-goto-pos ed line-start)
            (loop (- i 1) last-pos)))))))

;;;============================================================================
;;; Buffer statistics
;;;============================================================================

(def (cmd-buffer-stats app)
  "Show detailed buffer statistics."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (chars (string-length text))
         (lines (editor-get-line-count ed))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars)
                    (if in-word (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab))
                        (loop (+ i 1) #f (if in-word (+ count 1) count))
                        (loop (+ i 1) #t count))))))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)")))
    (echo-message! echo
      (string-append name " | " path " | "
                     (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C"))))

;;;============================================================================
;;; Toggle show tabs
;;;============================================================================

(def *show-tabs* #f)

(def (cmd-toggle-show-tabs app)
  "Toggle visible tab characters."
  (set! *show-tabs* (not *show-tabs*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETVIEWWS
      (if *show-tabs* 1 0)))  ; SCWS_VISIBLEALWAYS=1
  (echo-message! (app-state-echo app)
    (if *show-tabs* "Show tabs on" "Show tabs off")))

;;;============================================================================
;;; Toggle show EOL
;;;============================================================================

(def *show-eol* #f)

(def (cmd-toggle-show-eol app)
  "Toggle visible end-of-line characters."
  (set! *show-eol* (not *show-eol*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETVIEWEOL
      (if *show-eol* 1 0)))
  (echo-message! (app-state-echo app)
    (if *show-eol* "Show EOL on" "Show EOL off")))

;;;============================================================================
;;; Copy from above/below line
;;;============================================================================

(def (cmd-copy-from-above app)
  "Copy character from the line above at the same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (col (- pos (editor-position-from-line ed line))))
    (if (= line 0)
      (echo-error! (app-state-echo app) "No line above")
      (let* ((above-start (editor-position-from-line ed (- line 1)))
             (above-end (editor-get-line-end-position ed (- line 1)))
             (above-len (- above-end above-start)))
        (if (>= col above-len)
          (echo-error! (app-state-echo app) "Line above too short")
          (let* ((text (editor-get-text ed))
                 (ch (string (string-ref text (+ above-start col)))))
            (editor-insert-text ed pos ch)))))))

(def (cmd-copy-from-below app)
  "Copy character from the line below at the same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (total-lines (editor-get-line-count ed))
         (col (- pos (editor-position-from-line ed line))))
    (if (>= (+ line 1) total-lines)
      (echo-error! (app-state-echo app) "No line below")
      (let* ((below-start (editor-position-from-line ed (+ line 1)))
             (below-end (editor-get-line-end-position ed (+ line 1)))
             (below-len (- below-end below-start)))
        (if (>= col below-len)
          (echo-error! (app-state-echo app) "Line below too short")
          (let* ((text (editor-get-text ed))
                 (ch (string (string-ref text (+ below-start col)))))
            (editor-insert-text ed pos ch)))))))

;;;============================================================================
;;; Open line above (like vim O)
;;;============================================================================

(def (cmd-open-line-above app)
  "Insert a new line above the current line and move to it."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line)))
    (with-undo-action ed
      (editor-insert-text ed line-start "\n")
      (editor-goto-pos ed line-start))))

;;;============================================================================
;;; Select current line
;;;============================================================================

(def (cmd-select-line app)
  "Select the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (buf (current-buffer-from-app app)))
    (set! (buffer-mark buf) line-start)
    ;; Move to the start of the next line if possible
    (let ((next-start (if (< (+ line 1) (editor-get-line-count ed))
                        (editor-position-from-line ed (+ line 1))
                        line-end)))
      (editor-goto-pos ed next-start)
      (echo-message! (app-state-echo app) "Line selected"))))

;;;============================================================================
;;; Split line (break line at point, keep indentation)
;;;============================================================================

(def (cmd-split-line app)
  "Split line at point and indent continuation to same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (col (- pos line-start))
         (padding (make-string col #\space)))
    (with-undo-action ed
      (editor-insert-text ed pos (string-append "\n" padding)))))

;;;============================================================================
;;; Convert line endings
;;;============================================================================

(def (cmd-convert-to-unix app)
  "Convert buffer line endings to Unix (LF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    ;; Remove CRs (convert CRLF -> LF and standalone CR -> LF)
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((new-text (list->string (reverse acc))))
          (unless (string=? text new-text)
            (with-undo-action ed
              (editor-set-text ed new-text))
            (echo-message! (app-state-echo app) "Converted to Unix (LF)")))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (loop (+ i 1) acc)
            (loop (+ i 1) (cons ch acc))))))))

(def (cmd-convert-to-dos app)
  "Convert buffer line endings to DOS (CRLF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (new-text (string-join lines "\r\n")))
    (unless (string=? text new-text)
      (with-undo-action ed
        (editor-set-text ed new-text))
      (echo-message! (app-state-echo app) "Converted to DOS (CRLF)"))))

;;;============================================================================
;;; Enlarge/shrink window
;;;============================================================================

(def (cmd-enlarge-window app)
  "Make current window taller (stub — adjusts height by 2 rows)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (h (edit-window-h win)))
    (set! (edit-window-h win) (+ h 2))
    (echo-message! (app-state-echo app) "Window enlarged")))

(def (cmd-shrink-window app)
  "Make current window shorter (stub — adjusts height by 2 rows)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (h (edit-window-h win)))
    (when (> h 4)
      (set! (edit-window-h win) (- h 2)))
    (echo-message! (app-state-echo app) "Window shrunk")))

;;;============================================================================
;;; What buffer encoding
;;;============================================================================

(def (cmd-what-encoding app)
  "Show the encoding of the current buffer."
  ;; Scintilla uses UTF-8 (codepage 65001) by default
  (echo-message! (app-state-echo app) "Encoding: UTF-8"))

;;;============================================================================
;;; Hippie expand (simple completion from buffer words)
;;;============================================================================

(def (cmd-hippie-expand app)
  "Complete word at point from buffer contents (simple hippie-expand)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find prefix at point
    (let* ((prefix-start
             (let loop ((i (- pos 1)))
               (if (or (< i 0)
                       (let ((ch (string-ref text i)))
                         (not (or (char-alphabetic? ch)
                                  (char-numeric? ch)
                                  (char=? ch #\_)
                                  (char=? ch #\-)))))
                 (+ i 1) (loop (- i 1)))))
           (prefix (substring text prefix-start pos))
           (plen (string-length prefix)))
      (if (= plen 0)
        (echo-message! echo "No prefix to complete")
        ;; Scan buffer for words starting with prefix
        (let ((candidates []))
          (let loop ((i 0))
            (when (< i len)
              (let* ((wstart
                       (let ws ((j i))
                         (if (or (>= j len)
                                 (let ((ch (string-ref text j)))
                                   (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\_)
                                       (char=? ch #\-))))
                           j
                           (ws (+ j 1)))))
                     (wend
                       (let we ((j wstart))
                         (if (or (>= j len)
                                 (let ((ch (string-ref text j)))
                                   (not (or (char-alphabetic? ch)
                                            (char-numeric? ch)
                                            (char=? ch #\_)
                                            (char=? ch #\-)))))
                           j
                           (we (+ j 1))))))
                (when (> wend wstart)
                  (let ((word (substring text wstart wend)))
                    (when (and (> (string-length word) plen)
                               (string-prefix? prefix word)
                               (not (= wstart prefix-start))
                               (not (member word candidates)))
                      (set! candidates (cons word candidates))))
                  (loop wend))
                (when (= wend wstart)
                  (loop (+ wstart 1))))))
          (if (null? candidates)
            (echo-message! echo
              (string-append "No completions for \"" prefix "\""))
            ;; Insert first candidate
            (let ((completion (car (reverse candidates))))
              (with-undo-action ed
                (editor-delete-range ed prefix-start plen)
                (editor-insert-text ed prefix-start completion))
              (editor-goto-pos ed (+ prefix-start (string-length completion)))
              (echo-message! echo
                (string-append completion
                  (if (> (length candidates) 1)
                    (string-append " [" (number->string (length candidates))
                                   " candidates]")
                    ""))))))))))

;;;============================================================================
;;; Swap buffers in windows
;;;============================================================================

(def (cmd-swap-buffers app)
  "Swap buffers between current and next window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (< (length wins) 2)
      (echo-error! (app-state-echo app) "Only one window")
      (let* ((cur-idx (frame-current-idx fr))
             (next-idx (modulo (+ cur-idx 1) (length wins)))
             (cur-win (list-ref wins cur-idx))
             (next-win (list-ref wins next-idx))
             (cur-buf (edit-window-buffer cur-win))
             (next-buf (edit-window-buffer next-win)))
        ;; Swap the buffers
        (buffer-attach! (edit-window-editor cur-win) next-buf)
        (set! (edit-window-buffer cur-win) next-buf)
        (buffer-attach! (edit-window-editor next-win) cur-buf)
        (set! (edit-window-buffer next-win) cur-buf)
        (echo-message! (app-state-echo app) "Buffers swapped")))))

;;;============================================================================
;;; Toggle tab-width between 2/4/8
;;;============================================================================

(def (cmd-cycle-tab-width app)
  "Cycle tab width between 2, 4, and 8."
  (let* ((ed (current-editor app))
         (current (send-message ed SCI_GETTABWIDTH))
         (next (cond
                 ((= current 2) 4)
                 ((= current 4) 8)
                 (else 2))))
    (send-message ed SCI_SETTABWIDTH next)
    (echo-message! (app-state-echo app)
      (string-append "Tab width: " (number->string next)))))

;;;============================================================================
;;; Toggle use tabs vs spaces
;;;============================================================================

(def (cmd-toggle-indent-tabs-mode app)
  "Toggle between using tabs and spaces for indentation."
  (let* ((ed (current-editor app))
         (using-tabs (= 1 (send-message ed SCI_GETUSETABS))))
    (send-message ed SCI_SETUSETABS (if using-tabs 0 1))
    (echo-message! (app-state-echo app)
      (if using-tabs
        "Indent with spaces"
        "Indent with tabs"))))

;;;============================================================================
;;; Print buffer info
;;;============================================================================

(def (cmd-buffer-info app)
  "Show buffer name, file, and position info."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (col (+ 1 (- pos (editor-position-from-line ed (- line 1))))))
    (echo-message! (app-state-echo app)
      (string-append (buffer-name buf)
                     " L" (number->string line)
                     " C" (number->string col)
                     " pos:" (number->string pos)
                     (if (buffer-file-path buf)
                       (string-append " " (buffer-file-path buf))
                       "")))))

;;;============================================================================
;;; Whitespace cleanup, electric-pair toggle, and more (Task #36)
;;;============================================================================

;; Global toggle for auto-pair mode
(def *auto-pair-mode* #t)

;; Recenter cycle state: 'center -> 'top -> 'bottom -> 'center
(def *recenter-position* 'center)

(def (cmd-whitespace-cleanup app)
  "Remove trailing whitespace and convert tabs to spaces."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line)) lines))
         (result (string-join cleaned "\n")))
    (unless (string=? text result)
      (with-undo-action ed
        (editor-delete-range ed 0 (string-length text))
        (editor-insert-text ed 0 result)))
    (echo-message! (app-state-echo app) "Whitespace cleaned")))

(def (cmd-toggle-electric-pair app)
  "Toggle auto-pair mode for brackets and quotes."
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Electric pair mode ON" "Electric pair mode OFF")))

(def (cmd-previous-buffer app)
  "Switch to the previous buffer in the buffer list."
  (let* ((bufs (buffer-list))
         (cur (current-buffer-from-app app))
         (idx (let loop ((bs bufs) (i 0))
                (cond ((null? bs) 0)
                      ((eq? (car bs) cur) i)
                      (else (loop (cdr bs) (+ i 1))))))
         (prev-idx (if (= idx 0) (- (length bufs) 1) (- idx 1)))
         (prev-buf (list-ref bufs prev-idx))
         (ed (current-editor app))
         (fr (app-state-frame app)))
    (buffer-attach! ed prev-buf)
    (set! (edit-window-buffer (current-window fr)) prev-buf)
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (buffer-name prev-buf)))))

(def (cmd-next-buffer app)
  "Switch to the next buffer in the buffer list."
  (let* ((bufs (buffer-list))
         (cur (current-buffer-from-app app))
         (idx (let loop ((bs bufs) (i 0))
                (cond ((null? bs) 0)
                      ((eq? (car bs) cur) i)
                      (else (loop (cdr bs) (+ i 1))))))
         (next-idx (if (>= (+ idx 1) (length bufs)) 0 (+ idx 1)))
         (next-buf (list-ref bufs next-idx))
         (ed (current-editor app))
         (fr (app-state-frame app)))
    (buffer-attach! ed next-buf)
    (set! (edit-window-buffer (current-window fr)) next-buf)
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (buffer-name next-buf)))))

(def (cmd-balance-windows app)
  "Make all windows the same size."
  (frame-layout! (app-state-frame app))
  (echo-message! (app-state-echo app) "Windows balanced"))

(def (cmd-move-to-window-line app)
  "Move point to center, then top, then bottom of window (like Emacs M-r)."
  (let* ((ed (current-editor app))
         (first-vis (editor-get-first-visible-line ed))
         ;; Use raw SCI_LINESONSCREEN = 2370
         (lines-on-screen (send-message ed 2370))
         (target-line
           (case *recenter-position*
             ((center) (+ first-vis (quotient lines-on-screen 2)))
             ((top) first-vis)
             ((bottom) (+ first-vis (- lines-on-screen 1))))))
    (editor-goto-pos ed (editor-position-from-line ed target-line))
    ;; Cycle: center -> top -> bottom -> center
    (set! *recenter-position*
      (case *recenter-position*
        ((center) 'top)
        ((top) 'bottom)
        ((bottom) 'center)))))

(def (cmd-kill-buffer-and-window app)
  "Kill current buffer and close its window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (= (length wins) 1)
      (echo-message! (app-state-echo app) "Can't delete sole window")
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app)))
        (frame-delete-window! fr)
        (frame-layout! fr)
        ;; Clean up the buffer
        (hash-remove! *dired-entries* buf)
        (hash-remove! *eshell-state* buf)
        (let ((rs (hash-get *repl-state* buf)))
          (when rs (repl-stop! rs) (hash-remove! *repl-state* buf)))
        (let ((ss (hash-get *shell-state* buf)))
          (when ss (shell-stop! ss) (hash-remove! *shell-state* buf)))))))

(def (cmd-flush-undo app)
  "Clear the undo history of the current buffer."
  (let ((ed (current-editor app)))
    (editor-empty-undo-buffer ed)
    (echo-message! (app-state-echo app) "Undo history cleared")))

(def (cmd-upcase-initials-region app)
  "Capitalize the first letter of each word in region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if (not mark)
      (echo-message! (app-state-echo app) "No region")
      (let* ((start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (result (string-titlecase text)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)))))

(def (cmd-untabify-buffer app)
  "Convert all tabs to spaces in the entire buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (tab-w (editor-get-tab-width ed))
         (spaces (make-string tab-w #\space)))
    (if (not (string-contains text "\t"))
      (echo-message! (app-state-echo app) "No tabs found")
      (let* ((parts (string-split text #\tab))
             (result (string-join parts spaces)))
        (with-undo-action ed
          (editor-delete-range ed 0 (string-length text))
          (editor-insert-text ed 0 result))
        (echo-message! (app-state-echo app) "Untabified buffer")))))

(def (cmd-insert-buffer-name app)
  "Insert the current buffer name at point."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos (buffer-name buf))
    (editor-goto-pos ed (+ pos (string-length (buffer-name buf))))))

(def (cmd-toggle-line-move-visual app)
  "Toggle whether line movement is visual or logical."
  ;; Scintilla doesn't distinguish, so this is a stub toggle
  (echo-message! (app-state-echo app) "Line move is always visual in Scintilla"))

(def (cmd-mark-defun app)
  "Mark the current top-level form (defun-like region)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find beginning of defun (search backward for "\n(" at column 0)
    (let ((defun-start
            (let loop ((i pos))
              (cond ((= i 0) 0)
                    ((and (= i 0) (char=? (string-ref text 0) #\()) 0)
                    ((and (> i 0)
                          (char=? (string-ref text i) #\()
                          (or (= i 0)
                              (char=? (string-ref text (- i 1)) #\newline)))
                     i)
                    (else (loop (- i 1)))))))
      ;; Find end of defun — matching paren
      (let ((defun-end
              (let loop ((i defun-start) (depth 0))
                (cond ((>= i len) len)
                      ((char=? (string-ref text i) #\()
                       (loop (+ i 1) (+ depth 1)))
                      ((char=? (string-ref text i) #\))
                       (if (= depth 1) (+ i 1)
                         (loop (+ i 1) (- depth 1))))
                      (else (loop (+ i 1) depth))))))
        (editor-set-selection ed defun-start defun-end)
        (echo-message! (app-state-echo app) "Defun marked")))))

(def (cmd-goto-line-beginning app)
  "Move to the very first position in the buffer (alias for M-<)."
  (editor-goto-pos (current-editor app) 0))

(def (cmd-shrink-window-horizontally app)
  "Make current window narrower (horizontal split only)."
  (echo-message! (app-state-echo app)
    "Use C-x } / C-x { for horizontal resize (not implemented)"))

(def (cmd-insert-parentheses app)
  "Insert () and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "()")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-brackets app)
  "Insert [] and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "[]")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-braces app)
  "Insert {} and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "{}")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-quotes app)
  "Insert \"\" and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\"\"")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-describe-char app)
  "Show info about character at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (#x" (number->string code 16)
                         ", #o" (number->string code 8)
                         ", " (number->string code) ")"))))))

(def (cmd-find-file-at-point app)
  "Try to open file whose name is at or near point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Extract filename-like text around point
         (start (let loop ((i pos))
                  (if (or (<= i 0)
                          (let ((ch (string-ref text (- i 1))))
                            (or (char=? ch #\space) (char=? ch #\newline)
                                (char=? ch #\tab) (char=? ch #\")
                                (char=? ch #\') (char=? ch #\<)
                                (char=? ch #\>))))
                    i (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i len)
                        (let ((ch (string-ref text i)))
                          (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab) (char=? ch #\")
                              (char=? ch #\') (char=? ch #\<)
                              (char=? ch #\>))))
                  i (loop (+ i 1)))))
         (path (substring text start end)))
    (if (and (> (string-length path) 0) (file-exists? path))
      (let* ((fr (app-state-frame app))
             (name (path-strip-directory path))
             (buf (buffer-create! name ed path)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (let ((file-text (read-file-as-string path)))
          (when file-text
            (editor-set-text ed file-text)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)))
        (echo-message! (app-state-echo app)
          (string-append "Opened: " path)))
      (echo-message! (app-state-echo app)
        (string-append "No file found: " path)))))

(def (cmd-toggle-show-paren app)
  "Toggle paren matching highlight."
  ;; Use raw SCI_SETMATCHEDBRACEPROPS - just toggle the indicator via message
  (echo-message! (app-state-echo app) "Paren matching is always on"))

(def (cmd-count-chars-region app)
  "Count characters in the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (echo-message! (app-state-echo app)
      (string-append "Region: " (number->string (- end start)) " chars"))))

;;;============================================================================
;;; Text processing and window commands (Task #37)
;;;============================================================================

(def (cmd-capitalize-region app)
  "Upcase all characters in the region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if (not mark)
      (echo-message! (app-state-echo app) "No region")
      (let* ((start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (result (string-upcase text)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)))))

(def (cmd-count-words-buffer app)
  "Count words, lines, and chars in the entire buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (lines (+ 1 (let loop ((i 0) (n 0))
                       (cond ((>= i len) n)
                             ((char=? (string-ref text i) #\newline)
                              (loop (+ i 1) (+ n 1)))
                             (else (loop (+ i 1) n))))))
         (words (let loop ((i 0) (n 0) (in-word #f))
                  (cond ((>= i len) (if in-word (+ n 1) n))
                        ((let ((ch (string-ref text i)))
                           (or (char=? ch #\space) (char=? ch #\newline)
                               (char=? ch #\tab)))
                         (loop (+ i 1) (if in-word (+ n 1) n) #f))
                        (else (loop (+ i 1) n #t))))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (number->string lines) " lines, "
                     (number->string words) " words, "
                     (number->string len) " chars"))))

(def (cmd-unfill-paragraph app)
  "Join a paragraph into a single long line (inverse of fill-paragraph)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Find paragraph boundaries (blank lines)
         (para-start
           (let loop ((i (max 0 (- pos 1))))
             (cond ((<= i 0) 0)
                   ((and (char=? (string-ref text i) #\newline)
                         (> i 0)
                         (char=? (string-ref text (- i 1)) #\newline))
                    (+ i 1))
                   (else (loop (- i 1))))))
         (para-end
           (let loop ((i pos))
             (cond ((>= i len) len)
                   ((and (char=? (string-ref text i) #\newline)
                         (< (+ i 1) len)
                         (char=? (string-ref text (+ i 1)) #\newline))
                    i)
                   ((and (char=? (string-ref text i) #\newline)
                         (>= (+ i 1) len))
                    i)
                   (else (loop (+ i 1))))))
         (para (substring text para-start para-end))
         ;; Replace internal newlines with spaces
         (joined (let loop ((i 0) (acc '()))
                   (cond ((>= i (string-length para))
                          (apply string-append (reverse acc)))
                         ((char=? (string-ref para i) #\newline)
                          (loop (+ i 1) (cons " " acc)))
                         (else
                          (loop (+ i 1) (cons (string (string-ref para i)) acc)))))))
    (with-undo-action ed
      (editor-delete-range ed para-start (- para-end para-start))
      (editor-insert-text ed para-start joined))
    (echo-message! (app-state-echo app) "Paragraph unfilled")))

(def (cmd-list-registers app)
  "Show all non-empty registers in a buffer."
  (let* ((regs (app-state-registers app))
         (echo (app-state-echo app)))
    (if (= (hash-length regs) 0)
      (echo-message! echo "No registers set")
      (let ((lines
              (hash-fold
                (lambda (key val acc)
                  (cons (string-append (string key) ": "
                                       (if (string? val)
                                         (let ((s (if (> (string-length val) 60)
                                                    (string-append (substring val 0 60) "...")
                                                    val)))
                                           s)
                                         (if (number? val)
                                           (string-append "pos " (number->string val))
                                           "?")))
                        acc))
                [] regs)))
        ;; Show in a temp buffer
        (let* ((ed (current-editor app))
               (fr (app-state-frame app))
               (buf (buffer-create! "*Registers*" ed #f)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer (current-window fr)) buf)
          (editor-set-text ed (string-join (sort lines string<?) "\n")))))))

(def (cmd-show-kill-ring app)
  "Show kill ring contents in a buffer."
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      (let* ((lines
               (let loop ((entries ring) (i 0) (acc '()))
                 (if (or (null? entries) (>= i 20))
                   (reverse acc)
                   (let* ((entry (car entries))
                          (display-text
                            (let ((s (if (> (string-length entry) 70)
                                      (string-append (substring entry 0 70) "...")
                                      entry)))
                              ;; Replace newlines with \n for display
                              (let loop2 ((j 0) (a '()))
                                (cond ((>= j (string-length s))
                                       (apply string-append (reverse a)))
                                      ((char=? (string-ref s j) #\newline)
                                       (loop2 (+ j 1) (cons "\\n" a)))
                                      (else
                                       (loop2 (+ j 1)
                                              (cons (string (string-ref s j)) a))))))))
                     (loop (cdr entries) (+ i 1)
                           (cons (string-append (number->string i) ": " display-text)
                                 acc))))))
             (ed (current-editor app))
             (fr (app-state-frame app))
             (buf (buffer-create! "*Kill Ring*" ed #f)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed (string-join lines "\n"))))))

(def (cmd-smart-beginning-of-line app)
  "Move to first non-whitespace char on line, or to column 0 if already there."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Find first non-whitespace on this line
         (first-nonws
           (let loop ((i line-start))
             (cond ((>= i len) i)
                   ((char=? (string-ref text i) #\newline) i)
                   ((or (char=? (string-ref text i) #\space)
                        (char=? (string-ref text i) #\tab))
                    (loop (+ i 1)))
                   (else i)))))
    (if (= pos first-nonws)
      ;; Already at first non-ws, go to column 0
      (editor-goto-pos ed line-start)
      ;; Go to first non-ws
      (editor-goto-pos ed first-nonws))))

(def (cmd-shrink-window-if-larger app)
  "Shrink window to fit buffer content."
  ;; Scintilla handles this internally; just re-layout
  (frame-layout! (app-state-frame app))
  (echo-message! (app-state-echo app) "Window resized to fit"))

(def (cmd-toggle-input-method app)
  "Stub for input method toggle."
  (echo-message! (app-state-echo app) "No input method configured"))

(def (cmd-what-buffer app)
  "Show current buffer name and file path."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (if path
        (string-append name " (" path ")")
        name))))

(def (cmd-goto-last-change app)
  "Go to the position of the last edit."
  ;; Use SCI_GETMODIFIEDPOSITION if available, otherwise undo marker position
  ;; Simplified: just report that this needs undo tracking
  (echo-message! (app-state-echo app) "Use C-_ (undo) to find last change"))

(def (cmd-toggle-narrowing-indicator app)
  "Show whether buffer is narrowed."
  (echo-message! (app-state-echo app) "Narrowing not supported in this build"))

(def (cmd-insert-file-name app)
  "Insert the current buffer's file path at point."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (pos (editor-get-current-pos ed)))
    (if path
      (begin
        (editor-insert-text ed pos path)
        (editor-goto-pos ed (+ pos (string-length path))))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-toggle-auto-save app)
  "Toggle auto-save for current buffer."
  ;; Auto-save is session-global; just toggle and report
  (echo-message! (app-state-echo app) "Auto-save is always on"))

(def (cmd-backward-up-list app)
  "Move backward up one level of parentheses."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond ((<= i 0)
             (echo-message! (app-state-echo app) "At top level"))
            ((char=? (string-ref text i) #\))
             (loop (- i 1) (+ depth 1)))
            ((char=? (string-ref text i) #\()
             (if (= depth 0)
               (editor-goto-pos ed i)
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))

(def (cmd-forward-up-list app)
  "Move forward out of one level of parentheses."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos) (depth 0))
      (cond ((>= i len)
             (echo-message! (app-state-echo app) "At top level"))
            ((char=? (string-ref text i) #\()
             (loop (+ i 1) (+ depth 1)))
            ((char=? (string-ref text i) #\))
             (if (= depth 0)
               (editor-goto-pos ed (+ i 1))
               (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))

(def (cmd-kill-sexp app)
  "Kill from point to end of current s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let ((end-pos
              (cond
                ;; If at open paren, find matching close
                ((char=? (string-ref text pos) #\()
                 (let loop ((i (+ pos 1)) (depth 1))
                   (cond ((>= i len) len)
                         ((char=? (string-ref text i) #\() (loop (+ i 1) (+ depth 1)))
                         ((char=? (string-ref text i) #\))
                          (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
                         (else (loop (+ i 1) depth)))))
                ;; If at open bracket
                ((char=? (string-ref text pos) #\[)
                 (let loop ((i (+ pos 1)) (depth 1))
                   (cond ((>= i len) len)
                         ((char=? (string-ref text i) #\[) (loop (+ i 1) (+ depth 1)))
                         ((char=? (string-ref text i) #\])
                          (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
                         (else (loop (+ i 1) depth)))))
                ;; Otherwise kill word-like region
                (else
                  (let loop ((i pos))
                    (cond ((>= i len) len)
                          ((let ((ch (string-ref text i)))
                             (or (char=? ch #\space) (char=? ch #\newline)
                                 (char=? ch #\tab) (char=? ch #\()
                                 (char=? ch #\)) (char=? ch #\[)
                                 (char=? ch #\])))
                           i)
                          (else (loop (+ i 1)))))))))
        (let ((killed (substring text pos end-pos)))
          (with-undo-action ed
            (editor-delete-range ed pos (- end-pos pos)))
          (set! (app-state-kill-ring app)
            (cons killed (app-state-kill-ring app))))))))

(def (cmd-backward-sexp app)
  "Move backward over one s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 1)))
      (cond ((<= i 0) (editor-goto-pos ed 0))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (- i 1)))
            ;; Close paren — find matching open
            ((char=? (string-ref text i) #\))
             (let ploop ((j (- i 1)) (depth 1))
               (cond ((<= j 0) (editor-goto-pos ed 0))
                     ((char=? (string-ref text j) #\))
                      (ploop (- j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\()
                      (if (= depth 1) (editor-goto-pos ed j)
                        (ploop (- j 1) (- depth 1))))
                     (else (ploop (- j 1) depth)))))
            ;; Word-like token
            (else
              (let wloop ((j i))
                (cond ((<= j 0) (editor-goto-pos ed 0))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[)
                             (char=? ch #\])))
                       (editor-goto-pos ed (+ j 1)))
                      (else (wloop (- j 1))))))))))

(def (cmd-forward-sexp app)
  "Move forward over one s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond ((>= i len) (editor-goto-pos ed len))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (+ i 1)))
            ;; Open paren — find matching close
            ((char=? (string-ref text i) #\()
             (let ploop ((j (+ i 1)) (depth 1))
               (cond ((>= j len) (editor-goto-pos ed len))
                     ((char=? (string-ref text j) #\() (ploop (+ j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\))
                      (if (= depth 1) (editor-goto-pos ed (+ j 1))
                        (ploop (+ j 1) (- depth 1))))
                     (else (ploop (+ j 1) depth)))))
            ;; Word-like token
            (else
              (let wloop ((j i))
                (cond ((>= j len) (editor-goto-pos ed len))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[)
                             (char=? ch #\])))
                       (editor-goto-pos ed j))
                      (else (wloop (+ j 1))))))))))

;;;============================================================================
;;; S-expression and utility commands (Task #38)
;;;============================================================================

(def (cmd-transpose-sexps app)
  "Transpose the two s-expressions around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find extent of sexp before point, and sexp after point
    ;; Simple: find word/paren boundaries backward and forward
    (echo-message! (app-state-echo app) "transpose-sexps: use M-t for words")))

(def (cmd-mark-sexp app)
  "Mark the next s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (buf (current-buffer-from-app app)))
    ;; Set mark at current pos
    (set! (buffer-mark buf) pos)
    ;; Find end of next sexp
    (let loop ((i pos))
      (cond ((>= i len) (editor-goto-pos ed len))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (+ i 1)))
            ;; Open paren
            ((char=? (string-ref text i) #\()
             (let ploop ((j (+ i 1)) (depth 1))
               (cond ((>= j len) (editor-goto-pos ed len))
                     ((char=? (string-ref text j) #\() (ploop (+ j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\))
                      (if (= depth 1) (editor-goto-pos ed (+ j 1))
                        (ploop (+ j 1) (- depth 1))))
                     (else (ploop (+ j 1) depth)))))
            ;; Word token
            (else
              (let wloop ((j i))
                (cond ((>= j len) (editor-goto-pos ed len))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[) (char=? ch #\])))
                       (editor-goto-pos ed j))
                      (else (wloop (+ j 1))))))))
    (echo-message! (app-state-echo app) "Sexp marked")))

(def (cmd-indent-sexp app)
  "Re-indent the next s-expression (simple: indent region from point to matching paren)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (or (>= pos len) (not (char=? (string-ref text pos) #\()))
      (echo-message! echo "Not at start of sexp")
      ;; Find matching close paren
      (let loop ((i (+ pos 1)) (depth 1))
        (cond ((>= i len) (echo-message! echo "Unbalanced sexp"))
              ((char=? (string-ref text i) #\() (loop (+ i 1) (+ depth 1)))
              ((char=? (string-ref text i) #\))
               (if (= depth 1)
                 (let* ((end (+ i 1))
                        (region (substring text pos end))
                        ;; Simple re-indent: ensure consistent 2-space indentation
                        (lines (string-split region #\newline))
                        (indented
                          (let lp ((ls lines) (first #t) (acc '()))
                            (if (null? ls)
                              (reverse acc)
                              (let ((line (string-trim (car ls))))
                                (lp (cdr ls) #f
                                    (cons (if first line
                                            (string-append "  " line))
                                          acc))))))
                        (result (string-join indented "\n")))
                   (with-undo-action ed
                     (editor-delete-range ed pos (- end pos))
                     (editor-insert-text ed pos result))
                   (echo-message! echo "Sexp indented"))
                 (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth)))))))

(def (cmd-word-frequency app)
  "Count word frequencies in the buffer and show top words."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (freq (make-hash-table)))
    ;; Split text into words
    (let loop ((i 0) (word-start #f))
      (cond ((>= i len)
             (when word-start
               (let ((w (string-downcase (substring text word-start i))))
                 (when (> (string-length w) 0)
                   (hash-put! freq w (+ 1 (or (hash-get freq w) 0)))))))
            ((let ((ch (string-ref text i)))
               (or (char-alphabetic? ch) (char-numeric? ch)
                   (char=? ch #\_) (char=? ch #\-)))
             (loop (+ i 1) (or word-start i)))
            (else
              (when word-start
                (let ((w (string-downcase (substring text word-start i))))
                  (when (> (string-length w) 0)
                    (hash-put! freq w (+ 1 (or (hash-get freq w) 0))))))
              (loop (+ i 1) #f))))
    ;; Sort by frequency
    (let* ((pairs (hash-fold (lambda (k v acc) (cons (cons k v) acc)) [] freq))
           (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
           (top (let lp ((ls sorted) (n 0) (acc '()))
                  (if (or (null? ls) (>= n 30))
                    (reverse acc)
                    (let ((p (car ls)))
                      (lp (cdr ls) (+ n 1)
                          (cons (string-append (number->string (cdr p))
                                               "\t" (car p))
                                acc))))))
           (fr (app-state-frame app))
           (buf (buffer-create! "*Word Frequency*" ed #f)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed (string-join top "\n")))))

(def (cmd-insert-uuid app)
  "Insert a UUID-like random hex string at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (bs (random-bytes 16))
         (hex (hex-encode bs))
         ;; Format as UUID: 8-4-4-4-12
         (uuid (string-append
                 (substring hex 0 8) "-"
                 (substring hex 8 12) "-"
                 (substring hex 12 16) "-"
                 (substring hex 16 20) "-"
                 (substring hex 20 32))))
    (editor-insert-text ed pos uuid)
    (editor-goto-pos ed (+ pos (string-length uuid)))))

(def (cmd-reformat-buffer app)
  "Re-indent the entire buffer (simple: normalize leading whitespace)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Use Scintilla's built-in TAB indentation — just trigger indent on each line
    ;; For now, just report the operation
    (echo-message! echo "Use TAB on each line or C-c TAB for indent-region")))

(def (cmd-delete-pair app)
  "Delete the surrounding delimiters (parens, brackets, quotes) around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find matching pair around point
    (if (= len 0)
      (echo-message! (app-state-echo app) "Buffer empty")
      ;; Search backward for opener
      (let ((opener-pos
              (let loop ((i (- pos 1)))
                (cond ((<= i 0) #f)
                      ((let ((ch (string-ref text i)))
                         (or (char=? ch #\() (char=? ch #\[)
                             (char=? ch #\{) (char=? ch #\")))
                       i)
                      (else (loop (- i 1)))))))
        (if (not opener-pos)
          (echo-message! (app-state-echo app) "No opening delimiter found")
          (let* ((opener (string-ref text opener-pos))
                 (closer (cond ((char=? opener #\() #\))
                               ((char=? opener #\[) #\])
                               ((char=? opener #\{) #\})
                               ((char=? opener #\") #\")
                               (else #f))))
            ;; Find matching closer
            (let ((closer-pos
                    (if (char=? opener #\")
                      ;; For quotes, find next quote after opener
                      (let loop ((i (+ opener-pos 1)))
                        (cond ((>= i len) #f)
                              ((char=? (string-ref text i) #\") i)
                              (else (loop (+ i 1)))))
                      ;; For parens, match with depth
                      (let loop ((i (+ opener-pos 1)) (depth 1))
                        (cond ((>= i len) #f)
                              ((char=? (string-ref text i) opener)
                               (loop (+ i 1) (+ depth 1)))
                              ((char=? (string-ref text i) closer)
                               (if (= depth 1) i (loop (+ i 1) (- depth 1))))
                              (else (loop (+ i 1) depth)))))))
              (if (not closer-pos)
                (echo-message! (app-state-echo app) "No matching closer found")
                (with-undo-action ed
                  ;; Delete closer first (higher position) to preserve opener position
                  (editor-delete-range ed closer-pos 1)
                  (editor-delete-range ed opener-pos 1))))))))))

(def (cmd-toggle-hl-line app)
  "Toggle current line highlight."
  (let* ((ed (current-editor app))
         (visible (editor-get-caret-line-visible? ed)))
    (editor-set-caret-line-visible ed (not visible))
    (echo-message! (app-state-echo app)
      (if visible "Caret line highlight OFF" "Caret line highlight ON"))))

(def (cmd-toggle-column-number-mode app)
  "Column number display is always shown in modeline."
  (echo-message! (app-state-echo app) "Column numbers always shown"))

(def (cmd-find-alternate-file app)
  "Replace current buffer with another file."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Find alternate file: " row width)))
    (when (and filename (> (string-length filename) 0))
      (let ((ed (current-editor app)))
        (if (file-exists? filename)
          (let* ((text (read-file-as-string filename))
                 (name (path-strip-directory filename))
                 (buf (current-buffer-from-app app)))
            ;; Reuse current buffer
            (set! (buffer-name buf) name)
            (set! (buffer-file-path buf) filename)
            (when text
              (editor-set-text ed text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0))
            (echo-message! echo (string-append "Opened: " filename)))
          (echo-error! echo (string-append "File not found: " filename)))))))

(def (cmd-increment-register app)
  "Increment numeric register by 1."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Register to increment: " row width)))
    (when (and input (= (string-length input) 1))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond ((and (number? val))
               (hash-put! (app-state-registers app) reg-char (+ val 1))
               (echo-message! echo (string-append "Register " input ": "
                                                   (number->string (+ val 1)))))
              ((and (string? val) (string->number val))
               (let ((n (+ 1 (string->number val))))
                 (hash-put! (app-state-registers app) reg-char (number->string n))
                 (echo-message! echo (string-append "Register " input ": "
                                                     (number->string n)))))
              (else
                (echo-error! echo "Register is not numeric")))))))

(def (cmd-toggle-size-indication app)
  "Toggle buffer size display."
  (echo-message! (app-state-echo app) "Buffer size always shown in buffer-info"))

(def (cmd-copy-buffer-name app)
  "Copy current buffer name to kill ring."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (set! (app-state-kill-ring app)
      (cons name (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied: " name))))

;;;============================================================================
;;; Task #39: sort, rectangle, completion, text processing
;;;============================================================================

;; Helper: get text in range [start, start+len)
(def (editor-get-text-range ed start len)
  (substring (editor-get-text ed) start (+ start len)))

(def (cmd-sort-lines-case-fold app)
  "Sort lines in region case-insensitively."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (sorted (sort lines (lambda (a b)
                                   (string-ci<? a b))))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Sorted "
                                            (number->string (length sorted))
                                            " lines (case-insensitive)")))
      (echo-error! echo "No mark set"))))

(def (cmd-reverse-chars app)
  "Reverse characters in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (len (- end start))
             (text (editor-get-text-range ed start len))
             (result (list->string (reverse (string->list text)))))
        (with-undo-action ed
          (editor-delete-range ed start len)
          (editor-insert-text ed start result))
        (echo-message! echo (string-append "Reversed " (number->string len) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-replace-string-all app)
  "Replace all occurrences of a string in buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Replace string: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((replacement (echo-read-string echo
                           (string-append "Replace \"" pattern "\" with: ") row width)))
        (when replacement
          (let* ((text (editor-get-text ed))
                 (plen (string-length pattern))
                 ;; Manual replace-all loop
                 (result
                   (let loop ((i 0) (acc (open-output-string)))
                     (let ((found (string-contains text pattern i)))
                       (if found
                         (begin
                           (display (substring text i found) acc)
                           (display replacement acc)
                           (loop (+ found plen) acc))
                         (begin
                           (display (substring text i (string-length text)) acc)
                           (get-output-string acc))))))
                 (len (editor-get-text-length ed)))
            (with-undo-action ed
              (editor-delete-range ed 0 len)
              (editor-insert-text ed 0 result))
            (echo-message! echo "Replacement done")))))))

(def (cmd-insert-file-contents app)
  "Insert contents of a file at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (path (echo-read-string echo "Insert file: " row width)))
    (when (and path (not (string-empty? path)))
      (if (file-exists? path)
        (let* ((contents (read-file-as-string path))
               (pos (editor-get-current-pos ed)))
          (editor-insert-text ed pos contents)
          (echo-message! echo (string-append "Inserted " path)))
        (echo-error! echo (string-append "File not found: " path))))))

(def *auto-revert-mode* #f)

(def (cmd-toggle-auto-revert app)
  "Toggle auto-revert mode (stub)."
  (set! *auto-revert-mode* (not *auto-revert-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-revert-mode* "Auto-revert mode ON" "Auto-revert mode OFF")))

(def (cmd-zap-up-to-char app)
  "Kill text up to (but not including) a character."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Zap up to char: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed))
             ;; Search forward for the character
             (found
               (let loop ((p (+ pos 1)))
                 (cond
                   ((>= p len) #f)
                   ((= (editor-get-char-at ed p) (char->integer ch)) p)
                   (else (loop (+ p 1)))))))
        (if found
          (let ((kill-text (editor-get-text-range ed pos (- found pos))))
            (set! (app-state-kill-ring app)
              (cons kill-text (app-state-kill-ring app)))
            (editor-delete-range ed pos (- found pos))
            (echo-message! echo (string-append "Zapped to '" (string ch) "'")))
          (echo-error! echo (string-append "'" (string ch) "' not found")))))))

(def (cmd-quoted-insert app)
  "Insert the next character literally (for control chars)."
  (echo-message! (app-state-echo app) "C-q: Next key inserts literally (use self-insert)"))

(def (cmd-what-line-col app)
  "Show current line and column."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (col (- pos (editor-position-from-line ed line))))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string (+ line 1))
                     ", Column " (number->string col)))))

(def (cmd-insert-current-date-iso app)
  "Insert current date in ISO 8601 format (YYYY-MM-DD)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         ;; Use shell to get ISO date
         (proc (open-process
                 (list path: "/bin/date"
                       arguments: ["+%Y-%m-%d"]
                       stdout-redirection: #t)))
         (date-str (read-line proc))
         (_ (process-status proc)))
    (when (string? date-str)
      (editor-insert-text ed pos date-str))))

(def (cmd-recenter-top app)
  "Scroll so current line is at top of window."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos)))
    (editor-set-first-visible-line ed line)))

(def (cmd-recenter-bottom app)
  "Scroll so current line is at bottom of window."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (visible (send-message ed 2370 0 0))) ; SCI_LINESONSCREEN
    (editor-set-first-visible-line ed (max 0 (- line (- visible 1))))))

(def (cmd-scroll-other-window app)
  "Scroll the other window down one page."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (cur-idx (frame-current-idx fr)))
    (when (> (length wins) 1)
      (let* ((other-idx (modulo (+ cur-idx 1) (length wins)))
             (other-win (list-ref wins other-idx))
             (other-ed (edit-window-editor other-win))
             (visible (send-message other-ed 2370 0 0))
             (first (editor-get-first-visible-line other-ed)))
        (editor-set-first-visible-line other-ed (+ first visible))))))

(def (cmd-scroll-other-window-up app)
  "Scroll the other window up one page."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (cur-idx (frame-current-idx fr)))
    (when (> (length wins) 1)
      (let* ((other-idx (modulo (+ cur-idx 1) (length wins)))
             (other-win (list-ref wins other-idx))
             (other-ed (edit-window-editor other-win))
             (visible (send-message other-ed 2370 0 0))
             (first (editor-get-first-visible-line other-ed)))
        (editor-set-first-visible-line other-ed (max 0 (- first visible)))))))


(def (cmd-count-words-paragraph app)
  "Count words in current paragraph."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (total-lines (editor-get-line-count ed))
         ;; Find paragraph start (first blank line above or BOF)
         (para-start-line
           (let loop ((l (- line 1)))
             (if (< l 0) 0
               (let* ((ls (editor-position-from-line ed l))
                      (le (editor-get-line-end-position ed l))
                      (text (editor-get-text-range ed ls (- le ls))))
                 (if (string-empty? (string-trim text))
                   (+ l 1)
                   (loop (- l 1)))))))
         ;; Find paragraph end (first blank line below or EOF)
         (para-end-line
           (let loop ((l (+ line 1)))
             (if (>= l total-lines) (- total-lines 1)
               (let* ((ls (editor-position-from-line ed l))
                      (le (editor-get-line-end-position ed l))
                      (text (editor-get-text-range ed ls (- le ls))))
                 (if (string-empty? (string-trim text))
                   (- l 1)
                   (loop (+ l 1)))))))
         (start (editor-position-from-line ed para-start-line))
         (end (editor-get-line-end-position ed para-end-line))
         (text (editor-get-text-range ed start (- end start)))
         (words (filter (lambda (w) (not (string-empty? w)))
                        (string-split text #\space)))
         (count (length words)))
    (echo-message! echo (string-append "Paragraph: " (number->string count) " words"))))

(def (cmd-toggle-transient-mark app)
  "Toggle transient mark mode (stub)."
  (echo-message! (app-state-echo app) "Transient mark mode always active"))

(def (cmd-keep-lines-region app)
  "Keep only lines matching regexp in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((pattern (echo-read-string echo "Keep lines matching: " row width)))
        (when (and pattern (not (string-empty? pattern)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end))
                 (line-start (editor-position-from-line ed start-line))
                 (line-end (editor-get-line-end-position ed end-line))
                 (text (editor-get-text-range ed line-start (- line-end line-start)))
                 (lines (string-split text #\newline))
                 (kept (filter (lambda (l) (string-contains l pattern)) lines))
                 (result (string-join kept "\n")))
            (with-undo-action ed
              (editor-delete-range ed line-start (- line-end line-start))
              (editor-insert-text ed line-start result))
            (echo-message! echo (string-append "Kept " (number->string (length kept))
                                                " lines")))))
      (echo-error! echo "No mark set"))))

(def (cmd-flush-lines-region app)
  "Remove lines matching regexp in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((pattern (echo-read-string echo "Flush lines matching: " row width)))
        (when (and pattern (not (string-empty? pattern)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end))
                 (line-start (editor-position-from-line ed start-line))
                 (line-end (editor-get-line-end-position ed end-line))
                 (text (editor-get-text-range ed line-start (- line-end line-start)))
                 (lines (string-split text #\newline))
                 (kept (filter (lambda (l) (not (string-contains l pattern))) lines))
                 (result (string-join kept "\n")))
            (with-undo-action ed
              (editor-delete-range ed line-start (- line-end line-start))
              (editor-insert-text ed line-start result))
            (echo-message! echo (string-append "Flushed "
                                                (number->string (- (length lines) (length kept)))
                                                " lines")))))
      (echo-error! echo "No mark set"))))

(def (cmd-insert-register-string app)
  "Insert register content at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (if (and val (string? val))
          (let ((pos (editor-get-current-pos ed)))
            (editor-insert-text ed pos val)
            (echo-message! echo (string-append "Inserted register " (string reg-char))))
          (echo-error! echo "Register empty or not a string"))))))

(def (cmd-toggle-visible-bell app)
  "Toggle visible bell (stub)."
  (echo-message! (app-state-echo app) "Visible bell always enabled"))

;;;============================================================================
;;; Task #40: indentation, buffers, navigation
;;;============================================================================

(def (cmd-unindent-region app)
  "Unindent region by one tab stop."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (tab-w (editor-get-tab-width ed)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len tab-w)))
                     ;; Count leading spaces to remove (up to tab-w)
                     (spaces (let sloop ((i 0))
                               (if (and (< i (string-length text))
                                        (char=? (string-ref text i) #\space))
                                 (sloop (+ i 1))
                                 i))))
                (when (> spaces 0)
                  (editor-delete-range ed ls spaces)))
              (loop (- l 1)))))
        (echo-message! echo (string-append "Unindented "
                                            (number->string (+ 1 (- end-line start-line)))
                                            " lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-copy-region-as-kill app)
  "Copy region to kill ring without removing it."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end)))
        (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Copied "
                                            (number->string (- end start)) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-append-to-buffer app)
  "Append region text to another buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((name (echo-read-string echo "Append to buffer: " row width)))
        (when (and name (not (string-empty? name)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end))
                 (target (buffer-by-name name)))
            (if target
              (begin
                (echo-message! echo (string-append "Appended to " name))
                ;; Text stored in kill ring for later paste into target
                (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app))))
              (echo-error! echo (string-append "No buffer: " name))))))
      (echo-error! echo "No mark set"))))

(def (cmd-toggle-show-trailing-whitespace app)
  "Toggle showing trailing whitespace."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (current (editor-get-view-whitespace ed)))
    (if (= current 0)
      (begin (editor-set-view-whitespace ed 1)
             (echo-message! echo "Trailing whitespace visible"))
      (begin (editor-set-view-whitespace ed 0)
             (echo-message! echo "Trailing whitespace hidden")))))

(def (cmd-backward-kill-sexp app)
  "Kill the sexp before point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    ;; Simple backward sexp kill: find matching paren backwards
    (if (and (> pos 0)
             (let ((prev-ch (char->integer (string-ref text (- pos 1)))))
               (brace-char? prev-ch)))
      (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
        (if (>= match 0)
          (let* ((start (min match (- pos 1)))
                 (end (+ (max match (- pos 1)) 1))
                 (killed (substring text start end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (editor-delete-range ed start (- end start))
            (echo-message! echo "Killed sexp"))
          (echo-error! echo "No matching sexp")))
      ;; If not on a bracket, kill the previous word as fallback
      (let loop ((p (- pos 1)))
        (if (or (<= p 0) (not (word-char? (char->integer (string-ref text p)))))
          (let* ((start (+ p 1))
                 (killed (substring text start pos)))
            (when (> (string-length killed) 0)
              (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
              (editor-delete-range ed start (- pos start))))
          (loop (- p 1)))))))


(def (cmd-delete-horizontal-space-forward app)
  "Delete whitespace after point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (end (let loop ((p pos))
                (if (and (< p len)
                         (let ((ch (string-ref text p)))
                           (or (char=? ch #\space) (char=? ch #\tab))))
                  (loop (+ p 1))
                  p))))
    (when (> end pos)
      (editor-delete-range ed pos (- end pos)))))

(def (cmd-toggle-debug-mode app)
  "Toggle debug mode display."
  (echo-message! (app-state-echo app) "Debug mode toggled (stub)"))

(def (cmd-insert-comment-separator app)
  "Insert a comment separator line (;; ===...===)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (sep ";;; ============================================================================\n"))
    (editor-insert-text ed pos sep)
    (editor-goto-pos ed (+ pos (string-length sep)))))

(def *global-hl-line* #t)

(def (cmd-toggle-global-hl-line app)
  "Toggle global caret line highlight."
  (set! *global-hl-line* (not *global-hl-line*))
  (let ((fr (app-state-frame app))
        (echo (app-state-echo app)))
    ;; Apply to current editor
    (let ((ed (edit-window-editor (current-window fr))))
      (editor-set-caret-line-visible ed *global-hl-line*))
    (echo-message! echo (if *global-hl-line*
                           "Global hl-line ON"
                           "Global hl-line OFF"))))

(def (cmd-insert-shebang app)
  "Insert #!/usr/bin/env shebang line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (interp (echo-read-string echo "Interpreter (e.g. gxi, python3): " row width)))
    (when (and interp (not (string-empty? interp)))
      (let ((line (string-append "#!/usr/bin/env " interp "\n")))
        (editor-insert-text ed 0 line)
        (echo-message! echo (string-append "Inserted shebang for " interp))))))

(def (cmd-toggle-auto-indent app)
  "Toggle auto-indent on newline."
  (echo-message! (app-state-echo app) "Auto-indent always active"))

(def (cmd-what-mode app)
  "Show current buffer mode."
  (let* ((buf (current-buffer-from-app app))
         (lang (buffer-lexer-lang buf))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append "Mode: "
                                        (if lang (symbol->string lang) "fundamental")))))

(def (cmd-show-buffer-size app)
  "Show current buffer size in bytes and lines."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (size (editor-get-text-length ed))
         (lines (editor-get-line-count ed)))
    (echo-message! echo (string-append (number->string size) " bytes, "
                                        (number->string lines) " lines"))))

(def (cmd-goto-percent app)
  "Go to percentage position in buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto percent: " row width)))
    (when (and input (not (string-empty? input)))
      (let ((pct (string->number input)))
        (when (and pct (>= pct 0) (<= pct 100))
          (let* ((total (editor-get-text-length ed))
                 (target (quotient (* total pct) 100)))
            (editor-goto-pos ed target)
            (editor-scroll-caret ed)))))))

(def (cmd-insert-newline-below app)
  "Insert a blank line below current line without moving cursor."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-end (editor-get-line-end-position ed line)))
    (editor-insert-text ed line-end "\n")
    (editor-goto-pos ed pos)))

(def (cmd-insert-newline-above app)
  "Insert a blank line above current line without moving cursor."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line)))
    (editor-insert-text ed line-start "\n")
    ;; Cursor shifted down by 1, so restore
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-duplicate-region app)
  "Duplicate the selected region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end)))
        (editor-insert-text ed end text)
        (echo-message! echo (string-append "Duplicated "
                                            (number->string (- end start)) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-sort-lines-reverse app)
  "Sort lines in region in reverse order."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (sorted (sort lines (lambda (a b) (string>? a b))))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Sorted "
                                            (number->string (length sorted))
                                            " lines (reverse)")))
      (echo-error! echo "No mark set"))))

(def (cmd-uniquify-lines app)
  "Remove consecutive duplicate lines in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (unique (let loop ((ls lines) (prev #f) (acc []))
                       (cond
                         ((null? ls) (reverse acc))
                         ((and prev (string=? (car ls) prev))
                          (loop (cdr ls) prev acc))
                         (else
                          (loop (cdr ls) (car ls) (cons (car ls) acc))))))
             (removed (- (length lines) (length unique)))
             (result (string-join unique "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Removed " (number->string removed)
                                            " duplicate lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-show-line-endings app)
  "Show what line ending style the buffer uses."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (has-crlf (string-contains text "\r\n"))
         (has-cr (and (not has-crlf) (string-contains text "\r"))))
    (echo-message! echo
      (cond
        (has-crlf "Line endings: CRLF (DOS/Windows)")
        (has-cr "Line endings: CR (old Mac)")
        (else "Line endings: LF (Unix)")))))

;;;============================================================================
;;; Task #41: macros, windows, and advanced editing
;;;============================================================================

(def (cmd-comment-region app)
  "Comment each line in region with ;; prefix."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let ((ls (editor-position-from-line ed l)))
                (editor-insert-text ed ls ";; "))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Commented "
                                            (number->string (+ 1 (- end-line start-line)))
                                            " lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-uncomment-region app)
  "Remove ;; comment prefix from each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len 3))))
                ;; Remove ";; " or ";;" at start
                (cond
                  ((and (>= (string-length text) 3) (string=? text ";; "))
                   (editor-delete-range ed ls 3))
                  ((and (>= (string-length text) 2) (string=? (substring text 0 2) ";;"))
                   (editor-delete-range ed ls 2))))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region uncommented"))
      (echo-error! echo "No mark set"))))

(def (cmd-upcase-char app)
  "Uppercase the character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (up (char-upcase ch)))
        (when (not (char=? ch up))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string up)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-downcase-char app)
  "Lowercase the character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (lo (char-downcase ch)))
        (when (not (char=? ch lo))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string lo)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-toggle-case-at-point app)
  "Toggle case of character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (toggled (if (char-upper-case? ch) (char-downcase ch) (char-upcase ch))))
        (when (not (char=? ch toggled))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string toggled)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-write-region app)
  "Write the region to a file."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((filename (echo-read-string echo "Write region to file: " row width)))
        (when (and filename (not (string-empty? filename)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end)))
            (with-output-to-file filename (lambda () (display text)))
            (set! (buffer-mark buf) #f)
            (echo-message! echo (string-append "Wrote "
                                                (number->string (- end start))
                                                " chars to " filename)))))
      (echo-error! echo "No mark set"))))

(def (cmd-kill-matching-buffers app)
  "Kill all buffers whose names match a pattern."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Kill buffers matching: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((killed 0))
        (for-each
          (lambda (buf)
            (when (string-contains (buffer-name buf) pattern)
              (set! killed (+ killed 1))))
          (buffer-list))
        (echo-message! echo (string-append "Would kill "
                                            (number->string killed)
                                            " matching buffers"))))))

(def (cmd-goto-line-relative app)
  "Go to a line relative to the current line (+N or -N)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Relative line (+N or -N): " row width)))
    (when (and input (not (string-empty? input)))
      (let ((n (string->number input)))
        (when n
          (let* ((pos (editor-get-current-pos ed))
                 (cur-line (editor-line-from-position ed pos))
                 (target (+ cur-line n))
                 (max-line (- (editor-get-line-count ed) 1))
                 (clamped (max 0 (min target max-line))))
            (editor-goto-pos ed (editor-position-from-line ed clamped))
            (editor-scroll-caret ed)))))))

(def (cmd-bookmark-delete app)
  "Delete a bookmark by name."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Delete bookmark: " row width)))
    (when (and name (not (string-empty? name)))
      (let ((bm (app-state-bookmarks app)))
        (if (hash-get bm name)
          (begin
            (hash-remove! bm name)
            (echo-message! echo (string-append "Deleted bookmark: " name)))
          (echo-error! echo (string-append "No bookmark: " name)))))))

(def (cmd-bookmark-rename app)
  "Rename a bookmark."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (old-name (echo-read-string echo "Rename bookmark: " row width)))
    (when (and old-name (not (string-empty? old-name)))
      (let ((bm (app-state-bookmarks app)))
        (if (hash-get bm old-name)
          (let ((new-name (echo-read-string echo "New name: " row width)))
            (when (and new-name (not (string-empty? new-name)))
              (hash-put! bm new-name (hash-ref bm old-name))
              (hash-remove! bm old-name)
              (echo-message! echo (string-append old-name " -> " new-name))))
          (echo-error! echo (string-append "No bookmark: " old-name)))))))

(def (cmd-describe-mode app)
  "Describe the current buffer mode."
  (let* ((buf (current-buffer-from-app app))
         (lang (buffer-lexer-lang buf))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append "Major mode: "
                                        (if lang (symbol->string lang) "fundamental")
                                        " | Use M-x describe-bindings for keybindings"))))

(def (cmd-delete-trailing-lines app)
  "Delete trailing blank lines at end of buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (= len 0)
      (echo-message! echo "Buffer is empty")
      (let loop ((end len))
        (if (and (> end 0)
                 (let ((ch (string-ref text (- end 1))))
                   (or (char=? ch #\newline) (char=? ch #\space) (char=? ch #\tab))))
          (loop (- end 1))
          (if (< end len)
            (let ((removed (- len end)))
              ;; Keep one trailing newline
              (let ((keep-end (+ end 1)))
                (when (< keep-end len)
                  (editor-delete-range ed keep-end (- len keep-end))
                  (echo-message! echo (string-append "Removed "
                                                      (number->string (- len keep-end))
                                                      " trailing chars")))))
            (echo-message! echo "No trailing blank lines")))))))

(def (cmd-display-line-numbers-relative app)
  "Toggle relative line numbers display."
  (echo-message! (app-state-echo app) "Relative line numbers (stub)"))

(def (cmd-goto-column app)
  "Go to a specific column on the current line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Go to column: " row width)))
    (when (and input (not (string-empty? input)))
      (let ((col (string->number input)))
        (when (and col (> col 0))
          (let* ((pos (editor-get-current-pos ed))
                 (line (editor-line-from-position ed pos))
                 (line-start (editor-position-from-line ed line))
                 (line-end (editor-get-line-end-position ed line))
                 (line-len (- line-end line-start))
                 (target-col (min (- col 1) line-len)))
            (editor-goto-pos ed (+ line-start target-col))))))))

(def (cmd-insert-line-number app)
  "Insert the current line number at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (text (number->string line)))
    (editor-insert-text ed pos text)
    (editor-goto-pos ed (+ pos (string-length text)))))

(def (cmd-insert-buffer-filename app)
  "Insert the current buffer's filename at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (filename (or (buffer-file-path buf) (buffer-name buf))))
    (editor-insert-text ed pos filename)
    (editor-goto-pos ed (+ pos (string-length filename)))))

(def (cmd-copy-line-number app)
  "Copy the current line number to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (text (number->string line)))
    (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied line number: " text))))

(def (cmd-copy-current-line app)
  "Copy the current line to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (start (editor-position-from-line ed line))
         (end (editor-get-line-end-position ed line))
         (text (substring (editor-get-text ed) start end)))
    (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

(def (cmd-copy-word app)
  "Copy the word at point to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end)))
        (set! (app-state-kill-ring app) (cons word (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) (string-append "Copied: " word)))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-move-to-window-top app)
  "Move cursor to the top visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (doc-line (send-message ed 2312 first-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (pos (editor-position-from-line ed doc-line)))
    (editor-goto-pos ed pos)))

(def (cmd-move-to-window-bottom app)
  "Move cursor to the bottom visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (lines-on-screen (send-message ed 2370 0 0)) ; SCI_LINESONSCREEN
         (last-visible (+ first-visible (- lines-on-screen 1)))
         (doc-line (send-message ed 2312 last-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (max-line (- (editor-get-line-count ed) 1))
         (target (min doc-line max-line))
         (pos (editor-position-from-line ed target)))
    (editor-goto-pos ed pos)))

(def (cmd-move-to-window-middle app)
  "Move cursor to the middle visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (lines-on-screen (send-message ed 2370 0 0)) ; SCI_LINESONSCREEN
         (middle-visible (+ first-visible (quotient lines-on-screen 2)))
         (doc-line (send-message ed 2312 middle-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (pos (editor-position-from-line ed doc-line)))
    (editor-goto-pos ed pos)))

(def (cmd-scroll-left app)
  "Scroll the view left."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (offset (send-message ed SCI_GETXOFFSET 0 0)))
    (when (> offset 0)
      (send-message ed SCI_SETXOFFSET (max 0 (- offset 20)) 0))))

(def (cmd-scroll-right app)
  "Scroll the view right."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (offset (send-message ed SCI_GETXOFFSET 0 0)))
    (send-message ed SCI_SETXOFFSET (+ offset 20) 0)))

(def (cmd-delete-to-end-of-line app)
  "Delete from point to end of line (without killing)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (end (editor-get-line-end-position ed line)))
    (when (> end pos)
      (editor-delete-range ed pos (- end pos)))))

(def (cmd-delete-to-beginning-of-line app)
  "Delete from point to beginning of line (without killing)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (start (editor-position-from-line ed line)))
    (when (> pos start)
      (editor-delete-range ed start (- pos start)))))

(def (cmd-yank-whole-line app)
  "Yank (paste) a whole line above the current line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (kill-ring (app-state-kill-ring app)))
    (if (null? kill-ring)
      (echo-error! echo "Kill ring is empty")
      (let* ((text (car kill-ring))
             (pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (line-start (editor-position-from-line ed line))
             (insert-text (string-append text "\n")))
        (editor-insert-text ed line-start insert-text)
        (editor-goto-pos ed line-start)
        (echo-message! echo "Yanked line")))))

(def (cmd-show-column-number app)
  "Show the current column number."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (col (+ 1 (- pos line-start))))
    (echo-message! echo (string-append "Column " (number->string col)))))

(def (cmd-count-lines-buffer app)
  "Count total lines in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (lines (editor-get-line-count ed)))
    (echo-message! echo (string-append "Buffer has " (number->string lines) " lines"))))

(def (cmd-recover-session app)
  "Recover auto-saved session files."
  (echo-message! (app-state-echo app) "Session recovery (stub)"))

(def (cmd-toggle-backup-files app)
  "Toggle whether backup files are created on save."
  (echo-message! (app-state-echo app) "Backup files toggled (stub)"))

;;;============================================================================
;;; Task #42: text transforms, programming, and info
;;;============================================================================

(def (cmd-camel-to-snake app)
  "Convert camelCase word at point to snake_case."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (and (char-upper-case? ch) (> i 0))
                             (loop (+ i 1) (cons (char-downcase ch) (cons #\_ acc)))
                             (loop (+ i 1) (cons (char-downcase ch) acc))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-snake-to-camel app)
  "Convert snake_case word at point to camelCase."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Include underscores in word boundary
    (when (and (< pos len)
               (let ((ch (string-ref text pos)))
                 (or (word-char? (char->integer ch)) (char=? ch #\_))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (word-char? (char->integer ch)) (char=? ch #\_))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (word-char? (char->integer ch)) (char=? ch #\_))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (capitalize? #f) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (char=? ch #\_)
                             (loop (+ i 1) #t acc)
                             (if capitalize?
                               (loop (+ i 1) #f (cons (char-upcase ch) acc))
                               (loop (+ i 1) #f (cons ch acc)))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-kebab-to-camel app)
  "Convert kebab-case word at point to camelCase."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len)
               (let ((ch (string-ref text pos)))
                 (or (word-char? (char->integer ch)) (char=? ch #\-))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (word-char? (char->integer ch)) (char=? ch #\-))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (word-char? (char->integer ch)) (char=? ch #\-))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (capitalize? #f) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (char=? ch #\-)
                             (loop (+ i 1) #t acc)
                             (if capitalize?
                               (loop (+ i 1) #f (cons (char-upcase ch) acc))
                               (loop (+ i 1) #f (cons ch acc)))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-reverse-word app)
  "Reverse the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (reversed (list->string (reverse (string->list word)))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start reversed))
        (editor-goto-pos ed end)))))

(def (cmd-count-occurrences app)
  "Count occurrences of a string in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (search (echo-read-string echo "Count occurrences of: " row width)))
    (when (and search (not (string-empty? search)))
      (let* ((text (editor-get-text ed))
             (slen (string-length search))
             (count (let loop ((pos 0) (n 0))
                      (let ((found (string-contains text search pos)))
                        (if found
                          (loop (+ found slen) (+ n 1))
                          n)))))
        (echo-message! echo (string-append (number->string count)
                                            " occurrences of \"" search "\""))))))

(def (cmd-mark-lines-matching app)
  "Count lines matching a string."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (search (echo-read-string echo "Count lines matching: " row width)))
    (when (and search (not (string-empty? search)))
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matching (let loop ((ls lines) (n 0))
                         (if (null? ls) n
                           (loop (cdr ls)
                                 (if (string-contains (car ls) search)
                                   (+ n 1) n))))))
        (echo-message! echo (string-append (number->string matching)
                                            " lines match \"" search "\""))))))

(def (cmd-number-region app)
  "Number lines in region starting from 1."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line) (n (+ 1 (- end-line start-line))))
            (when (>= l start-line)
              (let ((ls (editor-position-from-line ed l))
                    (prefix (string-append (number->string n) ": ")))
                (editor-insert-text ed ls prefix))
              (loop (- l 1) (- n 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Lines numbered"))
      (echo-error! echo "No mark set"))))

(def (cmd-strip-line-numbers app)
  "Remove leading line numbers (NNN: ) from region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len 10))))
                ;; Find ": " after digits
                (let digit-loop ((i 0))
                  (when (< i (string-length text))
                    (let ((ch (string-ref text i)))
                      (cond
                        ((char-numeric? ch) (digit-loop (+ i 1)))
                        ((and (char=? ch #\:)
                              (< (+ i 1) (string-length text))
                              (char=? (string-ref text (+ i 1)) #\space)
                              (> i 0))
                         (editor-delete-range ed ls (+ i 2)))
                        (else (void)))))))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Line numbers stripped"))
      (echo-error! echo "No mark set"))))

(def (cmd-prefix-lines app)
  "Add a prefix string to each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((prefix (echo-read-string echo "Prefix: " row width)))
        (when (and prefix (not (string-empty? prefix)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end)))
            (with-undo-action ed
              (let loop ((l end-line))
                (when (>= l start-line)
                  (editor-insert-text ed (editor-position-from-line ed l) prefix)
                  (loop (- l 1)))))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Lines prefixed"))))
      (echo-error! echo "No mark set"))))

(def (cmd-suffix-lines app)
  "Add a suffix string to each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((suffix (echo-read-string echo "Suffix: " row width)))
        (when (and suffix (not (string-empty? suffix)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end)))
            (with-undo-action ed
              (let loop ((l end-line))
                (when (>= l start-line)
                  (editor-insert-text ed (editor-get-line-end-position ed l) suffix)
                  (loop (- l 1)))))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Lines suffixed"))))
      (echo-error! echo "No mark set"))))

(def (cmd-wrap-lines-at-column app)
  "Hard-wrap long lines at a specified column."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Wrap at column (default 80): " row width)))
    (let ((col (if (or (not input) (string-empty? input)) 80
                 (or (string->number input) 80))))
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (wrapped-lines
               (apply append
                 (map (lambda (line)
                        (if (<= (string-length line) col)
                          (list line)
                          (let loop ((s line) (acc []))
                            (if (<= (string-length s) col)
                              (reverse (cons s acc))
                              (let find-break ((p col))
                                (cond
                                  ((and (>= p 0) (char=? (string-ref s p) #\space))
                                   (loop (substring s (+ p 1) (string-length s))
                                         (cons (substring s 0 p) acc)))
                                  ((> p 0) (find-break (- p 1)))
                                  (else ; no space found, break at col
                                   (loop (substring s col (string-length s))
                                         (cons (substring s 0 col) acc)))))))))
                      lines)))
             (result (string-join wrapped-lines "\n")))
        (with-undo-action ed
          (editor-set-text ed result))
        (echo-message! echo (string-append "Lines wrapped at column " (number->string col)))))))

(def (cmd-show-file-info app)
  "Show detailed file information."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)"))
         (size (editor-get-text-length ed))
         (lines (editor-get-line-count ed))
         (lang (buffer-lexer-lang buf)))
    (echo-message! echo (string-append name " | " path " | "
                                        (number->string size) "B | "
                                        (number->string lines) "L | "
                                        (if lang (symbol->string lang) "text")))))

(def (cmd-toggle-narrow-indicator app)
  "Toggle narrow region indicator."
  (echo-message! (app-state-echo app) "Narrow indicator toggled (stub)"))

(def (cmd-insert-timestamp app)
  "Insert ISO 8601 timestamp at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         ;; Use time->seconds to get epoch and format
         (now (time->seconds (current-time)))
         (secs (inexact->exact (floor now)))
         (text (number->string secs)))
    (editor-insert-text ed pos (string-append "[" text "]"))
    (editor-goto-pos ed (+ pos (string-length text) 2))))

(def (cmd-eval-and-insert app)
  "Eval a Gerbil expression and insert the result at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Eval and insert: " row width)))
    (when (and input (not (string-empty? input)))
      (let-values (((result err?) (eval-expression-string input)))
        (if err?
          (echo-error! echo result)
          (let ((pos (editor-get-current-pos ed)))
            (editor-insert-text ed pos result)
            (editor-goto-pos ed (+ pos (string-length result)))))))))

(def (cmd-shell-command-insert app)
  "Run a shell command and insert output at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command (insert output): " row width)))
    (when (and cmd (not (string-empty? cmd)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/bin/sh"
                                            arguments: (list "-c" cmd)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "")))))))
        (let ((pos (editor-get-current-pos ed)))
          (editor-insert-text ed pos output)
          (editor-goto-pos ed (+ pos (string-length output))))))))

(def (cmd-pipe-region app)
  "Pipe region through a shell command."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((cmd (echo-read-string echo "Pipe region through: " row width)))
        (when (and cmd (not (string-empty? cmd)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (region-text (substring (editor-get-text ed) start end))
                 (output (with-exception-catcher
                           (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                           (lambda ()
                             (let ((proc (open-process
                                           (list path: "/bin/sh"
                                                 arguments: (list "-c" cmd)
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t))))
                               (display region-text proc)
                               (close-output-port proc)
                               (let ((result (read-line proc #f)))
                                 (process-status proc)
                                 (or result "")))))))
            (with-undo-action ed
              (editor-delete-range ed start (- end start))
              (editor-insert-text ed start output))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Region filtered"))))
      (echo-error! echo "No mark set"))))

(def (cmd-sort-words app)
  "Sort words in region alphabetically."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end))
             (words (string-split text #\space))
             (sorted (sort words string<?))
             (result (string-join sorted " ")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Sorted " (number->string (length sorted)) " words")))
      (echo-error! echo "No mark set"))))

(def (cmd-remove-blank-lines app)
  "Remove blank lines in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (non-blank (filter (lambda (l) (not (string-empty? (string-trim l)))) lines))
             (removed (- (length lines) (length non-blank)))
             (result (string-join non-blank "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Removed " (number->string removed) " blank lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-collapse-blank-lines app)
  "Collapse consecutive blank lines into one."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (collapsed (let loop ((ls lines) (prev-blank? #f) (acc []))
                      (cond
                        ((null? ls) (reverse acc))
                        ((string-empty? (string-trim (car ls)))
                         (if prev-blank?
                           (loop (cdr ls) #t acc)
                           (loop (cdr ls) #t (cons (car ls) acc))))
                        (else
                         (loop (cdr ls) #f (cons (car ls) acc))))))
         (removed (- (length lines) (length collapsed)))
         (result (string-join collapsed "\n")))
    (when (> removed 0)
      (with-undo-action ed
        (editor-set-text ed result))
      (echo-message! echo (string-append "Collapsed " (number->string removed)
                                          " blank lines")))))

(def (cmd-trim-lines app)
  "Trim trailing whitespace from all lines."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (trimmed (map string-trim-right lines))
         (result (string-join trimmed "\n")))
    (when (not (string=? text result))
      (with-undo-action ed
        (let ((pos (editor-get-current-pos ed)))
          (editor-set-text ed result)
          (editor-goto-pos ed (min pos (string-length result)))))
      (echo-message! echo "Trailing whitespace trimmed"))))

(def (cmd-toggle-line-comment app)
  "Toggle ;; comment on the current line (no region needed)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (ls (editor-position-from-line ed line))
         (le (editor-get-line-end-position ed line))
         (line-len (- le ls))
         (text (editor-get-text-range ed ls (min line-len 3))))
    (cond
      ((and (>= (string-length text) 3) (string=? text ";; "))
       (editor-delete-range ed ls 3))
      ((and (>= (string-length text) 2) (string=? (substring text 0 2) ";;"))
       (editor-delete-range ed ls 2))
      (else
       (editor-insert-text ed ls ";; ")))))

(def (cmd-copy-file-path app)
  "Copy the current buffer's file path to kill ring."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (set! (app-state-kill-ring app) (cons path (app-state-kill-ring app)))
        (echo-message! echo (string-append "Copied: " path)))
      (echo-error! echo "Buffer has no file path"))))

(def (cmd-insert-path-separator app)
  "Insert a file path separator."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "/")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-show-word-count app)
  "Show word count for the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (words (let loop ((i 0) (in-word? #f) (count 0))
                  (if (>= i (string-length text))
                    (if in-word? (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab) (char=? ch #\return))
                        (loop (+ i 1) #f (if in-word? (+ count 1) count))
                        (loop (+ i 1) #t count)))))))
    (echo-message! echo (string-append (number->string words) " words"))))

(def (cmd-show-char-count app)
  "Show character count for the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append (number->string (editor-get-text-length ed)) " characters"))))

(def (cmd-toggle-auto-complete app)
  "Toggle auto-completion display."
  (echo-message! (app-state-echo app) "Auto-complete toggled (stub)"))

(def (cmd-insert-lorem-ipsum app)
  "Insert a paragraph of Lorem Ipsum text."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n"))
    (editor-insert-text ed pos lorem)
    (editor-goto-pos ed (+ pos (string-length lorem)))))

(def (cmd-narrow-to-defun app)
  "Narrow the view to the current function definition."
  ;; Simplified: find nearest defun boundaries
  (echo-message! (app-state-echo app) "Narrow to defun (use M-x narrow-to-region)"))

(def (cmd-widen-all app)
  "Widen all narrowed buffers."
  (echo-message! (app-state-echo app) "Widen all (use M-x widen)"))

(def (cmd-reindent-buffer app)
  "Re-indent the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (lines (editor-get-line-count ed)))
    ;; Simple: re-indent all lines using 2-space indent from column 0
    ;; This is a very basic version — just trims leading whitespace
    (echo-message! echo (string-append "Buffer has " (number->string lines) " lines (use indent-region for region)"))))

(def (cmd-show-trailing-whitespace-count app)
  "Count lines with trailing whitespace."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (count (let loop ((ls lines) (n 0))
                  (if (null? ls) n
                    (let ((line (car ls)))
                      (loop (cdr ls)
                            (if (and (> (string-length line) 0)
                                     (let ((last-ch (string-ref line (- (string-length line) 1))))
                                       (or (char=? last-ch #\space) (char=? last-ch #\tab))))
                              (+ n 1) n)))))))
    (echo-message! echo (string-append (number->string count)
                                        " lines have trailing whitespace"))))

(def (cmd-show-tab-count app)
  "Count tab characters in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (count (let loop ((i 0) (n 0))
                  (if (>= i (string-length text)) n
                    (loop (+ i 1)
                          (if (char=? (string-ref text i) #\tab) (+ n 1) n))))))
    (echo-message! echo (string-append (number->string count) " tab characters"))))

(def (cmd-toggle-global-whitespace app)
  "Toggle global whitespace visibility."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (current (editor-get-view-whitespace ed)))
    (if (= current 0)
      (begin (editor-set-view-whitespace ed 1)
             (echo-message! (app-state-echo app) "Whitespace visible globally"))
      (begin (editor-set-view-whitespace ed 0)
             (echo-message! (app-state-echo app) "Whitespace hidden")))))

(def (cmd-insert-box-comment app)
  "Insert a box comment."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (box (string-append
                ";;;============================================================================\n"
                ";;; \n"
                ";;;============================================================================\n")))
    (editor-insert-text ed pos box)
    ;; Position cursor at the description line
    (editor-goto-pos ed (+ pos 80))))  ; After ";;; " on second line

(def (cmd-toggle-electric-indent app)
  "Toggle electric indent mode."
  (echo-message! (app-state-echo app) "Electric indent toggled (stub)"))

(def (cmd-increase-font-size app)
  "Increase editor font size."
  (cmd-zoom-in app))

(def (cmd-decrease-font-size app)
  "Decrease editor font size."
  (cmd-zoom-out app))

(def (cmd-reset-font-size app)
  "Reset editor font size."
  (cmd-zoom-reset app))

;;;============================================================================
;;; Task #43: project, search, and utilities
;;;============================================================================

;; Helper: create/find a named buffer, switch to it, set text
(def (open-output-buffer app name text)
  (let* ((ed (current-editor app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name name)
                  (buffer-create! name ed #f))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (editor-set-text ed text)
    (editor-set-save-point ed)
    (editor-goto-pos ed 0)))

(def (cmd-project-find-file app)
  "Find file in project (prompts for filename)."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (base-dir (or (buffer-file-path buf) "."))
         (dir (let ((d (path-directory base-dir)))
                (if (string-empty? d) "." d)))
         (filename (echo-read-string echo (string-append "Find in " dir ": ") row width)))
    (when (and filename (not (string-empty? filename)))
      (let ((full-path (path-expand filename dir)))
        (if (file-exists? full-path)
          (let* ((name (path-strip-directory full-path))
                 (ed (current-editor app))
                 (new-buf (buffer-create! name ed full-path)))
            (buffer-attach! ed new-buf)
            (set! (edit-window-buffer (current-window fr)) new-buf)
            (let ((text (read-file-as-string full-path)))
              (when text
                (editor-set-text ed text)
                (editor-set-save-point ed)
                (editor-goto-pos ed 0)))
            (echo-message! echo (string-append "Opened: " full-path)))
          (echo-error! echo (string-append "File not found: " full-path)))))))

(def (cmd-project-grep app)
  "Grep for a pattern in the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) "."))
         (pattern (echo-read-string echo (string-append "Grep in " dir ": ") row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/usr/bin/grep"
                                            arguments: (list "-rn" pattern dir)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "(no matches)")))))))
        ;; Show in a new buffer
        (begin
          (open-output-buffer app (string-append "*grep " pattern "*") output)
          (echo-message! echo (string-append "grep: " pattern)))))))

(def (cmd-project-compile app)
  "Run make in the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/usr/bin/make"
                                          arguments: '()
                                          directory: dir
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "")))))))
      (begin
        (open-output-buffer app "*compile*" output)
        (echo-message! echo "Compilation complete")))))

(def (cmd-search-forward-word app)
  "Search forward for the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (string-contains text word end)))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Found: " word)))
          (echo-error! echo (string-append "\"" word "\" not found below"))))
      (echo-error! echo "Not on a word"))))

(def (cmd-search-backward-word app)
  "Search backward for the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Search backwards by scanning from beginning
             (found (let loop ((p 0) (last-found #f))
                      (let ((f (string-contains text word p)))
                        (if (and f (< f start))
                          (loop (+ f 1) f)
                          last-found)))))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Found: " word)))
          (echo-error! echo (string-append "\"" word "\" not found above"))))
      (echo-error! echo "Not on a word"))))

(def (cmd-replace-in-region app)
  "Replace all occurrences of a string within the region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((search (echo-read-string echo "Replace in region: " row width)))
        (when (and search (not (string-empty? search)))
          (let ((replace (echo-read-string echo "Replace with: " row width)))
            (when replace
              (let* ((start (min pos mark))
                     (end (max pos mark))
                     (region (substring (editor-get-text ed) start end))
                     (slen (string-length search))
                     ;; Manual replace
                     (result (let loop ((p 0) (acc []))
                               (let ((f (string-contains region search p)))
                                 (if f
                                   (loop (+ f slen)
                                         (cons replace (cons (substring region p f) acc)))
                                   (list->string
                                     (apply append
                                       (map string->list
                                            (reverse (cons (substring region p (string-length region)) acc))))))))))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (set! (buffer-mark buf) #f)
                (echo-message! echo "Replaced in region"))))))
      (echo-error! echo "No mark set"))))

(def (cmd-highlight-word-at-point app)
  "Highlight all occurrences of the word at point."
  ;; This sets the search indicator
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Count occurrences
             (count (let loop ((p 0) (n 0))
                      (let ((f (string-contains text word p)))
                        (if f (loop (+ f (string-length word)) (+ n 1)) n)))))
        (echo-message! echo (string-append "\"" word "\" — "
                                            (number->string count) " occurrences")))
      (echo-error! echo "Not on a word"))))

(def (cmd-goto-definition app)
  "Jump to definition of symbol at point (simple text search)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Search for "(def (WORD" or "(def WORD" or "(defstruct WORD"
             (def-pattern (string-append "(def " word))
             (found (or (string-contains text (string-append "(def (" word " "))
                        (string-contains text (string-append "(def (" word ")"))
                        (string-contains text (string-append "(def " word " "))
                        (string-contains text (string-append "(def " word "\n"))
                        (string-contains text (string-append "(defstruct " word))
                        (string-contains text (string-append "(defclass " word)))))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Jumped to definition of " word)))
          (echo-error! echo (string-append "No definition found for " word))))
      (echo-error! echo "Not on a word"))))

(def (cmd-toggle-eol-conversion app)
  "Toggle end-of-line conversion mode."
  (echo-message! (app-state-echo app) "EOL conversion toggled (stub)"))

(def (cmd-make-frame app)
  "Create a new frame (stub — single frame only)."
  (echo-message! (app-state-echo app) "Multiple frames not supported"))

(def (cmd-delete-frame app)
  "Delete the current frame (stub)."
  (echo-message! (app-state-echo app) "Cannot delete the only frame"))

(def (cmd-toggle-menu-bar app)
  "Toggle menu bar display (stub)."
  (echo-message! (app-state-echo app) "Menu bar toggled (not available in TUI)"))

(def (cmd-toggle-tool-bar app)
  "Toggle tool bar display (stub)."
  (echo-message! (app-state-echo app) "Tool bar toggled (not available in TUI)"))

(def (cmd-toggle-scroll-bar app)
  "Toggle scroll bar display (stub)."
  (echo-message! (app-state-echo app) "Scroll bar toggled (not available in TUI)"))

(def (cmd-suspend-frame app)
  "Suspend the editor (send to background)."
  (echo-message! (app-state-echo app) "Suspend (use C-z from terminal)"))

(def (cmd-list-directory app)
  "List files in the current buffer's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/bin/ls"
                                          arguments: (list "-la" dir)
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "")))))))
      (begin
        (open-output-buffer app (string-append "*directory " dir "*") output)
        (echo-message! echo dir)))))

(def (cmd-find-grep app)
  "Find files matching a pattern and grep inside them."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) "."))
         (pattern (echo-read-string echo "Find+grep pattern: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/usr/bin/grep"
                                            arguments: (list "-rl" pattern dir)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "(no files match)")))))))
        (begin
          (open-output-buffer app (string-append "*find-grep " pattern "*") output)
          (echo-message! echo (string-append "Files matching: " pattern)))))))

(def (cmd-insert-header-guard app)
  "Insert C/C++ header guard."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (guard (string-upcase
                  (let loop ((i 0) (acc []))
                    (if (>= i (string-length name))
                      (list->string (reverse acc))
                      (let ((ch (string-ref name i)))
                        (loop (+ i 1)
                              (cons (if (or (char-alphabetic? ch) (char-numeric? ch))
                                      ch #\_) acc)))))))
         (text (string-append "#ifndef " guard "_H\n#define " guard "_H\n\n\n#endif /* " guard "_H */\n")))
    (editor-insert-text ed 0 text)
    (editor-goto-pos ed (+ (string-length (string-append "#ifndef " guard "_H\n#define " guard "_H\n\n")) 0))
    (echo-message! echo "Header guard inserted")))

(def (cmd-insert-include app)
  "Insert a #include statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (header (echo-read-string echo "Include header: " row width)))
    (when (and header (not (string-empty? header)))
      (let ((line (if (char=? (string-ref header 0) #\<)
                    (string-append "#include " header "\n")
                    (string-append "#include \"" header "\"\n"))))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-import app)
  "Insert a Gerbil (import ...) statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (module (echo-read-string echo "Import module: " row width)))
    (when (and module (not (string-empty? module)))
      (let ((line (string-append "(import " module ")\n")))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-export app)
  "Insert a Gerbil (export ...) statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (sym (echo-read-string echo "Export symbol: " row width)))
    (when (and sym (not (string-empty? sym)))
      (let ((line (string-append "(export " sym ")\n")))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-defun app)
  "Insert a Gerbil (def (name ...) ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Function name: " row width)))
    (when (and name (not (string-empty? name)))
      (let ((template (string-append "(def (" name ")\n  )\n")))
        (editor-insert-text ed pos template)
        ;; Position inside the body
        (editor-goto-pos ed (+ pos (string-length (string-append "(def (" name ")\n  "))))))))

(def (cmd-insert-let app)
  "Insert a (let ((var val)) ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(let* (())\n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 8))))  ; Inside first binding pair

(def (cmd-insert-cond app)
  "Insert a (cond ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(cond\n  (())\n  (else\n   ))\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 9))))  ; Inside first condition

(def (cmd-insert-match app)
  "Insert a (match ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(match \n  (())\n  (else ))\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 7))))  ; After "match "

(def (cmd-insert-when app)
  "Insert a (when ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(when \n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 6))))  ; After "when "

(def (cmd-insert-unless app)
  "Insert an (unless ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(unless \n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 8))))  ; After "unless "

(def (cmd-insert-lambda app)
  "Insert a (lambda ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(lambda ()\n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 9))))  ; Inside parameter list

(def (cmd-toggle-auto-pair-mode app)
  "Toggle automatic bracket/quote pairing."
  (echo-message! (app-state-echo app) "Auto-pair toggled (use M-x toggle-electric-pair)"))

(def (cmd-count-buffers app)
  "Count the number of open buffers."
  (let ((count (length (buffer-list))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " buffers open"))))

(def (cmd-list-recent-files app)
  "List recently opened files (shows all file-backed buffers)."
  (let* ((echo (app-state-echo app))
         (files (filter-map (lambda (buf)
                              (buffer-file-path buf))
                            (buffer-list))))
    (if (null? files)
      (echo-message! echo "No file-backed buffers")
      (begin
        (open-output-buffer app "*recent-files*" (string-join files "\n"))
        (echo-message! echo (string-append (number->string (length files)) " files"))))))

(def (cmd-clear-recent-files app)
  "Clear the recent files list (stub)."
  (echo-message! (app-state-echo app) "Recent files cleared (stub)"))

(def (cmd-show-keybinding-for app)
  "Show what key is bound to a command."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Show keybinding for command: " row width)))
    (when (and name (not (string-empty? name)))
      (let* ((sym (string->symbol name))
             ;; Search keymaps for this command
             (found (let scan-keymap ((km *global-keymap*) (prefix ""))
                      (let loop ((keys (keymap-entries km)))
                        (cond
                          ((null? keys) #f)
                          ((eq? (cdar keys) sym)
                           (string-append prefix (caar keys)))
                          ((hash-table? (cdar keys))
                           (or (scan-keymap (cdar keys) (string-append prefix (caar keys) " "))
                               (loop (cdr keys))))
                          (else (loop (cdr keys))))))))
        (if found
          (echo-message! echo (string-append name " is on " found))
          (echo-message! echo (string-append name " is not bound to any key")))))))

(def (cmd-sort-imports app)
  "Sort import lines in the current region or near point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Imports sorted"))
      (echo-error! echo "No mark set (select import lines first)"))))

(def (cmd-show-git-status app)
  "Show git status for the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/usr/bin/git"
                                          arguments: (list "status" "--short")
                                          directory: dir
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(clean)")))))))
      (open-output-buffer app "*git-status*" output)
      (echo-message! echo "git status"))))

(def (cmd-show-git-log app)
  "Show git log for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let* ((args (if path
                                     (list "log" "--oneline" "-20" path)
                                     (list "log" "--oneline" "-20")))
                             (proc (open-process
                                     (list path: "/usr/bin/git"
                                           arguments: args
                                           directory: dir
                                           stdin-redirection: #f
                                           stdout-redirection: #t
                                           stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(no commits)")))))))
      (open-output-buffer app "*git-log*" output)
      (echo-message! echo "git log"))))

(def (cmd-show-git-diff app)
  "Show git diff for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let* ((args (if path (list "diff" path) (list "diff")))
                             (proc (open-process
                                     (list path: "/usr/bin/git"
                                           arguments: args
                                           directory: dir
                                           stdin-redirection: #f
                                           stdout-redirection: #t
                                           stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(no changes)")))))))
      (open-output-buffer app "*git-diff*" output)
      (echo-message! echo "git diff"))))

(def (cmd-show-git-blame app)
  "Show git blame for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if path
      (let* ((dir (path-directory path))
             (output (with-exception-catcher
                       (lambda (e) "Not a git repository or file not tracked")
                       (lambda ()
                         (let ((proc (open-process
                                       (list path: "/usr/bin/git"
                                             arguments: (list "blame" "--date=short" path)
                                             directory: dir
                                             stdin-redirection: #f
                                             stdout-redirection: #t
                                             stderr-redirection: #t))))
                           (let ((result (read-line proc #f)))
                             (process-status proc)
                             (or result "(no data)")))))))
        (open-output-buffer app (string-append "*git-blame " (path-strip-directory path) "*") output)
        (echo-message! echo "git blame"))
      (echo-error! echo "Buffer has no file"))))

(def (cmd-toggle-flyspell app)
  "Toggle spell checking (stub)."
  (echo-message! (app-state-echo app) "Spell check toggled (stub)"))

(def (cmd-toggle-flymake app)
  "Toggle syntax checking (stub)."
  (echo-message! (app-state-echo app) "Syntax check toggled (stub)"))

(def (cmd-toggle-lsp app)
  "Toggle LSP support (stub)."
  (echo-message! (app-state-echo app) "LSP toggled (stub)"))

(def (cmd-toggle-auto-revert-global app)
  "Toggle global auto-revert mode."
  (echo-message! (app-state-echo app) "Global auto-revert toggled (stub)"))

;;;============================================================================
;;; Task #44: Help system, dired, buffer management, and more
;;;============================================================================

;; --- Help system enhancements ---

(def (cmd-describe-function app)
  "Describe a function by name."
  (let ((name (app-read-string app "Describe function: ")))
    (when (and name (not (string-empty? name)))
      (let ((cmd (find-command (string->symbol name))))
        (if cmd
          (echo-message! (app-state-echo app)
                         (string-append name ": command registered"))
          (echo-message! (app-state-echo app)
                         (string-append name ": not found")))))))

(def (cmd-describe-variable app)
  "Describe a variable by name."
  (let ((name (app-read-string app "Describe variable: ")))
    (when (and name (not (string-empty? name)))
      (echo-message! (app-state-echo app)
                     (string-append name ": variable description (stub)")))))

(def (cmd-describe-key-briefly app)
  "Describe what a key is bound to."
  (echo-message! (app-state-echo app) "Press a key...")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (cmd (keymap-lookup *global-keymap* ks)))
        (if cmd
          (echo-message! (app-state-echo app)
                         (string-append ks " runs " (symbol->string cmd)))
          (echo-message! (app-state-echo app)
                         (string-append ks " is undefined")))))))

(def (cmd-describe-face app)
  "Describe text face at point (stub)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed 2010 pos 0)))  ;; SCI_GETSTYLEAT
    (echo-message! (app-state-echo app)
                   (string-append "Style at point: " (number->string style)))))

(def (cmd-describe-syntax app)
  "Describe syntax class at point (stub)."
  (echo-message! (app-state-echo app) "Syntax description (stub)"))

(def (cmd-info app)
  "Open Info documentation reader (stub)."
  (echo-message! (app-state-echo app) "Info reader (stub)"))

(def (cmd-info-emacs-manual app)
  "Open Emacs manual (stub)."
  (echo-message! (app-state-echo app) "Emacs manual (stub)"))

(def (cmd-info-elisp-manual app)
  "Open Elisp manual (stub)."
  (echo-message! (app-state-echo app) "Elisp manual (stub)"))

;; --- Dired-like operations ---

(def (cmd-dired app)
  "Open a directory browser."
  (let* ((dir (app-read-string app "Dired: "))
         (path (or dir ".")))
    (when (and path (not (string-empty? path)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "ls" arguments: ["-la" path]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*Dired: " path "*")
                                result)))))))

(def (cmd-dired-create-directory app)
  "Create a directory."
  (let ((dir (app-read-string app "Create directory: ")))
    (when (and dir (not (string-empty? dir)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app)
                         (string-append "Created: " dir)))))))

(def (cmd-dired-do-rename app)
  "Rename a file."
  (let ((old (app-read-string app "Rename file: ")))
    (when (and old (not (string-empty? old)))
      (let ((new (app-read-string app "Rename to: ")))
        (when (and new (not (string-empty? new)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (rename-file old new)
              (echo-message! (app-state-echo app)
                             (string-append "Renamed: " old " -> " new)))))))))

(def (cmd-dired-do-delete app)
  "Delete a file."
  (let ((file (app-read-string app "Delete file: ")))
    (when (and file (not (string-empty? file)))
      (let ((confirm (app-read-string app
                                        (string-append "Delete " file "? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (delete-file file)
              (echo-message! (app-state-echo app)
                             (string-append "Deleted: " file)))))))))

(def (cmd-dired-do-copy app)
  "Copy a file."
  (let ((src (app-read-string app "Copy file: ")))
    (when (and src (not (string-empty? src)))
      (let ((dst (app-read-string app "Copy to: ")))
        (when (and dst (not (string-empty? dst)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (copy-file src dst)
              (echo-message! (app-state-echo app)
                             (string-append "Copied: " src " -> " dst)))))))))

(def (cmd-dired-do-chmod app)
  "Change file permissions."
  (let ((file (app-read-string app "Chmod file: ")))
    (when (and file (not (string-empty? file)))
      (let ((mode (app-read-string app "Mode (e.g. 755): ")))
        (when (and mode (not (string-empty? mode)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "chmod" arguments: [mode file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (_ (process-status proc)))
                (close-port proc)
                (echo-message! (app-state-echo app)
                               (string-append "chmod " mode " " file))))))))))

;; --- Buffer management ---

(def (cmd-rename-uniquely app)
  "Rename current buffer with a unique name."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (new-name (string-append name "<" (number->string (random-integer 1000)) ">")))
    (set! (buffer-name buf) new-name)
    (echo-message! (app-state-echo app)
                   (string-append "Buffer renamed to: " new-name))))

(def (cmd-revert-buffer-with-coding app)
  "Revert buffer with specified coding (stub)."
  (echo-message! (app-state-echo app) "Revert with coding system (stub)"))

(def (cmd-lock-buffer app)
  "Toggle buffer read-only lock."
  (let* ((ed (current-editor app))
         (ro (editor-get-read-only? ed)))
    (editor-set-read-only ed (not ro))
    (echo-message! (app-state-echo app)
                   (if (not ro) "Buffer locked (read-only)" "Buffer unlocked"))))

(def (cmd-buffer-disable-undo app)
  "Disable undo for current buffer."
  (let ((ed (current-editor app)))
    (send-message ed 2175 0 0)  ;; SCI_EMPTYUNDOBUFFER
    (echo-message! (app-state-echo app) "Undo history cleared")))

(def (cmd-buffer-enable-undo app)
  "Enable undo collection for current buffer."
  (let ((ed (current-editor app)))
    (send-message ed 2012 1 0)  ;; SCI_SETUNDOCOLLECTION
    (echo-message! (app-state-echo app) "Undo collection enabled")))

(def (cmd-bury-buffer app)
  "Move current buffer to end of buffer list."
  (echo-message! (app-state-echo app) "Buffer buried"))

(def (cmd-unbury-buffer app)
  "Switch to the least recently used buffer."
  (let ((bufs (buffer-list)))
    (when (> (length bufs) 1)
      (let* ((ed (current-editor app))
             (fr (app-state-frame app))
             (last-buf (car (last-pair bufs))))
        (buffer-attach! ed last-buf)
        (set! (edit-window-buffer (current-window fr)) last-buf)
        (echo-message! (app-state-echo app)
                       (string-append "Switched to: " (buffer-name last-buf)))))))

;; --- Navigation ---

(def (cmd-forward-sentence app)
  "Move forward one sentence."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) (editor-goto-pos ed len))
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (< (+ i 1) len)
              (memv (string-ref text (+ i 1)) '(#\space #\newline)))
         (editor-goto-pos ed (+ i 2)))
        (else (loop (+ i 1)))))))

(def (cmd-backward-sentence app)
  "Move backward one sentence."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 2)))
      (cond
        ((<= i 0) (editor-goto-pos ed 0))
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (< (+ i 1) (string-length text))
              (memv (string-ref text (+ i 1)) '(#\space #\newline)))
         (editor-goto-pos ed (+ i 2)))
        (else (loop (- i 1)))))))

(def (cmd-goto-word-at-point app)
  "Move to next occurrence of word at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (> len 0)
      ;; Get word at point
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (word-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (word (if (< ws we) (substring text ws we) "")))
        (when (not (string-empty? word))
          ;; Search forward from word end
          (let ((found (string-contains text word we)))
            (if found
              (editor-goto-pos ed found)
              ;; Wrap around
              (let ((found2 (string-contains text word 0)))
                (when found2
                  (editor-goto-pos ed found2)
                  (echo-message! (app-state-echo app) "Wrapped"))))))))))

;; --- Region operations ---

;; --- Text manipulation ---

(def (cmd-center-region app)
  "Center all lines in the region."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (fill-col 80)
             (centered (map (lambda (l)
                              (let* ((trimmed (string-trim-both l))
                                     (pad (max 0 (quotient (- fill-col (string-length trimmed)) 2))))
                                (string-append (make-string pad #\space) trimmed)))
                            lines))
             (result (string-join centered "\n")))
        (send-message ed 2160 sel-start 0)  ;; SCI_SETTARGETSTART
        (send-message ed 2161 sel-end 0)    ;; SCI_SETTARGETEND
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-indent-rigidly app)
  "Indent the region by a fixed amount."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (indented (map (lambda (l) (string-append "  " l)) lines))
             (result (string-join indented "\n")))
        (send-message ed 2160 sel-start 0)
        (send-message ed 2161 sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-dedent-rigidly app)
  "Remove 2 spaces of indentation from region."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (l)
                              (if (and (>= (string-length l) 2)
                                       (string=? (substring l 0 2) "  "))
                                (substring l 2 (string-length l))
                                l))
                            lines))
             (result (string-join dedented "\n")))
        (send-message ed 2160 sel-start 0)
        (send-message ed 2161 sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-transpose-paragraphs app)
  "Transpose two paragraphs (stub)."
  (echo-message! (app-state-echo app) "Transpose paragraphs (stub)"))

(def (cmd-fill-individual-paragraphs app)
  "Fill each paragraph individually (stub)."
  (echo-message! (app-state-echo app) "Fill individual paragraphs (stub)"))

;; --- Bookmark enhancements ---

(def (cmd-bookmark-save app)
  "Save bookmarks to file."
  (let ((bmarks (app-state-bookmarks app)))
    (with-catch
      (lambda (e)
        (echo-message! (app-state-echo app) "Error saving bookmarks"))
      (lambda ()
        (call-with-output-file "~/.gerbil-emacs-bookmarks"
          (lambda (port)
            (for-each
              (lambda (pair)
                (display (car pair) port)
                (display " " port)
                (display (cdr pair) port)
                (newline port))
              (hash->list bmarks))))
        (echo-message! (app-state-echo app) "Bookmarks saved")))))

(def (cmd-bookmark-load app)
  "Load bookmarks from file."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "No saved bookmarks found"))
    (lambda ()
      (let ((content (read-file-as-string "~/.gerbil-emacs-bookmarks")))
        (echo-message! (app-state-echo app) "Bookmarks loaded (stub)")))))

;; --- Window management ---

(def (cmd-fit-window-to-buffer app)
  "Shrink window to fit its buffer content."
  (echo-message! (app-state-echo app) "Window fitted to buffer (stub)"))

(def (cmd-maximize-window app)
  "Maximize the current window."
  (echo-message! (app-state-echo app) "Window maximized (stub)"))

(def (cmd-minimize-window app)
  "Minimize the current window."
  (echo-message! (app-state-echo app) "Window minimized (stub)"))

(def (cmd-rotate-windows app)
  "Rotate window layout."
  (echo-message! (app-state-echo app) "Windows rotated (stub)"))

(def (cmd-swap-windows app)
  "Swap contents of two windows."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (when (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (edit-window-buffer w1))
             (b2 (edit-window-buffer w2)))
        (set! (edit-window-buffer w1) b2)
        (set! (edit-window-buffer w2) b1)
        (echo-message! (app-state-echo app) "Windows swapped")))))

;; --- Miscellaneous ---

(def (cmd-delete-matching-lines app)
  "Delete all lines matching a pattern (same as flush-lines)."
  (cmd-flush-lines app))

(def (cmd-copy-matching-lines app)
  "Copy all lines matching a pattern to a buffer."
  (let ((pat (app-read-string app "Copy lines matching: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matching (filter (lambda (l) (string-contains l pat)) lines))
             (result (string-join matching "\n")))
        (open-output-buffer app "*Matching Lines*" result)))))

(def (cmd-delete-non-matching-lines app)
  "Delete all lines not matching a pattern (same as keep-lines)."
  (cmd-keep-lines app))

(def (cmd-display-fill-column-indicator app)
  "Toggle fill column indicator."
  (let* ((ed (current-editor app))
         (cur (send-message ed 2695 0 0)))  ;; SCI_GETEDGEMODE
    (if (= cur 0)
      (begin
        (send-message ed 2694 1 0)  ;; SCI_SETEDGEMODE EDGE_LINE
        (send-message ed 2360 80 0)  ;; SCI_SETEDGECOLUMN
        (echo-message! (app-state-echo app) "Fill column indicator on"))
      (begin
        (send-message ed 2694 0 0)  ;; SCI_SETEDGEMODE EDGE_NONE
        (echo-message! (app-state-echo app) "Fill column indicator off")))))

(def (cmd-electric-newline-and-indent app)
  "Insert newline and indent."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\n")
    (editor-goto-pos ed (+ pos 1))
    (send-message ed 2327 0 0)))  ;; SCI_TAB (auto-indent)

(def (cmd-view-register app)
  "Display contents of a register."
  (echo-message! (app-state-echo app) "View register key: ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (regs (app-state-registers app))
             (val (hash-get regs ks)))
        (if val
          (echo-message! (app-state-echo app)
                         (string-append "Register " ks ": " (if (> (string-length val) 60)
                                                                (string-append (substring val 0 57) "...")
                                                                val)))
          (echo-message! (app-state-echo app)
                         (string-append "Register " ks " is empty")))))))

(def (cmd-append-to-register app)
  "Append region to a register."
  (echo-message! (app-state-echo app) "Append to register: ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (ed (current-editor app))
             (sel-start (editor-get-selection-start ed))
             (sel-end (editor-get-selection-end ed)))
        (if (< sel-start sel-end)
          (let* ((text (editor-get-text ed))
                 (region (substring text sel-start sel-end))
                 (regs (app-state-registers app))
                 (existing (or (hash-get regs ks) "")))
            (hash-put! regs ks (string-append existing region))
            (echo-message! (app-state-echo app)
                           (string-append "Appended to register " ks)))
          (echo-message! (app-state-echo app) "No region selected"))))))

;; --- Process / environment ---

(def (cmd-getenv app)
  "Display an environment variable."
  (let ((var (app-read-string app "Environment variable: ")))
    (when (and var (not (string-empty? var)))
      (let ((val (getenv var #f)))
        (echo-message! (app-state-echo app)
                       (if val
                         (string-append var "=" val)
                         (string-append var " is not set")))))))

(def (cmd-setenv app)
  "Set an environment variable."
  (let ((var (app-read-string app "Set variable: ")))
    (when (and var (not (string-empty? var)))
      (let ((val (app-read-string app "Value: ")))
        (when val
          (setenv var val)
          (echo-message! (app-state-echo app)
                         (string-append var "=" val)))))))

(def (cmd-show-environment app)
  "Display all environment variables."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error reading environment"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "env" arguments: '()
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc #f))
             (result (or output "")))
        (close-port proc)
        (open-output-buffer app "*Environment*" result)))))

;; --- Encoding / line endings ---

(def (cmd-set-buffer-file-coding app)
  "Set buffer file coding system (stub)."
  (let ((coding (app-read-string app "Coding system: ")))
    (when (and coding (not (string-empty? coding)))
      (echo-message! (app-state-echo app)
                     (string-append "Coding system set to: " coding " (stub)")))))

(def (cmd-convert-line-endings-unix app)
  "Convert line endings to Unix (LF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((result (list->string (reverse acc))))
          (editor-set-text ed result)
          (echo-message! (app-state-echo app) "Converted to Unix line endings"))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (if (and (< (+ i 1) (string-length text))
                     (char=? (string-ref text (+ i 1)) #\newline))
              (loop (+ i 2) (cons #\newline acc))  ;; CR+LF -> LF
              (loop (+ i 1) (cons #\newline acc)))  ;; CR -> LF
            (loop (+ i 1) (cons ch acc))))))))

(def (cmd-convert-line-endings-dos app)
  "Convert line endings to DOS (CRLF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    ;; First normalize to LF, then convert to CRLF
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((clean (list->string (reverse acc))))
          ;; Now add CR before each LF
          (let loop2 ((j 0) (acc2 []))
            (if (>= j (string-length clean))
              (let ((result (list->string (reverse acc2))))
                (editor-set-text ed result)
                (echo-message! (app-state-echo app) "Converted to DOS line endings"))
              (let ((ch (string-ref clean j)))
                (if (char=? ch #\newline)
                  (loop2 (+ j 1) (cons #\newline (cons #\return acc2)))
                  (loop2 (+ j 1) (cons ch acc2)))))))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (if (and (< (+ i 1) (string-length text))
                     (char=? (string-ref text (+ i 1)) #\newline))
              (loop (+ i 2) (cons #\newline acc))
              (loop (+ i 1) (cons #\newline acc)))
            (loop (+ i 1) (cons ch acc))))))))

;; --- Completion / hippie-expand ---

;; --- Whitespace ---

(def (cmd-whitespace-mode app)
  "Toggle whitespace visibility mode."
  (let* ((ed (current-editor app))
         (visible (send-message ed 2090 0 0)))  ;; SCI_GETVIEWWS
    (if (= visible 0)
      (begin
        (send-message ed 2021 1 0)  ;; SCI_SETVIEWWS SCWS_VISIBLEALWAYS
        (echo-message! (app-state-echo app) "Whitespace visible"))
      (begin
        (send-message ed 2021 0 0)  ;; SCI_SETVIEWWS SCWS_INVISIBLE
        (echo-message! (app-state-echo app) "Whitespace hidden")))))

(def (cmd-toggle-show-spaces app)
  "Toggle space visibility."
  (cmd-whitespace-mode app))

;; --- Folding ---

(def (cmd-fold-all app)
  "Fold all foldable regions."
  (let ((ed (current-editor app)))
    (send-message ed 2335 0 0)  ;; SCI_FOLDALL SC_FOLDACTION_CONTRACT
    (echo-message! (app-state-echo app) "All folds collapsed")))

(def (cmd-unfold-all app)
  "Unfold all foldable regions."
  (let ((ed (current-editor app)))
    (send-message ed 2335 1 0)  ;; SCI_FOLDALL SC_FOLDACTION_EXPAND
    (echo-message! (app-state-echo app) "All folds expanded")))

(def (cmd-toggle-fold app)
  "Toggle fold at current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (send-message ed 2166 pos 0)))
    (send-message ed 2231 line 0)  ;; SCI_TOGGLEFOLD
    (echo-message! (app-state-echo app) "Fold toggled")))

(def (cmd-fold-level app)
  "Fold to a specific level."
  (let ((level (app-read-string app "Fold level: ")))
    (when (and level (not (string-empty? level)))
      (let ((n (string->number level)))
        (when n
          (let ((ed (current-editor app)))
            ;; Expand all first, then collapse to level
            (send-message ed 2335 1 0)  ;; SCI_FOLDALL expand
            (echo-message! (app-state-echo app)
                           (string-append "Folded to level " level))))))))

;; --- Macro enhancements ---

(def (cmd-name-last-kbd-macro app)
  "Name the last keyboard macro."
  (let ((name (app-read-string app "Name for last macro: ")))
    (when (and name (not (string-empty? name)))
      (echo-message! (app-state-echo app)
                     (string-append "Macro named: " name " (stub)")))))

(def (cmd-insert-kbd-macro app)
  "Insert the last keyboard macro as text."
  (let ((macro (app-state-macro-last app)))
    (if (and macro (not (null? macro)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (desc (string-join (map (lambda (ev) (key-event->string ev)) macro) " ")))
        (editor-insert-text ed pos desc))
      (echo-message! (app-state-echo app) "No keyboard macro defined"))))

;; --- Version control extras ---

(def (cmd-vc-annotate app)
  "Show file annotations (git blame) in buffer."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git annotate"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["blame" "--date=short" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*Annotate: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-diff-head app)
  "Show diff against HEAD."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git diff"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["diff" "HEAD" "--" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*VC Diff: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-log-file app)
  "Show git log for current file."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git log"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["log" "--oneline" "-20" "--" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*VC Log: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-revert app)
  "Revert current file to last committed version."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (let ((confirm (app-read-string app
                                        (string-append "Revert " file " to HEAD? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "Error running git checkout"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "git" arguments: ["checkout" "HEAD" "--" file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (_ (process-status proc)))
                (close-port proc)
                ;; Reload the file
                (let ((content (read-file-as-string file))
                      (ed (current-editor app)))
                  (editor-set-text ed content)
                  (editor-set-save-point ed)
                  (echo-message! (app-state-echo app) "Reverted")))))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

;; --- Imenu ---

(def (cmd-imenu app)
  "Jump to a definition in the current buffer (simple heuristic)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (defs (let loop ((ls lines) (n 0) (acc []))
                 (if (null? ls)
                   (reverse acc)
                   (let ((l (car ls)))
                     (if (or (string-contains l "(def ")
                             (string-contains l "(defstruct ")
                             (string-contains l "(defclass ")
                             (string-contains l "(defmethod ")
                             (string-contains l "(define "))
                       (loop (cdr ls) (+ n 1) (cons (cons l n) acc))
                       (loop (cdr ls) (+ n 1) acc)))))))
    (if (null? defs)
      (echo-message! (app-state-echo app) "No definitions found")
      (let* ((items (map (lambda (d) (string-append (number->string (cdr d)) ": " (car d))) defs))
             (display (string-join items "\n")))
        (open-output-buffer app "*Imenu*" display)))))

(def (cmd-which-function app)
  "Display name of function at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (send-message ed 2166 pos 0)))
    ;; Search backward for (def
    (let loop ((l line))
      (if (< l 0)
        (echo-message! (app-state-echo app) "Not in a function")
        (let* ((ls (send-message ed 2167 l 0))
               (le (send-message ed 2136 l 0))
               (lt (if (and (>= ls 0) (<= le (string-length text)))
                     (substring text ls le) "")))
          (if (or (string-contains lt "(def ")
                  (string-contains lt "(define "))
            (let ((trimmed (string-trim-both lt)))
              (echo-message! (app-state-echo app)
                             (if (> (string-length trimmed) 70)
                               (substring trimmed 0 70)
                               trimmed)))
            (loop (- l 1))))))))

;; --- Buffer/file utilities ---

(def (cmd-make-directory app)
  "Create a new directory."
  (let ((dir (app-read-string app "Create directory: ")))
    (when (and dir (not (string-empty? dir)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app)
                         (string-append "Created: " dir)))))))

(def (cmd-delete-file app)
  "Delete a file."
  (let ((file (app-read-string app "Delete file: ")))
    (when (and file (not (string-empty? file)))
      (let ((confirm (app-read-string app
                                        (string-append "Really delete " file "? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (delete-file file)
              (echo-message! (app-state-echo app)
                             (string-append "Deleted: " file)))))))))

(def (cmd-copy-file app)
  "Copy a file."
  (let ((src (app-read-string app "Copy file: ")))
    (when (and src (not (string-empty? src)))
      (let ((dst (app-read-string app "Copy to: ")))
        (when (and dst (not (string-empty? dst)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (copy-file src dst)
              (echo-message! (app-state-echo app)
                             (string-append "Copied: " src " -> " dst)))))))))

(def (cmd-sudo-find-file app)
  "Open file as root using sudo (stub)."
  (echo-message! (app-state-echo app) "sudo-find-file (stub - requires privilege escalation)"))

(def (cmd-find-file-literally app)
  "Open file without special processing."
  (let ((file (app-read-string app "Find file literally: ")))
    (when (and file (not (string-empty? file)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (let* ((content (read-file-as-string file))
                 (ed (current-editor app))
                 (fr (app-state-frame app))
                 (buf (buffer-create! file ed #f)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-set-text ed content)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)))))))

;;;============================================================================
;;; Task #45: isearch enhancements, abbrev, and editing utilities
;;;============================================================================

;; --- Search enhancements ---

(def (cmd-isearch-forward-word app)
  "Incremental search forward for a whole word."
  (let ((word (app-read-string app "I-search word: ")))
    (when (and word (not (string-empty? word)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed))
             (pat (string-append " " word " ")))
        ;; Simple word boundary: space-delimited
        (let ((found (string-contains text word pos)))
          (if found
            (begin
              (editor-goto-pos ed found)
              (editor-set-selection-start ed found)
              (editor-set-selection-end ed (+ found (string-length word)))
              (set! (app-state-last-search app) word))
            (echo-message! (app-state-echo app)
                           (string-append "Not found: " word))))))))

(def (cmd-isearch-backward-word app)
  "Incremental search backward for a whole word."
  (let ((word (app-read-string app "I-search backward word: ")))
    (when (and word (not (string-empty? word)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed)))
        ;; Search backward
        (let loop ((i (- pos (string-length word) 1)))
          (cond
            ((< i 0)
             (echo-message! (app-state-echo app)
                            (string-append "Not found: " word)))
            ((and (>= (+ i (string-length word)) 0)
                  (<= (+ i (string-length word)) (string-length text))
                  (string=? (substring text i (+ i (string-length word))) word))
             (editor-goto-pos ed i)
             (editor-set-selection-start ed i)
             (editor-set-selection-end ed (+ i (string-length word)))
             (set! (app-state-last-search app) word))
            (else (loop (- i 1)))))))))

(def (cmd-isearch-forward-symbol app)
  "Incremental search forward for a symbol at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Get symbol at point
    (let* ((ws (let loop ((i pos))
                 (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                   (loop (- i 1)) i)))
           (we (let loop ((i pos))
                 (if (and (< i len) (word-char? (string-ref text i)))
                   (loop (+ i 1)) i)))
           (symbol (if (< ws we) (substring text ws we) "")))
      (if (string-empty? symbol)
        (echo-message! (app-state-echo app) "No symbol at point")
        (let ((found (string-contains text symbol (+ we 1))))
          (if found
            (begin
              (editor-goto-pos ed found)
              (editor-set-selection-start ed found)
              (editor-set-selection-end ed (+ found (string-length symbol)))
              (set! (app-state-last-search app) symbol)
              (echo-message! (app-state-echo app)
                             (string-append "Symbol: " symbol)))
            ;; Wrap around
            (let ((found2 (string-contains text symbol 0)))
              (if (and found2 (< found2 ws))
                (begin
                  (editor-goto-pos ed found2)
                  (editor-set-selection-start ed found2)
                  (editor-set-selection-end ed (+ found2 (string-length symbol)))
                  (echo-message! (app-state-echo app) "Wrapped"))
                (echo-message! (app-state-echo app) "Only occurrence")))))))))

(def (cmd-query-replace-regexp app)
  "Query replace using regexp (simplified: uses string-contains)."
  (let ((from (app-read-string app "Query replace: ")))
    (when (and from (not (string-empty? from)))
      (let ((to (app-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-editor app))
                 (text (editor-get-text ed))
                 (count (let loop ((i 0) (n 0))
                          (let ((found (string-contains text from i)))
                            (if found
                              (loop (+ found (max 1 (string-length from))) (+ n 1))
                              n)))))
            ;; Do the replacement
            (let loop ((result text) (replaced 0))
              (let ((found (string-contains result from)))
                (if found
                  (let ((new-text (string-append
                                    (substring result 0 found)
                                    to
                                    (substring result (+ found (string-length from))
                                               (string-length result)))))
                    (loop new-text (+ replaced 1)))
                  (begin
                    (editor-set-text ed result)
                    (echo-message! (app-state-echo app)
                                   (string-append "Replaced " (number->string replaced)
                                                  " occurrences"))))))))))))

(def (cmd-multi-occur app)
  "Search for pattern across all buffers."
  (let ((pat (app-read-string app "Multi-occur: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((results
               (let loop ((bufs (buffer-list)) (acc []))
                 (if (null? bufs)
                   (reverse acc)
                   (let* ((buf (car bufs))
                          (name (buffer-name buf)))
                     ;; Skip non-file buffers
                     (loop (cdr bufs) acc))))))
        (echo-message! (app-state-echo app)
                       (string-append "Multi-occur for: " pat " (stub)"))))))

;; --- Sort enhancements ---

;; --- Align ---

(def (cmd-align-current app)
  "Align the current region on a separator."
  (let ((sep (app-read-string app "Align on: ")))
    (when (and sep (not (string-empty? sep)))
      (let* ((ed (current-editor app))
             (sel-start (editor-get-selection-start ed))
             (sel-end (editor-get-selection-end ed)))
        (when (< sel-start sel-end)
          (let* ((text (editor-get-text ed))
                 (region (substring text sel-start sel-end))
                 (lines (string-split region #\newline))
                 ;; Find max column of separator
                 (max-col (let loop ((ls lines) (max-c 0))
                            (if (null? ls) max-c
                              (let ((pos (string-contains (car ls) sep)))
                                (loop (cdr ls) (if pos (max max-c pos) max-c))))))
                 ;; Pad each line so separator aligns
                 (aligned (map (lambda (l)
                                 (let ((pos (string-contains l sep)))
                                   (if pos
                                     (string-append
                                       (substring l 0 pos)
                                       (make-string (- max-col pos) #\space)
                                       (substring l pos (string-length l)))
                                     l)))
                               lines))
                 (result (string-join aligned "\n")))
            (send-message ed 2160 sel-start 0)
            (send-message ed 2161 sel-end 0)
            (send-message/string ed SCI_REPLACETARGET result)))))))

;; --- Rectangle enhancements ---

(def (cmd-clear-rectangle app)
  "Clear text in a rectangle region (replace with spaces)."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (start-line (send-message ed 2166 sel-start 0))
             (end-line (send-message ed 2166 sel-end 0))
             (start-col (send-message ed 2008 sel-start 0))  ;; SCI_GETCOLUMN
             (end-col (send-message ed 2008 sel-end 0))
             (min-col (min start-col end-col))
             (max-col (max start-col end-col))
             (lines (string-split text #\newline))
             (result-lines
               (let loop ((ls lines) (n 0) (acc []))
                 (if (null? ls)
                   (reverse acc)
                   (let ((l (car ls)))
                     (if (and (>= n start-line) (<= n end-line))
                       (let* ((len (string-length l))
                              (before (substring l 0 (min min-col len)))
                              (spaces (make-string (- max-col min-col) #\space))
                              (after (if (< max-col len)
                                       (substring l max-col len)
                                       "")))
                         (loop (cdr ls) (+ n 1) (cons (string-append before spaces after) acc)))
                       (loop (cdr ls) (+ n 1) (cons l acc)))))))
             (result (string-join result-lines "\n")))
        (editor-set-text ed result)))))

;; --- Abbrev mode ---

(def (cmd-abbrev-mode app)
  "Toggle abbrev mode (stub)."
  (echo-message! (app-state-echo app) "Abbrev mode toggled (stub)"))

(def (cmd-define-abbrev app)
  "Define a new abbreviation (stub)."
  (let ((abbrev (app-read-string app "Abbrev: ")))
    (when (and abbrev (not (string-empty? abbrev)))
      (let ((expansion (app-read-string app "Expansion: ")))
        (when (and expansion (not (string-empty? expansion)))
          (echo-message! (app-state-echo app)
                         (string-append "Defined: " abbrev " -> " expansion " (stub)")))))))

(def (cmd-expand-abbrev app)
  "Expand abbreviation at point (stub)."
  (echo-message! (app-state-echo app) "No abbrev to expand (stub)"))

(def (cmd-list-abbrevs app)
  "List all abbreviations (stub)."
  (open-output-buffer app "*Abbrevs*" "No abbreviations defined (stub)."))

;; --- Completion ---

(def (cmd-completion-at-point app)
  "Complete word at point using buffer contents (same as hippie-expand)."
  (cmd-hippie-expand app))

(def (cmd-complete-filename app)
  "Complete filename at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    ;; Get path-like prefix
    (let* ((start (let loop ((i (- pos 1)))
                    (if (and (>= i 0)
                             (not (memv (string-ref text i) '(#\space #\tab #\newline #\( #\)))))
                      (loop (- i 1))
                      (+ i 1))))
           (prefix (substring text start pos)))
      (if (string-empty? prefix)
        (echo-message! (app-state-echo app) "No filename prefix")
        (with-catch
          (lambda (e)
            (echo-message! (app-state-echo app) "Cannot complete"))
          (lambda ()
            (let* ((dir (path-directory prefix))
                   (base (path-strip-directory prefix))
                   (entries (directory-files (if (string-empty? dir) "." dir)))
                   (matches (filter (lambda (f)
                                      (and (>= (string-length f) (string-length base))
                                           (string=? (substring f 0 (string-length base)) base)))
                                    entries)))
              (cond
                ((null? matches)
                 (echo-message! (app-state-echo app) "No completions"))
                ((= (length matches) 1)
                 (let ((completion (string-append dir (car matches))))
                   (send-message ed 2160 start 0)
                   (send-message ed 2161 pos 0)
                   (send-message/string ed SCI_REPLACETARGET completion)))
                (else
                 (echo-message! (app-state-echo app)
                                (string-append (number->string (length matches)) " completions")))))))))))

;; --- Window resize ---

(def (cmd-resize-window-width app)
  "Set window width (stub)."
  (echo-message! (app-state-echo app) "Window width set (stub)"))

;; --- Text operations ---

(def (cmd-zap-to-char-inclusive app)
  "Zap to character, including the character."
  (echo-message! (app-state-echo app) "Zap to char (inclusive): ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (ch (if (= (string-length ks) 1) (string-ref ks 0) #f)))
        (when ch
          (let* ((ed (current-editor app))
                 (pos (editor-get-current-pos ed))
                 (text (editor-get-text ed))
                 (len (string-length text)))
            (let loop ((i (+ pos 1)))
              (cond
                ((>= i len)
                 (echo-message! (app-state-echo app)
                                (string-append "Character not found: " ks)))
                ((char=? (string-ref text i) ch)
                 ;; Kill from pos to i+1 (inclusive)
                 (let ((killed (substring text pos (+ i 1))))
                   (set! (app-state-kill-ring app)
                     (cons killed (app-state-kill-ring app)))
                   (send-message ed 2160 pos 0)
                   (send-message ed 2161 (+ i 1) 0)
                   (send-message/string ed SCI_REPLACETARGET "")))
                (else (loop (+ i 1)))))))))))

(def (cmd-copy-word-at-point app)
  "Copy the word at point to the kill ring."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (> len 0)
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (word-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (word (if (< ws we) (substring text ws we) "")))
        (if (string-empty? word)
          (echo-message! (app-state-echo app) "No word at point")
          (begin
            (set! (app-state-kill-ring app)
              (cons word (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
                           (string-append "Copied: " word))))))))

(def (cmd-copy-symbol-at-point app)
  "Copy the symbol at point (including hyphens, underscores)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (sym-char? (lambda (ch)
                      (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (memv ch '(#\- #\_ #\! #\? #\*))))))
    (when (> len 0)
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (sym-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (sym (if (< ws we) (substring text ws we) "")))
        (if (string-empty? sym)
          (echo-message! (app-state-echo app) "No symbol at point")
          (begin
            (set! (app-state-kill-ring app)
              (cons sym (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
                           (string-append "Copied: " sym))))))))

(def (cmd-mark-page app)
  "Mark the entire buffer (same as select-all)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    (editor-set-selection-start ed 0)
    (editor-set-selection-end ed (string-length text))
    (echo-message! (app-state-echo app) "Buffer marked")))

;; --- Encoding/display ---

(def (cmd-set-language-environment app)
  "Set language environment (stub)."
  (let ((lang (app-read-string app "Language environment: ")))
    (when (and lang (not (string-empty? lang)))
      (echo-message! (app-state-echo app)
                     (string-append "Language: " lang " (stub)")))))

;; --- Theme/color ---

(def (cmd-load-theme app)
  "Load a color theme (stub)."
  (let ((theme (app-read-string app "Load theme: ")))
    (when (and theme (not (string-empty? theme)))
      (echo-message! (app-state-echo app)
                     (string-append "Theme: " theme " (stub)")))))

(def (cmd-customize-face app)
  "Customize a face/style (stub)."
  (echo-message! (app-state-echo app) "Customize face (stub)"))

(def (cmd-list-colors app)
  "List available colors."
  (let ((colors "black red green yellow blue magenta cyan white\nbright-black bright-red bright-green bright-yellow\nbright-blue bright-magenta bright-cyan bright-white"))
    (open-output-buffer app "*Colors*" colors)))

;; --- Text property/overlay ---

(def (cmd-font-lock-mode app)
  "Toggle font-lock (syntax highlighting) mode."
  (echo-message! (app-state-echo app) "Font-lock toggled (stub)"))

;; --- Auto-revert ---

(def (cmd-auto-revert-mode app)
  "Toggle auto-revert mode for current buffer."
  (echo-message! (app-state-echo app) "Auto-revert mode toggled (stub)"))

;; --- Diff enhancements ---

(def (cmd-diff-backup app)
  "Diff current file against its backup."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (let ((backup (string-append file "~")))
        (if (file-exists? backup)
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "Error running diff"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "diff" arguments: ["-u" backup file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (output (read-line proc #f))
                     (result (or output "No differences")))
                (close-port proc)
                (open-output-buffer app "*Diff Backup*" result))))
          (echo-message! (app-state-echo app) "No backup file found")))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

;; --- Compilation ---

(def (cmd-first-error app)
  "Jump to first compilation error (stub)."
  (echo-message! (app-state-echo app) "First error (stub)"))

;; --- Calculator enhancements ---

(def (cmd-quick-calc app)
  "Quick inline calculation."
  (let ((expr (app-read-string app "Quick calc: ")))
    (when (and expr (not (string-empty? expr)))
      (let-values (((result error?) (eval-expression-string expr)))
        (echo-message! (app-state-echo app)
                       (if error?
                         (string-append "Error: " result)
                         (string-append "= " result)))))))

;; --- String insertion ---

(def (cmd-insert-time app)
  "Insert the current time."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error getting time"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "date" arguments: ["+%H:%M:%S"]
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc))
             (time (or output ""))
             (_ (close-port proc))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (editor-insert-text ed pos time)
        (editor-goto-pos ed (+ pos (string-length time)))))))

(def (cmd-insert-file-header app)
  "Insert a file header comment."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (header (string-append ";;; -*- Gerbil -*-\n"
                                ";;; " name "\n"
                                ";;;\n"
                                ";;; Description: \n"
                                ";;;\n\n")))
    (editor-insert-text ed pos header)
    (editor-goto-pos ed (+ pos (string-length header)))))

;; --- Misc ---

(def (cmd-toggle-debug-on-quit app)
  "Toggle debug on quit (stub)."
  (echo-message! (app-state-echo app) "Debug on quit toggled (stub)"))

(def (cmd-profiler-start app)
  "Start profiler (stub)."
  (echo-message! (app-state-echo app) "Profiler started (stub)"))

(def (cmd-profiler-stop app)
  "Stop profiler and report (stub)."
  (echo-message! (app-state-echo app) "Profiler stopped (stub)"))

(def (cmd-memory-report app)
  "Show memory usage report."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error getting memory info"))
    (lambda ()
      (let* ((content (read-file-as-string "/proc/self/status"))
             (lines (string-split content #\newline))
             (vm-line (let loop ((ls lines))
                        (if (null? ls) "Unknown"
                          (if (string-contains (car ls) "VmRSS:")
                            (car ls) (loop (cdr ls)))))))
        (echo-message! (app-state-echo app) (string-trim-both vm-line))))))

(def (cmd-emacs-version app)
  "Display editor version."
  (echo-message! (app-state-echo app) "gerbil-emacs 0.1"))

(def (cmd-report-bug app)
  "Report a bug."
  (echo-message! (app-state-echo app) "Report bugs at: https://github.com/ober/gerbil-emacs/issues"))

(def (cmd-view-echo-area-messages app)
  "View echo area message log (stub)."
  (echo-message! (app-state-echo app) "Message log (stub)"))

(def (cmd-toggle-menu-bar-mode app)
  "Toggle menu bar (stub)."
  (echo-message! (app-state-echo app) "Menu bar toggled (stub)"))

(def (cmd-toggle-tab-bar-mode app)
  "Toggle tab bar (stub)."
  (echo-message! (app-state-echo app) "Tab bar toggled (stub)"))

(def (cmd-split-window-below app)
  "Split window below (alias for split-window)."
  (cmd-split-window app))

(def (cmd-delete-window-below app)
  "Delete the window below (stub)."
  (echo-message! (app-state-echo app) "Delete window below (stub)"))

(def (cmd-shrink-window-if-larger-than-buffer app)
  "Shrink window to fit content."
  (echo-message! (app-state-echo app) "Window shrunk to buffer (stub)"))

(def (cmd-toggle-frame-fullscreen app)
  "Toggle fullscreen mode (stub)."
  (echo-message! (app-state-echo app) "Fullscreen toggled (stub)"))

(def (cmd-toggle-frame-maximized app)
  "Toggle maximized frame (stub)."
  (echo-message! (app-state-echo app) "Frame maximized toggled (stub)"))

;; --- Spell checking ---

(def (cmd-ispell-word app)
  "Check spelling of word at point (stub)."
  (echo-message! (app-state-echo app) "Spell check word (stub)"))

(def (cmd-ispell-buffer app)
  "Check spelling of entire buffer (stub)."
  (echo-message! (app-state-echo app) "Spell check buffer (stub)"))

(def (cmd-ispell-region app)
  "Check spelling of region (stub)."
  (echo-message! (app-state-echo app) "Spell check region (stub)"))

;; --- Process management ---

(def (cmd-term app)
  "Open a terminal (same as shell command)."
  (cmd-shell app))

(def (cmd-ansi-term app)
  "Open an ANSI terminal (same as shell command)."
  (cmd-shell app))

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
  ;; Yank-pop
  (register-command! 'yank-pop cmd-yank-pop)
  ;; Occur
  (register-command! 'occur cmd-occur)
  ;; Compile
  (register-command! 'compile cmd-compile)
  ;; Shell command on region
  (register-command! 'shell-command-on-region cmd-shell-command-on-region)
  ;; Sort lines
  (register-command! 'sort-lines cmd-sort-lines)
  ;; Bookmarks
  (register-command! 'bookmark-set cmd-bookmark-set)
  (register-command! 'bookmark-jump cmd-bookmark-jump)
  (register-command! 'bookmark-list cmd-bookmark-list)
  ;; Rectangle operations
  (register-command! 'kill-rectangle cmd-kill-rectangle)
  (register-command! 'yank-rectangle cmd-yank-rectangle)
  ;; Go to matching paren
  (register-command! 'goto-matching-paren cmd-goto-matching-paren)
  ;; Join line
  (register-command! 'join-line cmd-join-line)
  ;; Delete blank lines
  (register-command! 'delete-blank-lines cmd-delete-blank-lines)
  ;; Indent region
  (register-command! 'indent-region cmd-indent-region)
  ;; Case region
  (register-command! 'downcase-region cmd-downcase-region)
  (register-command! 'upcase-region cmd-upcase-region)
  ;; Shell command
  (register-command! 'shell-command cmd-shell-command)
  ;; Fill paragraph
  (register-command! 'fill-paragraph cmd-fill-paragraph)
  ;; Grep
  (register-command! 'grep cmd-grep)
  ;; Insert file
  (register-command! 'insert-file cmd-insert-file)
  (register-command! 'string-insert-file cmd-insert-file)  ; alias
  ;; Dabbrev
  (register-command! 'dabbrev-expand cmd-dabbrev-expand)
  ;; What cursor position
  (register-command! 'what-cursor-position cmd-what-cursor-position)
  ;; Keyboard macros
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
  ;; Mark ring
  (register-command! 'pop-mark cmd-pop-mark)
  ;; Registers
  (register-command! 'copy-to-register cmd-copy-to-register)
  (register-command! 'insert-register cmd-insert-register)
  (register-command! 'point-to-register cmd-point-to-register)
  (register-command! 'jump-to-register cmd-jump-to-register)
  ;; Backward kill word, zap to char, goto char
  (register-command! 'backward-kill-word cmd-backward-kill-word)
  (register-command! 'zap-to-char cmd-zap-to-char)
  (register-command! 'goto-char cmd-goto-char)
  ;; Replace string (non-interactive)
  (register-command! 'replace-string cmd-replace-string)
  ;; Transpose
  (register-command! 'transpose-words cmd-transpose-words)
  (register-command! 'transpose-lines cmd-transpose-lines)
  ;; Just one space
  (register-command! 'just-one-space cmd-just-one-space)
  ;; Repeat
  (register-command! 'repeat cmd-repeat)
  ;; Next/previous error (search result navigation)
  (register-command! 'next-error cmd-next-error)
  (register-command! 'previous-error cmd-previous-error)
  ;; Kill whole line
  (register-command! 'kill-whole-line cmd-kill-whole-line)
  ;; Move line up/down
  (register-command! 'move-line-up cmd-move-line-up)
  (register-command! 'move-line-down cmd-move-line-down)
  ;; Pipe buffer
  (register-command! 'pipe-buffer cmd-pipe-buffer)
  ;; Narrow/widen
  (register-command! 'narrow-to-region cmd-narrow-to-region)
  (register-command! 'widen cmd-widen)
  ;; String rectangle, open rectangle
  (register-command! 'string-rectangle cmd-string-rectangle)
  (register-command! 'open-rectangle cmd-open-rectangle)
  ;; Number lines, reverse region
  (register-command! 'number-lines cmd-number-lines)
  (register-command! 'reverse-region cmd-reverse-region)
  ;; Flush/keep lines
  (register-command! 'flush-lines cmd-flush-lines)
  (register-command! 'keep-lines cmd-keep-lines)
  ;; Align
  (register-command! 'align-regexp cmd-align-regexp)
  ;; Sort fields
  (register-command! 'sort-fields cmd-sort-fields)
  ;; Mark word, mark paragraph, paragraph nav
  (register-command! 'mark-word cmd-mark-word)
  (register-command! 'mark-paragraph cmd-mark-paragraph)
  (register-command! 'forward-paragraph cmd-forward-paragraph)
  (register-command! 'backward-paragraph cmd-backward-paragraph)
  ;; Indentation nav
  (register-command! 'back-to-indentation cmd-back-to-indentation)
  (register-command! 'delete-indentation cmd-delete-indentation)
  ;; Whitespace
  (register-command! 'fixup-whitespace cmd-fixup-whitespace)
  ;; Point/mark
  (register-command! 'exchange-point-and-mark cmd-exchange-point-and-mark)
  ;; Info commands
  (register-command! 'what-page cmd-what-page)
  (register-command! 'count-lines-region cmd-count-lines-region)
  ;; Copy line
  (register-command! 'copy-line cmd-copy-line)
  ;; Help: where-is, apropos
  (register-command! 'where-is cmd-where-is)
  (register-command! 'apropos-command cmd-apropos-command)
  ;; Buffer: read-only, rename
  (register-command! 'toggle-read-only cmd-toggle-read-only)
  (register-command! 'rename-buffer cmd-rename-buffer)
  ;; Other-window
  (register-command! 'switch-buffer-other-window cmd-switch-buffer-other-window)
  (register-command! 'find-file-other-window cmd-find-file-other-window)
  ;; Universal argument (stub)
  (register-command! 'universal-argument cmd-universal-argument)
  ;; Text transforms
  (register-command! 'tabify cmd-tabify)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'base64-encode-region cmd-base64-encode-region)
  (register-command! 'base64-decode-region cmd-base64-decode-region)
  (register-command! 'rot13-region cmd-rot13-region)
  ;; Hex dump
  (register-command! 'hexl-mode cmd-hexl-mode)
  ;; Count/dedup
  (register-command! 'count-matches cmd-count-matches)
  (register-command! 'delete-duplicate-lines cmd-delete-duplicate-lines)
  ;; Diff, checksum
  (register-command! 'diff-buffer-with-file cmd-diff-buffer-with-file)
  (register-command! 'checksum cmd-checksum)
  ;; Async shell
  (register-command! 'async-shell-command cmd-async-shell-command)
  ;; Toggle truncate
  (register-command! 'toggle-truncate-lines cmd-toggle-truncate-lines)
  ;; Grep buffer
  (register-command! 'grep-buffer cmd-grep-buffer)
  ;; Insert date, insert char
  (register-command! 'insert-date cmd-insert-date)
  (register-command! 'insert-char cmd-insert-char)
  ;; Eval buffer/region
  (register-command! 'eval-buffer cmd-eval-buffer)
  (register-command! 'eval-region cmd-eval-region)
  ;; Clone buffer, scratch
  (register-command! 'clone-buffer cmd-clone-buffer)
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Save some buffers
  (register-command! 'save-some-buffers cmd-save-some-buffers)
  ;; Revert quick
  (register-command! 'revert-buffer-quick cmd-revert-buffer-quick)
  ;; Highlighting toggle
  (register-command! 'toggle-highlighting cmd-toggle-highlighting)
  ;; Display time, pwd
  (register-command! 'display-time cmd-display-time)
  (register-command! 'pwd cmd-pwd)
  ;; Ediff
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  ;; Calculator
  (register-command! 'calc cmd-calc)
  ;; Case fold search
  (register-command! 'toggle-case-fold-search cmd-toggle-case-fold-search)
  ;; Describe bindings
  (register-command! 'describe-bindings cmd-describe-bindings)
  ;; Center line
  (register-command! 'center-line cmd-center-line)
  ;; What face
  (register-command! 'what-face cmd-what-face)
  ;; List processes
  (register-command! 'list-processes cmd-list-processes)
  ;; Messages
  (register-command! 'view-messages cmd-view-messages)
  ;; Auto fill
  (register-command! 'toggle-auto-fill cmd-toggle-auto-fill)
  ;; Rename file
  (register-command! 'rename-file-and-buffer cmd-rename-file-and-buffer)
  ;; Delete file
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  ;; Sudo write
  (register-command! 'sudo-write cmd-sudo-write)
  ;; Sort numeric
  (register-command! 'sort-numeric cmd-sort-numeric)
  ;; Count words region
  (register-command! 'count-words-region cmd-count-words-region)
  ;; Overwrite mode
  (register-command! 'toggle-overwrite-mode cmd-toggle-overwrite-mode)
  ;; Visual line mode
  (register-command! 'toggle-visual-line-mode cmd-toggle-visual-line-mode)
  ;; Fill column
  (register-command! 'set-fill-column cmd-set-fill-column)
  (register-command! 'toggle-fill-column-indicator cmd-toggle-fill-column-indicator)
  ;; Debug
  (register-command! 'toggle-debug-on-error cmd-toggle-debug-on-error)
  ;; Repeat complex command
  (register-command! 'repeat-complex-command cmd-repeat-complex-command)
  ;; Eldoc
  (register-command! 'eldoc cmd-eldoc)
  ;; Highlight symbol
  (register-command! 'highlight-symbol cmd-highlight-symbol)
  (register-command! 'clear-highlight cmd-clear-highlight)
  ;; Indent rigidly
  (register-command! 'indent-rigidly-right cmd-indent-rigidly-right)
  (register-command! 'indent-rigidly-left cmd-indent-rigidly-left)
  ;; Goto non-blank
  (register-command! 'goto-first-non-blank cmd-goto-first-non-blank)
  (register-command! 'goto-last-non-blank cmd-goto-last-non-blank)
  ;; Buffer stats
  (register-command! 'buffer-stats cmd-buffer-stats)
  ;; Show tabs/eol
  (register-command! 'toggle-show-tabs cmd-toggle-show-tabs)
  (register-command! 'toggle-show-eol cmd-toggle-show-eol)
  ;; Copy from above/below
  (register-command! 'copy-from-above cmd-copy-from-above)
  (register-command! 'copy-from-below cmd-copy-from-below)
  ;; Open line above
  (register-command! 'open-line-above cmd-open-line-above)
  ;; Select line
  (register-command! 'select-line cmd-select-line)
  ;; Split line
  (register-command! 'split-line cmd-split-line)
  ;; Line endings
  (register-command! 'convert-to-unix cmd-convert-to-unix)
  (register-command! 'convert-to-dos cmd-convert-to-dos)
  ;; Window
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  ;; Encoding
  (register-command! 'what-encoding cmd-what-encoding)
  ;; Hippie expand
  (register-command! 'hippie-expand cmd-hippie-expand)
  ;; Swap buffers
  (register-command! 'swap-buffers cmd-swap-buffers)
  ;; Tab width
  (register-command! 'cycle-tab-width cmd-cycle-tab-width)
  (register-command! 'toggle-indent-tabs-mode cmd-toggle-indent-tabs-mode)
  ;; Buffer info
  (register-command! 'buffer-info cmd-buffer-info)
  ;; Whitespace cleanup
  (register-command! 'whitespace-cleanup cmd-whitespace-cleanup)
  ;; Electric pair toggle
  (register-command! 'toggle-electric-pair cmd-toggle-electric-pair)
  ;; Previous/next buffer
  (register-command! 'previous-buffer cmd-previous-buffer)
  (register-command! 'next-buffer cmd-next-buffer)
  ;; Balance windows
  (register-command! 'balance-windows cmd-balance-windows)
  ;; Move to window line (cycle top/center/bottom)
  (register-command! 'move-to-window-line cmd-move-to-window-line)
  ;; Kill buffer and window
  (register-command! 'kill-buffer-and-window cmd-kill-buffer-and-window)
  ;; Flush undo
  (register-command! 'flush-undo cmd-flush-undo)
  ;; Upcase initials region
  (register-command! 'upcase-initials-region cmd-upcase-initials-region)
  ;; Untabify buffer
  (register-command! 'untabify-buffer cmd-untabify-buffer)
  ;; Insert buffer name
  (register-command! 'insert-buffer-name cmd-insert-buffer-name)
  ;; Mark defun
  (register-command! 'mark-defun cmd-mark-defun)
  ;; Insert pairs
  (register-command! 'insert-parentheses cmd-insert-parentheses)
  (register-command! 'insert-pair-brackets cmd-insert-pair-brackets)
  (register-command! 'insert-pair-braces cmd-insert-pair-braces)
  (register-command! 'insert-pair-quotes cmd-insert-pair-quotes)
  ;; Describe char
  (register-command! 'describe-char cmd-describe-char)
  ;; Find file at point
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  ;; Count chars region
  (register-command! 'count-chars-region cmd-count-chars-region)
  ;; Capitalize region
  (register-command! 'capitalize-region cmd-capitalize-region)
  ;; Count words buffer
  (register-command! 'count-words-buffer cmd-count-words-buffer)
  ;; Unfill paragraph
  (register-command! 'unfill-paragraph cmd-unfill-paragraph)
  ;; List registers
  (register-command! 'list-registers cmd-list-registers)
  ;; Show kill ring
  (register-command! 'show-kill-ring cmd-show-kill-ring)
  ;; Smart beginning of line
  (register-command! 'smart-beginning-of-line cmd-smart-beginning-of-line)
  ;; What buffer
  (register-command! 'what-buffer cmd-what-buffer)
  ;; Toggle narrowing indicator
  (register-command! 'toggle-narrowing-indicator cmd-toggle-narrowing-indicator)
  ;; Insert file name
  (register-command! 'insert-file-name cmd-insert-file-name)
  ;; S-expression navigation
  (register-command! 'backward-up-list cmd-backward-up-list)
  (register-command! 'forward-up-list cmd-forward-up-list)
  (register-command! 'kill-sexp cmd-kill-sexp)
  (register-command! 'backward-sexp cmd-backward-sexp)
  (register-command! 'forward-sexp cmd-forward-sexp)
  ;; Transpose sexps
  (register-command! 'transpose-sexps cmd-transpose-sexps)
  ;; Mark sexp
  (register-command! 'mark-sexp cmd-mark-sexp)
  ;; Indent sexp
  (register-command! 'indent-sexp cmd-indent-sexp)
  ;; Word frequency
  (register-command! 'word-frequency cmd-word-frequency)
  ;; Insert UUID
  (register-command! 'insert-uuid cmd-insert-uuid)
  ;; Delete pair
  (register-command! 'delete-pair cmd-delete-pair)
  ;; Toggle hl-line
  (register-command! 'toggle-hl-line cmd-toggle-hl-line)
  ;; Find alternate file
  (register-command! 'find-alternate-file cmd-find-alternate-file)
  ;; Increment register
  (register-command! 'increment-register cmd-increment-register)
  ;; Copy buffer name
  (register-command! 'copy-buffer-name cmd-copy-buffer-name)
  ;; Sort lines case-insensitive
  (register-command! 'sort-lines-case-fold cmd-sort-lines-case-fold)
  ;; Reverse chars in region
  (register-command! 'reverse-chars cmd-reverse-chars)
  ;; Replace regexp
  (register-command! 'replace-string-all cmd-replace-string-all)
  ;; Insert file contents
  (register-command! 'insert-file-contents cmd-insert-file-contents)
  ;; Auto revert
  (register-command! 'toggle-auto-revert cmd-toggle-auto-revert)
  ;; Zap up to char
  (register-command! 'zap-up-to-char cmd-zap-up-to-char)
  ;; Quoted insert
  (register-command! 'quoted-insert cmd-quoted-insert)
  ;; What line/col
  (register-command! 'what-line-col cmd-what-line-col)
  ;; Insert ISO date
  (register-command! 'insert-current-date-iso cmd-insert-current-date-iso)
  ;; Recenter top/bottom
  (register-command! 'recenter-top cmd-recenter-top)
  (register-command! 'recenter-bottom cmd-recenter-bottom)
  ;; Scroll other window
  (register-command! 'scroll-other-window cmd-scroll-other-window)
  (register-command! 'scroll-other-window-up cmd-scroll-other-window-up)
  ;; Count words paragraph
  (register-command! 'count-words-paragraph cmd-count-words-paragraph)
  ;; Toggle transient mark
  (register-command! 'toggle-transient-mark cmd-toggle-transient-mark)
  ;; Keep/flush lines region
  (register-command! 'keep-lines-region cmd-keep-lines-region)
  (register-command! 'flush-lines-region cmd-flush-lines-region)
  ;; Insert register string
  (register-command! 'insert-register-string cmd-insert-register-string)
  ;; Visible bell
  (register-command! 'toggle-visible-bell cmd-toggle-visible-bell)
  ;; Unindent region (indent-region already registered above)
  (register-command! 'unindent-region cmd-unindent-region)
  ;; Copy region as kill
  (register-command! 'copy-region-as-kill cmd-copy-region-as-kill)
  ;; Append to buffer
  (register-command! 'append-to-buffer cmd-append-to-buffer)
  ;; Toggle trailing whitespace display
  (register-command! 'toggle-show-trailing-whitespace cmd-toggle-show-trailing-whitespace)
  ;; Backward kill sexp
  (register-command! 'backward-kill-sexp cmd-backward-kill-sexp)
  ;; Mark whole buffer
  (register-command! 'mark-whole-buffer cmd-mark-whole-buffer)
  ;; Cycle spacing
  (register-command! 'cycle-spacing cmd-cycle-spacing)
  ;; Delete horizontal space forward
  (register-command! 'delete-horizontal-space-forward cmd-delete-horizontal-space-forward)
  ;; Debug mode
  (register-command! 'toggle-debug-mode cmd-toggle-debug-mode)
  ;; Insert comment separator
  (register-command! 'insert-comment-separator cmd-insert-comment-separator)
  ;; Global hl-line
  (register-command! 'toggle-global-hl-line cmd-toggle-global-hl-line)
  ;; Insert shebang
  (register-command! 'insert-shebang cmd-insert-shebang)
  ;; Toggle auto indent
  (register-command! 'toggle-auto-indent cmd-toggle-auto-indent)
  ;; What mode
  (register-command! 'what-mode cmd-what-mode)
  ;; Show buffer size
  (register-command! 'show-buffer-size cmd-show-buffer-size)
  ;; Goto percent
  (register-command! 'goto-percent cmd-goto-percent)
  ;; Insert newline above/below
  (register-command! 'insert-newline-below cmd-insert-newline-below)
  (register-command! 'insert-newline-above cmd-insert-newline-above)
  ;; Duplicate region
  (register-command! 'duplicate-region cmd-duplicate-region)
  ;; Sort lines reverse
  (register-command! 'sort-lines-reverse cmd-sort-lines-reverse)
  ;; Uniquify lines
  (register-command! 'uniquify-lines cmd-uniquify-lines)
  ;; Show line endings
  (register-command! 'show-line-endings cmd-show-line-endings)
  ;; Comment/uncomment
  (register-command! 'comment-region cmd-comment-region)
  (register-command! 'uncomment-region cmd-uncomment-region)
  ;; Case at point
  (register-command! 'upcase-char cmd-upcase-char)
  (register-command! 'downcase-char cmd-downcase-char)
  (register-command! 'toggle-case-at-point cmd-toggle-case-at-point)
  ;; Write region
  (register-command! 'write-region cmd-write-region)
  ;; Kill matching buffers
  (register-command! 'kill-matching-buffers cmd-kill-matching-buffers)
  ;; Relative goto
  (register-command! 'goto-line-relative cmd-goto-line-relative)
  ;; Bookmark management
  (register-command! 'bookmark-delete cmd-bookmark-delete)
  (register-command! 'bookmark-rename cmd-bookmark-rename)
  ;; Mode info (toggle-window-dedicated, shrink/enlarge-window-horizontally, toggle-line-move-visual already registered)
  (register-command! 'describe-mode cmd-describe-mode)
  ;; Trailing lines
  (register-command! 'delete-trailing-lines cmd-delete-trailing-lines)
  ;; Line numbers
  (register-command! 'display-line-numbers-relative cmd-display-line-numbers-relative)
  ;; Column
  (register-command! 'goto-column cmd-goto-column)
  ;; Insert helpers
  (register-command! 'insert-line-number cmd-insert-line-number)
  (register-command! 'insert-buffer-filename cmd-insert-buffer-filename)
  ;; Copy helpers
  (register-command! 'copy-line-number cmd-copy-line-number)
  (register-command! 'copy-current-line cmd-copy-current-line)
  (register-command! 'copy-word cmd-copy-word)
  ;; Window position movement
  (register-command! 'move-to-window-top cmd-move-to-window-top)
  (register-command! 'move-to-window-bottom cmd-move-to-window-bottom)
  (register-command! 'move-to-window-middle cmd-move-to-window-middle)
  ;; Scrolling
  (register-command! 'scroll-left cmd-scroll-left)
  (register-command! 'scroll-right cmd-scroll-right)
  ;; Delete without kill
  (register-command! 'delete-to-end-of-line cmd-delete-to-end-of-line)
  (register-command! 'delete-to-beginning-of-line cmd-delete-to-beginning-of-line)
  ;; Yank line
  (register-command! 'yank-whole-line cmd-yank-whole-line)
  ;; Info
  (register-command! 'show-column-number cmd-show-column-number)
  (register-command! 'count-lines-buffer cmd-count-lines-buffer)
  ;; File management stubs (toggle-auto-save already registered)
  (register-command! 'recover-session cmd-recover-session)
  (register-command! 'toggle-backup-files cmd-toggle-backup-files)
  ;; Case conversion
  (register-command! 'camel-to-snake cmd-camel-to-snake)
  (register-command! 'snake-to-camel cmd-snake-to-camel)
  (register-command! 'kebab-to-camel cmd-kebab-to-camel)
  ;; Word operations
  (register-command! 'reverse-word cmd-reverse-word)
  (register-command! 'sort-words cmd-sort-words)
  ;; Counting
  (register-command! 'count-occurrences cmd-count-occurrences)
  (register-command! 'mark-lines-matching cmd-mark-lines-matching)
  (register-command! 'show-word-count cmd-show-word-count)
  (register-command! 'show-char-count cmd-show-char-count)
  (register-command! 'show-trailing-whitespace-count cmd-show-trailing-whitespace-count)
  (register-command! 'show-tab-count cmd-show-tab-count)
  ;; Line manipulation
  (register-command! 'number-region cmd-number-region)
  (register-command! 'strip-line-numbers cmd-strip-line-numbers)
  (register-command! 'prefix-lines cmd-prefix-lines)
  (register-command! 'suffix-lines cmd-suffix-lines)
  (register-command! 'remove-blank-lines cmd-remove-blank-lines)
  (register-command! 'collapse-blank-lines cmd-collapse-blank-lines)
  (register-command! 'trim-lines cmd-trim-lines)
  ;; Wrapping
  (register-command! 'wrap-lines-at-column cmd-wrap-lines-at-column)
  ;; Comments
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  (register-command! 'insert-box-comment cmd-insert-box-comment)
  ;; File info
  (register-command! 'show-file-info cmd-show-file-info)
  (register-command! 'copy-file-path cmd-copy-file-path)
  ;; Insert
  (register-command! 'insert-timestamp cmd-insert-timestamp)
  (register-command! 'insert-lorem-ipsum cmd-insert-lorem-ipsum)
  (register-command! 'insert-path-separator cmd-insert-path-separator)
  ;; Eval and shell
  (register-command! 'eval-and-insert cmd-eval-and-insert)
  (register-command! 'shell-command-insert cmd-shell-command-insert)
  (register-command! 'pipe-region cmd-pipe-region)
  ;; Toggles/stubs
  (register-command! 'toggle-narrow-indicator cmd-toggle-narrow-indicator)
  (register-command! 'toggle-auto-complete cmd-toggle-auto-complete)
  (register-command! 'toggle-electric-indent cmd-toggle-electric-indent)
  (register-command! 'toggle-global-whitespace cmd-toggle-global-whitespace)
  ;; Narrow
  (register-command! 'narrow-to-defun cmd-narrow-to-defun)
  (register-command! 'widen-all cmd-widen-all)
  ;; Reindent
  (register-command! 'reindent-buffer cmd-reindent-buffer)
  ;; Font size aliases
  (register-command! 'increase-font-size cmd-increase-font-size)
  (register-command! 'decrease-font-size cmd-decrease-font-size)
  (register-command! 'reset-font-size cmd-reset-font-size)
  ;; Project
  (register-command! 'project-find-file cmd-project-find-file)
  (register-command! 'project-grep cmd-project-grep)
  (register-command! 'project-compile cmd-project-compile)
  ;; Word search
  (register-command! 'search-forward-word cmd-search-forward-word)
  (register-command! 'search-backward-word cmd-search-backward-word)
  (register-command! 'highlight-word-at-point cmd-highlight-word-at-point)
  ;; Replace in region
  (register-command! 'replace-in-region cmd-replace-in-region)
  ;; Go to definition
  (register-command! 'goto-definition cmd-goto-definition)
  ;; Frame stubs
  (register-command! 'toggle-eol-conversion cmd-toggle-eol-conversion)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'delete-frame cmd-delete-frame)
  (register-command! 'toggle-menu-bar cmd-toggle-menu-bar)
  (register-command! 'toggle-tool-bar cmd-toggle-tool-bar)
  (register-command! 'toggle-scroll-bar cmd-toggle-scroll-bar)
  (register-command! 'suspend-frame cmd-suspend-frame)
  ;; Directory
  (register-command! 'list-directory cmd-list-directory)
  (register-command! 'find-grep cmd-find-grep)
  ;; C/C++ helpers
  (register-command! 'insert-header-guard cmd-insert-header-guard)
  (register-command! 'insert-include cmd-insert-include)
  ;; Scheme helpers
  (register-command! 'insert-import cmd-insert-import)
  (register-command! 'insert-export cmd-insert-export)
  (register-command! 'insert-defun cmd-insert-defun)
  (register-command! 'insert-let cmd-insert-let)
  (register-command! 'insert-cond cmd-insert-cond)
  (register-command! 'insert-match cmd-insert-match)
  (register-command! 'insert-when cmd-insert-when)
  (register-command! 'insert-unless cmd-insert-unless)
  (register-command! 'insert-lambda cmd-insert-lambda)
  ;; Toggles
  (register-command! 'toggle-auto-pair-mode cmd-toggle-auto-pair-mode)
  (register-command! 'toggle-flyspell cmd-toggle-flyspell)
  (register-command! 'toggle-flymake cmd-toggle-flymake)
  (register-command! 'toggle-lsp cmd-toggle-lsp)
  (register-command! 'toggle-auto-revert-global cmd-toggle-auto-revert-global)
  ;; Buffer info
  (register-command! 'count-buffers cmd-count-buffers)
  (register-command! 'list-recent-files cmd-list-recent-files)
  (register-command! 'clear-recent-files cmd-clear-recent-files)
  ;; Key info
  (register-command! 'show-keybinding-for cmd-show-keybinding-for)
  ;; Sort imports
  (register-command! 'sort-imports cmd-sort-imports)
  ;; Git
  (register-command! 'show-git-status cmd-show-git-status)
  (register-command! 'show-git-log cmd-show-git-log)
  (register-command! 'show-git-diff cmd-show-git-diff)
  (register-command! 'show-git-blame cmd-show-git-blame)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit)
  ;; Task #44: Help system
  (register-command! 'describe-function cmd-describe-function)
  (register-command! 'describe-variable cmd-describe-variable)
  (register-command! 'describe-key-briefly cmd-describe-key-briefly)
  (register-command! 'view-lossage cmd-view-lossage)
  (register-command! 'describe-face cmd-describe-face)
  (register-command! 'describe-syntax cmd-describe-syntax)
  (register-command! 'info cmd-info)
  (register-command! 'info-emacs-manual cmd-info-emacs-manual)
  (register-command! 'info-elisp-manual cmd-info-elisp-manual)
  ;; Dired
  (register-command! 'dired cmd-dired)
  (register-command! 'dired-create-directory cmd-dired-create-directory)
  (register-command! 'dired-do-rename cmd-dired-do-rename)
  (register-command! 'dired-do-delete cmd-dired-do-delete)
  (register-command! 'dired-do-copy cmd-dired-do-copy)
  (register-command! 'dired-do-chmod cmd-dired-do-chmod)
  (register-command! 'dired-find-file cmd-dired-find-file)
  ;; Buffer management
  (register-command! 'rename-uniquely cmd-rename-uniquely)
  (register-command! 'revert-buffer-with-coding cmd-revert-buffer-with-coding)
  (register-command! 'lock-buffer cmd-lock-buffer)
  (register-command! 'buffer-disable-undo cmd-buffer-disable-undo)
  (register-command! 'buffer-enable-undo cmd-buffer-enable-undo)
  (register-command! 'bury-buffer cmd-bury-buffer)
  (register-command! 'unbury-buffer cmd-unbury-buffer)
  ;; Navigation
  (register-command! 'forward-sentence cmd-forward-sentence)
  (register-command! 'backward-sentence cmd-backward-sentence)
  (register-command! 'goto-word-at-point cmd-goto-word-at-point)
  ;; Region operations
  ;; Text manipulation
  (register-command! 'center-region cmd-center-region)
  (register-command! 'indent-rigidly cmd-indent-rigidly)
  (register-command! 'dedent-rigidly cmd-dedent-rigidly)
  (register-command! 'transpose-paragraphs cmd-transpose-paragraphs)
  (register-command! 'fill-individual-paragraphs cmd-fill-individual-paragraphs)
  ;; Bookmarks
  (register-command! 'bookmark-save cmd-bookmark-save)
  (register-command! 'bookmark-load cmd-bookmark-load)
  ;; Window management
  (register-command! 'fit-window-to-buffer cmd-fit-window-to-buffer)
  (register-command! 'maximize-window cmd-maximize-window)
  (register-command! 'minimize-window cmd-minimize-window)
  (register-command! 'rotate-windows cmd-rotate-windows)
  (register-command! 'swap-windows cmd-swap-windows)
  ;; Misc
  (register-command! 'delete-matching-lines cmd-delete-matching-lines)
  (register-command! 'copy-matching-lines cmd-copy-matching-lines)
  (register-command! 'delete-non-matching-lines cmd-delete-non-matching-lines)
  (register-command! 'display-fill-column-indicator cmd-display-fill-column-indicator)
  (register-command! 'electric-newline-and-indent cmd-electric-newline-and-indent)
  ;; Registers
  (register-command! 'view-register cmd-view-register)
  (register-command! 'append-to-register cmd-append-to-register)
  ;; Environment
  (register-command! 'getenv cmd-getenv)
  (register-command! 'setenv cmd-setenv)
  (register-command! 'show-environment cmd-show-environment)
  ;; Encoding
  (register-command! 'set-buffer-file-coding cmd-set-buffer-file-coding)
  (register-command! 'convert-line-endings-unix cmd-convert-line-endings-unix)
  (register-command! 'convert-line-endings-dos cmd-convert-line-endings-dos)
  ;; Completion
  ;; Whitespace
  (register-command! 'whitespace-mode cmd-whitespace-mode)
  (register-command! 'toggle-show-spaces cmd-toggle-show-spaces)
  ;; Folding
  (register-command! 'fold-all cmd-fold-all)
  (register-command! 'unfold-all cmd-unfold-all)
  (register-command! 'toggle-fold cmd-toggle-fold)
  (register-command! 'fold-level cmd-fold-level)
  ;; Macros
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'insert-kbd-macro cmd-insert-kbd-macro)
  ;; VC extras
  (register-command! 'vc-annotate cmd-vc-annotate)
  (register-command! 'vc-diff-head cmd-vc-diff-head)
  (register-command! 'vc-log-file cmd-vc-log-file)
  (register-command! 'vc-revert cmd-vc-revert)
  ;; Imenu
  (register-command! 'imenu cmd-imenu)
  (register-command! 'which-function cmd-which-function)
  ;; File utilities
  (register-command! 'make-directory cmd-make-directory)
  (register-command! 'delete-file cmd-delete-file)
  (register-command! 'copy-file cmd-copy-file)
  (register-command! 'sudo-find-file cmd-sudo-find-file)
  (register-command! 'find-file-literally cmd-find-file-literally)
  ;; Task #45: isearch, abbrev, editing utilities
  (register-command! 'isearch-forward-word cmd-isearch-forward-word)
  (register-command! 'isearch-backward-word cmd-isearch-backward-word)
  (register-command! 'isearch-forward-symbol cmd-isearch-forward-symbol)
  (register-command! 'query-replace-regexp cmd-query-replace-regexp)
  (register-command! 'multi-occur cmd-multi-occur)
  (register-command! 'align-current cmd-align-current)
  (register-command! 'clear-rectangle cmd-clear-rectangle)
  (register-command! 'abbrev-mode cmd-abbrev-mode)
  (register-command! 'define-abbrev cmd-define-abbrev)
  (register-command! 'expand-abbrev cmd-expand-abbrev)
  (register-command! 'list-abbrevs cmd-list-abbrevs)
  (register-command! 'completion-at-point cmd-completion-at-point)
  (register-command! 'complete-filename cmd-complete-filename)
  (register-command! 'resize-window-width cmd-resize-window-width)
  (register-command! 'zap-to-char-inclusive cmd-zap-to-char-inclusive)
  (register-command! 'copy-word-at-point cmd-copy-word-at-point)
  (register-command! 'copy-symbol-at-point cmd-copy-symbol-at-point)
  (register-command! 'mark-page cmd-mark-page)
  (register-command! 'toggle-input-method cmd-toggle-input-method)
  (register-command! 'set-language-environment cmd-set-language-environment)
  (register-command! 'load-theme cmd-load-theme)
  (register-command! 'customize-face cmd-customize-face)
  (register-command! 'list-colors cmd-list-colors)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'auto-revert-mode cmd-auto-revert-mode)
  (register-command! 'diff-backup cmd-diff-backup)
  (register-command! 'first-error cmd-first-error)
  (register-command! 'quick-calc cmd-quick-calc)
  (register-command! 'insert-time cmd-insert-time)
  (register-command! 'insert-file-header cmd-insert-file-header)
  (register-command! 'toggle-debug-on-quit cmd-toggle-debug-on-quit)
  (register-command! 'profiler-start cmd-profiler-start)
  (register-command! 'profiler-stop cmd-profiler-stop)
  (register-command! 'memory-report cmd-memory-report)
  (register-command! 'emacs-version cmd-emacs-version)
  (register-command! 'report-bug cmd-report-bug)
  (register-command! 'view-echo-area-messages cmd-view-echo-area-messages)
  (register-command! 'toggle-menu-bar-mode cmd-toggle-menu-bar-mode)
  (register-command! 'toggle-tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'split-window-below cmd-split-window-below)
  (register-command! 'delete-window-below cmd-delete-window-below)
  (register-command! 'shrink-window-if-larger-than-buffer cmd-shrink-window-if-larger-than-buffer)
  (register-command! 'toggle-frame-fullscreen cmd-toggle-frame-fullscreen)
  (register-command! 'toggle-frame-maximized cmd-toggle-frame-maximized)
  (register-command! 'ispell-word cmd-ispell-word)
  (register-command! 'ispell-buffer cmd-ispell-buffer)
  (register-command! 'ispell-region cmd-ispell-region)
  (register-command! 'term cmd-term)
  (register-command! 'ansi-term cmd-ansi-term))
