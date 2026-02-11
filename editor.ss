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
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit))
