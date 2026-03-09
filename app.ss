;;; -*- Gerbil -*-
;;; Main application and event loop for gemacs

(export app-init! app-run! main tui-session-save!)

(import :std/sugar
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/repl
        :gemacs/shell
        :gemacs/terminal
        :gemacs/vtscreen
        :gemacs/chat
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor
        :gemacs/editor-core
        :gemacs/highlight
        :gemacs/persist
        :gemacs/shell-history
        :gemacs/ipc
        :gemacs/helm-commands
        (only-in :gemacs/editor-extra-editing tui-record-edit-position!)
        (only-in :gemacs/editor-extra-org *desktop-save-mode*)
        (only-in :gemacs/persist *which-key-mode* *which-key-delay* which-key-summary))

;;;============================================================================
;;; Which-key delayed display state (TUI)
;;;============================================================================

;; Counter for delayed which-key display in the TUI poll loop.
;; When a prefix key is pressed, *which-key-tui-countdown* is set to
;; the number of poll ticks before showing hints. Each tick is ~50ms.
(def *which-key-tui-countdown* 0)
(def *which-key-tui-keymap* #f)
(def *which-key-tui-prefix* #f)

(def (which-key-tui-schedule! keymap prefix-str)
  "Schedule which-key hints for TUI after *which-key-delay* seconds."
  (set! *which-key-tui-keymap* keymap)
  (set! *which-key-tui-prefix* prefix-str)
  ;; Convert seconds to poll ticks (50ms per tick)
  (set! *which-key-tui-countdown*
    (max 1 (inexact->exact (ceiling (* *which-key-delay* 20))))))

(def (which-key-tui-cancel!)
  "Cancel any pending which-key display."
  (set! *which-key-tui-countdown* 0)
  (set! *which-key-tui-keymap* #f)
  (set! *which-key-tui-prefix* #f))

(def (which-key-tui-tick! app)
  "Called each poll tick (~50ms). If countdown expires, show which-key hints."
  (when (> *which-key-tui-countdown* 0)
    (set! *which-key-tui-countdown* (- *which-key-tui-countdown* 1))
    (when (= *which-key-tui-countdown* 0)
      ;; Timer fired — show hints if still in prefix mode
      (when (and *which-key-tui-keymap*
                 (not (null? (key-state-prefix-keys (app-state-key-state app)))))
        (let ((hints (which-key-summary *which-key-tui-keymap* 12)))
          (when (> (string-length hints) 0)
            (echo-message! (app-state-echo app)
              (string-append *which-key-tui-prefix* "- " hints)))))
      (set! *which-key-tui-keymap* #f)
      (set! *which-key-tui-prefix* #f))))

;;;============================================================================
;;; Session persistence for TUI layer (desktop-save-mode)
;;;============================================================================

(def *tui-session-path*
  (path-expand ".gemacs-session" (user-info-home (user-info (user-name)))))

(def (tui-session-save! app)
  "Save current session (open file buffers) to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((current-buf (current-buffer-from-app app))
             (entries
               (filter-map
                 (lambda (buf)
                   (let ((path (buffer-file-path buf)))
                     (and path (cons path 0))))
                 *buffer-list*)))
        (call-with-output-file *tui-session-path*
          (lambda (port)
            (display (or (buffer-file-path current-buf) "") port)
            (newline port)
            (for-each
              (lambda (entry)
                (display (car entry) port)
                (display "\t" port)
                (display (number->string (cdr entry)) port)
                (newline port))
              entries)))))))

(def (tui-session-restore! app)
  "Restore session from disk: open saved file buffers."
  (when (file-exists? *tui-session-path*)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let* ((lines (call-with-input-file *tui-session-path*
                        (lambda (port)
                          (let loop ((acc []))
                            (let ((line (read-line port)))
                              (if (eof-object? line) (reverse acc)
                                (loop (cons line acc))))))))
               ;; First line is the current file path
               (current-file (and (pair? lines)
                                  (> (string-length (car lines)) 0)
                                  (car lines)))
               ;; Remaining lines are file\tposition entries
               (file-lines (if (pair? lines) (cdr lines) [])))
          ;; Open each file
          (for-each
            (lambda (line)
              (let ((tab-pos (let scan ((i 0))
                               (cond ((>= i (string-length line)) #f)
                                     ((char=? (string-ref line i) #\tab) i)
                                     (else (scan (+ i 1)))))))
                (when tab-pos
                  (let ((path (substring line 0 tab-pos)))
                    (when (and (> (string-length path) 0) (file-exists? path))
                      (open-file-in-app! app path))))))
            file-lines)
          ;; Switch to the buffer that was current when session was saved
          (when current-file
            (let loop ((bufs *buffer-list*))
              (when (pair? bufs)
                (if (equal? (buffer-file-path (car bufs)) current-file)
                  (let* ((fr (app-state-frame app))
                         (win (current-window fr))
                         (ed (edit-window-editor win)))
                    (set! (edit-window-buffer win) (car bufs))
                    (buffer-attach! ed (car bufs)))
                  (loop (cdr bufs)))))))))))

;;;============================================================================
;;; Application initialization
;;;============================================================================

(def (app-init! files)
  "Initialize the editor. Returns an app-state."
  ;; Set up TUI
  (tui-init!)
  (tui-set-input-mode! (bitwise-ior TB_INPUT_ALT TB_INPUT_MOUSE))
  (tui-set-output-mode! TB_OUTPUT_TRUECOLOR)
  ;; Set terminal background to match editor dark theme
  (tui-set-clear-attrs! #x00d8d8d8 #x00181818)

  ;; Set up keybindings, mode keymaps, and commands
  (setup-default-bindings!)
  (setup-mode-keymaps!)
  (setup-command-docs!)
  (register-all-commands!)
  (register-helm-commands!)

  ;; Load init file (applies settings like scroll-margin)
  (init-file-load!)

  ;; Load persistent state: recent files, minibuffer history, M-x history, save-place, shell history
  (recent-files-load!)
  (set! *minibuffer-history* (savehist-load!))
  (mx-history-load!)
  (save-place-load!)
  (gsh-history-load!)

  ;; Install hook to restore per-buffer highlighting on every buffer switch
  (add-hook! 'post-buffer-attach-hook
    (lambda (editor buf)
      (let ((fp (buffer-file-path buf)))
        (when fp
          (setup-highlighting-for-file! editor fp)))))

  ;; Create frame with one window
  (let* ((width (tui-width))
         (height (tui-height))
         (fr (frame-init! width height))
         (app (new-app-state fr)))

    ;; Configure dark theme, scroll margin, and editor defaults on all editors
    (for-each (lambda (win)
                (let ((ed (edit-window-editor win)))
                  (setup-editor-theme! ed)
                  (setup-scroll-margin! ed)
                  ;; Set default tab width and use spaces
                  (send-message ed SCI_SETTABWIDTH 4 0)
                  (send-message ed SCI_SETUSETABS 0 0)
                  ;; Enable indentation guides
                  (send-message ed SCI_SETINDENTATIONGUIDES SC_IV_LOOKBOTH 0)
                  (send-message ed SCI_SETINDENT 4 0)
                  ;; Enable line numbers in margin 0
                  (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
                  (send-message ed SCI_SETMARGINWIDTHN 0 5)
                  ;; Clear default margin 1 (Scintilla defaults to 16 pixels/chars)
                  (send-message ed SCI_SETMARGINWIDTHN 1 0)
                  ;; Style line number margin: dark gray on very dark background
                  (editor-style-set-foreground ed STYLE_LINENUMBER #x808080)
                  (editor-style-set-background ed STYLE_LINENUMBER #x181818)
                  ;; Enable multiple selection + typing into all selections
                  (send-message ed 2563 1 0)  ; SCI_SETMULTIPLESELECTION
                  (send-message ed 2565 1 0)  ; SCI_SETADDITIONALSELECTIONTYPING
                  (send-message ed 2608 1 0)  ; SCI_SETADDITIONALCARETSVISIBLE
                  (send-message ed 2567 1 0))) ; SCI_SETADDITIONALCARETSBLINK
              (frame-windows fr))

    ;; Restore scratch buffer from persistent storage, or set default
    (let ((ed (current-editor app)))
      (let ((saved (scratch-load!)))
        (if (and saved (> (string-length saved) 0))
          (editor-set-text ed saved)
          (editor-set-text ed (string-append
            ";; Gerbil Emacs — *scratch*\n"
            ";;\n"
            ";; Key Bindings:\n"
            ";;   C-x C-f   Find file        C-x C-s   Save buffer\n"
            ";;   C-x b     Switch buffer     C-x k     Kill buffer\n"
            ";;   C-x C-r   Recent files      M-x       Extended command\n"
            ";;   C-s       Search forward    M-%       Query replace\n"
            ";;   C-x 2     Split window      C-x o     Other window\n"
            ";;\n"
            ";; This buffer is for Gerbil Scheme evaluation.\n\n"))))
      (editor-set-save-point ed)
      (editor-goto-pos ed 0))

    ;; Restore session if desktop-save-mode is on and no files given
    (when (and *desktop-save-mode* (null? files))
      (tui-session-restore! app))

    ;; Open files from command line
    (for-each (lambda (file) (open-file-in-app! app file))
              files)

    ;; Start IPC server for gemacs-client
    (start-ipc-server!)

    ;; Run after-init-hook (parity with Qt layer)
    (run-hooks! 'after-init-hook)

    app))

(def (binary-file? path)
  "Check if a file has a known binary extension (images, archives, etc.)."
  (let ((ext (string-downcase (path-extension path))))
    (member ext '(".png" ".jpg" ".jpeg" ".gif" ".bmp" ".ico" ".svg" ".webp"
                  ".tiff" ".tif" ".psd" ".raw" ".heic" ".avif"
                  ".zip" ".gz" ".bz2" ".xz" ".tar" ".7z" ".rar"
                  ".pdf" ".doc" ".docx" ".xls" ".xlsx"
                  ".so" ".o" ".a" ".dylib" ".exe" ".dll"
                  ".mp3" ".mp4" ".avi" ".mkv" ".wav" ".flac" ".ogg"))))

(def (open-file-in-app! app filename)
  "Open a file or directory in a new buffer."
  ;; Directory -> dired
  (if (and (file-exists? filename)
           (eq? 'directory (file-info-type (file-info filename))))
    (dired-open-directory! app filename)
  ;; Binary file -> refuse
  (if (binary-file? filename)
    (echo-message! (app-state-echo app)
      (string-append "Binary file: " (path-strip-directory filename)
                     " (not opening in TUI)"))
  (let* ((name (uniquify-buffer-name filename))
         (ed (current-editor app))
         (buf (buffer-create! name ed filename))
         (fr (app-state-frame app)))
    ;; Track in recent files
    (recent-files-add! filename)
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
    ;; Record file modification time for external change detection
    (update-buffer-mod-time! buf)
    ;; Apply syntax highlighting: extension first, then shebang fallback
    (let ((lang (detect-file-language filename)))
      (if lang
        (setup-highlighting-for-file! ed filename)
        ;; Try shebang detection from file content
        (let ((text (editor-get-text ed)))
          (when (and text (> (string-length text) 2))
            (let ((shebang-lang (detect-language-from-shebang text)))
              (when shebang-lang
                (setup-highlighting-for-file! ed
                  (string-append "shebang." (symbol->string shebang-lang)))))))))
    ;; Activate major mode from auto-mode-alist
    (let ((mode (detect-major-mode filename)))
      (when mode
        (buffer-local-set! buf 'major-mode mode)
        (let ((mode-cmd (find-command mode)))
          (when mode-cmd (mode-cmd app)))))))))

;;;============================================================================
;;; IPC polling (files opened via gemacs-client)
;;;============================================================================

(def (poll-ipc-files! app)
  "Open any files received via the IPC server."
  (for-each (lambda (f) (open-file-in-app! app f))
            (ipc-poll-files!)))

;;;============================================================================
;;; REPL output polling
;;;============================================================================

(def (find-window-for-buffer fr buf)
  "Find the first window displaying a given buffer, or #f."
  (let loop ((wins (frame-windows fr)))
    (cond
      ((null? wins) #f)
      ((eq? (edit-window-buffer (car wins)) buf) (car wins))
      (else (loop (cdr wins))))))

(def (poll-repl-output! app)
  "Check all REPL buffers for new output from gxi and insert it."
  (for-each
    (lambda (buf)
      (when (repl-buffer? buf)
        (let ((rs (hash-get *repl-state* buf)))
          (when rs
            (let ((output (repl-read-available rs)))
              (when output
                (let ((win (find-window-for-buffer (app-state-frame app) buf)))
                  (when win
                    (let ((ed (edit-window-editor win)))
                      ;; Insert output + new prompt at end
                      (editor-append-text ed output)
                      (editor-append-text ed repl-prompt)
                      ;; Update prompt-pos to after the new prompt
                      (set! (repl-state-prompt-pos rs)
                        (editor-get-text-length ed))
                      ;; Move cursor to end and scroll
                      (editor-goto-pos ed (editor-get-text-length ed))
                      (editor-scroll-caret ed))))))))))
    (buffer-list)))

;;;============================================================================
;;; Shell/Terminal PTY output polling
;;;============================================================================

(def (poll-shell-pty-msg! app buf ss msg)
  "Handle one PTY message for a shell buffer.
   Uses VT100 screen buffer for cursor-addressing programs."
  (let ((tag (car msg))
        (data (cdr msg))
        (vt (shell-state-vtscreen ss)))
    (cond
      ((eq? tag 'data)
       (let ((win (find-window-for-buffer (app-state-frame app) buf)))
         (when win
           (let ((ed (edit-window-editor win)))
             ;; Save pre-PTY text on first data chunk
             (when (and vt (not (shell-state-pre-pty-text ss)))
               (set! (shell-state-pre-pty-text ss) (editor-get-text ed)))
             (if vt
               (begin
                 (vtscreen-feed! vt data)
                 (let* ((rendered (vtscreen-render vt))
                        (full (if (vtscreen-alt-screen? vt)
                                rendered
                                (string-append (or (shell-state-pre-pty-text ss) "")
                                               rendered))))
                   (editor-set-text ed full)
                   (editor-goto-pos ed (editor-get-text-length ed))
                   (editor-scroll-caret ed)))
               (begin
                 (editor-append-text ed (strip-ansi-codes data))
                 (editor-goto-pos ed (editor-get-text-length ed))
                 (editor-scroll-caret ed)))))))
      ((eq? tag 'done)
       (let* ((alt-screen? (and vt (vtscreen-alt-screen? vt)))
              (final-render (and vt (vtscreen-render vt)))
              (pre-text (shell-state-pre-pty-text ss)))
         (shell-cleanup-pty! ss)
         (let ((win (find-window-for-buffer (app-state-frame app) buf)))
           (when win
             (let ((ed (edit-window-editor win)))
               (when pre-text
                 (if alt-screen?
                   (editor-set-text ed pre-text)
                   (let* ((output (or final-render ""))
                          (sep (if (and (> (string-length output) 0)
                                       (not (char=? (string-ref output (- (string-length output) 1)) #\newline)))
                                 "\n" ""))
                          (full (string-append pre-text output sep)))
                     (editor-set-text ed full))))
               (let ((prompt (shell-prompt ss)))
                 (editor-append-text ed prompt)
                 (set! (shell-state-prompt-pos ss) (editor-get-text-length ed))
                 (editor-goto-pos ed (editor-get-text-length ed))
                 (editor-scroll-caret ed))))))))))

(def (poll-terminal-pty-msg! app buf ts msg)
  "Handle one PTY message for a terminal buffer.
   Uses VT100 screen buffer for cursor-addressing programs (top, vim, etc.)."
  (let ((tag (car msg))
        (data (cdr msg))
        (vt (terminal-state-vtscreen ts)))
    (cond
      ((eq? tag 'data)
       (let ((win (find-window-for-buffer (app-state-frame app) buf)))
         (when win
           (let ((ed (edit-window-editor win)))
             ;; Save pre-PTY text on first data chunk
             (when (and vt (not (terminal-state-pre-pty-text ts)))
               (set! (terminal-state-pre-pty-text ts)
                 (editor-get-text ed)))
             (if vt
               ;; Feed data to VT100 screen buffer, then render
               (begin
                 (vtscreen-feed! vt data)
                 (let* ((rendered (vtscreen-render vt))
                        (full (if (vtscreen-alt-screen? vt)
                                rendered
                                (string-append (or (terminal-state-pre-pty-text ts) "")
                                               rendered))))
                   (editor-set-text ed full)
                   (editor-goto-pos ed (editor-get-text-length ed))
                   (editor-scroll-caret ed)))
               ;; Fallback: strip ANSI and append (no vtscreen)
               (let* ((segments (parse-ansi-segments data))
                      (start-pos (editor-get-text-length ed)))
                 (terminal-insert-styled! ed segments start-pos)
                 (editor-goto-pos ed (editor-get-text-length ed))
                 (editor-scroll-caret ed)))))))
      ((eq? tag 'done)
       (let* ((alt-screen? (and vt (vtscreen-alt-screen? vt)))
              (final-render (and vt (vtscreen-render vt)))
              (pre-text (terminal-state-pre-pty-text ts)))
         (terminal-cleanup-pty! ts)
         (let ((win (find-window-for-buffer (app-state-frame app) buf)))
           (when win
             (let ((ed (edit-window-editor win)))
               ;; For full-screen programs: restore pre-PTY text
               ;; For simple commands: keep output
               (when pre-text
                 (if alt-screen?
                   (editor-set-text ed pre-text)
                   (let* ((output (or final-render ""))
                          (sep (if (and (> (string-length output) 0)
                                       (not (char=? (string-ref output (- (string-length output) 1)) #\newline)))
                                 "\n" ""))
                          (full (string-append pre-text output sep)))
                     (editor-set-text ed full))))
               (let* ((raw-prompt (terminal-prompt-raw ts))
                      (segments (parse-ansi-segments raw-prompt))
                      (start-pos (editor-get-text-length ed)))
                 (terminal-insert-styled! ed segments start-pos)
                 (set! (terminal-state-prompt-pos ts) (editor-get-text-length ed))
                 (editor-goto-pos ed (editor-get-text-length ed))
                 (editor-scroll-caret ed))))))))))

(def (poll-pty-output! app)
  "Check all shell and terminal buffers for async PTY output."
  (for-each
    (lambda (buf)
      ;; Shell buffers with active PTY
      (when (shell-buffer? buf)
        (let ((ss (hash-get *shell-state* buf)))
          (when (and ss (shell-pty-busy? ss))
            (let drain ()
              (let ((msg (shell-poll-output ss)))
                (when msg
                  (poll-shell-pty-msg! app buf ss msg)
                  (when (eq? (car msg) 'data)
                    (drain))))))))
      ;; Terminal buffers with active PTY
      (when (terminal-buffer? buf)
        (let ((ts (hash-get *terminal-state* buf)))
          (when (and ts (terminal-pty-busy? ts))
            (let drain ()
              (let ((msg (terminal-poll-output ts)))
                (when msg
                  (poll-terminal-pty-msg! app buf ts msg)
                  (when (eq? (car msg) 'data)
                    (drain)))))))))
    (buffer-list)))

;;;============================================================================
;;; Chat output polling (Claude CLI streaming responses)
;;;============================================================================

(def (poll-chat-output! app)
  "Check all chat buffers for new output from Claude CLI and insert it."
  (for-each
    (lambda (buf)
      (when (chat-buffer? buf)
        (let ((cs (hash-get *chat-state* buf)))
          (when (and cs (chat-busy? cs))
            (let ((result (chat-read-available cs)))
              (when result
                (let ((win (find-window-for-buffer (app-state-frame app) buf)))
                  (when win
                    (let ((ed (edit-window-editor win)))
                      (cond
                        ;; String chunk — append it
                        ((string? result)
                         (editor-append-text ed result)
                         (editor-goto-pos ed (editor-get-text-length ed))
                         (editor-scroll-caret ed))
                        ;; (string . done) — final chunk + done
                        ((and (pair? result) (string? (car result)))
                         (editor-append-text ed (car result))
                         (editor-append-text ed "\n\nYou: ")
                         (set! (chat-state-prompt-pos cs) (editor-get-text-length ed))
                         (editor-goto-pos ed (editor-get-text-length ed))
                         (editor-scroll-caret ed))
                        ;; 'done — response complete
                        ((eq? result 'done)
                         (editor-append-text ed "\n\nYou: ")
                         (set! (chat-state-prompt-pos cs) (editor-get-text-length ed))
                         (editor-goto-pos ed (editor-get-text-length ed))
                         (editor-scroll-caret ed))))))))))))
    (buffer-list)))

;;;============================================================================
;;; Event loop
;;;============================================================================

(def (app-run! app)
  "Main event loop."
  (let loop ()
    (when (app-state-running app)
      ;; Process Scintilla notifications
      (for-each (lambda (win)
                  (editor-poll-notifications (edit-window-editor win)))
                (frame-windows (app-state-frame app)))

      ;; Update brace matching for current editor
      (update-brace-match! (edit-window-editor
                             (current-window (app-state-frame app))))

      ;; Poll REPL subprocess output
      (poll-repl-output! app)

      ;; Poll Shell/Terminal PTY output
      (poll-pty-output! app)

      ;; Poll AI chat output
      (poll-chat-output! app)

      ;; Poll IPC queue for files opened via gemacs-client
      (poll-ipc-files! app)

      ;; Tick pulse highlight countdown
      (pulse-tick!)

      ;; Tick which-key delayed display
      (which-key-tui-tick! app)

      ;; Auto-save and external modification check (~30s at 50ms poll)
      (set! *auto-save-counter* (+ *auto-save-counter* 1))
      (when (>= *auto-save-counter* *auto-save-interval*)
        (set! *auto-save-counter* 0)
        (auto-save-buffers! app)
        (check-file-modifications! app))

      ;; Draw modelines, dividers, and echo area FIRST into the termbox buffer.
      ;; This must happen before editor-refresh because Scintilla's
      ;; Refresh() calls tb_present() internally.
      (draw-all-modelines! app)
      (frame-draw-dividers! (app-state-frame app))
      (let* ((fr (app-state-frame app))
             (echo-row (- (frame-height fr) 1))
             (width (frame-width fr)))
        (echo-draw! (app-state-echo app) echo-row width))

      ;; Refresh all editors (paints editor content + calls tb_present)
      (frame-refresh! (app-state-frame app))

      ;; Position cursor at caret in current window
      (position-cursor! app)

      ;; Wait for event with timeout
      (let ((ev (tui-peek-event 50)))
        (when ev
          (dispatch-event! app ev)))

      (loop))))

;;;============================================================================
;;; Editor theme (dark colors matching scintilla-termbox defaults)
;;;============================================================================

(def (setup-scroll-margin! ed)
  "Set vertical caret policy for scroll margin on a Scintilla editor.
   SCI_SETYCARETPOLICY = 2403, CARET_SLOP=1, CARET_STRICT=4."
  (when (> *scroll-margin* 0)
    (send-message ed 2403 5 *scroll-margin*)))  ;; 5 = CARET_SLOP|CARET_STRICT

(def (setup-editor-theme! ed)
  "Configure dark terminal theme for an editor."
  ;; Default style: light gray on dark gray (matching semester.c reference)
  (editor-style-set-foreground ed STYLE_DEFAULT #xd8d8d8)
  (editor-style-set-background ed STYLE_DEFAULT #x181818)
  (send-message ed SCI_STYLECLEARALL)  ; propagate to all styles
  ;; White caret for visibility
  (editor-set-caret-foreground ed #xFFFFFF)
  ;; Highlight current line with visible background
  (editor-set-caret-line-visible ed #t)
  (editor-set-caret-line-background ed #x333333)
  ;; Brace matching styles
  (editor-style-set-foreground ed STYLE_BRACELIGHT #x00FF00)  ; green for match
  (editor-style-set-bold ed STYLE_BRACELIGHT #t)
  (editor-style-set-foreground ed STYLE_BRACEBAD #xFF0000)    ; red for mismatch
  (editor-style-set-bold ed STYLE_BRACEBAD #t)
  ;; Enable multiple selection + typing into all selections
  (send-message ed 2563 1 0)  ; SCI_SETMULTIPLESELECTION
  (send-message ed 2565 1 0)  ; SCI_SETADDITIONALSELECTIONTYPING
  (send-message ed 2608 1 0)  ; SCI_SETADDITIONALCARETSVISIBLE
  (send-message ed 2567 1 0)) ; SCI_SETADDITIONALCARETSBLINK

;;;============================================================================
;;; Brace/paren matching
;;;============================================================================

(def (update-brace-match! ed)
  "Highlight matching braces at cursor position."
  (let* ((pos (editor-get-current-pos ed))
         (ch-at (send-message ed SCI_GETCHARAT pos 0))
         ;; Also check char before cursor
         (ch-before (if (> pos 0) (send-message ed SCI_GETCHARAT (- pos 1) 0) 0)))
    (cond
      ;; Check char at cursor
      ((brace-char? ch-at)
       (let ((match (send-message ed SCI_BRACEMATCH pos 0)))
         (if (>= match 0)
           (send-message ed SCI_BRACEHIGHLIGHT pos match)
           (send-message ed SCI_BRACEBADLIGHT pos 0))))
      ;; Check char before cursor
      ((brace-char? ch-before)
       (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
         (if (>= match 0)
           (send-message ed SCI_BRACEHIGHLIGHT (- pos 1) match)
           (send-message ed SCI_BRACEBADLIGHT (- pos 1) 0))))
      ;; No brace at cursor — clear highlights
      ;; Scintilla uses INVALID_POSITION (-1) to mean "no position"
      ;; wparam is unsigned-long, lparam is signed long
      (else
       (send-message ed SCI_BRACEHIGHLIGHT #xFFFFFFFFFFFFFFFF -1)))))

;;;============================================================================
;;; Drawing helpers
;;;============================================================================

(def (draw-all-modelines! app)
  (let* ((fr (app-state-frame app))
         (cur-idx (frame-current-idx fr))
         (windows (frame-windows fr)))
    (let loop ((wins windows) (i 0))
      (when (pair? wins)
        (modeline-draw! (car wins) (= i cur-idx))
        (loop (cdr wins) (+ i 1))))))

(def (position-cursor! app)
  "Position terminal cursor at the caret location in the current editor."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         ;; Use POINTX/POINTY for screen-relative coordinates
         ;; (accounts for margins, scroll, tab width)
         (screen-x (send-message ed SCI_POINTXFROMPOSITION 0 pos))
         (screen-y (send-message ed SCI_POINTYFROMPOSITION 0 pos))
         (win-x (edit-window-x win))
         (win-y (edit-window-y win)))
    (tui-set-cursor! (+ win-x screen-x) (+ win-y screen-y))
    ;; Present to make cursor position visible immediately
    (tui-present!)))

(def (digit-key-code? code)
  (and (>= code (char->integer #\0))
       (<= code (char->integer #\9))))

(def (handle-prefix-digit-or-sign! app code)
  "Consume digit or '-' keys while building a prefix argument."
  (let ((prefix (app-state-prefix-arg app)))
    (cond
     ((and (list? prefix) (= code (char->integer #\-)))
      (cmd-negative-argument app)
      #t)
     ((and (digit-key-code? code)
           (or (list? prefix)
               (eq? prefix '-)
               (app-state-prefix-digit-mode? app)))
      (cmd-digit-argument app (- code (char->integer #\0)))
      #t)
     (else #f))))

;;;============================================================================
;;; Event dispatch
;;;============================================================================

(def (dispatch-event! app ev)
  (cond
    ;; Resize event
    ((tui-event-resize? ev)
     (let ((w (tui-event-w ev))
           (h (tui-event-h ev)))
       (frame-resize! (app-state-frame app) w h)))

    ;; Mouse event
    ((tui-event-mouse? ev)
     (dispatch-mouse! app ev))

    ;; Key event
    ((tui-event-key? ev)
     (dispatch-key! app ev))))

(def (dispatch-mouse! app ev)
  "Forward mouse event to the appropriate editor."
  (let* ((mx (tui-event-x ev))
         (my (tui-event-y ev))
         (key (tui-event-key ev))
         (fr (app-state-frame app)))
    ;; Find which window the mouse is in
    (let loop ((wins (frame-windows fr)) (i 0))
      (when (pair? wins)
        (let* ((win (car wins))
               (wy (edit-window-y win))
               (wh (- (edit-window-h win) 1)))  ; edit area height
          (if (and (>= my wy) (< my (+ wy wh)))
            ;; Mouse is in this window's edit area
            (let ((ed (edit-window-editor win))
                  (event-type (cond
                                ((= key TB_KEY_MOUSE_LEFT) SCM_PRESS)
                                ((= key TB_KEY_MOUSE_RELEASE) SCM_RELEASE)
                                (else SCM_PRESS))))
              ;; Focus this window
              (set! (frame-current-idx fr) i)
              (editor-send-mouse ed event-type 1
                                 (- my wy) mx))
            (loop (cdr wins) (+ i 1))))))))

;;; Check if a TUI event is a plain printable character (no modifiers).
(def (tui-event-printable-char ev)
  "Return the character if ev is a plain printable keystroke, or #f."
  (let ((ch (tui-event-ch ev))
        (mod (tui-event-mod ev)))
    (and (> ch 31)
         (zero? (bitwise-and mod TB_MOD_ALT))
         (integer->char ch))))

(def (dispatch-key-normal! app ev)
  "Process a key event through the keymap state machine (no chord detection)."
  ;; Repeat-mode: check active repeat map before normal dispatch
  (let ((repeat-handled
         (and (active-repeat-map)
              (let* ((key-str (key-event->string ev))
                     (repeat-cmd (repeat-map-lookup key-str)))
                (if repeat-cmd
                  (begin (execute-command! app repeat-cmd) #t)
                  (begin (clear-repeat-map!) #f))))))
    (unless repeat-handled
  (let-values (((action data new-state)
                (let ((cur-buf (with-catch (lambda (_) #f)
                                 (lambda () (edit-window-buffer
                                              (current-window (app-state-frame app)))))))
                  (key-state-feed! (app-state-key-state app) ev cur-buf))))
    (set! (app-state-key-state app) new-state)
    ;; Quoted insert: insert next key literally (C-q)
    (if *quoted-insert-pending*
      (begin
        (set! *quoted-insert-pending* #f)
        (let* ((fr (app-state-frame app))
               (ed (edit-window-editor (current-window fr)))
               (pos (editor-get-current-pos ed)))
          (cond
            ;; Self-insert: data is a char code
            ((eq? action 'self-insert)
             (let ((ch (integer->char data)))
               (editor-insert-text ed pos (string ch))
               (echo-message! (app-state-echo app) (string-append "Inserted: " (string ch)))))
            ;; Command key with a printable char in the event
            (else
             (let ((ch (tui-event-ch ev)))
               (if (and ch (> ch 0))
                 (let ((c (integer->char ch)))
                   (editor-insert-text ed pos (string c))
                   (echo-message! (app-state-echo app) (string-append "Inserted: " (string c))))
                 ;; Control char: convert key code to character
                 (let ((key (tui-event-key ev)))
                   (when (and key (< key 32))
                     (editor-insert-text ed pos (string (integer->char key)))
                     (echo-message! (app-state-echo app)
                       (string-append "Inserted control char: ^" (string (integer->char (+ key 64)))))))))))))
      (begin
    ;; Cancel which-key timer on any non-prefix action
    (when (not (eq? action 'prefix))
      (which-key-tui-cancel!))
    (case action
      ((command)
       ;; Record macro step (skip macro control commands themselves)
       (when (and (app-state-macro-recording app)
                  (not (memq data '(start-kbd-macro end-kbd-macro call-last-kbd-macro
                                    call-named-kbd-macro name-last-kbd-macro
                                    list-kbd-macros save-kbd-macros load-kbd-macros))))
         (set! (app-state-macro-recording app)
           (cons (cons 'command data)
                 (app-state-macro-recording app))))
       (execute-command! app data))
      ((prefix)
       ;; Show prefix indicator in echo area; schedule which-key hints after delay
       (let* ((prefix-str (let loop ((keys (key-state-prefix-keys new-state))
                                     (acc ""))
                            (if (null? keys) acc
                              (loop (cdr keys)
                                    (if (string=? acc "")
                                      (car keys)
                                      (string-append acc " " (car keys)))))))
              (current-km (key-state-keymap new-state)))
         (if *which-key-mode*
           ;; Which-key mode: show prefix now, schedule hints after delay
           (begin
             (echo-message! (app-state-echo app)
               (string-append prefix-str "-"))
             (which-key-tui-schedule! current-km prefix-str))
           ;; Which-key off: just show the prefix indicator
           (echo-message! (app-state-echo app)
             (string-append prefix-str "-")))))
      ((self-insert)
       ;; Apply key translation (e.g., bracket/paren swap)
       (let ((translated (char->integer (key-translate-char (integer->char data)))))
         (if (handle-prefix-digit-or-sign! app translated)
           (void)
           (begin
             ;; Record macro step
             (when (app-state-macro-recording app)
               (set! (app-state-macro-recording app)
                 (cons (cons 'self-insert translated)
                       (app-state-macro-recording app))))
             (cmd-self-insert! app translated)
             ;; Track edit position for goto-last-change
             (tui-record-edit-position! app)
             (set! (app-state-prefix-arg app) #f)
             (set! (app-state-prefix-digit-mode? app) #f)))))
      ((undefined)
       (echo-error! (app-state-echo app)
                    (string-append data " is undefined"))))))))) ;; close case + begin + let-values + unless + let
  ) ;; close let repeat-handled

(def (dispatch-key! app ev)
  "Process a key event with chord detection and key translation."
  (let ((echo (app-state-echo app)))
    ;; Record keystroke in lossage ring
    (key-lossage-record! app (key-event->string ev))

    ;; Clear echo message on next key press (unless in prefix)
    (when (and (echo-state-message echo)
               (null? (key-state-prefix-keys (app-state-key-state app))))
      (echo-clear! echo))

    ;; Chord detection: if this is a plain printable char that could start
    ;; a chord and we're at the top-level keymap, wait briefly for a second key
    (let ((ch (tui-event-printable-char ev)))
      (if (and ch
               *chord-mode*
               (null? (key-state-prefix-keys (app-state-key-state app)))
               (chord-start-char? ch))
        ;; Potential chord starter — peek for second key within timeout
        (let ((ev2 (tui-peek-event *chord-timeout*)))
          (if ev2
            (let ((ch2 (and (tui-event-key? ev2) (tui-event-printable-char ev2))))
              (if ch2
                ;; Got a second printable char — check chord
                (let ((cmd (chord-lookup ch ch2)))
                  (if cmd
                    ;; Chord matched — execute command
                    (begin
                      (key-lossage-record! app (key-event->string ev2))
                      (execute-command! app cmd))
                    ;; Not a chord — process both keys normally
                    (begin
                      (dispatch-key-normal! app ev)
                      (dispatch-key! app ev2))))
                ;; Second event is not a printable char — process both
                (begin
                  (dispatch-key-normal! app ev)
                  (dispatch-event! app ev2))))
            ;; Timeout — no second key, process the first normally
            (dispatch-key-normal! app ev)))
        ;; Not a chord starter — normal processing
        (dispatch-key-normal! app ev)))))

;;;============================================================================
;;; Main entry point
;;;============================================================================

(def main
  (lambda args
    (let ((app (app-init! args)))
      (try
        (app-run! app)
        (finally
          ;; Save persistent state before exit
          (recent-files-save!)
          (savehist-save! *minibuffer-history*)
          (mx-history-save!)
          (save-place-save!)
          (gsh-history-save!)
          ;; Save scratch buffer content
          (let* ((scratch-buf (buffer-by-name "*scratch*"))
                 (fr (app-state-frame app)))
            (when scratch-buf
              (let ((win (find-window-for-buffer fr scratch-buf)))
                (when win
                  (scratch-save! (editor-get-text (edit-window-editor win)))))))
          (stop-ipc-server!)
          (frame-shutdown! (app-state-frame app))
          (tui-shutdown!))))))
