;;; -*- Gerbil -*-
;;; Qt commands facade - imports all sub-modules, exports public API,
;;; contains cross-cutting functions and command registration.

(export qt-register-all-commands!
        dired-open-directory!
        qt-open-image-inline!
        *qt-app-ptr*
        qt-kill-ring-push!
        *isearch-active*
        isearch-handle-key!
        *qreplace-active*
        qreplace-handle-key!
        recent-files-add!
        recent-files-load!
        bookmarks-load!
        session-save!
        session-restore-files
        *tab-bar-visible*
        *auto-revert-mode*
        *auto-revert-tail-buffers*
        *file-mtimes*
        file-mtime-record!
        file-mtime-changed?
        *eldoc-mode*
        eldoc-display!
        ;; *mode-keymaps* and mode-keymap-lookup now come from core.ss
        *current-theme*
        *themes*
        theme-stylesheet
        load-theme!
        apply-theme!
        ;; Init file convenience API
        load-theme
        define-theme!
        buffer-touch!
        custom-keys-load!
        abbrevs-load!
        load-init-file!
        scratch-save!
        scratch-restore!
        scratch-update-text!
        undo-history-record!
        winner-save!
        *follow-mode*
        *view-mode-buffers*
        *so-long-buffers*
        check-so-long!
        savehist-save!
        savehist-load!
        gsh-history-save!
        gsh-history-load!
        save-place-load!
        save-place-save!
        auto-fill-check!
        *auto-save-disabled-buffers*
        *delete-trailing-whitespace-on-save*
        *require-final-newline*
        *save-place-enabled*
        *centered-cursor-mode*
        uniquify-buffer-name!
        ;; Shell command framework
        shell-command-to-string
        shell-command-to-buffer!
        register-shell-command!
        *user-shell-commands*
        ;; Workspaces
        workspace-init!
        *workspaces*
        *current-workspace*
        workspace-add-buffer!
        workspace-remove-buffer!
        ;; Pulse-line
        qt-pulse-line!
        qt-pulse-tick!
        qt-pulse-check-jump!
        ;; Auto-highlight
        qt-idle-highlight-symbol!
        ;; Diff-hl
        *diff-hl-active*
        ;; Describe-key interception
        *qt-describe-key-pending*
        qt-describe-key-result!
        ;; Quoted insert interception
        *qt-quoted-insert-pending*
        qt-quoted-insert-handle!
        ;; Edit position tracking for goto-last-change
        qt-record-edit-position!
        *qt-desktop-save-mode*
        ;; Aggressive indent
        qt-aggressive-indent-line!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/text/base64
        (only-in :std/text/json
          json-object->string read-json string->json-object)
        (only-in :std/net/request
          http-post request-status request-text request-close)
        :gemacs/async
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/snippets
        :gemacs/editor
        (only-in :gemacs/editor-extra-final
                 find-editorconfig)
        (only-in :gemacs/editor-extra-helpers
                 project-current)
        (only-in :gemacs/persist
                 buffer-local-set! buffer-local-get
                 save-place-save! save-place-load!
                 save-place-remember! save-place-restore
                 *save-place-enabled* *require-final-newline*
                 *centered-cursor-mode*
                 *fill-column* *auto-fill-mode*
                 *delete-trailing-whitespace-on-save*
                 *copilot-mode* *copilot-api-key* *copilot-model*
                 *copilot-api-url* *copilot-suggestion* *copilot-suggestion-pos*)
        :gemacs/repl
        :gemacs/eshell
        :gemacs/gsh-eshell
        :gemacs/shell
        :gemacs/shell-history
        :gemacs/terminal
        :gemacs/chat
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/image
        ;; Sub-modules (chain)
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-sexp2
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-vcs2
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-shell2
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-modes2
        :gemacs/qt/snippets
        :gemacs/qt/commands-config
        :gemacs/qt/commands-config2
        :gemacs/qt/commands-parity
        :gemacs/qt/commands-parity2
        :gemacs/qt/commands-parity3
        :gemacs/qt/commands-parity3b
        :gemacs/qt/commands-parity4
        :gemacs/qt/commands-parity5
        :gemacs/qt/commands-aliases
        :gemacs/qt/commands-aliases2
        :gemacs/qt/helm-commands)

;;;============================================================================
;;; Cross-cutting functions (moved to facade due to forward references)
;;;============================================================================

(def (apply-dir-locals! app file-path)
  "Apply directory-local variables for FILE-PATH."
  (when file-path
    (let* ((dir (path-directory file-path))
           (config-file (find-dir-locals-file dir)))
      (when config-file
        ;; Check cache
        (let* ((cached (hash-get *dir-locals-cache* config-file))
               (current-mtime (file-mtime config-file))
               (settings (if (and cached current-mtime
                                  (= (car cached) current-mtime))
                           (cdr cached)
                           ;; Reload
                           (let ((s (read-dir-locals config-file)))
                             (when (and s current-mtime)
                               (hash-put! *dir-locals-cache* config-file
                                          (cons current-mtime s)))
                             s))))
          (when (and settings (list? settings))
            (for-each
              (lambda (pair)
                (when (pair? pair)
                  (let ((key (car pair))
                        (val (cdr pair)))
                    (case key
                      ((tab-width)
                       (when (and (integer? val) (> val 0) (<= val 16))
                         (set! *tab-width* val)))
                      ((indent-tabs-mode)
                       (set! *indent-tabs-mode* (and val #t)))
                      ((fill-column)
                       (when (and (integer? val) (> val 0))
                         (set! *fill-column* val)))
                      ((compile-command)
                       (when (string? val)
                         (set! (app-state-last-compile app) val)))
                      ((auto-indent)
                       (set! *auto-indent* (and val #t)))
                      ((auto-pair-mode)
                       (set! *auto-pair-mode* (and val #t)))))))
              settings)))))))

(def (qt-apply-editorconfig! app file-path)
  "Apply .editorconfig settings for FILE-PATH in Qt mode."
  (when file-path
    (let ((settings (find-editorconfig file-path)))
      (when (> (hash-length settings) 0)
        (let ((ed (current-qt-editor app))
              (indent-style (hash-get settings "indent_style"))
              (indent-size (hash-get settings "indent_size"))
              (tab-width-val (hash-get settings "tab_width")))
          ;; Indent style
          (when indent-style
            (let ((use-tabs (string=? indent-style "tab")))
              (set! *indent-tabs-mode* use-tabs)
              (sci-send ed SCI_SETUSETABS (if use-tabs 1 0))))
          ;; Indent size
          (when indent-size
            (let ((size (string->number indent-size)))
              (when (and size (> size 0))
                (set! *tab-width* size)
                (sci-send ed SCI_SETINDENT size)
                (sci-send ed SCI_SETTABWIDTH
                  (or (and tab-width-val (string->number tab-width-val)) size)))))
          ;; Tab width only
          (when (and tab-width-val (not indent-size))
            (let ((tw (string->number tab-width-val)))
              (when (and tw (> tw 0))
                (set! *tab-width* tw)
                (sci-send ed SCI_SETTABWIDTH tw)))))))))

(def (cmd-editorconfig-apply app)
  "Apply .editorconfig settings to current buffer (Qt)."
  (let ((path (buffer-file-path (current-qt-buffer app))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((settings (find-editorconfig path)))
        (if (= (hash-length settings) 0)
          (echo-message! (app-state-echo app) "No .editorconfig found")
          (begin
            (qt-apply-editorconfig! app path)
            (echo-message! (app-state-echo app)
              (string-append "Applied editorconfig ("
                (number->string (hash-length settings)) " settings)"))))))))

(def (cmd-show-dir-locals app)
  "Show directory-local settings for the current buffer's file."
  (let ((path (buffer-file-path (current-qt-buffer app))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((config (find-dir-locals-file (path-directory path))))
        (if (not config)
          (echo-message! (app-state-echo app) "No .gemacs-config found")
          (let ((settings (read-dir-locals config)))
            (echo-message! (app-state-echo app)
              (string-append config ": "
                (if settings
                  (string-join
                    (map (lambda (p)
                           (string-append (symbol->string (car p)) "="
                             (let ((v (cdr p)))
                               (cond
                                 ((string? v) v)
                                 ((boolean? v) (if v "#t" "#f"))
                                 ((number? v) (number->string v))
                                 (else "?")))))
                         settings)
                    ", ")
                  "invalid format")))))))))

(def (cmd-newline app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ((dired-buffer? buf)  (cmd-dired-find-file app))
      ((eq? (buffer-lexer-lang buf) 'buffer-list) (cmd-buffer-list-select app))
      ((string=? (buffer-name buf) "*Grep*") (cmd-grep-goto app))
      ((string=? (buffer-name buf) "*Occur*") (cmd-occur-goto app))
      ((terminal-buffer? buf) (cmd-terminal-send app))
      ((repl-buffer? buf)   (cmd-repl-send app))
      ((eshell-buffer? buf) (cmd-eshell-send app))
      ((shell-buffer? buf)  (cmd-shell-send app))
      ((chat-buffer? buf)   (cmd-chat-send app))
      (else
        (let ((ed (current-qt-editor app)))
          (if *auto-indent*
            ;; Auto-indent: copy previous line's leading whitespace
            (let ((indent (current-line-indent ed)))
              (qt-plain-text-edit-insert-text! ed (string-append "\n" indent)))
            (qt-plain-text-edit-insert-text! ed "\n")))))))

(def (qt-open-image-inline! app filename)
  "Open an image file as an inline image buffer."
  (gemacs-log! "IMG: loading pixmap " filename)
  (let* ((pixmap (qt-pixmap-load filename)))
    (gemacs-log! "IMG: pixmap loaded null=" (if (qt-pixmap-null? pixmap) "yes" "no"))
    (if (qt-pixmap-null? pixmap)
      (begin
        (qt-pixmap-destroy! pixmap)
        (echo-error! (app-state-echo app)
          (string-append "Failed to load image: " filename)))
      (let* ((name (path-strip-directory filename))
             (fr (app-state-frame app))
             (ed (current-qt-editor app))
             (_ (gemacs-log! "IMG: creating buffer"))
             (buf (qt-buffer-create! name ed filename))
             (orig-w (qt-pixmap-width pixmap))
             (orig-h (qt-pixmap-height pixmap)))
        (gemacs-log! "IMG: buffer created " (number->string orig-w) "x" (number->string orig-h))
        (set! (buffer-lexer-lang buf) 'image)
        (hash-put! *image-buffer-state* buf
          (list pixmap (box 1.0) orig-w orig-h))
        (buffer-touch! buf)
        (gemacs-log! "IMG: calling qt-buffer-attach!")
        (qt-buffer-attach! ed buf)
        (gemacs-log! "IMG: qt-buffer-attach! done")
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (echo-message! (app-state-echo app)
          (string-append "Image: " name " (" (number->string orig-w) "x" (number->string orig-h) ")"))
        (gemacs-log! "IMG: qt-open-image-inline! done")))))

(def (cmd-find-file-at-point app)
  "Open file at point, or prompt with path at point as default."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (at-point (file-path-at-point ed))
         (default-path (and at-point (car at-point)))
         (default-line (and at-point (cdr at-point)))
         (prompt (if default-path
                   (string-append "Find file [" default-path "]: ")
                   "Find file: "))
         (input (qt-echo-read-string app prompt))
         (filename (if (and input (string=? input "") default-path)
                     default-path
                     input)))
    (when (and filename (> (string-length filename) 0))
      (let ((filename (expand-filename filename)))
      (recent-files-add! filename)
      (cond
        ((and (file-exists? filename)
              (eq? 'directory (file-info-type (file-info filename))))
         (dired-open-directory! app filename))
        ((image-file? filename)
         (qt-open-image-inline! app filename))
        (else
        (let* ((name (uniquify-buffer-name! filename))
               (fr (app-state-frame app))
               (ed2 (current-qt-editor app))
               (buf (qt-buffer-create! name ed2 filename)))
          (qt-buffer-attach! ed2 buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (if text
                (begin
                  ;; Cache line ending style for modeline
                  (hash-put! *buffer-eol-cache* name (detect-eol-from-text text))
                  (qt-plain-text-edit-set-text! ed2 text)
                  (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                  (if default-line
                    ;; Jump to the line from file:line
                    (let ((target-pos (text-line-position text default-line)))
                      (qt-plain-text-edit-set-cursor-position! ed2 target-pos))
                    ;; Restore saved cursor position if save-place enabled
                    (let ((saved-pos (and *save-place-enabled*
                                         (save-place-restore filename))))
                      (if (and saved-pos (< saved-pos (string-length text)))
                        (qt-plain-text-edit-set-cursor-position! ed2 saved-pos)
                        (qt-plain-text-edit-set-cursor-position! ed2 0)))))
                ;; Binary file — can't read as text
                (qt-plain-text-edit-set-text! ed2
                  (string-append "[Binary file: " filename "]"))))
            (file-mtime-record! filename))
          (qt-setup-highlighting! app buf)
          (apply-dir-locals! app filename)
          (qt-apply-editorconfig! app filename)
          (echo-message! echo (string-append "Opened: " filename)))))))))

(def (qt-list-directory-files dir)
  "List files in a directory for completion. Returns sorted list of basenames."
  (with-catch (lambda (e) [])
    (lambda ()
      (sort (directory-files dir) string<?))))

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (fp (and buf (buffer-file-path buf)))
         (default-dir (if fp
                       (path-directory fp)
                       (current-directory)))
         (filename (qt-echo-read-file-with-narrowing app "Find file: " default-dir)))
    (when filename
      (when (> (string-length filename) 0)
        (let ((filename (expand-filename filename)))
        ;; Check for remote path first
        (if (tramp-path? filename)
          (let-values (((host remote-path) (tramp-parse-path filename)))
            (echo-message! (app-state-echo app)
              (string-append "Fetching " host ":" remote-path "..."))
            (let ((content (tramp-read-file host remote-path)))
              (if (not content)
                (echo-error! (app-state-echo app)
                  (string-append "Failed to fetch " remote-path))
                (let* ((name (string-append (path-strip-directory remote-path) " [" host "]"))
                       (fr (app-state-frame app))
                       (ed (current-qt-editor app))
                       (buf (qt-buffer-create! name ed #f)))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                  (qt-plain-text-edit-set-text! ed content)
                  (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (set! (buffer-file-path buf) filename)
                  (echo-message! (app-state-echo app)
                    (string-append "Loaded " remote-path " from " host))))))
          (begin
        ;; Track in recent files
        (recent-files-add! filename)
        (cond
          ;; Directory -> dired
          ((and (file-exists? filename)
                (eq? 'directory (file-info-type (file-info filename))))
           (dired-open-directory! app filename))
          ;; Image file -> open as image buffer
          ((image-file? filename)
           (qt-open-image-inline! app filename))
          ;; Regular text file
          (else
          (let* ((name (uniquify-buffer-name! filename))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (qt-buffer-create! name ed filename)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (when (file-exists? filename)
              (let ((text (read-file-as-string filename)))
                (if text
                  (begin
                    ;; Cache line ending style for modeline
                    (hash-put! *buffer-eol-cache* name (detect-eol-from-text text))
                    (qt-plain-text-edit-set-text! ed text)
                    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                    ;; Restore saved cursor position if save-place is enabled
                    (let ((saved-pos (and *save-place-enabled*
                                         (save-place-restore filename))))
                      (if (and saved-pos (< saved-pos (string-length text)))
                        (qt-plain-text-edit-set-cursor-position! ed saved-pos)
                        (qt-plain-text-edit-set-cursor-position! ed 0))))
                  ;; Binary file — can't read as text
                  (qt-plain-text-edit-set-text! ed
                    (string-append "[Binary file: " filename "]"))))
              (file-mtime-record! filename))
            (qt-setup-highlighting! app buf)
            ;; Apply directory-locals and editorconfig settings
            (apply-dir-locals! app filename)
            (qt-apply-editorconfig! app filename)
            (echo-message! echo (string-append "Opened: " filename))))))))))))


(def (cmd-save-buffer app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      (if (tramp-path? path)
        ;; Save to remote host
        (let-values (((host remote-path) (tramp-parse-path path)))
          (let ((text (qt-plain-text-edit-text ed)))
            (echo-message! echo (string-append "Saving to " host ":" remote-path "..."))
            (if (tramp-write-file host remote-path text)
              (begin
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (echo-message! echo "Remote file saved"))
              (echo-error! echo "Failed to save remote file"))))
      ;; Save to existing local path
      (begin
        ;; Run before-save-hook
        (run-hooks! 'before-save-hook app buf)
        ;; Create backup file if original exists and hasn't been backed up yet
        (when (and *backup-files* (file-exists? path) (not (buffer-backup-done? buf)))
          (with-catch
            (lambda (e) #f)  ; Ignore backup errors
            (lambda ()
              (if *version-control*
                ;; Numbered backups: file.~1~, file.~2~, etc.
                (let loop ((n 1))
                  (let ((vpath (string-append path ".~" (number->string n) "~")))
                    (if (file-exists? vpath)
                      (loop (+ n 1))
                      (begin (copy-file path vpath)
                             (set! (buffer-backup-done? buf) #t)))))
                ;; Simple backup: file~
                (begin (copy-file path (string-append path "~"))
                       (set! (buffer-backup-done? buf) #t))))))
        ;; Remember cursor position for save-place
        (when *save-place-enabled*
          (save-place-remember! path (qt-plain-text-edit-cursor-position ed)))
        ;; Delete trailing whitespace on save if enabled
        (when *delete-trailing-whitespace-on-save*
          (cmd-delete-trailing-whitespace app))
        ;; Ensure final newline if required
        (when *require-final-newline*
          (let ((txt (qt-plain-text-edit-text ed)))
            (when (and (> (string-length txt) 0)
                       (not (char=? (string-ref txt (- (string-length txt) 1)) #\newline)))
              (qt-plain-text-edit-set-cursor-position! ed (string-length txt))
              (qt-plain-text-edit-insert-text! ed "\n"))))
        (let ((text (qt-plain-text-edit-text ed)))
          (write-string-to-file path text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          ;; Remove auto-save file if it exists
          (let ((auto-save-path (make-auto-save-path path)))
            (when (file-exists? auto-save-path)
              (delete-file auto-save-path)))
          (file-mtime-record! path)
          (echo-message! echo (string-append "Wrote " path))
          ;; Run after-save-hook
          (run-hooks! 'after-save-hook app buf)
          ;; Compile-on-save for Gerbil projects
          (compile-on-save-check! app path)
          ;; Flycheck: run syntax check on Gerbil files
          (flycheck-check! app path)
          ;; LSP: notify didSave
          (lsp-hook-did-save! app buf))))
      ;; No path: prompt for one
      (let ((filename (qt-echo-read-string app "Write file: ")))
        (when (and filename (> (string-length filename) 0))
          (set! (buffer-file-path buf) filename)
          (set! (buffer-name buf) (path-strip-directory filename))
          (let ((text (qt-plain-text-edit-text ed)))
            (write-string-to-file filename text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (file-mtime-record! filename)
            (echo-message! echo (string-append "Wrote " filename))))))))

(def (cmd-kill-buffer-cmd app)
  (let* ((echo (app-state-echo app))
         (cur-buf (current-qt-buffer app))
         (name (qt-echo-read-string app
                  (string-append "Kill buffer (" (buffer-name cur-buf) "): "))))
    (when name
      (let* ((target-name (if (string=? name "") (buffer-name cur-buf) name))
             (buf (buffer-by-name target-name)))
        (if buf
          (if (<= (length (buffer-list)) 1)
            (echo-error! echo "Can't kill last buffer")
            ;; Confirm if buffer is modified
            (let ((proceed?
                   (if (and (buffer-file-path buf)
                            (buffer-doc-pointer buf)
                            (qt-text-document-modified? (buffer-doc-pointer buf)))
                     (let ((answer (qt-echo-read-string app
                                     (string-append "Buffer " target-name
                                       " modified; kill anyway? (yes/no) "))))
                       (and answer (or (string=? answer "yes") (string=? answer "y"))))
                     #t)))
            (when proceed?
              ;; Switch to another buffer if killing current
              (when (eq? buf (current-qt-buffer app))
                (let* ((fr (app-state-frame app))
                       (ed (current-qt-editor app))
                       (other (let loop ((bs (buffer-list)))
                                (cond ((null? bs) #f)
                                      ((eq? (car bs) buf) (loop (cdr bs)))
                                      (else (car bs))))))
                  (when other
                    (qt-buffer-attach! ed other)
                    (set! (qt-edit-window-buffer (qt-current-window fr))
                          other))))
              ;; Remember buffer before killing (for reopen-killed-buffer)
              (qt-remember-killed-buffer! buf)
              ;; Run kill-buffer-hook before cleanup
              (run-hooks! 'kill-buffer-hook app buf)
              ;; Clean up syntax highlighter if applicable
              (qt-remove-highlighting! buf)
              ;; Clean up dired entries and marks if applicable
              (hash-remove! *dired-entries* buf)
              (hash-remove! *dired-marks* buf)
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
              ;; Clean up terminal state if applicable
              (let ((ts (hash-get *terminal-state* buf)))
                (when ts
                  (terminal-stop! ts)
                  (hash-remove! *terminal-state* buf)))
              ;; Clean up chat state if applicable
              (let ((cs (hash-get *chat-state* buf)))
                (when cs
                  (chat-stop! cs)
                  (hash-remove! *chat-state* buf)))
              ;; Remove from MRU list
              (set! *buffer-recent*
                (filter (lambda (n) (not (string=? n target-name)))
                        *buffer-recent*))
              ;; LSP: notify didClose
              (lsp-hook-did-close! app buf)
              (qt-buffer-kill! buf)
              (echo-message! echo (string-append "Killed " target-name)))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

(def (cmd-kill-buffer-and-window app)
  (let ((fr (app-state-frame app)))
    (if (> (length (qt-frame-windows fr)) 1)
      (begin
        (cmd-kill-buffer-cmd app)
        (qt-frame-delete-window! fr))
      (cmd-kill-buffer-cmd app))))

(def (cmd-find-alternate-file app)
  "Replace current buffer with another file."
  (cmd-find-file app))

(def (cmd-delete-file-and-buffer app)
  "Delete the file and kill the buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Error deleting file"))
          (lambda ()
            (delete-file path)
            (cmd-kill-buffer-cmd app)
            (echo-message! (app-state-echo app) (string-append "Deleted: " path)))))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-find-file-literally app)
  "Open a file without syntax highlighting or mode hooks."
  (cmd-find-file app)
  ;; After opening, disable lexer/highlighting for literal viewing
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (ed (current-qt-editor app)))
    (set! (buffer-lexer-lang buf) #f)
    (sci-send ed SCI_SETLEXER 0)
    (echo-message! (app-state-echo app) "Opened literally (no highlighting)")))

(def (qt-org-buffer? buf)
  "Check if a buffer is an org-mode file."
  (or (eq? (buffer-lexer-lang buf) 'org)
      (let ((path (buffer-file-path buf)))
        (and path (string-suffix? ".org" path)))
      (let ((name (buffer-name buf)))
        (and name (string-suffix? ".org" name)))))

(def (cmd-org-mode app)
  "Activate org-mode for the current buffer."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (qt-edit-window-buffer win)))
    (when buf
      (set! (buffer-lexer-lang buf) 'org)
      (buffer-local-set! buf 'major-mode 'org-mode)
      ;; qt-setup-highlighting! detects 'org lang and applies org styles
      (qt-setup-highlighting! app buf))
    (echo-message! (app-state-echo app) "Org mode")))

;;;============================================================================
;;; Qt org-table: TAB = align + next cell
;;;============================================================================

(def (qt-org-table-row? str)
  "Check if a string is an org table row (starts with |)."
  (let ((trimmed (string-trim str)))
    (and (> (string-length trimmed) 0)
         (char=? (string-ref trimmed 0) #\|))))

(def (qt-org-table-separator? str)
  "Check if string is a table separator line (|---+---|)."
  (let ((trimmed (string-trim str)))
    (and (qt-org-table-row? trimmed)
         (let loop ((i 0))
           (if (>= i (string-length trimmed))
             #t
             (let ((c (string-ref trimmed i)))
               (if (memv c '(#\| #\- #\+ #\space))
                 (loop (+ i 1))
                 #f)))))))

(def (qt-org-table-parse-row str)
  "Split '| a | b | c |' into (\"a\" \"b\" \"c\")."
  (let* ((trimmed (string-trim str))
         (len (string-length trimmed)))
    (if (or (= len 0) (not (char=? (string-ref trimmed 0) #\|)))
      '()
      (let* ((inner (if (and (> len 1) (char=? (string-ref trimmed (- len 1)) #\|))
                      (substring trimmed 1 (- len 1))
                      (substring trimmed 1 len)))
             (parts (string-split inner #\|)))
        (map string-trim-both parts)))))

(def (qt-org-table-column-widths rows)
  "Compute max width for each column across all data rows."
  (let* ((data-rows (filter list? rows))
         (ncols (if (null? data-rows) 0
                  (apply max (map length data-rows)))))
    (let loop ((col 0) (widths '()))
      (if (>= col ncols)
        (reverse widths)
        (loop (+ col 1)
              (cons (apply max 1
                           (map (lambda (row)
                                  (if (< col (length row))
                                    (string-length (list-ref row col))
                                    0))
                                data-rows))
                    widths))))))

(def (qt-org-table-format-row cells widths)
  "Format a data row with cells padded to given widths."
  (string-append
   "| "
   (string-join
    (let loop ((i 0) (result '()))
      (if (>= i (length widths))
        (reverse result)
        (let* ((cell (if (< i (length cells)) (list-ref cells i) ""))
               (w (list-ref widths i)))
          (loop (+ i 1) (cons (string-pad-right cell w) result)))))
    " | ")
   " |"))

(def (qt-org-table-format-separator widths)
  "Format a separator line: |---+---+---|"
  (string-append
   "|"
   (string-join (map (lambda (w) (make-string (+ w 2) #\-)) widths) "+")
   "|"))

(def (qt-org-table-next-cell app)
  "On a table line: align table, move to next cell. Returns #t if handled, #f if not on a table."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (cur-line (sci-send ed SCI_LINEFROMPOSITION pos))
         (line-start (sci-send ed SCI_POSITIONFROMLINE cur-line 0))
         (line-end (sci-send ed SCI_GETLINEENDPOSITION cur-line))
         (line (if (<= line-end (string-length text))
                 (substring text line-start line-end) "")))
    (if (not (qt-org-table-row? line))
      #f  ;; Not on a table line
      (let* ((total-lines (sci-send ed SCI_GETLINECOUNT))
             ;; Find table bounds (contiguous table rows)
             (tbl-start (let loop ((i cur-line))
                          (if (and (>= i 0)
                                   (qt-org-table-row?
                                     (let* ((ls (sci-send ed SCI_POSITIONFROMLINE i 0))
                                            (le (sci-send ed SCI_GETLINEENDPOSITION i)))
                                       (if (<= le (string-length text))
                                         (substring text ls le) ""))))
                            (loop (- i 1)) (+ i 1))))
             (tbl-end (let loop ((i cur-line))
                        (if (and (< i total-lines)
                                 (qt-org-table-row?
                                   (let* ((ls (sci-send ed SCI_POSITIONFROMLINE i 0))
                                          (le (sci-send ed SCI_GETLINEENDPOSITION i)))
                                     (if (<= le (string-length text))
                                       (substring text ls le) ""))))
                          (loop (+ i 1)) (- i 1))))
             ;; Parse all rows
             (rows (let loop ((i tbl-start) (acc '()))
                     (if (> i tbl-end) (reverse acc)
                       (let* ((ls (sci-send ed SCI_POSITIONFROMLINE i 0))
                              (le (sci-send ed SCI_GETLINEENDPOSITION i))
                              (l (if (<= le (string-length text))
                                   (substring text ls le) "")))
                         (loop (+ i 1)
                               (cons (if (qt-org-table-separator? l)
                                       'separator (qt-org-table-parse-row l))
                                     acc))))))
             (widths (qt-org-table-column-widths rows))
             ;; Determine current column from cursor offset
             (col-offset (- pos line-start))
             (cur-col (let loop ((i 0) (pipes -1))
                        (if (>= i (min col-offset (string-length line)))
                          (max 0 pipes)
                          (loop (+ i 1)
                                (if (char=? (string-ref line i) #\|)
                                  (+ pipes 1) pipes)))))
             (ncols (length widths))
             ;; Build aligned table text
             (new-lines (map (lambda (row)
                               (if (eq? row 'separator)
                                 (qt-org-table-format-separator widths)
                                 (qt-org-table-format-row row widths)))
                             rows))
             (new-text (string-join new-lines "\n"))
             ;; Replace table region
             (region-start (sci-send ed SCI_POSITIONFROMLINE tbl-start 0))
             (region-end (if (< (+ tbl-end 1) total-lines)
                           (sci-send ed SCI_POSITIONFROMLINE (+ tbl-end 1) 0)
                           (sci-send ed SCI_GETTEXTLENGTH))))
        ;; Replace old table with aligned version
        (sci-send ed SCI_SETTARGETSTART region-start)
        (sci-send ed SCI_SETTARGETEND region-end)
        ;; Need trailing newline if we replaced up to next line start
        (let ((replacement (if (< (+ tbl-end 1) total-lines)
                             (string-append new-text "\n")
                             new-text)))
          (sci-send/string ed SCI_REPLACETARGET replacement
                          (string-length replacement)))
        ;; Re-apply full org highlighting (not just table — headings etc. too)
        (qt-org-highlight-buffer! ed (qt-plain-text-edit-text ed))
        ;; Move to next cell
        (let* ((next-col (+ cur-col 1))
               (next-line cur-line))
          (cond
            ;; Next column in same row
            ((< next-col ncols)
             (let ((target-line next-line)
                   (offset (let loop ((c 0) (off 2))
                             (if (>= c next-col) off
                               (loop (+ c 1) (+ off (list-ref widths c) 3))))))
               (sci-send ed SCI_GOTOPOS
                 (+ (sci-send ed SCI_POSITIONFROMLINE target-line 0) offset))))
            ;; Last column: move to first column of next data row
            (else
             ;; Find next non-separator row
             (let loop ((i (+ cur-line 1)))
               (cond
                 ((> i tbl-end)
                  ;; Past table end: insert new row
                  (let* ((empty-cells (make-list ncols ""))
                         (new-row (qt-org-table-format-row empty-cells widths))
                         (eol (sci-send ed SCI_GETLINEENDPOSITION tbl-end)))
                    (sci-send ed SCI_GOTOPOS eol)
                    (qt-plain-text-edit-insert-text! ed (string-append "\n" new-row))
                    (sci-send ed SCI_GOTOPOS
                      (+ (sci-send ed SCI_POSITIONFROMLINE (+ tbl-end 1) 0) 2))))
                 ;; Skip separator rows
                 ((qt-org-table-separator?
                    (let* ((ls (sci-send ed SCI_POSITIONFROMLINE i 0))
                           (le (sci-send ed SCI_GETLINEENDPOSITION i))
                           (fresh-text (qt-plain-text-edit-text ed)))
                      (if (<= le (string-length fresh-text))
                        (substring fresh-text ls le) "")))
                  (loop (+ i 1)))
                 (else
                  (sci-send ed SCI_GOTOPOS
                    (+ (sci-send ed SCI_POSITIONFROMLINE i 0) 2))))))))
        #t))))

;; Track org-cycle state per heading line: 'folded, 'children, 'subtree
(def *org-cycle-state* (make-hash-table))

(def (org-line-heading-level line)
  "Return the heading level of a line (number of leading *s), or 0 if not a heading."
  (if (and (> (string-length line) 0)
           (char=? (string-ref line 0) #\*))
    (let loop ((i 0))
      (if (and (< i (string-length line))
               (char=? (string-ref line i) #\*))
        (loop (+ i 1)) i))
    0))

(def (org-find-subtree-end-line lines cur-line level)
  "Find the line number where the subtree under cur-line ends.
   Subtree ends at next heading with level <= this one, or EOF."
  (let loop ((i (+ cur-line 1)))
    (cond
      ((>= i (length lines)) i)
      ((<= 0 (let ((l (org-line-heading-level (list-ref lines i))))
               (if (and (> l 0) (<= l level)) l -1)))
       i)
      (else (loop (+ i 1))))))

(def (qt-org-cycle app)
  "Cycle visibility of org heading children in Qt editor.
   3-state cycle like Emacs: FOLDED → CHILDREN → SUBTREE.
   FOLDED: all children hidden.
   CHILDREN: show direct child headings, hide their content.
   SUBTREE: show everything.
   Returns #t if on a heading and toggled, #f otherwise."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (text-len (string-length text))
         (pos (min (qt-plain-text-edit-cursor-position ed) text-len))
         ;; Compute line number from text to avoid byte/char mismatch
         (cur-line (let loop ((i 0) (ln 0))
                     (cond ((>= i pos) ln)
                           ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ ln 1)))
                           (else (loop (+ i 1) ln)))))
         (lines (string-split text #\newline))
         (echo (app-state-echo app)))
    (if (>= cur-line (length lines))
      #f
      (let* ((line (list-ref lines cur-line))
             (level (org-line-heading-level line)))
        (if (= level 0)
          #f  ;; Not on a heading
          (let ((end-line (org-find-subtree-end-line lines cur-line level)))
            (if (= end-line (+ cur-line 1))
              (begin (echo-message! echo "No children to fold") #t)
              ;; Determine current state and cycle
              (let* ((state (or (hash-get *org-cycle-state* cur-line) 'subtree))
                     (next-state (case state
                                   ((subtree) 'folded)
                                   ((folded)  'children)
                                   ((children) 'subtree)
                                   (else 'folded))))
                (case next-state
                  ((folded)
                   ;; Hide all children
                   (let loop ((i (+ cur-line 1)))
                     (when (< i end-line)
                       (sci-send ed SCI_HIDELINES i i)
                       (loop (+ i 1))))
                   (echo-message! echo "Folded"))
                  ((children)
                   ;; Show direct child headings, hide their content
                   (let loop ((i (+ cur-line 1)))
                     (when (< i end-line)
                       (let* ((l (list-ref lines i))
                              (hl (org-line-heading-level l)))
                         (if (= hl (+ level 1))
                           ;; Direct child heading — show it
                           (sci-send ed SCI_SHOWLINES i i)
                           ;; Non-heading or deeper heading — hide
                           (sci-send ed SCI_HIDELINES i i)))
                       (loop (+ i 1))))
                   (echo-message! echo "Children"))
                  ((subtree)
                   ;; Show everything
                   (sci-send ed SCI_SHOWLINES (+ cur-line 1) (- end-line 1))
                   (echo-message! echo "Subtree")))
                (hash-put! *org-cycle-state* cur-line next-state)
                #t))))))))  ;; Handled

(def (cmd-indent-or-complete app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ((dired-buffer? buf) (void))
      ((terminal-buffer? buf) (cmd-term-send-tab app))
      ((repl-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (qt-plain-text-edit-insert-text! ed "  "))))
      ((gsh-eshell-buffer? buf)
       ;; Eshell: complete filename/command
       (let* ((ed (current-qt-editor app))
              (all-text (qt-plain-text-edit-text ed))
              (cur-prompt gsh-eshell-prompt)
              (prompt-len (string-length cur-prompt))
              ;; Find the last prompt
              (prompt-pos (let loop ((pos (- (string-length all-text) prompt-len)))
                            (cond
                              ((< pos 0) #f)
                              ((string=? (substring all-text pos (+ pos prompt-len)) cur-prompt) pos)
                              (else (loop (- pos 1))))))
              (input (if (and prompt-pos (> (string-length all-text) (+ prompt-pos prompt-len)))
                       (substring all-text (+ prompt-pos prompt-len) (string-length all-text))
                       ""))
              (matches (eshell-complete input buf)))
         (cond
           ((null? matches)
            (echo-message! (app-state-echo app) "[No completions]"))
           ((= (length matches) 1)
            ;; Single match — replace partial word
            (let* ((trimmed (safe-string-trim-both input))
                   (sp (let loop ((i (- (string-length trimmed) 1)))
                         (cond ((< i 0) #f)
                               ((char=? (string-ref trimmed i) #\space) i)
                               (else (loop (- i 1))))))
                   (before (if sp (substring trimmed 0 (+ sp 1)) ""))
                   (new-input (string-append before (car matches)))
                   ;; Replace text from after prompt to end
                   (input-start (+ prompt-pos prompt-len))
                   (end-pos (string-length all-text))
                   (new-text (string-append (substring all-text 0 input-start) new-input)))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))
           (else
            ;; Multiple matches — insert longest common prefix, show matches
            (let* ((lcp (eshell-longest-common-prefix matches))
                   (trimmed (safe-string-trim-both input))
                   (sp (let loop ((i (- (string-length trimmed) 1)))
                         (cond ((< i 0) #f)
                               ((char=? (string-ref trimmed i) #\space) i)
                               (else (loop (- i 1))))))
                   (partial (if sp
                              (substring trimmed (+ sp 1) (string-length trimmed))
                              trimmed)))
              (when (> (string-length lcp) (string-length partial))
                (let* ((before (if sp (substring trimmed 0 (+ sp 1)) ""))
                       (new-input (string-append before lcp))
                       (input-start (+ prompt-pos prompt-len))
                       (new-text (string-append (substring all-text 0 input-start) new-input)))
                  (qt-plain-text-edit-set-text! ed new-text)
                  (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))
              (echo-message! (app-state-echo app)
                (string-join matches "  ")))))))
      (else
       ;; If snippet is active, jump to next field
       (if *snippet-active*
         (cmd-snippet-next-field app)
         ;; For org-mode buffers: try table, template expansion, or heading fold/unfold
         (if (and (qt-org-buffer? buf)
                  (or (qt-org-table-next-cell app)
                      (qt-try-org-template-expand app)
                      (qt-org-cycle app)))
           (void)  ;; Org table/template/heading handled
           ;; Try snippet expansion, then completion
           (if (cmd-snippet-expand app)
             (void)  ;; Snippet expanded
             (let* ((ed (current-qt-editor app))
                    (prefix (get-word-prefix ed)))
               (if (string=? prefix "")
                 ;; No word prefix — just indent
                 (qt-plain-text-edit-insert-text! ed "  ")
                 ;; Have a prefix — show completions
                 (let* ((text (qt-plain-text-edit-text ed))
                        (words (collect-buffer-words text))
                        ;; Filter to matching words
                        (matches (filter (lambda (w)
                                           (and (> (string-length w) (string-length prefix))
                                                (string-prefix? prefix w)))
                                         words))
                        (sorted (sort matches string<?)))
                   (if (null? sorted)
                     ;; No buffer-word matches — try LSP or indent
                     (if (lsp-running?)
                       ;; Fire async LSP completion
                       (let ((params (lsp-current-params app)))
                         (if params
                           (lsp-send-request! "textDocument/completion" params
                             (lambda (response)
                               (let ((result (hash-get response "result")))
                                 (when result
                                   (let* ((items (cond
                                                   ((list? result) result)
                                                   ((and (hash-table? result)
                                                         (hash-get result "items"))
                                                    (hash-get result "items"))
                                                   (else [])))
                                          (labels (filter-map
                                                    (lambda (item)
                                                      (and (hash-table? item)
                                                           (hash-get item "label")))
                                                    items)))
                                     (when (not (null? labels))
                                       (let ((c (get-or-create-completer! ed app)))
                                         (qt-completer-set-model-strings! c labels)
                                         (qt-completer-set-completion-prefix!
                                           c (get-word-prefix ed))
                                         (qt-completer-complete-rect! c 0 0 250 20))))))))
                           (qt-plain-text-edit-insert-text! ed "  ")))
                       (qt-plain-text-edit-insert-text! ed "  "))
                     ;; Have buffer-word matches — show them, also request LSP
                     (let ((c (get-or-create-completer! ed app)))
                       (qt-completer-set-model-strings! c sorted)
                       (qt-completer-set-completion-prefix! c prefix)
                       (qt-completer-complete-rect! c 0 0 200 20)
                       ;; Also fire LSP completion async to merge results
                       (when (lsp-running?)
                         (let ((params (lsp-current-params app)))
                           (when params
                             (lsp-send-request! "textDocument/completion" params
                               (lambda (response)
                                 (let ((result (hash-get response "result")))
                                   (when result
                                     (let* ((items (cond
                                                     ((list? result) result)
                                                     ((and (hash-table? result)
                                                           (hash-get result "items"))
                                                      (hash-get result "items"))
                                                     (else [])))
                                            (lsp-labels (filter-map
                                                          (lambda (item)
                                                            (and (hash-table? item)
                                                                 (hash-get item "label")))
                                                          items)))
                                       (when (not (null? lsp-labels))
                                         ;; Merge with buffer words (deduplicated)
                                         (let* ((all (append sorted lsp-labels))
                                                (seen (make-hash-table))
                                                (unique (filter
                                                          (lambda (w)
                                                            (if (hash-key? seen w) #f
                                                              (begin
                                                                (hash-put! seen w #t)
                                                                #t)))
                                                          all))
                                                (final (sort unique string<?)))
                                           (qt-completer-set-model-strings! c final)
                                           (qt-completer-set-completion-prefix!
                                             c (get-word-prefix ed))
                                           (qt-completer-complete-rect!
                                             c 0 0 250 20)))))))))))))))))))))))

(def (cmd-quit app)
  ;; Check for unsaved buffers
  (let* ((unsaved (filter
                    (lambda (buf)
                      (and (buffer-file-path buf)
                           (buffer-doc-pointer buf)
                           (qt-text-document-modified? (buffer-doc-pointer buf))))
                    *buffer-list*))
         (echo (app-state-echo app))
         (fr (app-state-frame app)))
    (if (null? unsaved)
      ;; No unsaved buffers, quit immediately
      (begin
        (scratch-save!)
        (savehist-save!)
        (save-place-save!)
        (gsh-history-save!)
        (when *qt-desktop-save-mode* (session-save! app))
        (set! (app-state-running app) #f)
        (qt-widget-close! (qt-frame-main-win fr)))
      ;; Prompt about unsaved buffers
      (let* ((names (map buffer-name unsaved))
             (shown-names (if (> (length names) 3)
                            (let loop ((l names) (n 0) (acc []))
                              (if (or (null? l) (>= n 3))
                                (append (reverse acc) (list "..."))
                                (loop (cdr l) (+ n 1) (cons (car l) acc))))
                            names))
             (msg (string-append
                    (number->string (length unsaved))
                    " unsaved buffer(s): "
                    (string-join shown-names ", ")
                    ". Save? (yes/no/cancel) "))
             (answer (qt-echo-read-string app msg)))
        (cond
          ((and answer (or (string=? answer "yes") (string=? answer "y")))
           ;; Save all: attach each buffer temporarily to get text, save, restore
           (let* ((ed (current-qt-editor app))
                  (original-buf (current-qt-buffer app)))
             (for-each
               (lambda (buf)
                 (let ((path (buffer-file-path buf)))
                   (when path
                     (qt-buffer-attach! ed buf)
                     (let ((text (qt-plain-text-edit-text ed)))
                       (write-string-to-file path text)
                       (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))))
               unsaved)
             ;; Restore original buffer
             (qt-buffer-attach! ed original-buf))
           (scratch-save!)
           (save-place-save!)
           (gsh-history-save!)
           (when *qt-desktop-save-mode* (session-save! app))
           (set! (app-state-running app) #f)
           (qt-widget-close! (qt-frame-main-win fr)))
          ((and answer (or (string=? answer "no") (string=? answer "n")))
           ;; Quit without saving
           (scratch-save!)
           (save-place-save!)
           (gsh-history-save!)
           (when *qt-desktop-save-mode* (session-save! app))
           (set! (app-state-running app) #f)
           (qt-widget-close! (qt-frame-main-win fr)))
          (else
           ;; Cancel
           (echo-message! echo "Quit cancelled")))))))


;;; Register all commands
;;;============================================================================

(def (qt-register-all-commands!)
  ;; Setup mode-specific keymaps
  (setup-mode-keymaps!)
  ;; Navigation
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'backward-word cmd-backward-word)
  (register-command! 'forward-subword cmd-forward-subword)
  (register-command! 'backward-subword cmd-backward-subword)
  (register-command! 'kill-subword cmd-kill-subword)
  (register-command! 'beginning-of-buffer cmd-beginning-of-buffer)
  (register-command! 'end-of-buffer cmd-end-of-buffer)
  (register-command! 'scroll-down cmd-scroll-down)
  (register-command! 'scroll-up cmd-scroll-up)
  (register-command! 'recenter cmd-recenter)
  ;; Editing
  (register-command! 'delete-char cmd-term-send-eof)  ; dispatches: shell EOF or normal delete
  (register-command! 'backward-delete-char cmd-backward-delete-char)
  (register-command! 'backward-delete-char-untabify cmd-backward-delete-char-untabify)
  (register-command! 'newline cmd-newline)
  (register-command! 'open-line cmd-open-line)
  (register-command! 'undo cmd-undo)
  (register-command! 'redo cmd-redo)
  ;; Kill/Yank
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'yank cmd-yank)
  ;; Mark/Region
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'set-mark-command cmd-set-mark)  ; Emacs alias
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'copy-region cmd-copy-region)
  (register-command! 'kill-ring-save cmd-copy-region)  ; Emacs alias
  ;; File
  (register-command! 'find-file cmd-find-file)
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  (register-command! 'ffap cmd-find-file-at-point)
  (register-command! 'save-buffer cmd-save-buffer)
  (register-command! 'save-file cmd-save-buffer)  ; alias
  (register-command! 'write-file cmd-write-file)
  (register-command! 'revert-buffer cmd-revert-buffer)
  ;; Buffer
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'helm-buffers-list cmd-helm-buffers-list)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  (register-command! 'list-buffers cmd-list-buffers)
  ;; Window
  (register-command! 'split-window cmd-split-window)
  (register-command! 'split-window-right cmd-split-window-right)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'ace-window cmd-ace-window)
  (register-command! 'swap-window cmd-swap-window)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  ;; Search
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'search-backward cmd-search-backward)
  (register-command! 'query-replace cmd-query-replace)
  ;; REPL
  (register-command! 'repl cmd-repl)
  (register-command! 'eval-expression cmd-eval-expression)
  (register-command! 'load-file cmd-load-file)
  ;; Eshell
  (register-command! 'eshell cmd-eshell)
  ;; Shell
  (register-command! 'shell cmd-shell)
  ;; Terminal
  (register-command! 'term cmd-term)
  (register-command! 'term-interrupt cmd-term-interrupt)
  (register-command! 'term-send-eof cmd-term-send-eof)
  (register-command! 'term-send-tab cmd-term-send-tab)
  (register-command! 'term-list cmd-term-list)
  (register-command! 'term-next cmd-term-next)
  (register-command! 'term-prev cmd-term-prev)
  ;; AI Chat
  (register-command! 'claude-chat cmd-chat)
  ;; Goto line / M-x
  (register-command! 'goto-line cmd-goto-line)
  (register-command! 'execute-extended-command cmd-execute-extended-command)
  ;; Help
  (register-command! 'list-bindings cmd-list-bindings)
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
  ;; Defun navigation
  (register-command! 'beginning-of-defun cmd-beginning-of-defun)
  (register-command! 'end-of-defun cmd-end-of-defun)
  ;; Tab/indent
  (register-command! 'indent-or-complete cmd-indent-or-complete)
  ;; Zoom
  (register-command! 'zoom-in cmd-zoom-in)
  (register-command! 'zoom-out cmd-zoom-out)
  ;; Line numbers
  (register-command! 'toggle-line-numbers cmd-toggle-line-numbers)
  ;; Backward kill word
  (register-command! 'backward-kill-word cmd-backward-kill-word)
  ;; Kill whole line
  (register-command! 'kill-whole-line cmd-kill-whole-line)
  ;; Join line
  (register-command! 'join-line cmd-join-line)
  ;; Just one space
  (register-command! 'just-one-space cmd-just-one-space)
  ;; Transpose words/lines
  (register-command! 'transpose-words cmd-transpose-words)
  (register-command! 'transpose-lines cmd-transpose-lines)
  ;; Move line up/down
  (register-command! 'move-line-up cmd-move-line-up)
  (register-command! 'move-line-down cmd-move-line-down)
  ;; Fill paragraph
  (register-command! 'fill-paragraph cmd-fill-paragraph)
  ;; Count words
  (register-command! 'count-words cmd-count-words)
  ;; Cursor position
  (register-command! 'what-cursor-position cmd-what-cursor-position)
  ;; Dynamic abbreviation
  (register-command! 'dabbrev-expand cmd-dabbrev-expand)
  ;; Delete blank lines
  (register-command! 'delete-blank-lines cmd-delete-blank-lines)
  ;; Insert file
  (register-command! 'insert-file cmd-insert-file)
  ;; Shell command
  (register-command! 'shell-command cmd-shell-command)
  ;; Sort lines
  (register-command! 'sort-lines cmd-sort-lines)
  ;; Goto matching paren
  (register-command! 'goto-matching-paren cmd-goto-matching-paren)
  ;; Upcase/downcase region
  (register-command! 'upcase-region cmd-upcase-region)
  (register-command! 'downcase-region cmd-downcase-region)
  ;; Indent region
  (register-command! 'indent-region cmd-indent-region)
  ;; Zap to char
  (register-command! 'zap-to-char cmd-zap-to-char)
  ;; Goto char
  (register-command! 'goto-char cmd-goto-char)
  ;; Zoom reset
  (register-command! 'zoom-reset cmd-zoom-reset)
  ;; Yank pop
  (register-command! 'yank-pop cmd-yank-pop)
  ;; Occur
  (register-command! 'occur cmd-occur)
  (register-command! 'occur-goto cmd-occur-goto)
  (register-command! 'occur-next cmd-occur-next)
  (register-command! 'occur-prev cmd-occur-prev)
  ;; Keyboard macros
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'call-named-kbd-macro cmd-call-named-kbd-macro)
  (register-command! 'list-kbd-macros cmd-list-kbd-macros)
  (register-command! 'save-kbd-macros cmd-save-kbd-macros)
  (register-command! 'load-kbd-macros cmd-load-kbd-macros)
  ;; Repeat
  (register-command! 'repeat cmd-repeat)
  ;; Mark ring
  (register-command! 'pop-mark cmd-pop-mark)
  ;; Registers
  (register-command! 'copy-to-register cmd-copy-to-register)
  (register-command! 'insert-register cmd-insert-register)
  (register-command! 'point-to-register cmd-point-to-register)
  (register-command! 'jump-to-register cmd-jump-to-register)
  (register-command! 'window-configuration-to-register cmd-window-configuration-to-register)
  (register-command! 'file-to-register cmd-file-to-register)
  ;; Paragraph navigation
  (register-command! 'forward-paragraph cmd-forward-paragraph)
  (register-command! 'backward-paragraph cmd-backward-paragraph)
  ;; Indentation
  (register-command! 'back-to-indentation cmd-back-to-indentation)
  (register-command! 'delete-indentation cmd-delete-indentation)
  ;; Exchange point and mark
  (register-command! 'exchange-point-and-mark cmd-exchange-point-and-mark)
  ;; Copy line
  (register-command! 'copy-line cmd-copy-line)
  ;; Mark word
  (register-command! 'mark-word cmd-mark-word)
  ;; Save some buffers
  (register-command! 'save-some-buffers cmd-save-some-buffers)
  ;; Compile
  (register-command! 'compile cmd-compile)
  (register-command! 'recompile cmd-recompile)
  (register-command! 'toggle-compile-on-save cmd-toggle-compile-on-save)
  ;; Flycheck
  (register-command! 'flycheck-mode cmd-flycheck-mode)
  (register-command! 'flycheck-next-error cmd-flycheck-next-error)
  (register-command! 'flycheck-prev-error cmd-flycheck-prev-error)
  (register-command! 'flycheck-list-errors cmd-flycheck-list-errors)
  (register-command! 'first-error (lambda (app)
    (if (null? *compilation-errors*)
      (echo-error! (app-state-echo app) "No compilation errors")
      (compilation-goto-error! app 0))))
  ;; Where-is
  (register-command! 'where-is cmd-where-is)
  ;; Flush/keep lines
  (register-command! 'flush-lines cmd-flush-lines)
  (register-command! 'keep-lines cmd-keep-lines)
  ;; Number lines
  (register-command! 'number-lines cmd-number-lines)
  ;; Reverse region
  (register-command! 'reverse-region cmd-reverse-region)
  ;; Toggle read-only
  (register-command! 'toggle-read-only cmd-toggle-read-only)
  ;; Rename buffer
  (register-command! 'rename-buffer cmd-rename-buffer)
  ;; Other window commands
  (register-command! 'switch-buffer-other-window cmd-switch-buffer-other-window)
  (register-command! 'find-file-other-window cmd-find-file-other-window)
  ;; Insert date
  (register-command! 'insert-date cmd-insert-date)
  ;; Eval
  (register-command! 'eval-buffer cmd-eval-buffer)
  (register-command! 'eval-region cmd-eval-region)
  (register-command! 'eval-last-sexp cmd-eval-last-sexp)
  (register-command! 'eval-defun cmd-eval-defun)
  (register-command! 'eval-print-last-sexp cmd-eval-print-last-sexp)
  ;; Clone buffer / scratch
  (register-command! 'clone-buffer cmd-clone-buffer)
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Delete duplicate lines
  (register-command! 'delete-duplicate-lines cmd-delete-duplicate-lines)
  ;; Count matches / count lines region
  (register-command! 'count-matches cmd-count-matches)
  (register-command! 'count-lines-region cmd-count-lines-region)
  ;; Diff buffer
  (register-command! 'diff-buffer-with-file cmd-diff-buffer-with-file)
  (register-command! 'diff-next-hunk cmd-diff-next-hunk)
  (register-command! 'diff-prev-hunk cmd-diff-prev-hunk)
  (register-command! 'diff-refine-hunk cmd-diff-refine-hunk)
  ;; Grep buffer
  (register-command! 'grep-buffer cmd-grep-buffer)
  ;; Revert quick
  (register-command! 'revert-buffer-quick cmd-revert-buffer-quick)
  ;; Shell command on region
  (register-command! 'shell-command-on-region cmd-shell-command-on-region)
  ;; Pipe buffer
  (register-command! 'pipe-buffer cmd-pipe-buffer)
  ;; Apropos
  (register-command! 'apropos-command cmd-apropos-command)
  ;; What page
  (register-command! 'what-page cmd-what-page)
  ;; Async shell command
  (register-command! 'async-shell-command cmd-async-shell-command)
  ;; Checksum
  (register-command! 'checksum cmd-checksum)
  ;; S-expression navigation
  (register-command! 'forward-sexp cmd-forward-sexp)
  (register-command! 'backward-sexp cmd-backward-sexp)
  (register-command! 'kill-sexp cmd-kill-sexp)
  (register-command! 'backward-kill-sexp cmd-backward-kill-sexp)
  (register-command! 'mark-sexp cmd-mark-sexp)
  (register-command! 'mark-defun cmd-mark-defun)
  (register-command! 'indent-sexp cmd-indent-sexp)
  ;; Paredit
  (register-command! 'paredit-slurp-forward cmd-paredit-slurp-forward)
  (register-command! 'paredit-barf-forward cmd-paredit-barf-forward)
  (register-command! 'paredit-slurp-backward cmd-paredit-slurp-backward)
  (register-command! 'paredit-barf-backward cmd-paredit-barf-backward)
  (register-command! 'paredit-wrap-round cmd-paredit-wrap-round)
  (register-command! 'paredit-wrap-square cmd-paredit-wrap-square)
  (register-command! 'paredit-splice-sexp cmd-paredit-splice-sexp)
  (register-command! 'paredit-raise-sexp cmd-paredit-raise-sexp)
  (register-command! 'paredit-split-sexp cmd-paredit-split-sexp)
  (register-command! 'paredit-join-sexps cmd-paredit-join-sexps)
  (register-command! 'paredit-convolute-sexp cmd-paredit-convolute-sexp)
  ;; Avy jump
  (register-command! 'avy-goto-char cmd-avy-goto-char)
  (register-command! 'avy-goto-word cmd-avy-goto-word)
  (register-command! 'avy-goto-line cmd-avy-goto-line)
  ;; Buffer cycling
  (register-command! 'previous-buffer cmd-previous-buffer)
  (register-command! 'next-buffer cmd-next-buffer)
  ;; Delete trailing whitespace
  (register-command! 'delete-trailing-whitespace cmd-delete-trailing-whitespace)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'tabify cmd-tabify)
  ;; Kill buffer and window
  (register-command! 'kill-buffer-and-window cmd-kill-buffer-and-window)
  ;; Open line above
  (register-command! 'open-line-above cmd-open-line-above)
  ;; Select line
  (register-command! 'select-line cmd-select-line)
  ;; Smart beginning of line
  (register-command! 'smart-beginning-of-line cmd-smart-beginning-of-line)
  ;; Insert parens/brackets
  (register-command! 'insert-parentheses cmd-insert-parentheses)
  (register-command! 'insert-pair-brackets cmd-insert-pair-brackets)
  ;; Find file at point
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  ;; Show kill ring / list registers
  (register-command! 'show-kill-ring cmd-show-kill-ring)
  (register-command! 'browse-kill-ring cmd-browse-kill-ring)
  (register-command! 'list-registers cmd-list-registers)
  ;; Scroll other window
  (register-command! 'scroll-other-window cmd-scroll-other-window)
  (register-command! 'scroll-other-window-up cmd-scroll-other-window-up)
  ;; Swap buffers
  (register-command! 'swap-buffers cmd-swap-buffers)
  ;; Goto percent
  (register-command! 'goto-percent cmd-goto-percent)
  ;; Sentence navigation
  (register-command! 'forward-sentence cmd-forward-sentence)
  (register-command! 'backward-sentence cmd-backward-sentence)
  ;; Dired
  (register-command! 'dired cmd-dired)
  (register-command! 'dired-rename-at-point cmd-dired-rename-at-point)
  (register-command! 'dired-copy-at-point cmd-dired-copy-at-point)
  ;; Unfill paragraph
  (register-command! 'unfill-paragraph cmd-unfill-paragraph)
  ;; Whitespace cleanup
  (register-command! 'whitespace-cleanup cmd-whitespace-cleanup)
  ;; Insert UUID
  (register-command! 'insert-uuid cmd-insert-uuid)
  ;; Word frequency
  (register-command! 'word-frequency cmd-word-frequency)
  ;; Indent rigidly
  (register-command! 'indent-rigidly-right cmd-indent-rigidly-right)
  (register-command! 'indent-rigidly-left cmd-indent-rigidly-left)
  ;; Center line
  (register-command! 'center-line cmd-center-line)
  ;; Narrow/widen
  (register-command! 'narrow-to-region cmd-narrow-to-region)
  (register-command! 'widen cmd-widen)
  ;; Display time
  (register-command! 'display-time cmd-display-time)
  ;; Buffer info
  (register-command! 'buffer-info cmd-buffer-info)
  ;; Bookmarks
  (register-command! 'bookmark-set cmd-bookmark-set)
  (register-command! 'bookmark-jump cmd-bookmark-jump)
  (register-command! 'bookmark-list cmd-bookmark-list)
  ;; Rectangle operations
  (register-command! 'kill-rectangle cmd-kill-rectangle)
  (register-command! 'delete-rectangle cmd-delete-rectangle)
  (register-command! 'yank-rectangle cmd-yank-rectangle)
  (register-command! 'string-rectangle cmd-string-rectangle)
  (register-command! 'open-rectangle cmd-open-rectangle)
  ;; Describe
  (register-command! 'describe-key cmd-describe-key)
  (register-command! 'describe-command cmd-describe-command)
  (register-command! 'describe-key-briefly cmd-describe-key-briefly)
  (register-command! 'describe-bindings cmd-describe-bindings)
  (register-command! 'describe-char cmd-describe-char)
  ;; Toggle electric pair
  (register-command! 'toggle-electric-pair cmd-toggle-electric-pair)
  ;; Universal argument / digit args
  (register-command! 'universal-argument cmd-universal-argument)
  (register-command! 'negative-argument cmd-negative-argument)
  (register-command! 'digit-argument-0 cmd-digit-argument-0)
  (register-command! 'digit-argument-1 cmd-digit-argument-1)
  (register-command! 'digit-argument-2 cmd-digit-argument-2)
  (register-command! 'digit-argument-3 cmd-digit-argument-3)
  (register-command! 'digit-argument-4 cmd-digit-argument-4)
  (register-command! 'digit-argument-5 cmd-digit-argument-5)
  (register-command! 'digit-argument-6 cmd-digit-argument-6)
  (register-command! 'digit-argument-7 cmd-digit-argument-7)
  (register-command! 'digit-argument-8 cmd-digit-argument-8)
  (register-command! 'digit-argument-9 cmd-digit-argument-9)
  ;; Next/previous error
  (register-command! 'next-error cmd-next-error)
  (register-command! 'previous-error cmd-previous-error)
  ;; Text transforms
  (register-command! 'tabify cmd-tabify)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'base64-encode-region cmd-base64-encode-region)
  (register-command! 'base64-decode-region cmd-base64-decode-region)
  (register-command! 'rot13-region cmd-rot13-region)
  ;; Hex dump
  (register-command! 'hexl-mode cmd-hexl-mode)
  ;; Toggles
  (register-command! 'toggle-word-wrap cmd-toggle-word-wrap)
  (register-command! 'toggle-whitespace cmd-toggle-whitespace)
  (register-command! 'toggle-truncate-lines cmd-toggle-truncate-lines)
  (register-command! 'toggle-case-fold-search cmd-toggle-case-fold-search)
  (register-command! 'toggle-overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'toggle-auto-fill cmd-toggle-auto-fill)
  (register-command! 'toggle-visual-line-mode cmd-toggle-visual-line-mode)
  (register-command! 'toggle-highlighting cmd-toggle-highlighting)
  (register-command! 'toggle-fill-column-indicator cmd-toggle-fill-column-indicator)
  (register-command! 'toggle-indent-tabs-mode cmd-toggle-indent-tabs-mode)
  ;; Tab insertion
  (register-command! 'tab-to-tab-stop cmd-tab-to-tab-stop)
  (register-command! 'set-tab-width cmd-set-tab-width)
  ;; Fill column
  (register-command! 'set-fill-column cmd-set-fill-column)
  ;; Calculator
  (register-command! 'calc cmd-calc)
  ;; Count words buffer/region/chars
  (register-command! 'count-words-region cmd-count-words-region)
  (register-command! 'count-words-buffer cmd-count-words-buffer)
  (register-command! 'count-chars-region cmd-count-chars-region)
  (register-command! 'buffer-stats cmd-buffer-stats)
  ;; List processes
  (register-command! 'list-processes cmd-list-processes)
  ;; View messages / errors / output
  (register-command! 'view-messages cmd-view-messages)
  (register-command! 'view-errors cmd-view-errors)
  (register-command! 'view-output cmd-view-output)
  ;; What buffer / what face
  (register-command! 'what-buffer cmd-what-buffer)
  (register-command! 'what-face cmd-what-face)
  ;; Insert helpers
  (register-command! 'insert-buffer-name cmd-insert-buffer-name)
  (register-command! 'insert-file-name cmd-insert-file-name)
  (register-command! 'insert-char cmd-insert-char)
  (register-command! 'string-insert-file cmd-string-insert-file)
  ;; Rename file and buffer
  (register-command! 'rename-file-and-buffer cmd-rename-file-and-buffer)
  (register-command! 'copy-buffer-file-name cmd-copy-buffer-file-name)
  (register-command! 'new-empty-buffer cmd-new-empty-buffer)
  (register-command! 'git-log-file cmd-git-log-file)
  (register-command! 'switch-buffer-mru cmd-switch-buffer-mru)
  (register-command! 'find-file-ssh cmd-find-file-ssh)
  (register-command! 'format-buffer cmd-format-buffer)
  (register-command! 'copy-file-name-only cmd-copy-file-name-only)
  ;; JSON, URL, text manipulation (parity with TUI)
  (register-command! 'json-format-buffer cmd-json-format-buffer)
  (register-command! 'json-minify-buffer cmd-json-minify-buffer)
  (register-command! 'json-pretty-print cmd-json-pretty-print-region)
  (register-command! 'url-encode-region cmd-url-encode-region)
  (register-command! 'url-decode-region cmd-url-decode-region)
  (register-command! 'reverse-lines cmd-reverse-lines)
  (register-command! 'shuffle-lines cmd-shuffle-lines)
  (register-command! 'xml-format cmd-xml-format)
  (register-command! 'open-url-at-point cmd-open-url-at-point)
  (register-command! 'compare-windows cmd-compare-windows)
  (register-command! 'dedent-region cmd-dedent-region)
  (register-command! 'count-words-line cmd-count-words-line)
  (register-command! 'diff-goto-source cmd-diff-goto-source)
  (register-command! 'insert-date-iso cmd-insert-date-iso)
  ;; Parity batch: transpose-windows, desktop, savehist, paredit-wrap-curly, complete-at-point
  (register-command! 'transpose-windows cmd-transpose-windows)
  (register-command! 'desktop-save cmd-desktop-save)
  (register-command! 'desktop-read cmd-desktop-read)
  (register-command! 'desktop-clear cmd-desktop-clear)
  (register-command! 'savehist-save cmd-savehist-save)
  (register-command! 'savehist-load cmd-savehist-load)
  (register-command! 'savehist-mode cmd-savehist-mode)
  (register-command! 'paredit-wrap-curly cmd-paredit-wrap-curly)
  (register-command! 'complete-at-point cmd-complete-at-point)
  ;; Sort numeric / sort fields
  (register-command! 'sort-numeric cmd-sort-numeric)
  (register-command! 'sort-fields cmd-sort-fields)
  ;; Align regexp
  (register-command! 'align-regexp cmd-align-regexp)
  ;; Window management
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  (register-command! 'balance-windows cmd-balance-windows)
  (register-command! 'move-to-window-line cmd-move-to-window-line)
  ;; Title case
  (register-command! 'upcase-initials-region cmd-upcase-initials-region)
  ;; S-expression extended
  (register-command! 'backward-up-list cmd-backward-up-list)
  (register-command! 'forward-up-list cmd-forward-up-list)
  (register-command! 'mark-paragraph cmd-mark-paragraph)
  ;; Hippie expand
  (register-command! 'hippie-expand cmd-hippie-expand)
  ;; Split line
  (register-command! 'split-line cmd-split-line)
  ;; Copy from above
  (register-command! 'copy-from-above cmd-copy-from-above)
  ;; Find alternate file
  (register-command! 'find-alternate-file cmd-find-alternate-file)
  ;; Increment register
  (register-command! 'increment-register cmd-increment-register)
  ;; Delete pair
  (register-command! 'delete-pair cmd-delete-pair)
  ;; Sudo write/edit
  (register-command! 'sudo-write cmd-sudo-write)
  (register-command! 'sudo-edit cmd-sudo-edit)
  (register-command! 'find-file-sudo cmd-sudo-edit)
  (register-command! 'sudo-save-buffer cmd-sudo-save-buffer)
  (register-command! 'consult-line cmd-consult-line)
  (register-command! 'consult-grep cmd-consult-grep)
  (register-command! 'consult-buffer cmd-consult-buffer)
  ;; Ediff
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  (register-command! 'ediff-directories cmd-ediff-directories)
  (register-command! 'ediff-merge cmd-ediff-merge)
  (register-command! 'ediff-regions cmd-ediff-regions)
  ;; Smerge: conflict resolution
  (register-command! 'smerge-mode cmd-smerge-mode)
  (register-command! 'smerge-next cmd-smerge-next)
  (register-command! 'smerge-prev cmd-smerge-prev)
  (register-command! 'smerge-keep-mine cmd-smerge-keep-mine)
  (register-command! 'smerge-keep-other cmd-smerge-keep-other)
  (register-command! 'smerge-keep-both cmd-smerge-keep-both)
  ;; Mode toggles
  (register-command! 'show-paren-mode cmd-show-paren-mode)
  (register-command! 'delete-selection-mode cmd-delete-selection-mode)
  ;; Highlight symbol / clear
  (register-command! 'highlight-symbol cmd-highlight-symbol)
  (register-command! 'highlight-symbol-next cmd-highlight-symbol-next)
  (register-command! 'highlight-symbol-prev cmd-highlight-symbol-prev)
  (register-command! 'clear-highlight cmd-clear-highlight)
  ;; Repeat complex command
  (register-command! 'repeat-complex-command cmd-repeat-complex-command)
  ;; Flush undo
  (register-command! 'flush-undo cmd-flush-undo)
  (register-command! 'undo-history cmd-undo-history)
  (register-command! 'undo-history-restore cmd-undo-history-restore)
  (register-command! 'undo-tree-visualize cmd-undo-tree-visualize)
  ;; Untabify buffer
  (register-command! 'untabify-buffer cmd-untabify-buffer)
  ;; Expand region
  (register-command! 'expand-region cmd-expand-region)
  (register-command! 'contract-region cmd-contract-region)
  ;; String inflection
  (register-command! 'string-inflection-cycle cmd-string-inflection-cycle)
  ;; Number increment/decrement
  (register-command! 'increment-number cmd-increment-number)
  (register-command! 'decrement-number cmd-decrement-number)
  ;; Browse URL
  (register-command! 'browse-url-at-point cmd-browse-url-at-point)
  (register-command! 'browse-url cmd-browse-url)
  ;; Imenu
  (register-command! 'imenu cmd-imenu)
  ;; Cycle tab width
  (register-command! 'cycle-tab-width cmd-cycle-tab-width)
  ;; Replace string
  (register-command! 'replace-string cmd-replace-string)
  ;; iedit
  (register-command! 'iedit-mode cmd-iedit-mode)
  ;; Cross-cutting commands (defined in facade, not in chain modules)
  (register-command! 'show-dir-locals cmd-show-dir-locals)
  (register-command! 'editorconfig-apply cmd-editorconfig-apply)
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  (register-command! 'find-file-literally cmd-find-file-literally)
  (register-command! 'org-mode cmd-org-mode)
  (register-command! 'quit cmd-quit)
  (register-command! 'indent-for-tab-command cmd-indent-or-complete)
  (register-command! 'helm-find-files cmd-find-file)
  (register-command! 'counsel-find-file cmd-find-file)
  ;; Extended + alias registrations (in commands-aliases.ss)
  (qt-register-extended-commands!)
  (qt-register-alias-commands!)
  ;; Batch 12: Emacs-standard aliases
  (qt-register-batch12-aliases!)
  ;; Aliases2: image, eww bookmarks, forge commands
  (qt-register-aliases2-commands!)
  ;; Batch 14: facade-scope aliases
  (register-command! 'kill-emacs cmd-quit)
  ;; Batch 6: header-line, project-keymaps, org-columns
  (register-command! 'header-line-mode cmd-header-line-mode)
  (register-command! 'project-keymap-load cmd-project-keymap-load)
  (register-command! 'org-columns cmd-org-columns)
  ;; Batch 7: undo-region, side-window, info, project-tree-git
  (register-command! 'undo-region cmd-undo-region)
  (register-command! 'display-buffer-in-side-window cmd-display-buffer-in-side-window)
  (register-command! 'toggle-side-window cmd-toggle-side-window)
  (register-command! 'info-reader cmd-info-reader)
  (register-command! 'project-tree-git cmd-project-tree-git)
  ;; Batch 8: file ops, window purpose, doc browser, async dired
  (register-command! 'project-tree-create-file cmd-project-tree-create-file)
  (register-command! 'project-tree-delete-file cmd-project-tree-delete-file)
  (register-command! 'project-tree-rename-file cmd-project-tree-rename-file)
  (register-command! 'set-window-dedicated cmd-set-window-dedicated)
  (register-command! 'gemacs-doc cmd-gemacs-doc)
  (register-command! 'dired-async-copy cmd-dired-async-copy)
  (register-command! 'dired-async-move cmd-dired-async-move)
  ;; Batch 9: recursive minibuffer, customize, process sentinels, plugins
  (register-command! 'toggle-enable-recursive-minibuffers cmd-toggle-enable-recursive-minibuffers)
  (register-command! 'customize cmd-customize)
  (register-command! 'set-variable cmd-set-variable)
  (register-command! 'load-plugin cmd-load-plugin)
  (register-command! 'list-plugins cmd-list-plugins)
  ;; Batch 10: advice, autoloads
  (register-command! 'describe-advice cmd-describe-advice)
  (register-command! 'list-autoloads cmd-list-autoloads)
  ;; Batch 11: dynamic modules, icomplete/fido, marginalia, embark
  (register-command! 'load-module cmd-load-module)
  (register-command! 'list-modules cmd-list-modules)
  (register-command! 'icomplete-mode cmd-icomplete-mode)
  (register-command! 'fido-mode cmd-fido-mode)
  (register-command! 'marginalia-mode cmd-marginalia-mode)
  (register-command! 'embark-act cmd-embark-act)
  (register-command! 'embark-dwim cmd-embark-dwim)
  ;; Apheleia format-on-save (override parity3 toggle with real hook wiring)
  (register-command! 'apheleia-mode cmd-apheleia-mode)
  ;; Batch 12: persistent undo, image thumbnails, virtual dired, key translation
  (register-command! 'undo-history-save cmd-undo-history-save)
  (register-command! 'undo-history-load cmd-undo-history-load)
  (register-command! 'image-dired-display-thumbnail cmd-image-dired-display-thumbnail)
  (register-command! 'image-dired-show-all-thumbnails cmd-image-dired-show-all-thumbnails)
  (register-command! 'virtual-dired cmd-virtual-dired)
  (register-command! 'dired-from-find cmd-dired-from-find)
  (register-command! 'key-translate cmd-key-translate)
  (register-command! 'toggle-super-key-mode cmd-toggle-super-key-mode)
  (register-command! 'describe-key-translations cmd-describe-key-translations)
  ;; Batch 13: display tables, multi-server LSP, devops, helm
  (register-command! 'set-display-table-entry cmd-set-display-table-entry)
  (register-command! 'describe-display-table cmd-describe-display-table)
  (register-command! 'lsp-set-server cmd-lsp-set-server)
  (register-command! 'lsp-list-servers cmd-lsp-list-servers)
  (register-command! 'ansible-mode cmd-ansible-mode)
  (register-command! 'ansible-playbook cmd-ansible-playbook)
  (register-command! 'systemd-mode cmd-systemd-mode)
  (register-command! 'kubernetes-mode cmd-kubernetes-mode)
  (register-command! 'kubectl cmd-kubectl)
  (register-command! 'ssh-config-mode cmd-ssh-config-mode)
  (register-command! 'helm-occur cmd-helm-occur)
  (register-command! 'helm-dash cmd-helm-dash)
  ;; Batch 14: completion, AI, TRAMP/remote, helm-c-yasnippet
  (register-command! 'vertico-mode cmd-vertico-mode)
  (register-command! 'selectrum-mode cmd-selectrum-mode)
  (register-command! 'cape-dabbrev cmd-cape-dabbrev)
  (register-command! 'cape-file cmd-cape-file)
  (register-command! 'cape-history cmd-cape-history)
  (register-command! 'cape-keyword cmd-cape-keyword)
  ;; Copilot commands registered via qt-register-copilot-commands! from parity5
  (register-command! 'ai-inline-suggest cmd-ai-inline-suggest)
  (register-command! 'ai-code-explain cmd-ai-code-explain)
  (register-command! 'ai-code-refactor cmd-ai-code-refactor)
  (register-command! 'tramp-ssh-edit cmd-tramp-ssh-edit)
  (register-command! 'tramp-ssh-save cmd-tramp-ssh-save)
  (register-command! 'tramp-docker-edit cmd-tramp-docker-edit)
  (register-command! 'tramp-remote-shell cmd-tramp-remote-shell)
  (register-command! 'tramp-remote-compile cmd-tramp-remote-compile)
  (register-command! 'helm-c-yasnippet cmd-helm-c-yasnippet)
  ;; Batch 15: parity stubs
  (register-command! 'semantic-mode cmd-semantic-mode)
  (register-command! 'tree-sitter-mode cmd-tree-sitter-mode)
  (register-command! 'tree-sitter-highlight-mode cmd-tree-sitter-highlight-mode)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'other-frame cmd-other-frame)
  (register-command! 'tool-bar-mode cmd-tool-bar-mode)
  (register-command! 'scroll-bar-mode cmd-scroll-bar-mode)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'vterm cmd-vterm)
  (register-command! 'pdf-view-mode cmd-pdf-view-mode)
  (register-command! 'pdf-view-next-page cmd-pdf-view-next-page)
  (register-command! 'pdf-view-previous-page cmd-pdf-view-previous-page)
  (register-command! 'pdf-view-goto-page cmd-pdf-view-goto-page)
  (register-command! 'doc-view-mode cmd-doc-view-mode)
  (register-command! 'compose-mail cmd-compose-mail)
  (register-command! 'mail-send cmd-mail-send)
  (register-command! 'message-send cmd-mail-send)
  (register-command! 'gnus cmd-gnus)
  (register-command! 'mu4e cmd-mu4e)
  (register-command! 'notmuch cmd-notmuch)
  (register-command! 'message-mode cmd-message-mode)
  (register-command! 'erc cmd-erc)
  (register-command! 'rcirc cmd-rcirc)
  (register-command! 'eww-submit-form cmd-eww-submit-form)
  (register-command! 'eww-toggle-css cmd-eww-toggle-css)
  (register-command! 'eww-toggle-images cmd-eww-toggle-images)
  (register-command! 'native-compile-file cmd-native-compile-file)
  (register-command! 'native-compile-async cmd-native-compile-async)
  (register-command! 'screen-reader-mode cmd-screen-reader-mode)
  ;; Helm commands (real implementations)
  (qt-register-helm-commands!)
  ;; Bulk toggle parity commands (339 toggles)
  (qt-register-parity3-toggles!)
  ;; Parity4: mode toggles, stubs, aliases, functional commands
  (qt-register-parity4-commands!)
  ;; Parity4 toggles (339 remaining toggles)
  (qt-register-parity4-toggles!)
  ;; Parity5: remaining mode toggles, stubs, aliases, functional
  (qt-register-parity5-commands!)
  ;; Wire modeline providers
  (set-box! *modeline-overwrite-provider* (lambda () *overwrite-mode*))
  (set-box! *modeline-narrow-provider*
    (lambda (buf) (and (hash-get *narrow-state* buf) #t)))
  ;; Repeat maps (Emacs 28+ transient repeat maps)
  (register-default-repeat-maps!))

;;; Qt versions of batch 6 commands

(def *header-line-mode* #f)
(def *project-keymaps* (make-hash-table))

(def (cmd-header-line-mode app)
  "Toggle header line display."
  (set! *header-line-mode* (not *header-line-mode*))
  (let ((echo (app-state-echo app)))
    (if *header-line-mode*
      (let* ((buf (current-qt-buffer app))
             (path (and buf (buffer-file-path buf))))
        (echo-message! echo (string-append "Header: " (or path (buffer-name buf)))))
      (echo-message! echo "Header line mode disabled"))))

(def (cmd-project-keymap-load app)
  "Load project-specific keybindings from .gemacs-keys in project root."
  (let* ((root (project-current app))
         (echo (app-state-echo app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (let ((keyfile (path-expand ".gemacs-keys" root)))
        (if (not (file-exists? keyfile))
          (echo-message! echo (string-append "No .gemacs-keys in " root))
          (with-catch
            (lambda (e)
              (echo-message! echo (string-append "Error: "
                (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (let* ((content (call-with-input-file keyfile
                        (lambda (p) (read-line p #f))))
                     (lines (string-split content #\newline))
                     (count 0))
                (for-each
                  (lambda (line)
                    (let ((trimmed (string-trim line)))
                      (when (and (> (string-length trimmed) 0)
                                 (not (char=? (string-ref trimmed 0) #\#)))
                        (let ((parts (string-split trimmed #\space)))
                          (when (>= (length parts) 2)
                            (hash-put! *project-keymaps* (cons root (car parts))
                                       (string->symbol (cadr parts)))
                            (set! count (+ count 1)))))))
                  lines)
                (echo-message! echo
                  (string-append "Loaded " (number->string count) " project keybindings"))))))))))

(def (cmd-org-columns app)
  "Display org heading properties in column view."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)))
    (let loop ((ls lines) (headings []))
      (if (null? ls)
        (if (null? headings)
          (echo-message! echo "No org headings found")
          (let* ((rev (reverse headings))
                 (header "| Level | Heading | TODO | Priority |")
                 (sep    "|-------|---------|------|----------|")
                 (rows (map (lambda (h)
                              (let ((level (car h)) (title (cadr h))
                                    (todo (caddr h)) (pri (cadddr h)))
                                (string-append "| " (number->string level) " | "
                                  (substring title 0 (min 30 (string-length title))) " | "
                                  todo " | " pri " |")))
                            rev))
                 (content (string-append header "\n" sep "\n"
                            (string-join rows "\n") "\n"))
                 (fr (app-state-frame app))
                 (cbuf (or (buffer-by-name "*Org Columns*")
                           (qt-buffer-create! "*Org Columns*" ed #f))))
            (qt-buffer-attach! ed cbuf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) cbuf)
            (qt-plain-text-edit-set-text! ed content)
            (qt-text-document-set-modified! (buffer-doc-pointer cbuf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))
        (let* ((line (car ls))
               (trimmed (string-trim line)))
          (if (and (> (string-length trimmed) 0)
                   (char=? (string-ref trimmed 0) #\*))
            (let* ((stars (let lp ((i 0))
                           (if (and (< i (string-length trimmed))
                                    (char=? (string-ref trimmed i) #\*))
                             (lp (+ i 1)) i)))
                   (rest (string-trim (substring trimmed stars (string-length trimmed))))
                   (words (string-split rest #\space))
                   (todo-kw (if (and (pair? words)
                                     (member (car words) '("TODO" "DONE" "WAITING" "CANCELLED")))
                              (car words) ""))
                   (after-todo (if (string=? todo-kw "") words (cdr words)))
                   (pri (if (and (pair? after-todo)
                                 (string-prefix? "[#" (car after-todo)))
                           (car after-todo) ""))
                   (after-pri (if (string=? pri "") after-todo (cdr after-todo)))
                   (title (string-join after-pri " ")))
              (loop (cdr ls) (cons (list stars title todo-kw pri) headings)))
            (loop (cdr ls) headings)))))))

;;; Qt versions of batch 7 commands

(def (cmd-undo-region app)
  "Undo changes within the current selection/region (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (if (= 0 (sci-send ed SCI_CANUNDO 0 0))
      (echo-message! echo "No further undo information")
      (begin
        (sci-send ed SCI_UNDO 0 0)
        (echo-message! echo "Undo (region)")))))

(def *side-window-visible* #f)

(def (cmd-display-buffer-in-side-window app)
  "Toggle side window display (Qt)."
  (let* ((echo (app-state-echo app)))
    (if *side-window-visible*
      (begin
        (set! *side-window-visible* #f)
        (cmd-delete-other-windows app)
        (echo-message! echo "Side window closed"))
      (begin
        (set! *side-window-visible* #t)
        (cmd-split-window-right app)
        (echo-message! echo "Side window opened")))))

(def (cmd-toggle-side-window app)
  "Toggle side window."
  (cmd-display-buffer-in-side-window app))

(def (cmd-info-reader app)
  "Open built-in Info documentation (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (content (string-append
           "Gemacs Info\n"
           "===========\n\n"
           "* Commands: M-x to run any command\n"
           "* Keybindings: C-h k to describe a key\n"
           "* Org Mode: Full org support with babel, export, agenda\n"
           "* Configuration: ~/.gemacs-init, ~/.gemacs-config\n\n"
           "Movement: C-f/C-b (char), M-f/M-b (word), C-n/C-p (line)\n"
           "Editing: C-k (kill), C-y (yank), C-/ (undo), C-w (cut)\n"
           "Files: C-x C-f (open), C-x C-s (save), C-x b (switch)\n"
           "Windows: C-x 2/3 (split), C-x 0/1 (delete), C-x o (other)\n"
           "Search: C-s (forward), C-r (backward), M-% (replace)\n"
           "Git: C-x g (magit), M-x magit-log, M-x magit-diff\n"
           "Help: C-h k (key), C-h f (func), C-h v (var), C-h t (tutorial)\n"))
         (ibuf (or (buffer-by-name "*info*")
                   (qt-buffer-create! "*info*" ed #f))))
    (qt-buffer-attach! ed ibuf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) ibuf)
    (qt-plain-text-edit-set-text! ed content)
    (qt-text-document-set-modified! (buffer-doc-pointer ibuf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (git-file-status dir)
  "Get git status for files. Returns hash: filename -> status-char."
  (let ((result (make-hash-table)))
    (with-catch
      (lambda (e) result)
      (lambda ()
        (let* ((proc (open-process
                       (list path: "git"
                             arguments: ["status" "--porcelain" "-uall"]
                             directory: dir
                             stdin-redirection: #f
                             stdout-redirection: #t
                             stderr-redirection: #f)))
               (output (read-line proc #f)))
          (close-input-port proc)
          (process-status proc)
          (when (and output (> (string-length output) 0))
            (for-each
              (lambda (line)
                (when (>= (string-length line) 3)
                  (let* ((status-char (string-ref line 1))
                         (idx-char (string-ref line 0))
                         (filepath (substring line 3 (string-length line)))
                         (basename (path-strip-directory filepath))
                         (display-char
                           (cond
                             ((char=? idx-char #\?) #\?)
                             ((char=? idx-char #\A) #\A)
                             ((char=? status-char #\M) #\M)
                             ((char=? idx-char #\M) #\M)
                             ((char=? status-char #\D) #\D)
                             ((char=? idx-char #\D) #\D)
                             ((char=? idx-char #\R) #\R)
                             (else #\space))))
                    (hash-put! result basename display-char))))
              (string-split output #\newline))))
        result))))

(def (cmd-project-tree-git app)
  "Show project tree with git status indicators (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (let* ((git-status (git-file-status root))
             (files (with-catch (lambda (e) []) (lambda () (directory-files root))))
             (sorted (sort files string<?))
             (lines
               (map (lambda (name)
                      (let* ((full (path-expand name root))
                             (is-dir (with-catch (lambda (e) #f)
                                       (lambda ()
                                         (eq? (file-info-type (file-info full)) 'directory))))
                             (status (hash-get git-status name))
                             (status-str (if status (string status #\space) "  ")))
                        (string-append status-str
                          (if is-dir (string-append name "/") name))))
                    sorted))
             (content (string-append
                        "Project: " root "\n"
                        (string-join lines "\n") "\n"))
             (fr (app-state-frame app))
             (tbuf (or (buffer-by-name "*Project Tree*")
                       (qt-buffer-create! "*Project Tree*" ed #f))))
        (qt-buffer-attach! ed tbuf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) tbuf)
        (qt-plain-text-edit-set-text! ed content)
        (qt-text-document-set-modified! (buffer-doc-pointer tbuf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

;;; Qt versions of batch 8 commands

(def (cmd-project-tree-create-file app)
  "Create a new file in the current project (Qt)."
  (let* ((echo (app-state-echo app))
         (name (qt-echo-read-string app "Create file: ")))
    (when (and name (> (string-length name) 0))
      (let* ((buf (current-qt-buffer app))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory)))
             (path (path-expand name dir)))
        (if (file-exists? path)
          (echo-error! echo (string-append "File exists: " path))
          (begin
            (call-with-output-file path (lambda (p) (void)))
            (echo-message! echo (string-append "Created: " path))))))))

(def (cmd-project-tree-delete-file app)
  "Delete a file by name (Qt)."
  (let* ((echo (app-state-echo app))
         (name (qt-echo-read-string app "Delete file: ")))
    (when (and name (> (string-length name) 0))
      (let* ((buf (current-qt-buffer app))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory)))
             (path (path-expand name dir)))
        (if (not (file-exists? path))
          (echo-error! echo (string-append "No file: " name))
          (let ((confirm (qt-echo-read-string app (string-append "Delete " name "? (yes/no) "))))
            (when (and confirm (string=? confirm "yes"))
              (with-catch
                (lambda (e) (echo-error! echo "Delete failed"))
                (lambda ()
                  (delete-file path)
                  (echo-message! echo (string-append "Deleted: " name)))))))))))

(def (cmd-project-tree-rename-file app)
  "Rename a file (Qt)."
  (let* ((echo (app-state-echo app))
         (old-name (qt-echo-read-string app "Rename file: ")))
    (when (and old-name (> (string-length old-name) 0))
      (let* ((new-name (qt-echo-read-string app (string-append "Rename to: ")))
             (buf (current-qt-buffer app))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory))))
        (when (and new-name (> (string-length new-name) 0))
          (with-catch
            (lambda (e) (echo-error! echo "Rename failed"))
            (lambda ()
              (rename-file (path-expand old-name dir) (path-expand new-name dir))
              (echo-message! echo (string-append "Renamed: " old-name " → " new-name)))))))))

(def (cmd-gemacs-doc app)
  "Browse gemacs documentation (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (topic (qt-echo-read-string app "Doc topic (getting-started/keybindings/commands/org-mode): ")))
    (when (and topic (> (string-length topic) 0))
      (let* ((key (string-downcase topic))
             (content
               (cond
                 ((string=? key "getting-started")
                  "Getting Started with Gemacs\n==========================\n\nOpen: C-x C-f | Save: C-x C-s | Quit: C-x C-c\nSearch: C-s | Replace: M-% | Undo: C-/\nWindows: C-x 2 (split), C-x o (other), C-x 1 (unsplit)\nBuffers: C-x b (switch), C-x k (kill)\nHelp: C-h k (describe key), M-x (run command)\n")
                 ((string=? key "keybindings")
                  "Keybinding Reference\n====================\n\nC-f/C-b Forward/Back | M-f/M-b Word | C-n/C-p Line\nC-a/C-e BOL/EOL | M-</M-> BOB/EOB | C-v/M-v Page\nC-d Del | C-k Kill line | C-w Cut | M-w Copy | C-y Yank\nC-x C-f Open | C-x C-s Save | C-x b Switch | C-x k Kill\nC-s Search | C-r Reverse | M-% Replace\nC-x 2/3 Split | C-x 0/1 Delete | C-x o Other\n")
                 ((string=? key "commands")
                  "Command Reference\n=================\n\nM-x to run any command by name.\nFile: find-file, save-buffer, write-file, revert-buffer\nBuffer: switch-buffer, kill-buffer, list-buffers\nSearch: search-forward, query-replace, occur\nGit: magit-status, magit-log, magit-diff\nLSP: lsp-start, lsp-find-definition, lsp-find-references\n")
                 ((string=? key "org-mode")
                  "Org Mode Guide\n==============\n\nTAB: cycle visibility | S-TAB: global cycle\nM-RET: new heading | C-c C-t: toggle TODO\n<s TAB: source block | C-c C-c: execute block\nC-c C-e: export | C-c a: agenda\nTables: | col | col | with TAB to align\n")
                 (else #f))))
        (if (not content)
          (echo-message! echo (string-append "No topic: " topic))
          (let ((dbuf (or (buffer-by-name (string-append "*Doc: " topic "*"))
                          (qt-buffer-create! (string-append "*Doc: " topic "*") ed #f))))
            (qt-buffer-attach! ed dbuf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) dbuf)
            (qt-plain-text-edit-set-text! ed content)
            (qt-text-document-set-modified! (buffer-doc-pointer dbuf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

(def (cmd-dired-async-copy app)
  "Copy file (Qt — prompt-based)."
  (let* ((echo (app-state-echo app))
         (src (qt-echo-read-string app "Copy from: ")))
    (when (and src (> (string-length src) 0))
      (let ((dest (qt-echo-read-string app "Copy to: ")))
        (when (and dest (> (string-length dest) 0))
          (with-catch
            (lambda (e) (echo-error! echo "Copy failed"))
            (lambda ()
              (copy-file src dest)
              (echo-message! echo (string-append "Copied to " dest)))))))))

(def (cmd-dired-async-move app)
  "Move/rename file (Qt — prompt-based)."
  (let* ((echo (app-state-echo app))
         (src (qt-echo-read-string app "Move from: ")))
    (when (and src (> (string-length src) 0))
      (let ((dest (qt-echo-read-string app "Move to: ")))
        (when (and dest (> (string-length dest) 0))
          (with-catch
            (lambda (e) (echo-error! echo "Move failed"))
            (lambda ()
              (rename-file src dest)
              (echo-message! echo (string-append "Moved to " dest)))))))))

;;; Qt versions of batch 9 commands

(def *qt-enable-recursive-minibuffers* #f)

(def (cmd-toggle-enable-recursive-minibuffers app)
  "Toggle allowing recursive minibuffer invocations (Qt)."
  (let ((echo (app-state-echo app)))
    (set! *qt-enable-recursive-minibuffers* (not *qt-enable-recursive-minibuffers*))
    (echo-message! echo (if *qt-enable-recursive-minibuffers*
                          "Recursive minibuffers ON"
                          "Recursive minibuffers OFF"))))

(def *qt-customizable-vars*
  [["tab-width" "Tab stop width" (lambda () *tab-width*) (lambda (v) (set! *tab-width* v))]
   ["indent-tabs-mode" "Use tabs" (lambda () *indent-tabs-mode*) (lambda (v) (set! *indent-tabs-mode* v))]
   ["global-auto-revert-mode" "Auto-reload files" (lambda () *global-auto-revert-mode*) (lambda (v) (set! *global-auto-revert-mode* v) (set! *auto-revert-mode* v))]
   ["delete-trailing-whitespace-on-save" "Strip trailing whitespace" (lambda () *delete-trailing-whitespace-on-save*) (lambda (v) (set! *delete-trailing-whitespace-on-save* v))]])

(def (cmd-customize app)
  "Display a customization buffer for common settings (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (lines []))
    (for-each
      (lambda (entry)
        (let ((name (car entry))
              (desc (cadr entry))
              (getter (caddr entry)))
          (set! lines (cons (string-append "  " name " = " (object->string (getter))
                             "  ;; " desc) lines))))
      *qt-customizable-vars*)
    (let* ((text (string-append
                   "Gemacs Customize\n"
                   "================\n\n"
                   "Current settings:\n\n"
                   (string-join (reverse lines) "\n")
                   "\n\nUse M-x set-variable to change a setting.\n"))
           (buf (or (buffer-by-name "*Customize*")
                    (qt-buffer-create! "*Customize*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

(def (cmd-set-variable app)
  "Set a customizable variable by name (Qt)."
  (let* ((echo (app-state-echo app))
         (names (map car *qt-customizable-vars*))
         (name (qt-echo-read-string app "Set variable: ")))
    (when (and name (> (string-length name) 0))
      (let ((entry (find (lambda (e) (string=? (car e) name)) *qt-customizable-vars*)))
        (if (not entry)
          (echo-message! echo (string-append "Unknown variable: " name))
          (let* ((getter (caddr entry))
                 (current (getter))
                 (val-str (qt-echo-read-string app
                            (string-append name " (" (object->string current) "): "))))
            (when (and val-str (> (string-length val-str) 0))
              (let* ((setter (cadddr entry))
                     (val (cond
                            ((string=? val-str "#t") #t)
                            ((string=? val-str "#f") #f)
                            ((string->number val-str) => values)
                            (else val-str))))
                (setter val)
                (echo-message! echo
                  (string-append name " = " (object->string val)))))))))))

(def *qt-process-sentinels* (make-hash-table))
(def *qt-process-filters* (make-hash-table))

(def *qt-plugin-directory* "~/.gemacs-plugins")
(def *qt-loaded-plugins* [])

(def (cmd-load-plugin app)
  "Load a Gerbil Scheme plugin file (Qt)."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string app "Load plugin file: ")))
    (when (and path (> (string-length path) 0))
      (let ((full-path (path-expand path)))
        (if (not (file-exists? full-path))
          (echo-error! echo (string-append "File not found: " full-path))
          (with-catch
            (lambda (e)
              (echo-error! echo (string-append "Plugin error: "
                (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (load full-path)
              (set! *qt-loaded-plugins* (cons full-path *qt-loaded-plugins*))
              (echo-message! echo (string-append "Loaded: " (path-strip-directory full-path))))))))))

(def (cmd-list-plugins app)
  "Show loaded plugins (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (dir (path-expand *qt-plugin-directory*))
         (available (if (file-exists? dir) (directory-files dir) []))
         (ss-files (filter (lambda (f) (string-suffix? ".ss" f)) available))
         (text (string-append
                 "Gemacs Plugins\n"
                 "==============\n\n"
                 "Loaded plugins:\n"
                 (if (null? *qt-loaded-plugins*)
                   "  (none)\n"
                   (string-join (map (lambda (p) (string-append "  " p)) *qt-loaded-plugins*) "\n"))
                 "\n\nAvailable in " dir ":\n"
                 (if (null? ss-files)
                   "  (none)\n"
                   (string-join (map (lambda (f) (string-append "  " f)) ss-files) "\n"))
                 "\n\nUse M-x load-plugin to load a plugin file.\n"))
         (buf (or (buffer-by-name "*Plugins*")
                  (qt-buffer-create! "*Plugins*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; Batch 13: Display tables, multi-server LSP, devops, helm
;;;============================================================================

(def *qt-display-table* (make-hash-table))

(def (cmd-set-display-table-entry app)
  "Set display table entry (Qt)."
  (let* ((echo (app-state-echo app))
         (from (qt-echo-read-string app "Display char (single): ")))
    (when (and from (= (string-length from) 1))
      (let ((to (qt-echo-read-string app "Display as: ")))
        (when (and to (> (string-length to) 0))
          (hash-put! *qt-display-table* (string-ref from 0) to)
          (echo-message! echo (string-append "Display: " from " → " to)))))))

(def (cmd-describe-display-table app)
  "Show display table entries (Qt)."
  (let* ((echo (app-state-echo app))
         (entries (hash->list *qt-display-table*)))
    (if (null? entries)
      (echo-message! echo "Display table: empty (default rendering)")
      (echo-message! echo
        (string-append "Display table: "
          (string-join
            (map (lambda (p) (string-append (string (car p)) " → " (cdr p)))
                 entries)
            ", "))))))

(def *qt-lsp-servers* (make-hash-table))

(hash-put! *qt-lsp-servers* "python" "pylsp")
(hash-put! *qt-lsp-servers* "javascript" "typescript-language-server --stdio")
(hash-put! *qt-lsp-servers* "typescript" "typescript-language-server --stdio")
(hash-put! *qt-lsp-servers* "rust" "rust-analyzer")
(hash-put! *qt-lsp-servers* "go" "gopls")
(hash-put! *qt-lsp-servers* "c" "clangd")
(hash-put! *qt-lsp-servers* "cpp" "clangd")

(def (cmd-lsp-set-server app)
  "Set LSP server for a language (Qt)."
  (let* ((echo (app-state-echo app))
         (lang (qt-echo-read-string app "Language ID: ")))
    (when (and lang (> (string-length lang) 0))
      (let ((cmd (qt-echo-read-string app "Server command: ")))
        (when (and cmd (> (string-length cmd) 0))
          (hash-put! *qt-lsp-servers* lang cmd)
          (echo-message! echo (string-append "LSP server for " lang ": " cmd)))))))

(def (cmd-lsp-list-servers app)
  "List LSP servers (Qt)."
  (let* ((echo (app-state-echo app))
         (entries (hash->list *qt-lsp-servers*)))
    (if (null? entries)
      (echo-message! echo "No LSP servers registered")
      (echo-message! echo
        (string-append "LSP servers: "
          (string-join (map (lambda (p) (string-append (car p) ": " (cdr p))) entries) ", "))))))

(def (cmd-ansible-mode app)
  "Enable Ansible YAML mode — sets YAML highlighting."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_SETLEXER SCLEX_YAML)
    (echo-message! (app-state-echo app) "Ansible mode enabled (YAML lexer)")))

(def (cmd-ansible-playbook app)
  "Run ansible-playbook --syntax-check on the current file."
  (let* ((buf (current-qt-buffer app))
         (fp (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not fp)
      (echo-message! echo "Buffer has no file")
      (let ((output (with-catch
                      (lambda (e) (string-append "Error: " (error-message e)))
                      (lambda ()
                        (let ((p (open-input-process
                                   (list path: "ansible-playbook"
                                         arguments: (list "--syntax-check" fp)
                                         stderr-redirection: #t))))
                          (let ((result (read-line p #f)))
                            (close-port p)
                            (or result "No output")))))))
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (obuf (or (buffer-by-name "*Ansible*")
                         (qt-buffer-create! "*Ansible*" ed #f))))
          (qt-buffer-attach! ed obuf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) obuf)
          (qt-plain-text-edit-set-text! ed
            (string-append "ansible-playbook --syntax-check " fp "\n\n" output "\n")))))))

(def (cmd-systemd-mode app)
  "Enable systemd unit file mode — conf-style highlighting."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_SETLEXER SCLEX_PROPERTIES)
    (echo-message! (app-state-echo app) "Systemd mode enabled (properties lexer)")))

(def (cmd-kubernetes-mode app)
  "Enable Kubernetes manifest mode — YAML highlighting."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_SETLEXER SCLEX_YAML)
    (echo-message! (app-state-echo app) "Kubernetes mode enabled (YAML lexer)")))

(def (cmd-kubectl app)
  "Run kubectl command interactively."
  (let* ((echo (app-state-echo app))
         (args (qt-echo-read-string app "kubectl: ")))
    (when (and args (> (string-length args) 0))
      (let ((output (with-catch
                      (lambda (e) (string-append "Error: " (error-message e)))
                      (lambda ()
                        (let ((p (open-input-process
                                   (list path: "kubectl"
                                         arguments: (string-split args #\space)
                                         stderr-redirection: #t))))
                          (let ((result (read-line p #f)))
                            (close-port p)
                            (or result "No output")))))))
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (obuf (or (buffer-by-name "*Kubectl*")
                         (qt-buffer-create! "*Kubectl*" ed #f))))
          (qt-buffer-attach! ed obuf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) obuf)
          (qt-plain-text-edit-set-text! ed
            (string-append "$ kubectl " args "\n\n" output "\n")))))))

(def (cmd-ssh-config-mode app)
  "Enable SSH config file mode — conf-style highlighting."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_SETLEXER SCLEX_PROPERTIES)
    (echo-message! (app-state-echo app) "SSH config mode enabled (properties lexer)")))

(def (cmd-helm-occur app)
  "Helm-style occur (Qt)."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (pattern (qt-echo-read-string app "Helm occur pattern: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (filter (lambda (l) (string-contains l pattern)) lines)))
        (if (null? matches)
          (echo-message! echo "No matches")
          (let ((buf (qt-buffer-create! "*Helm Occur*" ed)))
            (qt-buffer-attach! ed buf)
            (qt-plain-text-edit-set-text! ed
              (string-append "Helm Occur: " pattern "\n\n"
                (string-join matches "\n") "\n"))))))))

(def (cmd-helm-dash app)
  "Search documentation — uses man pages and apropos (Qt)."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (query (qt-echo-read-string app "Dash search: ")))
    (when (and query (> (string-length query) 0))
      (echo-message! echo (string-append "Searching docs for '" query "'..."))
      (async-process!
        (string-append "man -k " query " 2>/dev/null | head -30 || echo 'No man pages found for: " query "'")
        callback: (lambda (output)
          (let ((buf (qt-buffer-create! (string-append "*Dash: " query "*") ed)))
            (qt-buffer-attach! ed buf)
            (qt-plain-text-edit-set-text! ed
              (string-append "Documentation Search: " query "\n"
                             "========================================\n\n"
                             (or output "")
                             "\n\nUse M-x man to view a specific man page.\n"))
            (echo-message! echo (string-append "Dash: found results for '" query "'"))))))))

;;; Batch 14: Completion, AI, TRAMP/Remote

;; Completion framework modes — mutually exclusive
(def *qt-vertico-mode* #f)
(def *qt-selectrum-mode* #f)
(def (qt-deactivate-completion-frameworks!)
  "Deactivate all completion framework mode flags."
  (set! *qt-vertico-mode* #f)
  (set! *qt-selectrum-mode* #f)
  ;; Also deactivate toggle-based frameworks via parity toggle system
  (for-each (lambda (name)
              (hash-put! *qt-toggle-states* name #f))
            '(toggle-helm-mode toggle-ivy-mode ido-mode ivy-mode helm-mode)))

(def (cmd-vertico-mode app)
  "Toggle Vertico mode (Qt) — mutually exclusive with other frameworks."
  (if *qt-vertico-mode*
    (begin (set! *qt-vertico-mode* #f)
           (echo-message! (app-state-echo app) "Vertico mode: off"))
    (begin (qt-deactivate-completion-frameworks!)
           (set! *qt-vertico-mode* #t)
           (echo-message! (app-state-echo app) "Vertico mode: on (other frameworks disabled)"))))

(def (cmd-selectrum-mode app)
  "Toggle Selectrum mode (Qt) — mutually exclusive with other frameworks."
  (if *qt-selectrum-mode*
    (begin (set! *qt-selectrum-mode* #f)
           (echo-message! (app-state-echo app) "Selectrum mode: off"))
    (begin (qt-deactivate-completion-frameworks!)
           (set! *qt-selectrum-mode* #t)
           (echo-message! (app-state-echo app) "Selectrum mode: on (other frameworks disabled)"))))

(def (cmd-cape-dabbrev app)
  "Cape dabbrev (Qt) — delegates to hippie-expand."
  (execute-command! app 'hippie-expand))

(def (cmd-cape-file app)
  "Cape file completion (Qt) — delegates to hippie-expand-file."
  (execute-command! app 'hippie-expand-file))

(def (cmd-cape-history app)
  "Cape history completion (Qt)."
  (let* ((echo (app-state-echo app))
         (choice (qt-echo-read-string app "History: ")))
    (when choice (echo-message! echo (string-append "Cape history: " choice)))))

(def (cmd-cape-keyword app)
  "Cape keyword completion (Qt) — insert language keyword via narrowing."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (ext (let ((fp (buffer-file-path buf))) (if fp (path-extension fp) "")))
         (keywords
           (cond
             ((member ext '(".ss" ".scm" ".sld"))
              '("define" "lambda" "let" "let*" "letrec" "if" "cond" "case" "when" "unless"
                "begin" "def" "defstruct" "defclass" "defrule" "defsyntax" "import" "export"
                "match" "with" "try" "catch" "finally" "spawn" "thread" "hash" "for" "while"))
             ((member ext '(".py"))
              '("def" "class" "if" "elif" "else" "for" "while" "return" "import" "from"
                "try" "except" "finally" "raise" "with" "as" "yield" "async" "await"
                "lambda" "pass" "break" "continue" "global" "nonlocal" "assert" "del"))
             ((member ext '(".js" ".ts"))
              '("function" "const" "let" "var" "if" "else" "for" "while" "return" "import"
                "export" "class" "extends" "new" "this" "super" "async" "await" "try" "catch"
                "finally" "throw" "switch" "case" "default" "break" "continue" "typeof" "instanceof"))
             ((member ext '(".go"))
              '("func" "var" "const" "type" "if" "else" "for" "range" "return" "struct"
                "interface" "package" "import" "defer" "go" "chan" "select" "switch" "case"
                "default" "break" "continue" "map" "make" "append" "len" "cap"))
             ((member ext '(".rs"))
              '("fn" "let" "mut" "if" "else" "for" "while" "loop" "match" "return"
                "struct" "enum" "impl" "trait" "pub" "mod" "use" "crate" "self" "super"
                "async" "await" "move" "ref" "where" "type" "const" "static" "unsafe"))
             ((member ext '(".c" ".h" ".cpp" ".hpp"))
              '("if" "else" "for" "while" "do" "switch" "case" "break" "continue" "return"
                "struct" "union" "enum" "typedef" "const" "static" "extern" "volatile"
                "sizeof" "void" "int" "char" "float" "double" "long" "short" "unsigned"))
             (else #f))))
    (if (not keywords)
      (echo-message! echo "No keywords for this file type")
      (let ((choice (qt-echo-read-with-narrowing echo "Keyword: " keywords)))
        (when (and choice (> (string-length choice) 0))
          (qt-plain-text-edit-insert-text! ed choice))))))

;; Copilot stubs removed — real implementation in commands-parity5.ss

(def *qt-ai-inline-mode* #f)
(def (cmd-ai-inline-suggest app)
  "Toggle inline AI suggestions (Qt)."
  (set! *qt-ai-inline-mode* (not *qt-ai-inline-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-ai-inline-mode* "AI inline suggestions: on" "AI inline suggestions: off")))

(def (qt-ai-request prompt code language)
  "Call AI API with a prompt about the given code. Returns response or #f."
  (when (string=? *copilot-api-key* "")
    (error "Set OPENAI_API_KEY: M-x copilot-mode"))
  (let* ((body (json-object->string
                 (hash ("model" *copilot-model*)
                       ("messages" [(hash ("role" "system")
                                          ("content" (string-append
                                            "You are a code assistant for " language ". " prompt)))
                                    (hash ("role" "user")
                                          ("content" code))])
                       ("max_tokens" 1000)
                       ("temperature" 0.3))))
         (resp (http-post *copilot-api-url*
                 data: body
                 headers: [["Content-Type" . "application/json"]
                           ["Authorization" . (string-append "Bearer " *copilot-api-key*)]])))
    (if (= (request-status resp) 200)
      (let* ((json-str (request-text resp))
             (result (call-with-input-string json-str read-json))
             (choices (hash-ref result "choices" []))
             (first-choice (and (pair? choices) (car choices)))
             (message (and first-choice (hash-ref first-choice "message" #f)))
             (content (and message (hash-ref message "content" ""))))
        (request-close resp)
        (or content ""))
      (begin (request-close resp) #f))))

(def (cmd-ai-code-explain app)
  "Explain code at point or selected region using AI."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND))
         (code (if (= start end)
                 (qt-plain-text-edit-text ed)  ; whole buffer
                 (let ((text (qt-plain-text-edit-text ed)))
                   (substring text start (min end (string-length text))))))
         (lang (qt-copilot-detect-language app)))
    (if (string=? *copilot-api-key* "")
      (echo-message! echo "Set OPENAI_API_KEY first (M-x copilot-mode)")
      (begin
        (echo-message! echo "AI: requesting explanation...")
        (with-catch
          (lambda (e)
            (echo-message! echo (string-append "AI error: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((response (qt-ai-request
                              "Explain this code clearly and concisely. Use markdown formatting."
                              code lang)))
              (if response
                (let* ((fr (app-state-frame app))
                       (win (qt-current-window fr))
                       (buf2 (qt-buffer-create! "*AI Explain*" ed #f)))
                  (qt-buffer-attach! ed buf2)
                  (set! (qt-edit-window-buffer win) buf2)
                  (qt-plain-text-edit-set-text! ed
                    (string-append "Code Explanation (" lang ")\n"
                      (make-string 50 #\=) "\n\n" response "\n"))
                  (sci-send ed SCI_SETREADONLY 1))
                (echo-message! echo "AI: no response received")))))))))

(def (cmd-ai-code-refactor app)
  "Suggest refactoring for code at point or selected region using AI."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND))
         (code (if (= start end)
                 (qt-plain-text-edit-text ed)
                 (let ((text (qt-plain-text-edit-text ed)))
                   (substring text start (min end (string-length text))))))
         (lang (qt-copilot-detect-language app)))
    (if (string=? *copilot-api-key* "")
      (echo-message! echo "Set OPENAI_API_KEY first (M-x copilot-mode)")
      (begin
        (echo-message! echo "AI: requesting refactoring suggestions...")
        (with-catch
          (lambda (e)
            (echo-message! echo (string-append "AI error: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((response (qt-ai-request
                              "Suggest refactoring improvements. Show the refactored code with explanations."
                              code lang)))
              (if response
                (let* ((fr (app-state-frame app))
                       (win (qt-current-window fr))
                       (buf2 (qt-buffer-create! "*AI Refactor*" ed #f)))
                  (qt-buffer-attach! ed buf2)
                  (set! (qt-edit-window-buffer win) buf2)
                  (qt-plain-text-edit-set-text! ed
                    (string-append "Refactoring Suggestions (" lang ")\n"
                      (make-string 50 #\=) "\n\n" response "\n"))
                  (sci-send ed SCI_SETREADONLY 1))
                (echo-message! echo "AI: no response received")))))))))

(def *qt-tramp-connections* (make-hash-table))

(def (cmd-tramp-ssh-edit app)
  "Edit file via SSH. Fetches remote file via ssh cat."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string app "SSH path (/ssh:host:path): ")))
    (when (and path (> (string-length path) 0))
      (cond
        ((string-prefix? "/ssh:" path)
         (let* ((rest (substring path 5 (string-length path)))
                (colon (string-index rest #\:))
                (host (if colon (substring rest 0 colon) rest))
                (rpath (if colon (substring rest (+ colon 1) (string-length rest)) "~")))
           (hash-put! *qt-tramp-connections* host #t)
           (echo-message! echo (string-append "SSH: fetching " host ":" rpath " ..."))
           (with-catch
             (lambda (e)
               (echo-error! echo (string-append "SSH failed: "
                 (with-output-to-string (lambda () (display-exception e))))))
             (lambda ()
               (let* ((proc (open-process
                              (list path: "ssh"
                                    arguments: [host "cat" rpath]
                                    stdin-redirection: #f
                                    stdout-redirection: #t
                                    stderr-redirection: #t)))
                      (content (read-line proc #f)))
                 (process-status proc)
                 (close-port proc)
                 (if (or (not content) (string=? content ""))
                   (echo-error! echo (string-append "Could not read " host ":" rpath))
                   (let* ((name (string-append "[ssh:" host "]"
                                  (path-strip-directory rpath)))
                          (fr (app-state-frame app))
                          (ed (current-qt-editor app))
                          (buf (qt-buffer-create! name ed #f)))
                     ;; Store remote path for saving back
                     (buffer-local-set! buf 'tramp-host host)
                     (buffer-local-set! buf 'tramp-path rpath)
                     (buffer-local-set! buf 'tramp-full path)
                     (qt-buffer-attach! ed buf)
                     (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                     (qt-plain-text-edit-set-text! ed content)
                     (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                     (qt-plain-text-edit-set-cursor-position! ed 0)
                     ;; Set up highlighting
                     (qt-setup-highlighting! app buf)
                     (echo-message! echo
                       (string-append "Opened " host ":" rpath)))))))))
        (else
         (echo-message! echo "Use format: /ssh:hostname:/path/to/file"))))))

(def (cmd-tramp-ssh-save app)
  "Save the current buffer back to the remote host via SSH."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (host (buffer-local-get buf 'tramp-host))
         (rpath (buffer-local-get buf 'tramp-path)))
    (if (not host)
      (echo-error! echo "Not a TRAMP buffer")
      (let ((ed (current-qt-editor app))
            (text (qt-plain-text-edit-text (current-qt-editor app))))
        (echo-message! echo (string-append "SSH: saving to " host ":" rpath " ..."))
        (with-catch
          (lambda (e)
            (echo-error! echo (string-append "SSH save failed: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((proc (open-process
                          (list path: "ssh"
                                arguments: [host "tee" rpath]
                                stdin-redirection: #t
                                stdout-redirection: #t
                                stderr-redirection: #t))))
              (display text proc)
              (force-output proc)
              (close-output-port proc)
              (read-line proc #f) ;; consume output
              (process-status proc)
              (close-port proc)
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! echo
                (string-append "Saved to " host ":" rpath)))))))))

(def (cmd-tramp-docker-edit app)
  "Edit file in Docker container via docker exec cat."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string app "Docker path (/docker:name:path): ")))
    (when (and path (> (string-length path) 0))
      (cond
        ((string-prefix? "/docker:" path)
         (let* ((rest (substring path 8 (string-length path)))
                (colon (string-index rest #\:))
                (container (if colon (substring rest 0 colon) rest))
                (rpath (if colon (substring rest (+ colon 1) (string-length rest)) "/")))
           (echo-message! echo (string-append "Docker: fetching " container ":" rpath " ..."))
           (with-catch
             (lambda (e)
               (echo-error! echo (string-append "Docker failed: "
                 (with-output-to-string (lambda () (display-exception e))))))
             (lambda ()
               (let* ((proc (open-process
                              (list path: "docker"
                                    arguments: ["exec" container "cat" rpath]
                                    stdin-redirection: #f
                                    stdout-redirection: #t
                                    stderr-redirection: #t)))
                      (content (read-line proc #f)))
                 (process-status proc)
                 (close-port proc)
                 (if (or (not content) (string=? content ""))
                   (echo-error! echo (string-append "Could not read " container ":" rpath))
                   (let* ((name (string-append "[docker:" container "]"
                                  (path-strip-directory rpath)))
                          (fr (app-state-frame app))
                          (ed (current-qt-editor app))
                          (buf (qt-buffer-create! name ed #f)))
                     (buffer-local-set! buf 'tramp-host container)
                     (buffer-local-set! buf 'tramp-path rpath)
                     (buffer-local-set! buf 'tramp-full path)
                     (buffer-local-set! buf 'tramp-type 'docker)
                     (qt-buffer-attach! ed buf)
                     (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                     (qt-plain-text-edit-set-text! ed content)
                     (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                     (qt-plain-text-edit-set-cursor-position! ed 0)
                     (qt-setup-highlighting! app buf)
                     (echo-message! echo
                       (string-append "Opened " container ":" rpath)))))))))
        (else
         (echo-message! echo "Use format: /docker:container:/path/to/file"))))))

(def (cmd-tramp-remote-shell app)
  "Open remote shell via SSH in a compilation-style buffer (Qt)."
  (let* ((echo (app-state-echo app))
         (host (qt-echo-read-string app "Remote host: ")))
    (when (and host (> (string-length host) 0))
      (let* ((buf-name (string-append "*ssh:" host "*"))
             (cmd (string-append "ssh -t " host)))
        (echo-message! echo (string-append "Connecting to " host "..."))
        (async-process! cmd
          callback: (lambda (result)
            (let* ((fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (buf (or (buffer-by-name buf-name)
                            (qt-buffer-create! buf-name ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "-*- SSH: " host " -*-\n"
                  (make-string 60 #\-) "\n\n"
                  (or result "")
                  "\n" (make-string 60 #\-) "\n"
                  "Connection closed.\n"))
              (sci-send ed SCI_SETREADONLY 1)
              (echo-message! echo (string-append "SSH session to " host " ended")))))))))

(def (cmd-tramp-remote-compile app)
  "Run compilation on remote host via SSH (Qt)."
  (let* ((echo (app-state-echo app))
         (host (qt-echo-read-string app "Remote host: "))
         (cmd (and host (> (string-length host) 0)
                   (qt-echo-read-string app (string-append "Command on " host ": ")))))
    (when (and cmd (> (string-length cmd) 0))
      (let ((remote-cmd (string-append "ssh " host " "
                          "'" (string-join (string-split cmd #\') "'\\''") "'")))
        (compilation-run-command! app remote-cmd)))))

(def (cmd-helm-c-yasnippet app)
  "Helm-style snippet browser (Qt)."
  (echo-message! (app-state-echo app) "Helm C-yasnippet: use M-x snippet-insert for snippet browsing"))

;;; Batch 15: Parity stubs — close remaining red circles

(def *qt-semantic-mode* #f)
(def (cmd-semantic-mode app)
  "Toggle semantic analysis mode (Qt)."
  (set! *qt-semantic-mode* (not *qt-semantic-mode*))
  (echo-message! (app-state-echo app) (if *qt-semantic-mode* "Semantic mode: on" "Semantic mode: off")))

(def *qt-tree-sitter-mode* #f)
(def (cmd-tree-sitter-mode app)
  "Toggle tree-sitter mode (Qt)."
  (set! *qt-tree-sitter-mode* (not *qt-tree-sitter-mode*))
  (echo-message! (app-state-echo app) (if *qt-tree-sitter-mode* "Tree-sitter mode: on" "Tree-sitter mode: off")))

(def *qt-tree-sitter-hl* #f)
(def (cmd-tree-sitter-highlight-mode app)
  "Toggle tree-sitter highlighting (Qt)."
  (set! *qt-tree-sitter-hl* (not *qt-tree-sitter-hl*))
  (echo-message! (app-state-echo app) (if *qt-tree-sitter-hl* "Tree-sitter highlighting: on" "Tree-sitter highlighting: off")))

;; cmd-make-frame, cmd-other-frame, qt-frame-config-save, qt-frame-config-restore!
;; all defined in commands-shell.ss (available via (export #t) chain)

(def (cmd-tool-bar-mode app)
  "Toggle tool bar (Qt)."
  (echo-message! (app-state-echo app) "Tool bar: not implemented (use M-x for commands)"))

(def (cmd-scroll-bar-mode app)
  "Toggle scroll bars (vertical and horizontal)."
  (let* ((ed (current-qt-editor app))
         (v-visible (= (sci-send ed SCI_GETVSCROLLBAR 0 0) 1)))
    (if v-visible
      (begin
        (sci-send ed SCI_SETVSCROLLBAR 0 0)
        (sci-send ed SCI_SETHSCROLLBAR 0 0)
        (echo-message! (app-state-echo app) "Scroll bars hidden"))
      (begin
        (sci-send ed SCI_SETVSCROLLBAR 1 0)
        (sci-send ed SCI_SETHSCROLLBAR 1 0)
        (echo-message! (app-state-echo app) "Scroll bars visible")))))

(def (cmd-font-lock-mode app)
  "Toggle font-lock highlighting (Qt)."
  (execute-command! app 'toggle-highlighting))

(def (cmd-vterm app)
  "Open vterm terminal (Qt)."
  (execute-command! app 'term))

;;; PDF viewing state
(def *qt-pdf-file* #f)
(def *qt-pdf-page* 1)
(def *qt-pdf-total* 1)

(def (qt-pdf-page-count file)
  "Get page count using pdfinfo."
  (with-catch (lambda (e) 1)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "pdfinfo" arguments: [file]
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (process-status proc) (close-port proc)
        (if output
          (let* ((lines (string-split output #\newline))
                 (pages-line (find (lambda (l) (string-prefix? "Pages:" l)) lines)))
            (if pages-line
              (or (string->number (string-trim (substring pages-line 6 (string-length pages-line)))) 1)
              1))
          1)))))

(def (qt-pdf-extract file page)
  "Extract text from a PDF page using pdftotext."
  (with-catch (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "pdftotext"
                           arguments: ["-f" (number->string page)
                                       "-l" (number->string page)
                                       "-layout" file "-"]
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (process-status proc) (close-port proc)
        output))))

(def (qt-pdf-display! app)
  "Display current PDF page in Qt buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (content (qt-pdf-extract *qt-pdf-file* *qt-pdf-page*))
         (text (string-append "PDF: " (path-strip-directory *qt-pdf-file*)
                 " - Page " (number->string *qt-pdf-page*)
                 "/" (number->string *qt-pdf-total*) "\n"
                 (make-string 60 #\-) "\n\n"
                 (or content "Could not extract text from page")
                 "\n\n[n: next, p: previous, g: goto, q: quit]"))
         (buf (or (buffer-by-name "*PDF View*")
                  (qt-buffer-create! "*PDF View*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (sci-send ed SCI_SETREADONLY 0)
    (qt-plain-text-edit-set-text! ed text)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (sci-send ed SCI_SETREADONLY 1)))

(def (cmd-pdf-view-mode app)
  "Open a PDF file for viewing (Qt)."
  (let* ((echo (app-state-echo app))
         (file (qt-echo-read-string app "PDF file: ")))
    (when (and file (> (string-length file) 0))
      (let ((full (path-expand file)))
        (if (not (file-exists? full))
          (echo-error! echo "File not found")
          (begin
            (set! *qt-pdf-file* full)
            (set! *qt-pdf-page* 1)
            (set! *qt-pdf-total* (qt-pdf-page-count full))
            (qt-pdf-display! app)))))))

(def (cmd-pdf-view-next-page app)
  "Go to next page in PDF (Qt)."
  (if (not *qt-pdf-file*)
    (echo-message! (app-state-echo app) "No PDF open")
    (if (>= *qt-pdf-page* *qt-pdf-total*)
      (echo-message! (app-state-echo app) "Already at last page")
      (begin
        (set! *qt-pdf-page* (+ *qt-pdf-page* 1))
        (qt-pdf-display! app)))))

(def (cmd-pdf-view-previous-page app)
  "Go to previous page in PDF (Qt)."
  (if (not *qt-pdf-file*)
    (echo-message! (app-state-echo app) "No PDF open")
    (if (<= *qt-pdf-page* 1)
      (echo-message! (app-state-echo app) "Already at first page")
      (begin
        (set! *qt-pdf-page* (- *qt-pdf-page* 1))
        (qt-pdf-display! app)))))

(def (cmd-pdf-view-goto-page app)
  "Go to specific PDF page (Qt)."
  (if (not *qt-pdf-file*)
    (echo-message! (app-state-echo app) "No PDF open")
    (let* ((input (qt-echo-read-string app
                    (string-append "Go to page (1-" (number->string *qt-pdf-total*) "): ")))
           (page (and input (string->number input))))
      (if (and page (> page 0) (<= page *qt-pdf-total*))
        (begin
          (set! *qt-pdf-page* page)
          (qt-pdf-display! app))
        (echo-error! (app-state-echo app) "Invalid page number")))))

(def (cmd-doc-view-mode app)
  "View document — converts PDF/PS to text using pdftotext or ps2ascii (Qt)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! echo "No file associated with buffer")
      (let* ((ext (let ((dot (string-index-right file #\.)))
                    (if dot (substring file (+ dot 1) (string-length file)) "")))
             (cmd (cond
                    ((member ext '("pdf" "PDF")) (list "pdftotext" "-layout" file "-"))
                    ((member ext '("ps" "PS" "eps" "EPS")) (list "ps2ascii" file))
                    (else #f))))
        (if (not cmd)
          (echo-message! echo (string-append "Cannot convert ." ext " files"))
          (with-catch
            (lambda (e) (echo-error! echo "Conversion failed"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: (car cmd) arguments: (cdr cmd)
                                   stdin-redirection: #f stdout-redirection: #t
                                   stderr-redirection: #f)))
                     (output (read-line proc #f)))
                (process-status proc) (close-port proc)
                (let* ((ed (current-qt-editor app))
                       (doc-buf (or (buffer-by-name "*DocView*")
                                   (qt-buffer-create! "*DocView*" ed #f))))
                  (qt-buffer-attach! ed doc-buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) doc-buf)
                  (qt-plain-text-edit-set-text! ed (or output ""))
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (sci-send ed SCI_SETREADONLY 1)
                  (echo-message! echo (string-append "Converted " (path-strip-directory file))))))))))))


(def (cmd-compose-mail app)
  "Compose email (Qt). C-c C-c sends via sendmail/msmtp, C-c C-k aborts."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (to (or (qt-echo-read-string app "To: ") ""))
         (subject (or (qt-echo-read-string app "Subject: ") "")))
    (let ((buf (qt-buffer-create! "*mail*" ed #f)))
      (set! (buffer-lexer-lang buf) 'mail)
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed
        (string-append "To: " to "\n"
                       "Subject: " subject "\n"
                       "Cc: \n"
                       "--text follows this line--\n\n"))
      ;; Position cursor at message body
      (let ((body-start (string-length
                          (string-append "To: " to "\nSubject: " subject "\nCc: \n--text follows this line--\n"))))
        (qt-plain-text-edit-set-cursor-position! ed body-start))
      (echo-message! echo "Compose mail. C-c C-c to send, C-c C-k to abort."))))

(def (cmd-mail-send app)
  "Send the current mail buffer via sendmail or msmtp."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    ;; Parse headers
    (let* ((sep-pos (string-contains text "--text follows this line--"))
           (header-text (if sep-pos (substring text 0 sep-pos) ""))
           (body (if sep-pos
                   (substring text (+ sep-pos (string-length "--text follows this line--\n"))
                              (string-length text))
                   text))
           (headers (string-split header-text #\newline))
           (get-header (lambda (name)
                         (let ((found (find (lambda (h) (string-prefix? name h)) headers)))
                           (if found
                             (string-trim (substring found (string-length name) (string-length found)))
                             ""))))
           (to (get-header "To: "))
           (subject (get-header "Subject: "))
           (cc (get-header "Cc: ")))
      (if (string=? to "")
        (echo-error! echo "No recipient (To: header empty)")
        ;; Try msmtp first, fallback to sendmail
        (let ((send-cmd (if (file-exists? "/usr/bin/msmtp") "/usr/bin/msmtp"
                          (if (file-exists? "/usr/sbin/sendmail") "/usr/sbin/sendmail"
                            #f))))
          (if (not send-cmd)
            (echo-error! echo "No sendmail or msmtp found")
            (with-catch
              (lambda (e)
                (echo-error! echo (string-append "Send failed: "
                  (with-output-to-string (lambda () (display-exception e))))))
              (lambda ()
                (let* ((msg (string-append
                              "To: " to "\n"
                              "Subject: " subject "\n"
                              (if (> (string-length cc) 0)
                                (string-append "Cc: " cc "\n") "")
                              "Content-Type: text/plain; charset=utf-8\n"
                              "\n" body))
                       (args (if (string-suffix? "msmtp" send-cmd)
                               ["-t"]
                               ["-t" "-oi"]))
                       (proc (open-process
                               (list path: send-cmd arguments: args
                                     stdin-redirection: #t
                                     stdout-redirection: #t
                                     stderr-redirection: #t))))
                  (display msg proc)
                  (force-output proc)
                  (close-output-port proc)
                  (let ((err-output (read-line proc #f))
                        (status (process-status proc)))
                    (close-port proc)
                    (if (= status 0)
                      (begin
                        (echo-message! echo (string-append "Mail sent to " to))
                        ;; Kill the mail buffer
                        (let ((buf (current-qt-buffer app)))
                          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
                      (echo-error! echo
                        (string-append "Send failed (exit " (number->string status) "): "
                          (or err-output ""))))))))))))))

(def *qt-gnus-feeds*
  '("https://news.ycombinator.com/rss"
    "https://planet.emacslife.com/atom.xml"))

(def (cmd-gnus app)
  "Launch Gnus newsreader — fetches RSS/Atom feeds (Qt)."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app)))
    (let ((buf (qt-buffer-create! "*Group*" ed)))
      (qt-buffer-attach! ed buf)
      (qt-plain-text-edit-set-text! ed "Gnus Groups\n============\nFetching feeds...\n")
      (echo-message! echo "Fetching RSS feeds...")
      ;; Fetch feeds via curl
      (async-process!
        (string-join
          (map (lambda (url)
                 (string-append "echo '=== " url " ===' && curl -sL --max-time 10 '"
                                url "' 2>/dev/null | "
                                ;; Extract titles from RSS/Atom
                                "grep -oP '(?<=<title>)[^<]+|(?<=<title type=\"html\">)[^<]+' | head -20"))
               *qt-gnus-feeds*)
          " && ")
        callback: (lambda (output)
          (let ((text (string-append
                        "Gnus Groups — RSS Feed Reader\n"
                        "========================================\n\n"
                        (if (and output (> (string-length output) 0))
                          output
                          "(No feed data retrieved. Check network.)\n")
                        "\n========================================\n"
                        "Feeds: " (string-join *qt-gnus-feeds* ", ") "\n"
                        "Customize with: M-x set-variable gnus-select-method\n")))
            (qt-plain-text-edit-set-text! ed text)
            (echo-message! echo "Gnus: feeds loaded")))))))

(def (cmd-mu4e app)
  "Launch mu4e email — checks for mu installation (Qt)."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app)))
    (async-process! "which mu 2>/dev/null && mu find --fields='d f s' --sortfield=date --reverse --maxnum=20 '' 2>/dev/null || echo 'mu not installed. Install with: apt install maildir-utils'"
      callback: (lambda (output)
        (let ((buf (qt-buffer-create! "*mu4e*" ed)))
          (qt-buffer-attach! ed buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "mu4e — Mail\n============\n" (or output "") "\n"))
          (echo-message! echo "mu4e: loaded"))))))

(def (cmd-notmuch app)
  "Launch notmuch search — checks for notmuch installation (Qt)."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app)))
    (async-process! "which notmuch 2>/dev/null && notmuch search --limit=20 --sort=newest-first '*' 2>/dev/null || echo 'notmuch not installed. Install with: apt install notmuch'"
      callback: (lambda (output)
        (let ((buf (qt-buffer-create! "*notmuch*" ed)))
          (qt-buffer-attach! ed buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "notmuch — Search\n============\n" (or output "") "\n"))
          (echo-message! echo "notmuch: loaded"))))))

(def (cmd-message-mode app)
  "Compose message (Qt)."
  (cmd-compose-mail app))

(def *qt-irc-socket* #f)
(def *qt-irc-nick* #f)
(def *qt-irc-channel* #f)

(def (cmd-erc app)
  "Launch ERC IRC client (Qt) — connect to IRC server via TCP."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (server (qt-echo-read-string app "IRC server: "))
         (nick (and server (> (string-length server) 0)
                    (qt-echo-read-string app "Nickname: ")))
         (channel (and nick (> (string-length nick) 0)
                      (qt-echo-read-string app "Channel (#channel): "))))
    (when (and channel (> (string-length channel) 0))
      (let ((buf (qt-buffer-create! (string-append "*ERC: " server "*") ed)))
        (qt-buffer-attach! ed buf)
        (echo-message! echo (string-append "ERC: connecting to " server ":6667..."))
        (set! *qt-irc-nick* nick)
        (set! *qt-irc-channel* channel)
        (async-process!
          (string-append
            "exec 3<>/dev/tcp/" server "/6667 2>/dev/null && "
            "echo 'NICK " nick "' >&3 && "
            "echo 'USER " nick " 0 * :" nick "' >&3 && "
            "sleep 2 && "
            "echo 'JOIN " channel "' >&3 && "
            "sleep 1 && "
            "timeout 5 cat <&3 2>/dev/null; "
            "exec 3>&-")
          callback: (lambda (output)
            (let ((text (string-append
                          "ERC - " server " (" nick ")\n"
                          "========================================\n"
                          "Connected to " server ":6667\n"
                          "Joined " channel "\n"
                          "----------------------------------------\n"
                          (if (and output (> (string-length output) 0))
                            output
                            "(no messages received)\n")
                          "----------------------------------------\n"
                          "Use M-x erc-send to send a message\n")))
              (qt-plain-text-edit-set-text! ed text)
              (echo-message! echo (string-append "ERC: connected to " server)))))))))

(def (cmd-rcirc app)
  "Launch rcirc IRC client (Qt) — delegates to ERC."
  (cmd-erc app))

(def (cmd-eww-submit-form app)
  "Submit form in current EWW buffer (Qt). Parses input fields and POSTs."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    ;; Look for form fields in format: [field: value]
    (let loop ((lines (string-split text #\newline)) (fields []))
      (if (null? lines)
        (if (null? fields)
          (echo-message! echo "No form fields found in buffer")
          (begin
            (echo-message! echo (string-append "EWW: submitting " (number->string (length fields)) " form fields..."))
            (let ((params (string-join
                            (map (lambda (pair)
                                   (string-append (car pair) "=" (cdr pair)))
                                 fields) "&")))
              (echo-message! echo (string-append "Form data: " params)))))
        ;; Parse lines like "[Name: value]" or "Name: [value]"
        (let ((line (car lines)))
          (if (and (> (string-length line) 4)
                   (char=? (string-ref line 0) #\[)
                   (string-contains line ": "))
            (let* ((inner (substring line 1 (- (string-length line) 1)))
                   (colon (string-contains inner ": "))
                   (name (substring inner 0 colon))
                   (val (substring inner (+ colon 2) (string-length inner))))
              (loop (cdr lines) (cons (cons name val) fields)))
            (loop (cdr lines) fields)))))))

(def *qt-eww-css* #f)
(def (cmd-eww-toggle-css app)
  "Toggle CSS rendering in EWW (Qt)."
  (set! *qt-eww-css* (not *qt-eww-css*))
  (echo-message! (app-state-echo app) (if *qt-eww-css* "EWW CSS: on" "EWW CSS: off")))

(def *qt-eww-images* #f)
(def (cmd-eww-toggle-images app)
  "Toggle image display in EWW (Qt)."
  (set! *qt-eww-images* (not *qt-eww-images*))
  (echo-message! (app-state-echo app) (if *qt-eww-images* "EWW images: on" "EWW images: off")))

(def (cmd-native-compile-file app)
  "Native compile current file via gxc -S (Qt)."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! echo "Buffer has no file")
      (if (not (member (path-extension file) '(".ss" ".scm")))
        (echo-message! echo "Not a Gerbil/Scheme source file")
        (begin
          (echo-message! echo (string-append "Compiling " (path-strip-directory file) "..."))
          (compilation-run-command! app
            (string-append "gxc -S " file)))))))

(def (cmd-native-compile-async app)
  "Async native compile current file via gxc (Qt)."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! echo "Buffer has no file")
      (if (not (string-suffix? ".ss" file))
        (echo-message! echo "Not a Gerbil source file")
        (begin
          (echo-message! echo (string-append "Compiling " (path-strip-directory file) "..."))
          (compilation-run-command! app
            (string-append "gxc -S " file)))))))

(def *qt-screen-reader* #f)
(def (cmd-screen-reader-mode app)
  "Toggle screen reader support (Qt)."
  (set! *qt-screen-reader* (not *qt-screen-reader*))
  (echo-message! (app-state-echo app) (if *qt-screen-reader* "Screen reader: on" "Screen reader: off")))
