;;; -*- Gerbil -*-
;;; Qt commands facade - imports all sub-modules, exports public API,
;;; contains cross-cutting functions and command registration.

(export qt-register-all-commands!
        dired-open-directory!
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
        *mode-keymaps*
        mode-keymap-lookup
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
        save-place-load!
        save-place-save!
        auto-fill-check!
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
        ;; Diff-hl
        *diff-hl-active*)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        (only-in :gemacs/persist
                 buffer-local-set!
                 save-place-save! save-place-load!
                 save-place-remember! save-place-restore
                 *save-place-enabled* *require-final-newline*
                 *centered-cursor-mode*)
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/image
        ;; Sub-modules (chain)
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-config)

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
      (else
        (let ((ed (current-qt-editor app)))
          (if *auto-indent*
            ;; Auto-indent: copy previous line's leading whitespace
            (let ((indent (current-line-indent ed)))
              (qt-plain-text-edit-insert-text! ed (string-append "\n" indent)))
            (qt-plain-text-edit-insert-text! ed "\n")))))))

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
      (if (and (file-exists? filename)
               (eq? 'directory (file-info-type (file-info filename))))
        (dired-open-directory! app filename)
        (let* ((name (uniquify-buffer-name! filename))
               (fr (app-state-frame app))
               (ed2 (current-qt-editor app))
               (buf (qt-buffer-create! name ed2 filename)))
          (qt-buffer-attach! ed2 buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (when text
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
                      (qt-plain-text-edit-set-cursor-position! ed2 0))))))
            (file-mtime-record! filename))
          (qt-setup-highlighting! app buf)
          (apply-dir-locals! app filename)
          (echo-message! echo (string-append "Opened: " filename))))))))

(def (qt-list-directory-files dir)
  "List files in a directory for completion. Returns sorted list of basenames."
  (with-catch (lambda (e) [])
    (lambda ()
      (sort (directory-files dir) string<?))))

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-file-with-completion app "Find file: ")))
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
        ;; Check if it's a directory
        (if (and (file-exists? filename)
                 (eq? 'directory (file-info-type (file-info filename))))
          ;; Open as dired
          (dired-open-directory! app filename)
          ;; Open as regular file
          (let* ((name (uniquify-buffer-name! filename))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (qt-buffer-create! name ed filename)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (when (file-exists? filename)
              (let ((text (read-file-as-string filename)))
                (when text
                  ;; Cache line ending style for modeline
                  (hash-put! *buffer-eol-cache* name (detect-eol-from-text text))
                  (qt-plain-text-edit-set-text! ed text)
                  (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                  ;; Restore saved cursor position if save-place is enabled
                  (let ((saved-pos (and *save-place-enabled*
                                       (save-place-restore filename))))
                    (if (and saved-pos (< saved-pos (string-length text)))
                      (qt-plain-text-edit-set-cursor-position! ed saved-pos)
                      (qt-plain-text-edit-set-cursor-position! ed 0)))))
              (file-mtime-record! filename))
            (qt-setup-highlighting! app buf)
            (echo-message! echo (string-append "Opened: " filename)))))))))))

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
        ;; Create backup file if original exists and hasn't been backed up yet
        (when (and (file-exists? path) (not (buffer-backup-done? buf)))
          (let ((backup-path (string-append path "~")))
            (with-catch
              (lambda (e) #f)  ; Ignore backup errors
              (lambda ()
                (copy-file path backup-path)
                (set! (buffer-backup-done? buf) #t)))))
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
  "Open a file without any processing (same as find-file)."
  (cmd-find-file app))

;;;============================================================================
;;; Org structure templates (<s TAB, <e TAB, etc.)
;;;============================================================================

(def *qt-org-structure-templates*
  '(("s" "SRC"      #t)    ;; <s -> #+BEGIN_SRC ... #+END_SRC
    ("e" "EXAMPLE"  #f)    ;; <e -> #+BEGIN_EXAMPLE ... #+END_EXAMPLE
    ("q" "QUOTE"    #f)    ;; <q -> #+BEGIN_QUOTE ... #+END_QUOTE
    ("v" "VERSE"    #f)    ;; <v -> #+BEGIN_VERSE ... #+END_VERSE
    ("c" "CENTER"   #f)    ;; <c -> #+BEGIN_CENTER ... #+END_CENTER
    ("C" "COMMENT"  #f)    ;; <C -> #+BEGIN_COMMENT ... #+END_COMMENT
    ("l" "EXPORT latex" #f) ;; <l -> #+BEGIN_EXPORT latex ... #+END_EXPORT
    ("h" "EXPORT html" #f)  ;; <h -> #+BEGIN_EXPORT html ... #+END_EXPORT
    ("a" "EXPORT ascii" #f))) ;; <a -> #+BEGIN_EXPORT ascii ... #+END_EXPORT

(def (qt-org-template-lookup key)
  "Look up org template by shortcut key. Returns (block-type has-lang?) or #f."
  (let loop ((ts *qt-org-structure-templates*))
    (if (null? ts) #f
      (let ((t (car ts)))
        (if (string=? (car t) key)
          (cdr t)
          (loop (cdr ts)))))))

(def (qt-try-org-template-expand app)
  "Try to expand an org structure template at point. Returns #t if expanded."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (text-len (string-length text))
         (pos (min (qt-plain-text-edit-cursor-position ed) text-len))
         ;; Find line boundaries from text (avoids byte/char position mismatch)
         (line-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((char=? (string-ref text i) #\newline) (+ i 1))
                             (else (loop (- i 1))))))
         (line-end (let loop ((i pos))
                     (cond ((>= i text-len) text-len)
                           ((char=? (string-ref text i) #\newline) i)
                           (else (loop (+ i 1))))))
         (line (substring text line-start line-end))
         (trimmed (string-trim line)))
    (if (and (>= (string-length trimmed) 2)
             (char=? (string-ref trimmed 0) #\<))
      (let* ((key (substring trimmed 1 (string-length trimmed)))
             (tmpl (qt-org-template-lookup key)))
        (if (not tmpl) #f
          (let* ((block-type (car tmpl))
                 ;; Preserve leading whitespace
                 (indent (let loop ((i 0))
                           (if (and (< i (string-length line))
                                    (char=? (string-ref line i) #\space))
                             (loop (+ i 1))
                             (substring line 0 i))))
                 ;; For EXPORT blocks, end tag is just EXPORT
                 (end-type (let ((sp (string-contains block-type " ")))
                             (if sp (substring block-type 0 sp) block-type)))
                 (begin-line (string-append indent "#+BEGIN_" block-type))
                 (end-line (string-append indent "#+END_" end-type))
                 (expansion (string-append begin-line "\n"
                                           indent "\n"
                                           end-line)))
            ;; Replace the <X line with the expansion via full text rebuild
            ;; (avoids byte/char position mismatch with Qt selection APIs)
            (let ((new-text (string-append
                              (substring text 0 line-start)
                              expansion
                              (substring text line-end text-len))))
              (qt-plain-text-edit-set-text! ed new-text)
              ;; Place cursor on the blank line inside the block
              (qt-plain-text-edit-set-cursor-position! ed
                (+ line-start (string-length begin-line) 1
                   (string-length indent)))
              ;; Re-apply org highlighting (set-text clears all styles)
              (qt-setup-org-styles! ed)
              (qt-org-highlight-buffer! ed new-text))
            (echo-message! (app-state-echo app)
              (string-append "Expanded <" key " to #+BEGIN_" block-type))
            #t)))
      #f)))

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
                     (qt-plain-text-edit-insert-text! ed "  ")
                     (let ((c (get-or-create-completer! ed app)))
                       (qt-completer-set-model-strings! c sorted)
                       (qt-completer-set-completion-prefix! c prefix)
                       ;; Show popup at cursor position
                       (let* ((pos (qt-plain-text-edit-cursor-position ed))
                              (line (qt-plain-text-edit-cursor-line ed)))
                         (qt-completer-complete-rect! c 0 0 200 20))))))))))))))

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
        (session-save! app)
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
           (session-save! app)
           (set! (app-state-running app) #f)
           (qt-widget-close! (qt-frame-main-win fr)))
          ((and answer (or (string=? answer "no") (string=? answer "n")))
           ;; Quit without saving
           (scratch-save!)
           (save-place-save!)
           (session-save! app)
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
  (register-command! 'delete-char cmd-delete-char)
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
  ;; Eshell
  (register-command! 'eshell cmd-eshell)
  ;; Shell
  (register-command! 'shell cmd-shell)
  ;; Terminal
  (register-command! 'term cmd-term)
  (register-command! 'term-interrupt cmd-term-interrupt)
  (register-command! 'term-send-eof cmd-term-send-eof)
  (register-command! 'term-send-tab cmd-term-send-tab)
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
  ;; Keyboard macros
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
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
  (register-command! 'paredit-wrap-round cmd-paredit-wrap-round)
  (register-command! 'paredit-wrap-square cmd-paredit-wrap-square)
  (register-command! 'paredit-splice-sexp cmd-paredit-splice-sexp)
  (register-command! 'paredit-raise-sexp cmd-paredit-raise-sexp)
  (register-command! 'paredit-split-sexp cmd-paredit-split-sexp)
  (register-command! 'paredit-join-sexps cmd-paredit-join-sexps)
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
  ;; View messages
  (register-command! 'view-messages cmd-view-messages)
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
  ;; Sudo write
  (register-command! 'sudo-write cmd-sudo-write)
  (register-command! 'sudo-save-buffer cmd-sudo-save-buffer)
  (register-command! 'consult-line cmd-consult-line)
  (register-command! 'consult-grep cmd-consult-grep)
  (register-command! 'consult-buffer cmd-consult-buffer)
  ;; Ediff
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  (register-command! 'ediff-directories cmd-ediff-directories)
  (register-command! 'ediff-merge cmd-ediff-merge)
  (register-command! 'ediff-regions cmd-ediff-regions)
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
  ;; Batch 6: Navigation
  (register-command! 'goto-column cmd-goto-column)
  (register-command! 'goto-line-relative cmd-goto-line-relative)
  (register-command! 'recenter-top cmd-recenter-top)
  ;; Character case
  (register-command! 'upcase-char cmd-upcase-char)
  (register-command! 'downcase-char cmd-downcase-char)
  (register-command! 'toggle-case-at-point cmd-toggle-case-at-point)
  (register-command! 'capitalize-region cmd-capitalize-region)
  ;; Copy commands
  (register-command! 'copy-buffer-name cmd-copy-buffer-name)
  (register-command! 'copy-current-line cmd-copy-current-line)
  (register-command! 'copy-word cmd-copy-word)
  (register-command! 'copy-file-path cmd-copy-file-path)
  (register-command! 'copy-line-number cmd-copy-line-number)
  (register-command! 'copy-region-as-kill cmd-copy-region-as-kill)
  (register-command! 'yank-whole-line cmd-yank-whole-line)
  ;; Insert commands
  (register-command! 'insert-pair-braces cmd-insert-pair-braces)
  (register-command! 'insert-pair-quotes cmd-insert-pair-quotes)
  (register-command! 'insert-newline-above cmd-insert-newline-above)
  (register-command! 'insert-newline-below cmd-insert-newline-below)
  (register-command! 'insert-comment-separator cmd-insert-comment-separator)
  (register-command! 'insert-line-number cmd-insert-line-number)
  (register-command! 'insert-buffer-filename cmd-insert-buffer-filename)
  (register-command! 'insert-timestamp cmd-insert-timestamp)
  (register-command! 'insert-shebang cmd-insert-shebang)
  ;; Buffer management
  (register-command! 'count-buffers cmd-count-buffers)
  (register-command! 'rename-uniquely cmd-rename-uniquely)
  (register-command! 'bury-buffer cmd-bury-buffer)
  (register-command! 'unbury-buffer cmd-unbury-buffer)
  (register-command! 'append-to-buffer cmd-append-to-buffer)
  ;; File operations
  (register-command! 'make-directory cmd-make-directory)
  (register-command! 'delete-file cmd-delete-file)
  (register-command! 'copy-file cmd-copy-file)
  (register-command! 'list-directory cmd-list-directory)
  (register-command! 'pwd cmd-pwd)
  ;; Dired extended
  (register-command! 'dired-create-directory cmd-dired-create-directory)
  (register-command! 'dired-find-file cmd-dired-find-file)
  (register-command! 'dired-do-rename cmd-dired-do-rename)
  (register-command! 'dired-do-delete cmd-dired-do-delete)
  (register-command! 'dired-do-copy cmd-dired-do-copy)
  ;; Toggle commands
  (register-command! 'toggle-hl-line cmd-toggle-hl-line)
  (register-command! 'toggle-show-tabs cmd-toggle-show-tabs)
  (register-command! 'toggle-show-eol cmd-toggle-show-eol)
  (register-command! 'toggle-narrowing-indicator cmd-toggle-narrowing-indicator)
  (register-command! 'toggle-debug-on-error cmd-toggle-debug-on-error)
  (register-command! 'toggle-fold cmd-toggle-fold)
  ;; Info/describe
  (register-command! 'what-mode cmd-what-mode)
  (register-command! 'what-encoding cmd-what-encoding)
  (register-command! 'what-line-col cmd-what-line-col)
  (register-command! 'show-file-info cmd-show-file-info)
  (register-command! 'show-buffer-size cmd-show-buffer-size)
  (register-command! 'show-column-number cmd-show-column-number)
  (register-command! 'emacs-version cmd-emacs-version)
  ;; Git/VCS
  (register-command! 'show-git-status cmd-show-git-status)
  (register-command! 'show-git-log cmd-show-git-log)
  (register-command! 'show-git-diff cmd-show-git-diff)
  (register-command! 'show-git-blame cmd-show-git-blame)
  ;; Text manipulation
  (register-command! 'comment-region cmd-comment-region)
  (register-command! 'uncomment-region cmd-uncomment-region)
  (register-command! 'collapse-blank-lines cmd-collapse-blank-lines)
  (register-command! 'remove-blank-lines cmd-remove-blank-lines)
  (register-command! 'delete-trailing-lines cmd-delete-trailing-lines)
  (register-command! 'trim-lines cmd-trim-lines)
  (register-command! 'prefix-lines cmd-prefix-lines)
  (register-command! 'suffix-lines cmd-suffix-lines)
  ;; Sort variants
  (register-command! 'sort-lines-reverse cmd-sort-lines-reverse)
  (register-command! 'sort-lines-case-fold cmd-sort-lines-case-fold)
  (register-command! 'uniquify-lines cmd-uniquify-lines)
  (register-command! 'sort-words cmd-sort-words)
  ;; Case conversion
  (register-command! 'camel-to-snake cmd-camel-to-snake)
  (register-command! 'snake-to-camel cmd-snake-to-camel)
  ;; Search
  (register-command! 'highlight-word-at-point cmd-highlight-word-at-point)
  (register-command! 'grep cmd-grep)
  (register-command! 'rgrep cmd-rgrep)
  (register-command! 'grep-goto cmd-grep-goto)
  (register-command! 'next-grep-result cmd-next-grep-result)
  (register-command! 'previous-grep-result cmd-previous-grep-result)
  ;; Wgrep
  (register-command! 'wgrep-change-to-wgrep-mode cmd-wgrep-change-to-wgrep-mode)
  (register-command! 'wgrep-finish-edit cmd-wgrep-finish-edit)
  (register-command! 'wgrep-abort-changes cmd-wgrep-abort-changes)
  ;; Misc batch 6
  (register-command! 'quoted-insert cmd-quoted-insert)
  (register-command! 'quick-calc cmd-quick-calc)
  (register-command! 'eval-and-insert cmd-eval-and-insert)
  (register-command! 'shell-command-insert cmd-shell-command-insert)
  (register-command! 'pipe-region cmd-pipe-region)
  ;; Bookmark extensions
  (register-command! 'bookmark-delete cmd-bookmark-delete)
  (register-command! 'bookmark-save cmd-bookmark-save)
  (register-command! 'bookmark-load cmd-bookmark-load)
  ;; Region/text misc
  (register-command! 'duplicate-region cmd-duplicate-region)
  (register-command! 'reverse-chars cmd-reverse-chars)
  (register-command! 'reverse-word cmd-reverse-word)
  ;; Environment
  (register-command! 'getenv cmd-getenv)
  (register-command! 'setenv cmd-setenv)
  ;; Hl-todo
  (register-command! 'hl-todo-mode cmd-hl-todo-mode)
  (register-command! 'hl-todo-next cmd-hl-todo-next)
  (register-command! 'hl-todo-previous cmd-hl-todo-previous)
  ;; Shell command framework
  (register-command! 'run-user-shell-command cmd-run-user-shell-command)
  ;; Workspaces
  (register-command! 'workspace-create cmd-workspace-create)
  (register-command! 'workspace-switch cmd-workspace-switch)
  (register-command! 'workspace-delete cmd-workspace-delete)
  (register-command! 'workspace-add-buffer cmd-workspace-add-buffer)
  (register-command! 'workspace-list cmd-workspace-list)
  ;; Multiple cursors
  (register-command! 'mc-mark-next cmd-mc-mark-next)
  (register-command! 'mc-mark-all cmd-mc-mark-all)
  (register-command! 'mc-skip-and-mark-next cmd-mc-skip-and-mark-next)
  (register-command! 'mc-edit-lines cmd-mc-edit-lines)
  (register-command! 'mc-unmark-last cmd-mc-unmark-last)
  (register-command! 'mc-rotate cmd-mc-rotate)
  ;; Batch 7: More commands
  (register-command! 'transpose-sexps cmd-transpose-sexps)
  (register-command! 'transpose-paragraphs cmd-transpose-paragraphs)
  (register-command! 'zap-up-to-char cmd-zap-up-to-char)
  (register-command! 'zap-to-char-inclusive cmd-zap-to-char-inclusive)
  (register-command! 'query-replace-regexp cmd-query-replace-regexp)
  (register-command! 'isearch-forward-regexp cmd-search-forward-regexp)
  (register-command! 'copy-from-below cmd-copy-from-below)
  (register-command! 'copy-symbol-at-point cmd-copy-symbol-at-point)
  (register-command! 'copy-word-at-point cmd-copy-word-at-point)
  (register-command! 'delete-to-end-of-line cmd-delete-to-end-of-line)
  (register-command! 'delete-to-beginning-of-line cmd-delete-to-beginning-of-line)
  (register-command! 'delete-horizontal-space-forward cmd-delete-horizontal-space-forward)
  (register-command! 'cycle-spacing cmd-cycle-spacing)
  (register-command! 'swap-windows cmd-swap-windows)
  (register-command! 'rotate-windows cmd-rotate-windows)
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  (register-command! 'narrow-to-defun cmd-narrow-to-defun)
  (register-command! 'fold-all cmd-fold-all)
  (register-command! 'unfold-all cmd-unfold-all)
  (register-command! 'toggle-auto-pair-mode cmd-toggle-auto-pair-mode)
  (register-command! 'mark-page cmd-mark-page)
  (register-command! 'mark-whole-buffer cmd-mark-whole-buffer)
  (register-command! 'view-lossage cmd-view-lossage)
  (register-command! 'bookmark-rename cmd-bookmark-rename)
  (register-command! 'view-register cmd-view-register)
  (register-command! 'sort-imports cmd-sort-imports)
  (register-command! 'replace-string-all cmd-replace-string-all)
  (register-command! 'replace-in-region cmd-replace-in-region)
  (register-command! 'write-region cmd-write-region)
  (register-command! 'search-forward-word cmd-search-forward-word)
  (register-command! 'search-backward-word cmd-search-backward-word)
  (register-command! 'count-occurrences cmd-count-occurrences)
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  (register-command! 'find-file-literally cmd-find-file-literally)
  (register-command! 'kill-matching-buffers cmd-kill-matching-buffers)
  (register-command! 'list-recent-files cmd-list-recent-files)
  (register-command! 'clear-recent-files cmd-clear-recent-files)
  (register-command! 'recentf-open cmd-recentf-open)
  (register-command! 'recentf-cleanup cmd-recentf-cleanup)
  (register-command! 'multi-occur cmd-multi-occur)
  (register-command! 'align-current cmd-align-current)
  (register-command! 'clear-rectangle cmd-clear-rectangle)
  (register-command! 'describe-mode cmd-describe-mode)
  (register-command! 'describe-face cmd-describe-face)
  (register-command! 'describe-function cmd-describe-function)
  (register-command! 'describe-variable cmd-describe-variable)
  (register-command! 'describe-syntax cmd-describe-syntax)
  (register-command! 'insert-lorem-ipsum cmd-insert-lorem-ipsum)
  (register-command! 'insert-current-date-iso cmd-insert-current-date-iso)
  (register-command! 'insert-time cmd-insert-time)
  (register-command! 'goto-definition cmd-goto-definition)
  (register-command! 'xref-back cmd-xref-back)
  (register-command! 'xref-find-definitions cmd-xref-find-definitions)
  (register-command! 'xref-find-references cmd-xref-find-references)
  (register-command! 'xref-find-apropos cmd-xref-find-apropos)
  (register-command! 'xref-go-back cmd-xref-go-back)
  (register-command! 'xref-go-forward cmd-xref-go-forward)
  (register-command! 'number-to-register cmd-number-to-register)
  ;; Eldoc
  (register-command! 'eldoc-mode cmd-eldoc-mode)
  (register-command! 'toggle-global-eldoc cmd-toggle-global-eldoc)
  ;; Project
  (register-command! 'project-find-regexp cmd-project-find-regexp)
  (register-command! 'project-shell cmd-project-shell)
  (register-command! 'project-eshell cmd-project-eshell)
  ;; Diff
  (register-command! 'diff-mode cmd-diff-mode)
  (register-command! 'diff-apply-hunk cmd-diff-apply-hunk)
  (register-command! 'diff-revert-hunk cmd-diff-revert-hunk)
  (register-command! 'imenu cmd-imenu)
  (register-command! 'show-word-count cmd-show-word-count)
  (register-command! 'show-char-count cmd-show-char-count)
  (register-command! 'insert-path-separator cmd-insert-path-separator)
  (register-command! 'maximize-window cmd-maximize-window)
  (register-command! 'minimize-window cmd-minimize-window)
  (register-command! 'delete-matching-lines cmd-delete-matching-lines)
  (register-command! 'delete-non-matching-lines cmd-delete-non-matching-lines)
  (register-command! 'copy-matching-lines cmd-copy-matching-lines)
  (register-command! 'count-lines-buffer cmd-count-lines-buffer)
  (register-command! 'count-words-paragraph cmd-count-words-paragraph)
  (register-command! 'convert-to-unix cmd-convert-to-unix)
  (register-command! 'convert-to-dos cmd-convert-to-dos)
  (register-command! 'convert-line-endings-unix cmd-convert-to-unix)
  (register-command! 'convert-line-endings-dos cmd-convert-to-dos)
  (register-command! 'show-line-endings cmd-show-line-endings)
  (register-command! 'wrap-lines-at-column cmd-wrap-lines-at-column)
  (register-command! 'strip-line-numbers cmd-strip-line-numbers)
  (register-command! 'goto-word-at-point cmd-goto-word-at-point)
  (register-command! 'unindent-region cmd-unindent-region)
  (register-command! 'number-region cmd-number-region)
  (register-command! 'insert-kbd-macro cmd-insert-kbd-macro)
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'show-environment cmd-show-environment)
  (register-command! 'show-keybinding-for cmd-show-keybinding-for)
  (register-command! 'first-error cmd-first-error)
  (register-command! 'find-grep cmd-find-grep)
  (register-command! 'project-grep cmd-project-grep)
  (register-command! 'project-find-file cmd-project-find-file)
  (register-command! 'project-compile cmd-project-compile)
  (register-command! 'reindent-buffer cmd-reindent-buffer)
  (register-command! 'fill-individual-paragraphs cmd-fill-individual-paragraphs)
  ;; Batch 8: Font size
  (register-command! 'increase-font-size cmd-increase-font-size)
  (register-command! 'decrease-font-size cmd-decrease-font-size)
  (register-command! 'reset-font-size cmd-reset-font-size)
  ;; Navigation
  (register-command! 'goto-first-non-blank cmd-goto-first-non-blank)
  (register-command! 'goto-last-non-blank cmd-goto-last-non-blank)
  (register-command! 'move-to-window-top cmd-move-to-window-top)
  (register-command! 'move-to-window-middle cmd-move-to-window-middle)
  (register-command! 'move-to-window-bottom cmd-move-to-window-bottom)
  (register-command! 'recenter-bottom cmd-recenter-bottom)
  (register-command! 'scroll-left cmd-scroll-left)
  (register-command! 'scroll-right cmd-scroll-right)
  ;; Code insertion templates
  (register-command! 'insert-let cmd-insert-let)
  (register-command! 'insert-lambda cmd-insert-lambda)
  (register-command! 'insert-defun cmd-insert-defun)
  (register-command! 'insert-cond cmd-insert-cond)
  (register-command! 'insert-when cmd-insert-when)
  (register-command! 'insert-unless cmd-insert-unless)
  (register-command! 'insert-match cmd-insert-match)
  (register-command! 'insert-import cmd-insert-import)
  (register-command! 'insert-export cmd-insert-export)
  (register-command! 'insert-include cmd-insert-include)
  (register-command! 'insert-file-header cmd-insert-file-header)
  (register-command! 'insert-header-guard cmd-insert-header-guard)
  (register-command! 'insert-box-comment cmd-insert-box-comment)
  (register-command! 'insert-file-contents cmd-insert-file-contents)
  (register-command! 'insert-register-string cmd-insert-register-string)
  ;; Toggles
  (register-command! 'show-dir-locals cmd-show-dir-locals)
  (register-command! 'toggle-auto-indent cmd-toggle-auto-indent)
  (register-command! 'toggle-backup-files cmd-toggle-backup-files)
  (register-command! 'toggle-debug-mode cmd-toggle-debug-mode)
  (register-command! 'toggle-debug-on-quit cmd-toggle-debug-on-quit)
  (register-command! 'toggle-visible-bell cmd-toggle-visible-bell)
  (register-command! 'toggle-transient-mark cmd-toggle-transient-mark)
  (register-command! 'toggle-electric-indent cmd-toggle-electric-indent)
  (register-command! 'toggle-auto-revert cmd-toggle-auto-revert)
  (register-command! 'toggle-auto-revert-global cmd-toggle-auto-revert-global)
  (register-command! 'auto-revert-tail-mode cmd-auto-revert-tail-mode)
  (register-command! 'toggle-frame-fullscreen cmd-toggle-frame-fullscreen)
  (register-command! 'toggle-frame-maximized cmd-toggle-frame-maximized)
  (register-command! 'toggle-menu-bar cmd-toggle-menu-bar)
  (register-command! 'toggle-menu-bar-mode cmd-toggle-menu-bar-mode)
  (register-command! 'toggle-tool-bar cmd-toggle-tool-bar)
  (register-command! 'toggle-scroll-bar cmd-toggle-scroll-bar)
  (register-command! 'toggle-tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'set-scroll-margin cmd-set-scroll-margin)
  (register-command! 'toggle-scroll-margin cmd-toggle-scroll-margin)
  (register-command! 'toggle-input-method cmd-toggle-input-method)
  (register-command! 'toggle-eol-conversion cmd-toggle-eol-conversion)
  (register-command! 'toggle-flymake cmd-toggle-flymake)
  (register-command! 'toggle-flyspell cmd-toggle-flyspell)
  (register-command! 'toggle-lsp cmd-toggle-lsp)
  (register-command! 'toggle-global-hl-line cmd-toggle-global-hl-line)
  (register-command! 'toggle-global-whitespace cmd-toggle-global-whitespace)
  (register-command! 'toggle-show-spaces cmd-toggle-show-spaces)
  (register-command! 'toggle-show-trailing-whitespace cmd-toggle-show-trailing-whitespace)
  (register-command! 'toggle-narrow-indicator cmd-toggle-narrow-indicator)
  (register-command! 'toggle-auto-complete cmd-toggle-auto-complete)
  ;; Windows
  (register-command! 'split-window-below cmd-split-window-below)
  (register-command! 'delete-window-below cmd-delete-window-below)
  (register-command! 'fit-window-to-buffer cmd-fit-window-to-buffer)
  (register-command! 'shrink-window-if-larger-than-buffer cmd-shrink-window-if-larger-than-buffer)
  (register-command! 'resize-window-width cmd-resize-window-width)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'delete-frame cmd-delete-frame)
  (register-command! 'suspend-frame cmd-suspend-frame)
  ;; Editing
  (register-command! 'center-region cmd-center-region)
  (register-command! 'indent-rigidly cmd-indent-rigidly)
  (register-command! 'dedent-rigidly cmd-dedent-rigidly)
  (register-command! 'fixup-whitespace cmd-fixup-whitespace)
  (register-command! 'electric-newline-and-indent cmd-electric-newline-and-indent)
  (register-command! 'kebab-to-camel cmd-kebab-to-camel)
  (register-command! 'flush-lines-region cmd-flush-lines-region)
  (register-command! 'keep-lines-region cmd-keep-lines-region)
  ;; VCS
  (register-command! 'vc-annotate cmd-vc-annotate)
  (register-command! 'vc-diff-head cmd-vc-diff-head)
  (register-command! 'vc-log-file cmd-vc-log-file)
  (register-command! 'vc-revert cmd-vc-revert)
  ;; Search
  (register-command! 'isearch-forward-word cmd-isearch-forward-word)
  (register-command! 'isearch-backward-word cmd-isearch-backward-word)
  (register-command! 'isearch-forward-symbol cmd-isearch-forward-symbol)
  (register-command! 'mark-lines-matching cmd-mark-lines-matching)
  ;; Buffer/undo
  (register-command! 'buffer-disable-undo cmd-buffer-disable-undo)
  (register-command! 'buffer-enable-undo cmd-buffer-enable-undo)
  (register-command! 'lock-buffer cmd-lock-buffer)
  (register-command! 'auto-revert-mode cmd-auto-revert-mode)
  ;; Registers
  (register-command! 'append-to-register cmd-append-to-register)
  ;; Completion
  (register-command! 'complete-filename cmd-complete-filename)
  (register-command! 'completion-at-point cmd-completion-at-point)
  ;; Info/Help
  (register-command! 'info cmd-info)
  (register-command! 'info-emacs-manual cmd-info-emacs-manual)
  (register-command! 'info-elisp-manual cmd-info-elisp-manual)
  (register-command! 'report-bug cmd-report-bug)
  (register-command! 'memory-report cmd-memory-report)
  (register-command! 'view-echo-area-messages cmd-view-echo-area-messages)
  ;; Spelling
  (register-command! 'ispell-word cmd-ispell-word)
  (register-command! 'ispell-region cmd-ispell-region)
  (register-command! 'ispell-buffer cmd-ispell-buffer)
  ;; Abbreviations
  (register-command! 'abbrev-mode cmd-abbrev-mode)
  (register-command! 'define-abbrev cmd-define-abbrev)
  (register-command! 'expand-abbrev cmd-expand-abbrev)
  (register-command! 'list-abbrevs cmd-list-abbrevs)
  ;; Calendar
  ;; Key rebinding
  (register-command! 'global-set-key cmd-global-set-key)
  (register-command! 'global-unset-key cmd-global-unset-key)
  ;; Man page viewer
  (register-command! 'man cmd-man)
  ;; Web browser
  (register-command! 'eww cmd-eww)
  (register-command! 'eww-back cmd-eww-back)
  (register-command! 'eww-reload cmd-eww-reload)
  ;; Remote file editing
  (register-command! 'find-file-remote cmd-find-file-remote)
  (register-command! 'save-remote-buffer cmd-save-remote-buffer)
  ;; Calendar
  (register-command! 'calendar cmd-calendar)
  (register-command! 'calendar-prev-month cmd-calendar-prev-month)
  (register-command! 'calendar-next-month cmd-calendar-next-month)
  (register-command! 'calendar-prev-year cmd-calendar-prev-year)
  (register-command! 'calendar-next-year cmd-calendar-next-year)
  (register-command! 'calendar-today cmd-calendar-today)
  ;; Misc
  (register-command! 'display-fill-column-indicator cmd-display-fill-column-indicator)
  (register-command! 'display-line-numbers-relative cmd-display-line-numbers-relative)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'customize-face cmd-customize-face)
  (register-command! 'set-frame-font cmd-set-frame-font)
  (register-command! 'set-font-size cmd-set-font-size)
  (register-command! 'list-colors cmd-list-colors)
  (register-command! 'load-theme cmd-load-theme)
  (register-command! 'describe-theme cmd-describe-theme)
  (register-command! 'fold-level cmd-fold-level)
  (register-command! 'ansi-term cmd-ansi-term)
  (register-command! 'diff-backup cmd-diff-backup)
  (register-command! 'dired-do-chmod cmd-dired-do-chmod)
  (register-command! 'eldoc cmd-eldoc)
  (register-command! 'recover-session cmd-recover-session)
  (register-command! 'revert-buffer-with-coding cmd-revert-buffer-with-coding)
  (register-command! 'set-buffer-file-coding cmd-set-buffer-file-coding)
  (register-command! 'set-language-environment cmd-set-language-environment)
  (register-command! 'sudo-find-file cmd-sudo-find-file)
  (register-command! 'which-function cmd-which-function)
  (register-command! 'widen-all cmd-widen-all)
  (register-command! 'whitespace-mode cmd-whitespace-mode)
  (register-command! 'profiler-start cmd-profiler-start)
  (register-command! 'profiler-stop cmd-profiler-stop)
  (register-command! 'show-tab-count cmd-show-tab-count)
  (register-command! 'show-trailing-whitespace-count cmd-show-trailing-whitespace-count)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit)
  ;; Session persistence
  (register-command! 'session-save cmd-session-save)
  (register-command! 'session-restore cmd-session-restore)
  ;; Init file
  (register-command! 'load-init-file cmd-load-init-file)
  (register-command! 'find-init-file cmd-find-init-file)
  ;; Magit
  (register-command! 'magit-status cmd-magit-status)
  (register-command! 'magit-stage cmd-magit-stage)
  (register-command! 'magit-unstage cmd-magit-unstage)
  (register-command! 'magit-commit cmd-magit-commit)
  (register-command! 'magit-diff cmd-magit-diff)
  (register-command! 'magit-stage-all cmd-magit-stage-all)
  (register-command! 'magit-log cmd-magit-log)
  (register-command! 'magit-refresh cmd-magit-refresh)
  (register-command! 'magit-blame cmd-magit-blame)
  (register-command! 'magit-fetch cmd-magit-fetch)
  (register-command! 'magit-pull cmd-magit-pull)
  (register-command! 'magit-push cmd-magit-push)
  (register-command! 'magit-rebase cmd-magit-rebase)
  (register-command! 'magit-merge cmd-magit-merge)
  (register-command! 'magit-stash cmd-magit-stash)
  (register-command! 'magit-branch cmd-magit-branch)
  (register-command! 'magit-checkout cmd-magit-checkout)
  ;; Org-mode
  (register-command! 'org-mode cmd-org-mode)
  (register-command! 'org-todo-cycle cmd-org-todo-cycle)
  (register-command! 'org-promote cmd-org-promote)
  (register-command! 'org-demote cmd-org-demote)
  (register-command! 'org-toggle-checkbox cmd-org-toggle-checkbox)
  (register-command! 'org-insert-heading cmd-org-insert-heading)
  (register-command! 'org-next-heading cmd-org-next-heading)
  (register-command! 'org-prev-heading cmd-org-prev-heading)
  (register-command! 'org-move-subtree-up cmd-org-move-subtree-up)
  (register-command! 'org-move-subtree-down cmd-org-move-subtree-down)
  (register-command! 'org-outline cmd-org-outline)
  (register-command! 'org-table-align cmd-org-table-align)
  (register-command! 'org-table-insert-row cmd-org-table-insert-row)
  (register-command! 'org-table-delete-row cmd-org-table-delete-row)
  (register-command! 'org-table-move-row-up cmd-org-table-move-row-up)
  (register-command! 'org-table-move-row-down cmd-org-table-move-row-down)
  (register-command! 'org-table-delete-column cmd-org-table-delete-column)
  (register-command! 'org-table-insert-column cmd-org-table-insert-column)
  (register-command! 'org-table-move-column-left cmd-org-table-move-column-left)
  (register-command! 'org-table-move-column-right cmd-org-table-move-column-right)
  (register-command! 'org-table-insert-separator cmd-org-table-insert-separator)
  (register-command! 'org-table-sort cmd-org-table-sort)
  (register-command! 'org-table-sum cmd-org-table-sum)
  (register-command! 'org-table-recalculate cmd-org-table-recalculate)
  (register-command! 'org-table-create cmd-org-table-create)
  (register-command! 'org-table-export-csv cmd-org-table-export-csv)
  (register-command! 'org-table-import-csv cmd-org-table-import-csv)
  (register-command! 'org-table-transpose cmd-org-table-transpose)
  (register-command! 'org-template-expand
    (lambda (app) (qt-try-org-template-expand app)))
  ;; Markdown
  (register-command! 'markdown-promote cmd-markdown-promote)
  (register-command! 'markdown-demote cmd-markdown-demote)
  (register-command! 'markdown-next-heading cmd-markdown-next-heading)
  (register-command! 'markdown-prev-heading cmd-markdown-prev-heading)
  (register-command! 'markdown-insert-heading cmd-markdown-insert-heading)
  (register-command! 'markdown-toggle-bold cmd-markdown-toggle-bold)
  (register-command! 'markdown-toggle-italic cmd-markdown-toggle-italic)
  (register-command! 'markdown-toggle-code cmd-markdown-toggle-code)
  (register-command! 'markdown-insert-link cmd-markdown-insert-link)
  (register-command! 'markdown-insert-code-block cmd-markdown-insert-code-block)
  (register-command! 'markdown-toggle-checkbox cmd-markdown-toggle-checkbox)
  (register-command! 'markdown-outline cmd-markdown-outline)
  (register-command! 'markdown-preview cmd-markdown-preview)
  ;; Snippets
  (register-command! 'snippet-expand cmd-snippet-expand)
  (register-command! 'snippet-next-field cmd-snippet-next-field)
  (register-command! 'snippet-prev-field cmd-snippet-prev-field)
  (register-command! 'define-snippet cmd-define-snippet)
  (register-command! 'list-snippets cmd-list-snippets)
  ;; Winner mode
  (register-command! 'winner-undo cmd-winner-undo)
  (register-command! 'winner-redo cmd-winner-redo)
  ;; View mode
  (register-command! 'view-mode cmd-view-mode)
  ;; So-long mode
  (register-command! 'so-long-mode cmd-so-long-mode)
  ;; Follow mode
  (register-command! 'follow-mode cmd-follow-mode)
  ;; IBBuffer
  (register-command! 'ibuffer cmd-ibuffer)
  ;; WDired
  (register-command! 'wdired-mode cmd-wdired-mode)
  (register-command! 'wdired-finish cmd-wdired-finish)
  ;; Auto-fill mode (alias)
  (register-command! 'auto-fill-mode cmd-toggle-auto-fill)
  ;; Delete trailing whitespace
  (register-command! 'toggle-delete-trailing-whitespace-on-save cmd-toggle-delete-trailing-whitespace-on-save)
  (register-command! 'toggle-save-place-mode cmd-toggle-save-place-mode)
  (register-command! 'save-place-mode cmd-toggle-save-place-mode)
  (register-command! 'toggle-require-final-newline cmd-toggle-require-final-newline)
  (register-command! 'toggle-centered-cursor-mode cmd-toggle-centered-cursor-mode)
  (register-command! 'centered-cursor-mode cmd-toggle-centered-cursor-mode)
  ;; Delete horizontal space
  (register-command! 'delete-horizontal-space cmd-delete-horizontal-space)
  ;; Recentf open files
  (register-command! 'recentf-open-files cmd-recentf-open-files)
  ;; Ediff files
  (register-command! 'ediff-files cmd-ediff-files)
  ;; Comment-dwim
  (register-command! 'comment-dwim cmd-comment-dwim)
  ;; Auto-save mode
  (register-command! 'auto-save-mode cmd-auto-save-mode)
  ;; Keyboard macro counter
  (register-command! 'kbd-macro-counter-insert cmd-kbd-macro-counter-insert)
  (register-command! 'kbd-macro-counter-set cmd-kbd-macro-counter-set)
  ;; Dired enhancements
  (register-command! 'dired-mark cmd-dired-mark)
  (register-command! 'dired-unmark cmd-dired-unmark)
  (register-command! 'dired-unmark-all cmd-dired-unmark-all)
  (register-command! 'dired-toggle-marks cmd-dired-toggle-marks)
  (register-command! 'dired-do-delete-marked cmd-dired-do-delete-marked)
  (register-command! 'dired-do-copy-marked cmd-dired-do-copy-marked)
  (register-command! 'dired-do-rename-marked cmd-dired-do-rename-marked)
  (register-command! 'dired-mark-by-regexp cmd-dired-mark-by-regexp)
  (register-command! 'dired-sort-toggle cmd-dired-sort-toggle)
  ;; Global mark ring
  (register-command! 'pop-global-mark cmd-pop-global-mark)
  ;; Window horizontal resize
  (register-command! 'shrink-window-horizontally cmd-shrink-window-horizontally)
  (register-command! 'enlarge-window-horizontally cmd-enlarge-window-horizontally)
  ;; Recover file
  (register-command! 'recover-file cmd-recover-file)
  ;; Insert char by name
  (register-command! 'insert-char-by-name cmd-insert-char-by-name)
  ;; System info
  (register-command! 'display-battery cmd-display-battery)
  ;; Scratch
  (register-command! 'scratch-message cmd-scratch-message)
  ;; Kill sentence / paragraph
  (register-command! 'kill-sentence cmd-kill-sentence)
  (register-command! 'backward-kill-sentence cmd-backward-kill-sentence)
  (register-command! 'kill-paragraph cmd-kill-paragraph)
  ;; Recenter cycling
  (register-command! 'recenter-top-bottom cmd-recenter-top-bottom)
  ;; Sexp list navigation
  (register-command! 'up-list cmd-up-list)
  (register-command! 'down-list cmd-down-list)
  ;; Windmove
  (register-command! 'windmove-left cmd-windmove-left)
  (register-command! 'windmove-right cmd-windmove-right)
  (register-command! 'windmove-up cmd-windmove-up)
  (register-command! 'windmove-down cmd-windmove-down)
  ;; Variable customization
  (register-command! 'set-variable cmd-set-variable)
  (register-command! 'customize-variable cmd-customize-variable)
  ;; View-file and append-to-file
  (register-command! 'view-file cmd-view-file)
  (register-command! 'append-to-file cmd-append-to-file)
  ;; Spell check buffer
  (register-command! 'flyspell-buffer cmd-flyspell-buffer)
  ;; Profiler
  (register-command! 'profiler-report cmd-profiler-report)
  ;; Faces and line numbers
  (register-command! 'list-faces-display cmd-list-faces-display)
  (register-command! 'display-line-numbers-mode cmd-display-line-numbers-mode)
  ;; Find file read-only
  (register-command! 'find-file-read-only cmd-find-file-read-only)
  ;; Project commands
  (register-command! 'project-switch-project cmd-project-switch-project)
  (register-command! 'project-dired cmd-project-dired)
  (register-command! 'project-run-shell cmd-project-run-shell)
  ;; Global auto-revert
  (register-command! 'global-auto-revert-mode cmd-global-auto-revert-mode)
  ;; Project search
  (register-command! 'project-search cmd-project-search)
  ;; Goto last change
  (register-command! 'goto-last-change cmd-goto-last-change)
  ;; Diff-HL (git diff at point)
  (register-command! 'diff-hl-mode cmd-diff-hl-mode)
  ;; Pop-to-mark
  (register-command! 'pop-to-mark cmd-pop-to-mark)
  ;; Scratch buffer new
  (register-command! 'scratch-buffer-new cmd-scratch-buffer-new)
  ;; Duplicate line/region
  (register-command! 'duplicate-line-or-region cmd-duplicate-line-or-region)
  ;; Select current line
  (register-command! 'select-current-line cmd-select-current-line)
  ;; Smart join line
  (register-command! 'smart-join-line cmd-smart-join-line)
  ;; Copy buffer filename
  (register-command! 'copy-buffer-filename cmd-copy-buffer-filename)
  ;; Revert buffer confirm
  (register-command! 'revert-buffer-confirm cmd-revert-buffer-confirm)
  ;; Find file at line
  (register-command! 'find-file-at-line cmd-find-file-at-line)
  ;; Toggle line comment
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  ;; Window resize
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  (register-command! 'enlarge-window-horizontally cmd-enlarge-window)
  (register-command! 'shrink-window-horizontally cmd-shrink-window)
  (register-command! 'balance-windows cmd-balance-windows)
  ;; Key-chord commands
  (register-command! 'key-chord-mode cmd-key-chord-mode)
  (register-command! 'key-chord-define cmd-key-chord-define)
  (register-command! 'key-chord-list cmd-key-chord-list)
  ;; Key translation commands
  (register-command! 'toggle-bracket-paren-swap cmd-toggle-bracket-paren-swap)
  (register-command! 'key-translation-list cmd-key-translation-list)
  ;; Multi-terminal commands
  (register-command! 'multi-vterm cmd-multi-vterm)
  (register-command! 'vterm-copy-mode cmd-vterm-copy-mode)
  (register-command! 'vterm-copy-done cmd-vterm-copy-done)
  ;; Image mode commands
  (register-command! 'image-zoom-in cmd-image-zoom-in)
  (register-command! 'image-zoom-out cmd-image-zoom-out)
  (register-command! 'image-zoom-fit cmd-image-zoom-fit)
  (register-command! 'image-zoom-reset cmd-image-zoom-reset)
  ;; LSP commands
  (register-command! 'lsp cmd-toggle-lsp)   ; alias: M-x lsp
  (register-command! 'lsp-goto-definition cmd-lsp-goto-definition)
  (register-command! 'lsp-declaration cmd-lsp-declaration)
  (register-command! 'lsp-type-definition cmd-lsp-type-definition)
  (register-command! 'lsp-implementation cmd-lsp-implementation)
  (register-command! 'lsp-hover cmd-lsp-hover)
  (register-command! 'lsp-completion cmd-lsp-completion)
  (register-command! 'lsp-rename cmd-lsp-rename)
  (register-command! 'lsp-code-actions cmd-lsp-code-actions)
  (register-command! 'lsp-find-references cmd-lsp-find-references)
  (register-command! 'lsp-document-symbols cmd-lsp-document-symbols)
  (register-command! 'lsp-workspace-symbol cmd-lsp-workspace-symbol)
  (register-command! 'lsp-format-buffer cmd-lsp-format-buffer)
  (register-command! 'lsp-restart cmd-lsp-restart)
  (register-command! 'lsp-stop cmd-lsp-stop)
  (register-command! 'lsp-smart-goto-definition cmd-lsp-smart-goto-definition)
  ;; LSP keybindings in C-c l prefix map
  (keymap-bind! *ctrl-c-l-map* "d" 'lsp-goto-definition)
  (keymap-bind! *ctrl-c-l-map* "D" 'lsp-declaration)
  (keymap-bind! *ctrl-c-l-map* "h" 'lsp-hover)
  (keymap-bind! *ctrl-c-l-map* "c" 'lsp-completion)
  (keymap-bind! *ctrl-c-l-map* "r" 'lsp-rename)
  (keymap-bind! *ctrl-c-l-map* "a" 'lsp-code-actions)
  (keymap-bind! *ctrl-c-l-map* "R" 'lsp-find-references)
  (keymap-bind! *ctrl-c-l-map* "s" 'lsp-document-symbols)
  (keymap-bind! *ctrl-c-l-map* "S" 'lsp-workspace-symbol)
  (keymap-bind! *ctrl-c-l-map* "f" 'lsp-format-buffer)
  (keymap-bind! *ctrl-c-l-map* "t" 'lsp-type-definition)
  (keymap-bind! *ctrl-c-l-map* "i" 'lsp-implementation)
  (keymap-bind! *ctrl-c-l-map* "=" 'lsp-restart)
  (keymap-bind! *ctrl-c-l-map* "q" 'lsp-stop)
  ;; M-. smart dispatch: LSP when running, else text search
  (keymap-bind! *global-keymap* "M-." 'lsp-smart-goto-definition)
  ;; Multiple cursor keybindings in C-c m prefix map
  (keymap-bind! *ctrl-c-map* "m" *ctrl-c-m-map*)
  (keymap-bind! *ctrl-c-m-map* "n" 'mc-mark-next)
  (keymap-bind! *ctrl-c-m-map* "a" 'mc-mark-all)
  (keymap-bind! *ctrl-c-m-map* "s" 'mc-skip-and-mark-next)
  (keymap-bind! *ctrl-c-m-map* "l" 'mc-edit-lines)
  (keymap-bind! *ctrl-c-m-map* "u" 'mc-unmark-last)
  (keymap-bind! *ctrl-c-m-map* "r" 'mc-rotate)
  ;; fill-region, insert-buffer, prepend-to-buffer, copy-rectangle-to-register
  (register-command! 'fill-region cmd-fill-region)
  (register-command! 'insert-buffer cmd-insert-buffer)
  (register-command! 'prepend-to-buffer cmd-prepend-to-buffer)
  (register-command! 'copy-rectangle-to-register cmd-copy-rectangle-to-register)
  )

;;;============================================================================
;;; Key-chord commands
;;;============================================================================

(def (cmd-key-chord-mode app)
  "Toggle key-chord detection mode."
  (set! *chord-mode* (not *chord-mode*))
  (echo-message! (app-state-echo app)
    (if *chord-mode* "Key-chord mode enabled" "Key-chord mode disabled")))

(def (cmd-key-chord-define app)
  "Define a new key chord binding interactively."
  (let ((chord-str (qt-echo-read-string app "Chord (2 chars): ")))
    (when (and chord-str (= (string-length chord-str) 2))
      (let ((cmd-name (qt-echo-read-string app "Command: ")))
        (when (and cmd-name (> (string-length cmd-name) 0))
          (let ((sym (string->symbol cmd-name)))
            (if (find-command sym)
              (begin
                (key-chord-define-global chord-str sym)
                (echo-message! (app-state-echo app)
                  (string-append (string-upcase chord-str) " → " cmd-name)))
              (echo-error! (app-state-echo app)
                (string-append "Unknown command: " cmd-name)))))))))

(def (cmd-key-chord-list app)
  "List all defined key chord bindings."
  (let* ((entries (hash->list *chord-map*))
         (sorted (sort entries (lambda (a b) (string<? (car a) (car b)))))
         (lines (map (lambda (e)
                       (string-append "  " (car e) " → "
                                      (symbol->string (cdr e))))
                     sorted))
         (text (if (null? lines)
                 "No key chords defined."
                 (string-append "Key Chords:\n"
                                (string-join lines "\n")))))
    (echo-message! (app-state-echo app) text)))

;;;============================================================================
;;; Key translation commands
;;;============================================================================

;; State for the bracket/paren swap toggle
(def *bracket-paren-swapped* #f)

(def (cmd-toggle-bracket-paren-swap app)
  "Toggle swapping [ ↔ ( and ] ↔ ) for Lisp editing."
  (if *bracket-paren-swapped*
    ;; Remove the translations
    (begin
      (hash-remove! *key-translation-map* #\[)
      (hash-remove! *key-translation-map* #\])
      (hash-remove! *key-translation-map* #\()
      (hash-remove! *key-translation-map* #\))
      (set! *bracket-paren-swapped* #f)
      (echo-message! (app-state-echo app) "Bracket/paren swap disabled"))
    ;; Install the translations
    (begin
      (key-translate! #\[ #\()
      (key-translate! #\] #\))
      (key-translate! #\( #\[)
      (key-translate! #\) #\])
      (set! *bracket-paren-swapped* #t)
      (echo-message! (app-state-echo app) "Bracket/paren swap enabled"))))

(def (cmd-key-translation-list app)
  "List all active key translations."
  (let* ((entries (hash->list *key-translation-map*))
         (sorted (sort entries (lambda (a b) (char<? (car a) (car b)))))
         (lines (map (lambda (e)
                       (string-append "  " (string (car e)) " → "
                                      (string (cdr e))))
                     sorted))
         (text (if (null? lines)
                 "No key translations active."
                 (string-append "Key Translations:\n"
                                (string-join lines "\n")))))
    (echo-message! (app-state-echo app) text)))

;;;============================================================================
;;; Image mode commands
;;;============================================================================

(def (cmd-image-zoom-in app)
  "Zoom in on the current image buffer."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (ed (qt-current-editor fr)))
    (when (image-buffer? buf)
      (qt-image-zoom! app ed buf 1.25))))

(def (cmd-image-zoom-out app)
  "Zoom out on the current image buffer."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (ed (qt-current-editor fr)))
    (when (image-buffer? buf)
      (qt-image-zoom! app ed buf 0.8))))

(def (cmd-image-zoom-fit app)
  "Fit image to window."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (ed (qt-current-editor fr)))
    (when (image-buffer? buf)
      (qt-image-zoom! app ed buf 'fit))))

(def (cmd-image-zoom-reset app)
  "Reset image to 100% zoom."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (ed (qt-current-editor fr)))
    (when (image-buffer? buf)
      (qt-image-zoom! app ed buf 'reset))))
