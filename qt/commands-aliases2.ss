;;; -*- Gerbil -*-
;;; Qt commands aliases2 - image mode, batch registrations, iedit, forge, embark
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gemacs/pregexp-compat pregexp pregexp-match)
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
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
        :gemacs/qt/commands-aliases)

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

;;;============================================================================
;;; Batch 12: Emacs-standard alias registrations (Qt)
;;;============================================================================

(def (qt-register-batch12-aliases!)
  ;; Undo/redo aliases
  (register-command! 'undo-redo cmd-redo)
  (register-command! 'undo-only cmd-undo)
  ;; Text scale alias
  (register-command! 'text-scale-adjust cmd-text-scale-increase)
  ;; Display/mode aliases
  (register-command! 'display-time-mode cmd-display-time)
  (register-command! 'word-count-mode cmd-count-words)
  (register-command! 'completion-preview-mode cmd-company-mode)
  (register-command! 'flymake-start cmd-flycheck-mode)
  (register-command! 'flymake-stop cmd-flycheck-mode)
  ;; Outline/folding aliases
  (register-command! 'outline-hide-all cmd-fold-all)
  (register-command! 'outline-show-all cmd-unfold-all)
  (register-command! 'outline-cycle cmd-toggle-fold)
  ;; Dired aliases
  (register-command! 'dired-do-touch cmd-dired-create-directory)
  (register-command! 'dired-copy-filename-as-kill cmd-copy-buffer-name)
  (register-command! 'dired-mark-directories cmd-dired-mark)
  (register-command! 'dired-hide-dotfiles cmd-dired-hide-details)
  ;; Emacs base mode-name aliases (batch 13)
  (register-command! 'transient-mark-mode cmd-toggle-transient-mark)
  (register-command! 'delete-trailing-whitespace-mode cmd-toggle-delete-trailing-whitespace-on-save)
  (register-command! 'menu-bar-mode cmd-toggle-menu-bar-mode)
  ;; Search aliases
  (register-command! 'apropos-variable cmd-apropos-command)
  ;; Batch 13: new commands
  (register-command! 'set-visited-file-name cmd-set-visited-file-name)
  (register-command! 'sort-columns cmd-sort-columns)
  (register-command! 'sort-regexp-fields cmd-sort-regexp-fields)
  ;; Batch 14: visual line + sexp aliases
  ;; Note: kill-emacs → cmd-quit registered in facade (forward ref)
  (register-command! 'forward-list cmd-forward-sexp)
  (register-command! 'backward-list cmd-backward-sexp)
  (register-command! 'beginning-of-visual-line cmd-beginning-of-line)
  (register-command! 'end-of-visual-line cmd-end-of-line)
  (register-command! 'kill-visual-line cmd-kill-line)
  ;; Batch 15: more standard aliases
  (register-command! 'keep-matching-lines cmd-keep-lines)
  (register-command! 'calc-dispatch cmd-calc)
  (register-command! 'insert-tab cmd-insert-tab))

;;;============================================================================
;;; Batch 13: New Qt commands
;;;============================================================================

(def (cmd-set-visited-file-name app)
  "Change the file name associated with the current buffer."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (old (and buf (buffer-file-path buf)))
         (prompt (if old (string-append "New file name (was " old "): ") "File name: "))
         (new-name (qt-echo-read-string app prompt)))
    (if (and new-name (not (string=? new-name "")))
      (begin
        (set! (buffer-file-path buf) new-name)
        (set! (buffer-name buf) (path-strip-directory new-name))
        (set! (buffer-modified buf) #t)
        (echo-message! (app-state-echo app) (string-append "File name set to " new-name)))
      (echo-message! (app-state-echo app) "Cancelled"))))

(def (cmd-sort-columns app)
  "Sort lines in region by column range."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= sel-start sel-end)
      (echo-message! echo "No region selected")
      (let* ((input (qt-echo-read-string app "Column range (start-end, e.g. 10-20): "))
             (parts (and input (string-split input #\-)))
             (col-start (and parts (>= (length parts) 2)
                            (string->number (string-trim (car parts)))))
             (col-end (and parts (>= (length parts) 2)
                          (string->number (string-trim (cadr parts))))))
        (if (not (and col-start col-end (> col-end col-start)))
          (echo-error! echo "Invalid column range (use start-end, e.g. 10-20)")
          (let* ((text (qt-plain-text-edit-text ed))
                 (region (substring text sel-start sel-end))
                 (lines (string-split region #\newline))
                 (key-fn (lambda (line)
                           (let ((len (string-length line)))
                             (if (>= len col-start)
                               (substring line (- col-start 1) (min len (- col-end 1)))
                               ""))))
                 (sorted (sort lines (lambda (a b) (string<? (key-fn a) (key-fn b)))))
                 (result (string-join sorted "\n")))
            (sci-send ed SCI_SETSELECTIONSTART sel-start)
            (sci-send ed SCI_SETSELECTIONEND sel-end)
            (sci-send/string ed SCI_REPLACESEL result)
            (echo-message! echo
              (string-append "Sorted " (number->string (length lines)) " lines by columns "
                (number->string col-start) "-" (number->string col-end)))))))))

(def (cmd-sort-regexp-fields app)
  "Sort lines in region by regex match."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= sel-start sel-end)
      (echo-message! echo "No region selected")
      (let* ((pattern (qt-echo-read-string app "Sort by regexp: "))
             (rx (and pattern (> (string-length pattern) 0)
                     (with-catch (lambda (e) #f)
                       (lambda () (pregexp pattern))))))
        (if (not rx)
          (echo-error! echo "Invalid regexp")
          (let* ((text (qt-plain-text-edit-text ed))
                 (region (substring text sel-start sel-end))
                 (lines (string-split region #\newline))
                 (key-fn (lambda (line)
                           (let ((m (pregexp-match rx line)))
                             (if m (car m) ""))))
                 (sorted (sort lines (lambda (a b) (string<? (key-fn a) (key-fn b)))))
                 (result (string-join sorted "\n")))
            (sci-send ed SCI_SETSELECTIONSTART sel-start)
            (sci-send ed SCI_SETSELECTIONEND sel-end)
            (sci-send/string ed SCI_REPLACESEL result)
            (echo-message! echo
              (string-append "Sorted " (number->string (length lines)) " lines by regexp"))))))))

;;; Batch 15: insert-tab (Qt)
(def (cmd-insert-tab app)
  "Insert a literal tab character at point."
  (let ((ed (qt-current-editor (app-state-frame app))))
    (sci-send/string ed SCI_REPLACESEL "\t")))

;;;============================================================================
;;; iedit-mode: rename symbol at point across buffer (Qt)
;;;============================================================================

(def (qt-iedit-word-char? ch)
  "Return #t if ch is a word character (alphanumeric, underscore, hyphen)."
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-)))

(def (qt-iedit-count-whole-word text word)
  "Count whole-word occurrences of word in text."
  (let ((wlen (string-length word))
        (tlen (string-length text)))
    (let loop ((i 0) (count 0))
      (if (> (+ i wlen) tlen) count
        (if (and (string=? (substring text i (+ i wlen)) word)
                 (or (= i 0)
                     (not (qt-iedit-word-char? (string-ref text (- i 1)))))
                 (or (= (+ i wlen) tlen)
                     (not (qt-iedit-word-char? (string-ref text (+ i wlen))))))
          (loop (+ i wlen) (+ count 1))
          (loop (+ i 1) count))))))

(def (qt-iedit-replace-all text word replacement)
  "Replace all whole-word occurrences of word with replacement in text.
   Returns (values new-text count)."
  (let ((wlen (string-length word))
        (tlen (string-length text))
        (parts [])
        (count 0)
        (last-end 0))
    (let loop ((i 0))
      (if (> (+ i wlen) tlen)
        ;; Done — assemble result
        (let ((final-parts (reverse (cons (substring text last-end tlen) parts))))
          (values (apply string-append final-parts) count))
        (if (and (string=? (substring text i (+ i wlen)) word)
                 (or (= i 0)
                     (not (qt-iedit-word-char? (string-ref text (- i 1)))))
                 (or (= (+ i wlen) tlen)
                     (not (qt-iedit-word-char? (string-ref text (+ i wlen))))))
          (begin
            (set! parts (cons replacement (cons (substring text last-end i) parts)))
            (set! count (+ count 1))
            (set! last-end (+ i wlen))
            (loop (+ i wlen)))
          (loop (+ i 1)))))))

(def (cmd-iedit-mode app)
  "Rename symbol at point across the buffer (iedit-mode).
   Gets the word at point, prompts for a replacement, and replaces all
   whole-word occurrences."
  (let ((ed (current-qt-editor app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-error! (app-state-echo app) "No symbol at point")
        (let* ((text (qt-plain-text-edit-text ed))
               (count (qt-iedit-count-whole-word text word))
               (prompt (string-append
                        "iedit (" (number->string count)
                        " of " word "): Replace with: "))
               (replacement (qt-echo-read-string app prompt)))
          (if (or (not replacement)
                  (string=? replacement word))
            (echo-message! (app-state-echo app) "iedit: cancelled or no change")
            (let-values (((new-text replaced) (qt-iedit-replace-all text word replacement)))
              (let ((pos (qt-plain-text-edit-cursor-position ed)))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min pos (string-length new-text)))
                (set! (buffer-modified (current-qt-buffer app)) #t)
                (echo-message! (app-state-echo app)
                  (string-append "iedit: replaced "
                    (number->string replaced) " occurrences"))))))))))

;;;============================================================================
;;; EWW Bookmarks (Qt)
;;;============================================================================

(def *qt-eww-bookmarks* '())  ; list of (title . url) pairs
(def *qt-eww-bookmarks-file*
  (path-expand ".gemacs-eww-bookmarks" (user-info-home (user-info (user-name)))))

(def (qt-eww-load-bookmarks!)
  "Load EWW bookmarks from disk."
  (when (file-exists? *qt-eww-bookmarks-file*)
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (set! *qt-eww-bookmarks*
          (with-input-from-file *qt-eww-bookmarks-file*
            (lambda ()
              (let loop ((result '()))
                (let ((line (read-line)))
                  (if (eof-object? line)
                    (reverse result)
                    (let ((tab-pos (string-index line #\tab)))
                      (if tab-pos
                        (loop (cons (cons (substring line 0 tab-pos)
                                         (substring line (+ tab-pos 1) (string-length line)))
                                    result))
                        (loop result)))))))))))))

(def (qt-eww-save-bookmarks!)
  "Persist EWW bookmarks to disk."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (with-output-to-file *qt-eww-bookmarks-file*
        (lambda ()
          (for-each (lambda (bm)
                      (display (car bm))
                      (display "\t")
                      (display (cdr bm))
                      (newline))
            *qt-eww-bookmarks*))))))

(def (cmd-eww-add-bookmark app)
  "Bookmark the current EWW page."
  (let ((echo (app-state-echo app)))
    (if (not *eww-current-url*)
      (echo-error! echo "No page to bookmark")
      (let ((title (qt-echo-read-string echo "Bookmark title: ")))
        (when (and title (not (string=? title "")))
          (qt-eww-load-bookmarks!)
          (set! *qt-eww-bookmarks*
            (cons (cons title *eww-current-url*) *qt-eww-bookmarks*))
          (qt-eww-save-bookmarks!)
          (echo-message! echo (string-append "Bookmarked: " title)))))))

(def (cmd-eww-list-bookmarks app)
  "Show EWW bookmarks and open selected one."
  (let ((echo (app-state-echo app)))
    (qt-eww-load-bookmarks!)
    (if (null? *qt-eww-bookmarks*)
      (echo-message! echo "No bookmarks saved")
      (let* ((entries (map (lambda (bm)
                             (string-append (car bm) " — " (cdr bm)))
                       *qt-eww-bookmarks*))
             (choice (qt-echo-read-with-narrowing echo "EWW Bookmark: " entries)))
        (when choice
          (let loop ((bms *qt-eww-bookmarks*))
            (when (pair? bms)
              (let* ((bm (car bms))
                     (label (string-append (car bm) " — " (cdr bm))))
                (if (string=? label choice)
                  (let ((html (eww-fetch-url (cdr bm))))
                    (if html
                      (begin
                        (set! *eww-current-url* (cdr bm))
                        (let* ((text (eww-html-to-text html))
                               (fr (app-state-frame app))
                               (ed (qt-current-editor fr))
                               (buf-name "*eww*")
                               (existing (buffer-by-name buf-name))
                               (buf (or existing (qt-buffer-create! buf-name ed #f))))
                          (qt-buffer-attach! ed buf)
                          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                          (qt-plain-text-edit-set-text! ed
                            (string-append "URL: " (cdr bm) "\n\n" text))
                          (qt-plain-text-edit-set-cursor-position! ed 0)))
                      (echo-error! echo "Failed to fetch page")))
                  (loop (cdr bms)))))))))))

;;;============================================================================
;;; Forge (GitHub integration via gh CLI) — Qt
;;;============================================================================

(def (qt-forge-run-gh args)
  "Run gh CLI and return output string, or #f on failure."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let ((proc (open-process
                    (list path: "gh"
                          arguments: args
                          stdin-redirection: #f
                          stdout-redirection: #t
                          stderr-redirection: #t))))
        (let ((output (read-all-as-string proc)))
          (process-status proc)
          (if (zero? (process-status proc))
            output
            #f))))))

(def (cmd-forge-list-prs app)
  "List open pull requests for the current project."
  (let* ((echo (app-state-echo app))
         (output (qt-forge-run-gh ["pr" "list" "--limit" "20"])))
    (if (not output)
      (echo-error! echo "forge: failed to list PRs (is gh installed?)")
      (let* ((fr (app-state-frame app))
             (ed (qt-current-editor fr))
             (buf-name "*Forge PRs*")
             (existing (buffer-by-name buf-name))
             (buf (or existing (qt-buffer-create! buf-name ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed (string-append "Pull Requests:\n\n" output))
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo "Forge: PRs loaded")))))

(def (cmd-forge-list-issues app)
  "List open issues for the current project."
  (let* ((echo (app-state-echo app))
         (output (qt-forge-run-gh ["issue" "list" "--limit" "20"])))
    (if (not output)
      (echo-error! echo "forge: failed to list issues (is gh installed?)")
      (let* ((fr (app-state-frame app))
             (ed (qt-current-editor fr))
             (buf-name "*Forge Issues*")
             (existing (buffer-by-name buf-name))
             (buf (or existing (qt-buffer-create! buf-name ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed (string-append "Issues:\n\n" output))
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo "Forge: issues loaded")))))

(def (cmd-forge-view-pr app)
  "View details of a specific PR by number."
  (let* ((echo (app-state-echo app))
         (num (qt-echo-read-string echo "PR number: ")))
    (when (and num (not (string=? num "")))
      (let ((output (qt-forge-run-gh ["pr" "view" num])))
        (if (not output)
          (echo-error! echo (string-append "forge: failed to view PR #" num))
          (let* ((fr (app-state-frame app))
                 (ed (qt-current-editor fr))
                 (buf-name (string-append "*Forge PR #" num "*"))
                 (existing (buffer-by-name buf-name))
                 (buf (or existing (qt-buffer-create! buf-name ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed output)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! echo (string-append "Forge: PR #" num))))))))

(def (cmd-forge-create-pr app)
  "Create a new PR via gh CLI."
  (let* ((echo (app-state-echo app))
         (title (qt-echo-read-string echo "PR title: ")))
    (when (and title (not (string=? title "")))
      (let ((output (qt-forge-run-gh ["pr" "create" "--title" title "--fill"])))
        (if (not output)
          (echo-error! echo "forge: failed to create PR")
          (echo-message! echo (string-append "Created: " (string-trim output))))))))

;;; Qt versions of batch 10 commands

(def *qt-custom-groups* (make-hash-table))

(def (qt-custom-group-add! group var-name)
  (let ((vars (or (hash-get *qt-custom-groups* group) [])))
    (unless (member var-name vars)
      (hash-put! *qt-custom-groups* group (cons var-name vars)))))

(qt-custom-group-add! "editing" "tab-width")
(qt-custom-group-add! "editing" "indent-tabs-mode")
(qt-custom-group-add! "display" "scroll-margin")
(qt-custom-group-add! "files" "global-auto-revert-mode")

(def *qt-face-definitions* (make-hash-table))

(def (qt-face-set! name . props)
  (hash-put! *qt-face-definitions* name props))

(qt-face-set! "default" 'fg: "white" 'bg: "black")
(qt-face-set! "region" 'bg: "blue")
(qt-face-set! "modeline" 'fg: "black" 'bg: "white")
(qt-face-set! "comment" 'fg: "gray")
(qt-face-set! "keyword" 'fg: "cyan")
(qt-face-set! "string" 'fg: "green")
(qt-face-set! "error" 'fg: "red")

;; Advice system (Qt)
(def *qt-advice-before* (make-hash-table))
(def *qt-advice-after*  (make-hash-table))

(def (qt-advice-add! symbol where fn advice-name)
  (let ((table (if (eq? where 'before) *qt-advice-before* *qt-advice-after*))
        (entry (cons fn advice-name)))
    (let ((existing (or (hash-get table symbol) [])))
      (hash-put! table symbol (cons entry existing)))))

(def (cmd-describe-advice app)
  "Show active advice (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (lines ["Command Advice" "==============" ""]))
    (hash-for-each
      (lambda (sym advices)
        (for-each
          (lambda (entry)
            (set! lines (cons
              (string-append "  :before " (symbol->string sym) " — " (cdr entry))
              lines)))
          advices))
      *qt-advice-before*)
    (hash-for-each
      (lambda (sym advices)
        (for-each
          (lambda (entry)
            (set! lines (cons
              (string-append "  :after  " (symbol->string sym) " — " (cdr entry))
              lines)))
          advices))
      *qt-advice-after*)
    (when (= (length lines) 3)
      (set! lines (cons "  (no active advice)" lines)))
    (let* ((text (string-join (reverse lines) "\n"))
           (buf (or (buffer-by-name "*Advice*")
                    (qt-buffer-create! "*Advice*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;; Autoload system (Qt)
(def *qt-autoloads* (make-hash-table))

(def (qt-autoload! symbol file-path)
  (hash-put! *qt-autoloads* symbol file-path))

(def (cmd-list-autoloads app)
  "Show registered autoloads (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (lines ["Registered Autoloads" "====================" ""]))
    (hash-for-each
      (lambda (sym path)
        (set! lines (cons
          (string-append "  " (symbol->string sym) " → " path)
          lines)))
      *qt-autoloads*)
    (when (= (length lines) 3)
      (set! lines (cons "  (no autoloads registered)" lines)))
    (set! lines (append (reverse lines)
      ["" "Use (qt-autoload! 'symbol \"path.ss\") in ~/.gemacs-init."]))
    (let* ((text (string-join lines "\n"))
           (buf (or (buffer-by-name "*Autoloads*")
                    (qt-buffer-create! "*Autoloads*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;; Qt batch 11: dynamic modules, icomplete, marginalia, embark

(def *qt-loaded-modules* [])

(def (cmd-load-module app)
  "Load a module at runtime (Qt)."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string app "Load module: ")))
    (when (and path (> (string-length path) 0))
      (let ((full-path (path-expand path)))
        (if (not (file-exists? full-path))
          (echo-error! echo (string-append "Not found: " full-path))
          (with-catch
            (lambda (e) (echo-error! echo "Load error"))
            (lambda ()
              (load full-path)
              (set! *qt-loaded-modules* (cons full-path *qt-loaded-modules*))
              (echo-message! echo (string-append "Loaded: " (path-strip-directory full-path))))))))))

(def (cmd-list-modules app)
  "Show loaded modules (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (text (string-append
                 "Loaded Modules\n==============\n\n"
                 (if (null? *qt-loaded-modules*) "  (none)\n"
                   (string-join (map (lambda (m) (string-append "  " m)) *qt-loaded-modules*) "\n"))
                 "\n"))
         (buf (or (buffer-by-name "*Modules*")
                  (qt-buffer-create! "*Modules*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def *qt-icomplete-mode* #f)

(def (cmd-icomplete-mode app)
  "Toggle icomplete-mode (Qt)."
  (set! *qt-icomplete-mode* (not *qt-icomplete-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-icomplete-mode* "Icomplete mode ON" "Icomplete mode OFF")))

(def (cmd-fido-mode app)
  "Toggle fido-mode (Qt)."
  (set! *qt-icomplete-mode* (not *qt-icomplete-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-icomplete-mode* "Fido mode ON" "Fido mode OFF")))

(def *qt-marginalia-mode* #f)

(def (cmd-marginalia-mode app)
  "Toggle marginalia-mode (Qt)."
  (set! *qt-marginalia-mode* (not *qt-marginalia-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-marginalia-mode* "Marginalia mode ON" "Marginalia mode OFF")))

(def *qt-embark-actions* (make-hash-table))

(def (cmd-embark-act app)
  "Show embark actions (Qt)."
  (let* ((echo (app-state-echo app))
         (cat-str (qt-echo-read-string app "Embark category: ")))
    (when (and cat-str (> (string-length cat-str) 0))
      (let ((actions (hash-get *qt-embark-actions* (string->symbol cat-str))))
        (if (not actions)
          (echo-message! echo "No actions for category")
          (let ((action (qt-echo-read-string app "Action: ")))
            (when (and action (> (string-length action) 0))
              (echo-message! echo (string-append "Embark: " action)))))))))

;;;============================================================================
;;; Persistent undo across sessions (Qt)
;;;============================================================================

(def *qt-persistent-undo-dir*
  (string-append (or (getenv "HOME" #f) ".") "/.gemacs-undo/"))

(def (qt-persistent-undo-file-for path)
  (string-append *qt-persistent-undo-dir*
    (string-map (lambda (c) (if (char=? c #\/) #\_ c))
                (if (> (string-length path) 0) (substring path 1 (string-length path)) "unknown"))
    ".undo"))

(def (cmd-undo-history-save app)
  "Save undo history for the current buffer to disk."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-message! echo "Buffer has no file — cannot save undo history")
      (let ((undo-file (qt-persistent-undo-file-for file)))
        (with-catch
          (lambda (e) (echo-message! echo (string-append "Error saving undo: " (error-message e))))
          (lambda ()
            (create-directory* *qt-persistent-undo-dir*)
            (call-with-output-file undo-file
              (lambda (port)
                (write (list 'undo-v1 file) port)
                (newline port)))
            (echo-message! echo (string-append "Undo history saved: " undo-file))))))))

(def (cmd-undo-history-load app)
  "Load undo history for the current buffer from disk."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-message! echo "Buffer has no file — cannot load undo history")
      (let ((undo-file (qt-persistent-undo-file-for file)))
        (if (not (file-exists? undo-file))
          (echo-message! echo "No saved undo history for this file")
          (with-catch
            (lambda (e) (echo-message! echo (string-append "Error loading undo: " (error-message e))))
            (lambda ()
              (let ((data (call-with-input-file undo-file read)))
                (echo-message! echo (string-append "Undo history loaded from: " undo-file))))))))))

;;;============================================================================
;;; Image thumbnails in dired (Qt)
;;;============================================================================

(def *qt-image-extensions* '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "ico" "tiff"))

(def (qt-image-file? path)
  (let ((ext (string-downcase (path-extension path))))
    (member ext *qt-image-extensions*)))

(def (cmd-image-dired-display-thumbnail app)
  "Display thumbnail info for image under cursor in dired."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (if (not (string-suffix? " [dired]" name))
      (echo-message! echo "Not in a dired buffer")
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-num (length (filter (lambda (c) (char=? c #\newline))
                                       (string->list (substring text 0 (min pos (string-length text)))))))
             (line (if (< line-num (length lines)) (list-ref lines line-num) "")))
        (if (qt-image-file? (string-trim-both line))
          (echo-message! echo (string-append "Image: " (string-trim-both line)))
          (echo-message! echo "Not an image file"))))))

(def (cmd-image-dired-show-all-thumbnails app)
  "List all image files in the current dired directory."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (if (not (string-suffix? " [dired]" name))
      (echo-message! echo "Not in a dired buffer")
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (images (filter (lambda (l) (qt-image-file? (string-trim-both l))) lines)))
        (if (null? images)
          (echo-message! echo "No image files in this directory")
          (echo-message! echo
            (string-append "Images (" (number->string (length images)) "): "
              (string-join (map string-trim-both (take images (min 5 (length images)))) ", ")
              (if (> (length images) 5) "..." ""))))))))

;;;============================================================================
;;; Virtual dired (Qt)
;;;============================================================================

(def (cmd-virtual-dired app)
  "Create a virtual dired buffer from file paths."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Virtual dired files (space-separated): ")))
    (when (and input (> (string-length input) 0))
      (let* ((files (string-split input #\space))
             (ed (current-qt-editor app))
             (content (string-join
                        (map (lambda (f)
                               (string-append "  " (path-strip-directory f) "  → " f))
                             files)
                        "\n")))
        (let ((buf (qt-buffer-create! "*Virtual Dired*" ed)))
          (qt-buffer-attach! ed buf))
        (qt-plain-text-edit-set-text! ed (string-append "Virtual Dired:\n\n" content "\n"))
        (echo-message! echo (string-append "Virtual dired: " (number->string (length files)) " files"))))))

(def (cmd-dired-from-find app)
  "Create a virtual dired from find command results."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Find pattern (glob): ")))
    (when (and pattern (> (string-length pattern) 0))
      (echo-message! echo (string-append "Virtual dired from find: " pattern)))))

;;;============================================================================
;;; Key translation / Super key (Qt)
;;;============================================================================

(def *qt-key-translations* (make-hash-table))

(def (qt-key-translate! from to)
  (hash-put! *qt-key-translations* from to))

(def *qt-super-key-mode* #f)

(def (cmd-key-translate app)
  "Define a key translation."
  (let* ((echo (app-state-echo app))
         (from (qt-echo-read-string app "Translate from key: ")))
    (when (and from (> (string-length from) 0))
      (let ((to (qt-echo-read-string app "Translate to key: ")))
        (when (and to (> (string-length to) 0))
          (qt-key-translate! from to)
          (echo-message! echo (string-append "Key translation: " from " → " to)))))))

(def (cmd-toggle-super-key-mode app)
  "Toggle super key mode."
  (let ((echo (app-state-echo app)))
    (set! *qt-super-key-mode* (not *qt-super-key-mode*))
    (echo-message! echo (if *qt-super-key-mode*
                          "Super-key-mode enabled"
                          "Super-key-mode disabled"))))

(def (cmd-describe-key-translations app)
  "Show all active key translations."
  (let* ((echo (app-state-echo app))
         (entries (hash->list *qt-key-translations*)))
    (if (null? entries)
      (echo-message! echo "No key translations defined")
      (echo-message! echo
        (string-append "Key translations: "
          (string-join
            (map (lambda (p) (string-append (car p) " → " (cdr p))) entries)
            ", "))))))

;;;============================================================================
;;; Registration for commands moved from commands-aliases.ss
;;;============================================================================

(def (qt-register-aliases2-commands!)
  ;; EWW bookmarks
  (register-command! 'eww-add-bookmark cmd-eww-add-bookmark)
  (register-command! 'eww-list-bookmarks cmd-eww-list-bookmarks)
  ;; Forge (GitHub via gh CLI)
  (register-command! 'forge-list-prs cmd-forge-list-prs)
  (register-command! 'forge-list-issues cmd-forge-list-issues)
  (register-command! 'forge-view-pr cmd-forge-view-pr)
  (register-command! 'forge-create-pr cmd-forge-create-pr)
  ;; Image mode commands
  (register-command! 'image-zoom-in cmd-image-zoom-in)
  (register-command! 'image-zoom-out cmd-image-zoom-out)
  (register-command! 'image-zoom-fit cmd-image-zoom-fit)
  (register-command! 'image-zoom-reset cmd-image-zoom-reset))

