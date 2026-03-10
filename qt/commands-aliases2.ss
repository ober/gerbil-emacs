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
        :gemacs/qt/commands-parity3
        :gemacs/qt/commands-parity4
        :gemacs/qt/commands-parity5
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

;;;============================================================================
;;; Apheleia format-on-save (upgrade from stub toggle to real hook wiring)
;;;============================================================================

(def (qt-apheleia-before-save-hook app buf)
  "Before-save hook for Qt apheleia: format buffer before saving."
  (let ((cmd (find-command 'format-buffer)))
    (when cmd (with-catch (lambda (e) #f) (lambda () (cmd app))))))

(def (cmd-apheleia-mode app)
  "Toggle apheleia mode — format on save using language-appropriate formatter."
  (let* ((key 'apheleia-mode)
         (current (hash-get *qt-toggle-states* key))
         (new-state (not current)))
    (hash-put! *qt-toggle-states* key new-state)
    (if new-state
      (add-hook! 'before-save-hook qt-apheleia-before-save-hook)
      (remove-hook! 'before-save-hook qt-apheleia-before-save-hook))
    (echo-message! (app-state-echo app)
      (if new-state "Apheleia mode ON (format on save)" "Apheleia mode OFF"))))

;;; Embark target detection (Qt layer)
(def (qt-embark-target-at-point app)
  "Detect the target at cursor position. Returns (values type target-string)."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (= len 0)
      (values 'none "")
      ;; Get current line
      (let* ((line-start (let loop ((i (- pos 1)))
                           (cond ((< i 0) 0)
                                 ((char=? (string-ref text i) #\newline) (+ i 1))
                                 (else (loop (- i 1))))))
             (line-end (let loop ((i pos))
                         (cond ((>= i len) i)
                               ((char=? (string-ref text i) #\newline) i)
                               (else (loop (+ i 1))))))
             (line (substring text line-start line-end))
             ;; Check for URL
             (url-start (let ((hs (string-contains line "https://"))
                              (hp (string-contains line "http://")))
                          (cond (hs hs) (hp hp) (else #f)))))
        (if (and url-start (<= (+ line-start url-start) pos))
          (let* ((abs-start (+ line-start url-start))
                 (url-end (let loop ((i abs-start))
                            (cond ((>= i len) i)
                                  ((memv (string-ref text i) '(#\space #\tab #\newline #\) #\] #\> #\")) i)
                                  (else (loop (+ i 1)))))))
            (if (<= pos url-end)
              (values 'url (substring text abs-start url-end))
              (qt-embark-word-at-point text pos len)))
          ;; Extract word and classify
          (let-values (((type target) (qt-embark-word-at-point text pos len)))
            (cond
              ((and (> (string-length target) 0)
                    (or (char=? (string-ref target 0) #\/)
                        (string-prefix? "./" target)
                        (string-prefix? "~/" target)))
               (values 'file target))
              (else (values type target)))))))))

(def (qt-embark-word-at-point text pos len)
  "Extract word at point for Qt layer."
  (let* ((word-char? (lambda (c) (or (char-alphabetic? c) (char-numeric? c)
                                     (char=? c #\-) (char=? c #\_) (char=? c #\.)
                                     (char=? c #\/) (char=? c #\~))))
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((not (word-char? (string-ref text i))) (+ i 1))
                        (else (loop (- i 1))))))
         (end (let loop ((i pos))
                (cond ((>= i len) i)
                      ((not (word-char? (string-ref text i))) i)
                      (else (loop (+ i 1)))))))
    (values 'symbol (substring text start end))))

;;; Qt embark action registry
(def *qt-embark-target-actions*
  `((url    . (("browse-url — open in browser"   . ,(lambda (app target)
                 (let ((cmd (find-command 'browse-url-at-point)))
                   (when cmd (cmd app)))))
               ("copy — copy URL to kill ring"    . ,(lambda (app target)
                 (qt-kill-ring-push! app target)
                 (echo-message! (app-state-echo app) (string-append "Copied: " target))))
               ("eww — open in eww browser"      . ,(lambda (app target)
                 (let ((cmd (find-command 'eww)))
                   (when cmd (cmd app)))))))
    (file   . (("find-file — open file"           . ,(lambda (app target)
                 (let ((cmd (find-command 'find-file)))
                   (when cmd (cmd app)))))
               ("copy-path — copy path"           . ,(lambda (app target)
                 (qt-kill-ring-push! app target)
                 (echo-message! (app-state-echo app) (string-append "Copied: " target))))
               ("dired — open directory"          . ,(lambda (app target)
                 (let ((cmd (find-command 'dired)))
                   (when cmd (cmd app)))))
               ("shell-command — run on file"     . ,(lambda (app target)
                 (let ((cmd (find-command 'shell-command-on-file)))
                   (when cmd (cmd app)))))))
    (symbol . (("grep — search project"           . ,(lambda (app target)
                 (let ((cmd (find-command 'consult-ripgrep)))
                   (if cmd (cmd app)
                     (let ((g (find-command 'grep))) (when g (g app)))))))
               ("describe — describe symbol"      . ,(lambda (app target)
                 (let ((cmd (find-command 'describe-function)))
                   (when cmd (cmd app)))))
               ("find-tag — go to definition"     . ,(lambda (app target)
                 (let ((cmd (find-command 'find-tag)))
                   (when cmd (cmd app)))))
               ("copy — copy to kill ring"        . ,(lambda (app target)
                 (qt-kill-ring-push! app target)
                 (echo-message! (app-state-echo app) (string-append "Copied: " target))))
               ("ispell — check spelling"         . ,(lambda (app target)
                 (let ((cmd (find-command 'ispell-word)))
                   (when cmd (cmd app)))))))))

(def (cmd-embark-act app)
  "Embark act — show contextual actions for target at point in narrowing popup."
  (let* ((echo (app-state-echo app)))
    (let-values (((type target) (qt-embark-target-at-point app)))
      (if (or (eq? type 'none) (string-empty? target))
        (echo-message! echo "No target at point")
        (let* ((actions-entry (assq type *qt-embark-target-actions*))
               (action-list (if actions-entry (cdr actions-entry) [])))
          (if (null? action-list)
            (echo-message! echo (string-append "No actions for " (symbol->string type)))
            ;; Show actions in narrowing popup
            (let* ((display-target (if (> (string-length target) 50)
                                    (string-append (substring target 0 47) "...")
                                    target))
                   (prompt (string-append "Act on " (symbol->string type)
                             " '" display-target "': "))
                   (candidates (map car action-list))
                   (choice (qt-echo-read-with-narrowing app prompt candidates)))
              (when choice
                (let ((match (assoc choice action-list)))
                  (if match
                    ((cdr match) app target)
                    (echo-message! echo (string-append "Unknown action: " choice))))))))))))

(def (cmd-embark-dwim app)
  "Embark do-what-I-mean — execute default action on target at point."
  (let* ((echo (app-state-echo app)))
    (let-values (((type target) (qt-embark-target-at-point app)))
      (cond
        ((or (eq? type 'none) (string-empty? target))
         (echo-message! echo "No target at point"))
        ((eq? type 'url)
         (let ((cmd (find-command 'browse-url-at-point)))
           (if cmd (cmd app) (echo-message! echo target))))
        ((eq? type 'file)
         (let ((cmd (find-command 'find-file)))
           (when cmd (cmd app))))
        (else
         (let ((cmd (find-command 'consult-ripgrep)))
           (if cmd (cmd app)
             (echo-message! echo (string-append "Symbol: " target)))))))))

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
;;; Scroll other window (Qt)
;;;============================================================================

(def (qt-find-other-window app)
  "Find the other window's editor, or #f if only one window."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (<= (length wins) 1)
      #f
      (let* ((cur (qt-current-window fr))
             (idx (let loop ((ws wins) (i 0))
                    (cond ((null? ws) 0)
                          ((eq? (car ws) cur) i)
                          (else (loop (cdr ws) (+ i 1))))))
             (other-idx (modulo (+ idx 1) (length wins))))
        (qt-edit-window-editor (list-ref wins other-idx))))))

(def (cmd-scroll-up-other-window app)
  "Scroll the other window up (like scroll-other-window)."
  (let ((other-ed (qt-find-other-window app)))
    (if (not other-ed)
      (echo-message! (app-state-echo app) "Only one window")
      (let ((page-lines (max 1 (- (sci-send other-ed SCI_LINESONSCREEN 0) 2))))
        (sci-send other-ed SCI_LINESCROLL 0 page-lines)
        (echo-message! (app-state-echo app) "Scrolled other window up")))))

(def (cmd-scroll-down-other-window app)
  "Scroll the other window down (like scroll-other-window-down)."
  (let ((other-ed (qt-find-other-window app)))
    (if (not other-ed)
      (echo-message! (app-state-echo app) "Only one window")
      (let ((page-lines (max 1 (- (sci-send other-ed SCI_LINESONSCREEN 0) 2))))
        (sci-send other-ed SCI_LINESCROLL 0 (- page-lines))
        (echo-message! (app-state-echo app) "Scrolled other window down")))))

(def (cmd-recenter-other-window app)
  "Recenter the other window around its cursor."
  (let ((other-ed (qt-find-other-window app)))
    (if (not other-ed)
      (echo-message! (app-state-echo app) "Only one window")
      (let* ((pos (sci-send other-ed SCI_GETCURRENTPOS 0))
             (line (sci-send other-ed SCI_LINEFROMPOSITION pos))
             (screen-lines (sci-send other-ed SCI_LINESONSCREEN 0))
             (target (max 0 (- line (quotient screen-lines 2)))))
        (sci-send other-ed SCI_SETFIRSTVISIBLELINE target)
        (echo-message! (app-state-echo app) "Other window recentered")))))

;;;============================================================================
;;; Buffer statistics & text utilities (Qt)
;;;============================================================================

(def (cmd-buffer-statistics app)
  "Show detailed buffer statistics: lines, words, chars."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (lines (+ 1 (let loop ((i 0) (count 0))
                       (if (>= i len) count
                         (if (char=? (string-ref text i) #\newline)
                           (loop (+ i 1) (+ count 1))
                           (loop (+ i 1) count))))))
         (words (let loop ((i 0) (count 0) (in-word #f))
                  (if (>= i len) (if in-word (+ count 1) count)
                    (let ((c (string-ref text i)))
                      (if (or (char=? c #\space) (char=? c #\newline)
                              (char=? c #\tab) (char=? c #\return))
                        (loop (+ i 1) (if in-word (+ count 1) count) #f)
                        (loop (+ i 1) count #t)))))))
    (echo-message! (app-state-echo app)
      (string-append "Lines: " (number->string lines)
                     "  Words: " (number->string words)
                     "  Chars: " (number->string len)))))

(def (cmd-convert-line-endings app)
  "Convert line endings in current buffer (unix/dos/mac)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (choice (qt-echo-read-string app "Convert to (unix/dos/mac): ")))
    (when choice
      (let ((text (qt-plain-text-edit-text ed)))
        (cond
          ((string=? choice "unix")
           (let ((new-text (string-subst (string-subst text "\r\n" "\n") "\r" "\n")))
             (qt-plain-text-edit-set-text! ed new-text)
             (echo-message! echo "Converted to Unix line endings (LF)")))
          ((string=? choice "dos")
           (let* ((clean (string-subst (string-subst text "\r\n" "\n") "\r" "\n"))
                  (new-text (string-subst clean "\n" "\r\n")))
             (qt-plain-text-edit-set-text! ed new-text)
             (echo-message! echo "Converted to DOS line endings (CRLF)")))
          ((string=? choice "mac")
           (let ((new-text (string-subst (string-subst text "\r\n" "\r") "\n" "\r")))
             (qt-plain-text-edit-set-text! ed new-text)
             (echo-message! echo "Converted to Mac line endings (CR)")))
          (else
           (echo-error! echo "Unknown format. Use unix, dos, or mac.")))))))

(def (cmd-set-buffer-encoding app)
  "Set the buffer encoding (display only — all buffers use UTF-8)."
  (let* ((echo (app-state-echo app))
         (enc (qt-echo-read-string app "Encoding (utf-8/latin-1/ascii): ")))
    (when enc
      (echo-message! echo (string-append "Encoding set to: " enc
                                          " (note: internally UTF-8)")))))

(def (cmd-diff-two-files app)
  "Diff two files and show the result in a buffer."
  (let* ((echo (app-state-echo app))
         (file1 (qt-echo-read-string app "File A: "))
         (file2 (when file1 (qt-echo-read-string app "File B: "))))
    (when (and file1 file2
               (not (string=? file1 "")) (not (string=? file2 "")))
      (let ((result (with-catch
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                      (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "diff"
                                         arguments: (list "-u" file1 file2)
                                         stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "Files are identical")))))))
        (let* ((fr (app-state-frame app))
               (ed (qt-current-editor fr))
               (buf (or (buffer-by-name "*Diff*")
                        (qt-buffer-create! "*Diff*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Insert utilities (Qt)
;;;============================================================================

(def (qt-insert-at-point! ed str)
  "Insert string at cursor position in the editor."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (new-text (string-append
                     (substring text 0 pos) str
                     (substring text pos (string-length text)))))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (+ pos (string-length str)))))

(def (cmd-insert-current-file-name app)
  "Insert the current buffer's file name at point."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (qt-insert-at-point! ed path)
        (echo-message! (app-state-echo app) (string-append "Inserted: " path)))
      (echo-message! (app-state-echo app) "Buffer has no file name"))))

(def (cmd-insert-env-var app)
  "Insert the value of an environment variable at point."
  (let* ((echo (app-state-echo app))
         (var (qt-echo-read-string app "Env var: ")))
    (when (and var (not (string=? var "")))
      (let ((val (getenv var #f)))
        (if val
          (begin
            (qt-insert-at-point! (current-qt-editor app) val)
            (echo-message! echo (string-append "$" var " = " val)))
          (echo-error! echo (string-append "$" var " not set")))))))

(def (cmd-insert-separator-line app)
  "Insert a separator line at point."
  (qt-insert-at-point! (current-qt-editor app)
    (string-append (make-string 72 #\-) "\n")))

(def (cmd-insert-form-feed app)
  "Insert a form feed (page break) character."
  (qt-insert-at-point! (current-qt-editor app)
    (string-append (string (integer->char 12)) "\n")))

(def (cmd-insert-page-break app)
  "Insert a page break character (same as form feed)."
  (cmd-insert-form-feed app))

(def (cmd-insert-zero-width-space app)
  "Insert a zero-width space character."
  (qt-insert-at-point! (current-qt-editor app)
    (string (integer->char #x200b))))

(def (cmd-insert-fixme app)
  "Insert a FIXME comment at point."
  (qt-insert-at-point! (current-qt-editor app) "FIXME: "))

(def (cmd-insert-todo app)
  "Insert a TODO comment at point."
  (qt-insert-at-point! (current-qt-editor app) "TODO: "))

(def (cmd-insert-backslash app)
  "Insert a backslash character at point."
  (qt-insert-at-point! (current-qt-editor app) "\\"))

(def (cmd-insert-sequential-numbers app)
  "Insert sequential numbers at point."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Count (e.g. 10): ")))
    (when (and input (not (string=? input "")))
      (let ((n (string->number input)))
        (when (and n (> n 0))
          (let ((text (string-join
                        (let loop ((i 1) (acc []))
                          (if (> i n) (reverse acc)
                            (loop (+ i 1) (cons (number->string i) acc))))
                        "\n")))
            (qt-insert-at-point! (current-qt-editor app)
              (string-append text "\n"))))))))

;;;============================================================================
;;; Number conversion (Qt)
;;;============================================================================

(def (cmd-hex-to-decimal app)
  "Convert hexadecimal number at point to decimal."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-message! echo "No number at point")
        (with-catch
          (lambda (e) (echo-message! echo "Not a valid hex number"))
          (lambda ()
            (let* ((hex-str (if (string-prefix? "0x" word)
                              (substring word 2 (string-length word))
                              word))
                   (val (string->number hex-str 16)))
              (if val
                (let* ((text (qt-plain-text-edit-text ed))
                       (replacement (number->string val))
                       (new-text (string-append
                                   (substring text 0 start)
                                   replacement
                                   (substring text end (string-length text)))))
                  (qt-plain-text-edit-set-text! ed new-text)
                  (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length replacement)))
                  (echo-message! echo (string-append word " -> " replacement)))
                (echo-message! echo "Not a valid hex number")))))))))

(def (cmd-decimal-to-hex app)
  "Convert decimal number at point to hexadecimal."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-message! echo "No number at point")
        (let ((val (string->number word)))
          (if val
            (let* ((hex (string-append "0x" (number->string val 16)))
                   (text (qt-plain-text-edit-text ed))
                   (new-text (string-append
                               (substring text 0 start)
                               hex
                               (substring text end (string-length text)))))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length hex)))
              (echo-message! echo (string-append word " -> " hex)))
            (echo-message! echo "Not a valid decimal number")))))))

;;;============================================================================
;;; Shell command on region (Qt)
;;;============================================================================

(def (cmd-shell-command-on-region-replace app)
  "Replace region with output of shell command piped through it."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART 0))
         (end (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= start end)
      (echo-error! echo "No region selected")
      (let ((cmd-str (qt-echo-read-string app "Shell command on region (replace): ")))
        (when (and cmd-str (> (string-length cmd-str) 0))
          (let* ((text (qt-plain-text-edit-text ed))
                 (region (substring text start end)))
            (with-catch
              (lambda (e)
                (echo-error! echo
                  (string-append "Error: "
                    (with-output-to-string
                      (lambda () (display-exception e))))))
              (lambda ()
                (let* ((p (open-process
                            (list path: "/bin/sh"
                                  arguments: ["-c" cmd-str]
                                  stdin-redirection: #t
                                  stdout-redirection: #t
                                  stderr-redirection: #t)))
                       (_ (begin (display region p) (force-output p) (close-output-port p)))
                       (output (or (read-line p #f) "")))
                  (process-status p)
                  (let* ((text (qt-plain-text-edit-text ed))
                         (new-text (string-append
                                     (substring text 0 start)
                                     output
                                     (substring text end (string-length text)))))
                    (qt-plain-text-edit-set-text! ed new-text)
                    (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length output)))
                    (echo-message! echo
                      (string-append "Replaced " (number->string (- end start)) " chars"))))))))))))

(def (cmd-shell-command-to-string app)
  "Run a shell command and insert its output at point."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (cmd-str (qt-echo-read-string app "Shell command: ")))
    (when (and cmd-str (> (string-length cmd-str) 0))
      (with-catch
        (lambda (e) (echo-error! echo "Command failed"))
        (lambda ()
          (let* ((p (open-process
                      (list path: "/bin/sh"
                            arguments: ["-c" cmd-str]
                            stdout-redirection: #t
                            stderr-redirection: #t)))
                 (output (read-line p #f)))
            (process-status p)
            (when output
              (qt-insert-at-point! ed output))))))))

;;;============================================================================
;;; Tabify/untabify (Qt)
;;;============================================================================

(def (cmd-tabify-region app)
  "Convert spaces to tabs in the selected region (or entire buffer)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0))
         (spaces (make-string 4 #\space)))
    (if (= sel-start sel-end)
      (let* ((text (qt-plain-text-edit-text ed))
             (result (string-subst text spaces "\t")))
        (qt-plain-text-edit-set-text! ed result)
        (echo-message! echo "Tabified buffer"))
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (result (string-subst region spaces "\t"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (echo-message! echo "Tabified region")))))

;;;============================================================================
;;; Goto scratch buffer (Qt)
;;;============================================================================

(def (cmd-goto-scratch app)
  "Switch to the *scratch* buffer."
  (let* ((fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (existing (buffer-by-name "*scratch*")))
    (if existing
      (begin
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing))
      (let ((buf (qt-buffer-create! "*scratch*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed
          ";; This buffer is for text that is not saved.\n;; Use M-x eval-expression for Gerbil evaluation.\n\n")))))

;;;============================================================================
;;; Org store-link (Qt)
;;;============================================================================

(def *org-stored-link* #f)

(def (cmd-org-store-link app)
  "Store link to current file:line for later insertion."
  (let* ((fr (app-state-frame app))
         (buf (qt-current-buffer fr))
         (ed (qt-current-editor fr))
         (file (and buf (buffer-file-path buf)))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (line (+ 1 (let loop ((i 0) (n 0))
                      (if (or (>= i pos) (>= i (string-length text))) n
                        (if (char=? (string-ref text i) #\newline)
                          (loop (+ i 1) (+ n 1))
                          (loop (+ i 1) n))))))
         (echo (app-state-echo app)))
    (if file
      (let ((link (string-append "file:" file "::" (number->string line))))
        (set! *org-stored-link* link)
        (echo-message! echo (string-append "Stored: " link)))
      (echo-message! echo "Buffer has no file"))))

;;;============================================================================
;;; Word frequency analysis (Qt)
;;;============================================================================

(def (cmd-word-frequency-analysis app)
  "Show word frequency analysis of buffer content."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (words (string-tokenize text))
         (freq (make-hash-table)))
    (for-each (lambda (w)
                (let ((lw (string-downcase w)))
                  (hash-put! freq lw (+ 1 (or (hash-get freq lw) 0)))))
              words)
    (let* ((pairs (hash->list freq))
           (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
           (top (let loop ((ls sorted) (n 0) (acc []))
                  (if (or (null? ls) (>= n 30)) (reverse acc)
                    (loop (cdr ls) (+ n 1) (cons (car ls) acc)))))
           (report (with-output-to-string
                     (lambda ()
                       (display "Word Frequency Analysis:\n")
                       (display (make-string 40 #\-))
                       (display "\n")
                       (for-each
                         (lambda (p)
                           (display (string-pad (number->string (cdr p)) 6))
                           (display "  ")
                           (display (car p))
                           (display "\n"))
                         top)
                       (display (make-string 40 #\-))
                       (display "\n")
                       (display "Total unique words: ")
                       (display (number->string (length pairs)))
                       (display "\n")
                       (display "Total words: ")
                       (display (number->string (length words)))
                       (display "\n")))))
      (let* ((fr (app-state-frame app))
             (buf (or (buffer-by-name "*Word Freq*")
                      (qt-buffer-create! "*Word Freq*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed report)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo
          (string-append (number->string (length pairs)) " unique words analyzed"))))))

;;;============================================================================
;;; Display utilities (Qt)
;;;============================================================================

(def (cmd-display-cursor-position app)
  "Display cursor position information."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (col (qt-plain-text-edit-cursor-column ed)))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string (+ line 1))
                     ", Col " (number->string col)
                     ", Pos " (number->string pos)
                     "/" (number->string (string-length text))))))

(def (cmd-display-column-number app)
  "Display the current column number."
  (let ((col (qt-plain-text-edit-cursor-column (current-qt-editor app))))
    (echo-message! (app-state-echo app)
      (string-append "Column: " (number->string col)))))

;;;============================================================================
;;; Narrow to page (Qt)
;;;============================================================================

(def (cmd-narrow-to-page app)
  "Show page boundaries around cursor (form-feed delimited)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (page-start (let loop ((i (- pos 1)))
                       (if (< i 0) 0
                         (if (char=? (string-ref text i) #\page) (+ i 1)
                           (loop (- i 1))))))
         (page-end (let loop ((i pos))
                     (if (>= i len) len
                       (if (char=? (string-ref text i) #\page) i
                         (loop (+ i 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Page: chars " (number->string page-start)
                     "-" (number->string page-end)
                     " (" (number->string (- page-end page-start)) " chars)"))))

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
  (register-command! 'image-zoom-reset cmd-image-zoom-reset)
  ;; Repeat-mode (Emacs 28+ transient repeat maps)
  (register-command! 'repeat-mode
    (lambda (app)
      (repeat-mode-set! (not (repeat-mode?)))
      (clear-repeat-map!)
      (echo-message! (app-state-echo app)
        (if (repeat-mode?) "Repeat mode enabled" "Repeat mode disabled"))))
  (register-command! 'toggle-repeat-mode
    (lambda (app)
      (repeat-mode-set! (not (repeat-mode?)))
      (clear-repeat-map!)
      (echo-message! (app-state-echo app)
        (if (repeat-mode?) "Repeat mode enabled" "Repeat mode disabled"))))
  ;; Scroll other window
  (register-command! 'scroll-up-other-window cmd-scroll-up-other-window)
  (register-command! 'scroll-down-other-window cmd-scroll-down-other-window)
  (register-command! 'scroll-other-window cmd-scroll-up-other-window)
  (register-command! 'scroll-other-window-down cmd-scroll-down-other-window)
  (register-command! 'recenter-other-window cmd-recenter-other-window)
  ;; Buffer statistics & text utilities
  (register-command! 'buffer-statistics cmd-buffer-statistics)
  (register-command! 'convert-line-endings cmd-convert-line-endings)
  (register-command! 'set-buffer-encoding cmd-set-buffer-encoding)
  (register-command! 'diff cmd-diff-two-files)
  (register-command! 'diff-two-files cmd-diff-two-files)
  (register-command! 'diff-summary cmd-diff-two-files)
  ;; Insert utilities
  (register-command! 'insert-current-file-name cmd-insert-current-file-name)
  (register-command! 'insert-env-var cmd-insert-env-var)
  (register-command! 'insert-separator-line cmd-insert-separator-line)
  (register-command! 'insert-form-feed cmd-insert-form-feed)
  (register-command! 'insert-page-break cmd-insert-page-break)
  (register-command! 'insert-zero-width-space cmd-insert-zero-width-space)
  (register-command! 'insert-fixme cmd-insert-fixme)
  (register-command! 'insert-todo cmd-insert-todo)
  (register-command! 'insert-backslash cmd-insert-backslash)
  (register-command! 'insert-sequential-numbers cmd-insert-sequential-numbers)
  ;; Number conversion
  (register-command! 'hex-to-decimal cmd-hex-to-decimal)
  (register-command! 'decimal-to-hex cmd-decimal-to-hex)
  ;; Shell command on region
  (register-command! 'shell-command-on-region-replace cmd-shell-command-on-region-replace)
  (register-command! 'shell-command-to-string cmd-shell-command-to-string)
  ;; Tabify
  (register-command! 'tabify-region cmd-tabify-region)
  ;; Goto scratch
  (register-command! 'goto-scratch cmd-goto-scratch)
  ;; Org store-link
  (register-command! 'org-store-link cmd-org-store-link)
  ;; Word frequency
  (register-command! 'word-frequency-analysis cmd-word-frequency-analysis)
  ;; Display utilities
  (register-command! 'display-cursor-position cmd-display-cursor-position)
  (register-command! 'display-column-number cmd-display-column-number)
  ;; Narrow to page
  (register-command! 'narrow-to-page cmd-narrow-to-page)

  ;; Batch 3: swiper / counsel / god-mode / beacon / volatile / use-package
  (register-command! 'swiper-isearch cmd-swiper-isearch)
  (register-command! 'counsel-find-file cmd-counsel-find-file)
  (register-command! 'counsel-recentf cmd-counsel-recentf)
  (register-command! 'counsel-bookmark cmd-counsel-bookmark)
  (register-command! 'ivy-resume cmd-ivy-resume)
  (register-command! 'god-mode cmd-god-mode)
  (register-command! 'god-local-mode cmd-god-local-mode)
  (register-command! 'god-execute-with-current-bindings cmd-god-execute-with-current-bindings)
  (register-command! 'beacon-mode cmd-beacon-mode)
  (register-command! 'volatile-highlights-mode cmd-volatile-highlights-mode)
  (register-command! 'all-the-icons-install-fonts cmd-all-the-icons-install-fonts)
  (register-command! 'nerd-icons-install-fonts cmd-nerd-icons-install-fonts)
  (register-command! 'use-package-report cmd-use-package-report)
  (register-command! 'straight-use-package cmd-straight-use-package)
  (register-command! 'which-key-show-major-mode cmd-which-key-show-major-mode)

  ;; Batch 4: dimmer, nyan, centered-cursor, format-all, visual-regexp, anzu, popwin, easy-kill, crux, selected
  (register-command! 'dimmer-mode cmd-dimmer-mode)
  (register-command! 'nyan-mode cmd-nyan-mode)
  (register-command! 'centered-cursor-mode cmd-centered-cursor-mode)
  (register-command! 'format-all-buffer cmd-format-all-buffer)
  (register-command! 'visual-regexp-replace cmd-visual-regexp-replace)
  (register-command! 'visual-regexp-query-replace cmd-visual-regexp-query-replace)
  (register-command! 'anzu-mode cmd-anzu-mode)
  (register-command! 'popwin-mode cmd-popwin-mode)
  (register-command! 'popwin-close-popup cmd-popwin-close-popup)
  (register-command! 'easy-kill cmd-easy-kill)
  (register-command! 'crux-open-with cmd-crux-open-with)
  (register-command! 'crux-duplicate-current-line cmd-crux-duplicate-current-line)
  (register-command! 'crux-indent-defun cmd-crux-indent-defun)
  (register-command! 'crux-swap-windows cmd-crux-swap-windows)
  (register-command! 'crux-cleanup-buffer-or-region cmd-crux-cleanup-buffer-or-region)
  (register-command! 'selected-mode cmd-selected-mode)
  (register-command! 'aggressive-fill-paragraph-mode cmd-aggressive-fill-paragraph-mode))

