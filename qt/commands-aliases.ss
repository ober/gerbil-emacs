;;; -*- Gerbil -*-
;;; Qt command registration extensions + utility functions.
;;; Contains batch 6+ core registrations, parity/alias registrations,
;;; org template expansion, and utility functions (key-chord, key-translation, image-mode).
;;; Chain position: after commands-parity, before facade.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
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
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-lsp
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/snippets
        :gemacs/qt/commands-config
        :gemacs/qt/commands-parity)

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

;;;============================================================================
;;; Extended command registrations (batch 6+ core)
;;;============================================================================

(def (qt-register-extended-commands!)
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
  ;; delete-file-and-buffer, find-file-literally → registered in facade (cross-cutting)
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
  ;; show-dir-locals → registered in facade (cross-cutting)
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
  ;; Workspace tabs
  (register-command! 'tab-new cmd-tab-new)
  (register-command! 'tab-close cmd-tab-close)
  (register-command! 'tab-next cmd-tab-next)
  (register-command! 'tab-previous cmd-tab-previous)
  (register-command! 'tab-rename cmd-tab-rename)
  (register-command! 'tab-move cmd-tab-move)
  (register-command! 'tab-list cmd-tab-new)
  (register-command! 'tab-bar-new-tab cmd-tab-new)
  (register-command! 'tab-bar-close-tab cmd-tab-close)
  (register-command! 'tab-bar-switch-to-tab cmd-tab-next)
  ;; Rainbow delimiters
  (register-command! 'rainbow-delimiters-mode cmd-rainbow-delimiters-mode)
  (register-command! 'toggle-rainbow-delimiters-global cmd-rainbow-delimiters-mode)
  (register-command! 'toggle-rainbow-mode cmd-rainbow-delimiters-mode)
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
  ;; quit → registered in facade (cross-cutting)
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
  (register-command! 'magit-stash-pop cmd-magit-stash-pop)
  (register-command! 'magit-branch cmd-magit-branch)
  (register-command! 'magit-checkout cmd-magit-checkout)
  ;; Org-mode
  ;; org-mode → registered in facade (cross-cutting)
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
  (register-command! 'snippet-insert cmd-snippet-insert)
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
  ;; C-M-i: standard Emacs binding for completion-at-point
  (keymap-bind! *global-keymap* "C-M-i" 'lsp-completion)
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
  ;; Parity batch 6: org-mode, VCS, DAP
  (register-command! 'org-schedule cmd-org-schedule)
  (register-command! 'org-deadline cmd-org-deadline)
  (register-command! 'org-insert-src-block cmd-org-insert-src-block)
  (register-command! 'org-clock-in cmd-org-clock-in)
  (register-command! 'org-clock-out cmd-org-clock-out)
  (register-command! 'org-agenda cmd-org-agenda)
  (register-command! 'org-agenda-goto cmd-org-agenda-goto)
  (register-command! 'org-agenda-todo cmd-org-agenda-todo)
  (register-command! 'org-export cmd-org-export)
  (register-command! 'org-priority cmd-org-priority)
  (register-command! 'org-todo cmd-org-todo-cycle)  ;; alias
  (register-command! 'flycheck-previous-error cmd-flycheck-prev-error)  ;; alias
  (register-command! 'vc-pull cmd-vc-pull)
  (register-command! 'vc-push cmd-vc-push)
  (register-command! 'magit-stage-file cmd-magit-stage-file)
  (register-command! 'dap-debug cmd-dap-debug)
  (register-command! 'dap-breakpoint-toggle cmd-dap-breakpoint-toggle)
  (register-command! 'dap-step-over cmd-dap-step-over)
  (register-command! 'dap-step-in cmd-dap-step-in)
  (register-command! 'dap-step-out cmd-dap-step-out)
  ;; Parity batch 7: bookmark menu, clone buffer, macro, text-scale, magit-unstage-file
  (register-command! 'bookmark-bmenu-list cmd-bookmark-bmenu-list)
  (register-command! 'clone-indirect-buffer cmd-clone-indirect-buffer)
  (register-command! 'apply-macro-to-region cmd-apply-macro-to-region)
  (register-command! 'magit-unstage-file cmd-magit-unstage-file)
  (register-command! 'text-scale-increase cmd-text-scale-increase)
  (register-command! 'text-scale-decrease cmd-text-scale-decrease)
  (register-command! 'text-scale-reset cmd-text-scale-reset)
  ;; Parity batch 8: markdown formatting, git-blame, eval, insert helpers
  (register-command! 'markdown-bold cmd-markdown-bold)
  (register-command! 'markdown-italic cmd-markdown-italic)
  (register-command! 'markdown-code cmd-markdown-code)
  (register-command! 'markdown-insert-bold cmd-markdown-insert-bold)
  (register-command! 'markdown-insert-code cmd-markdown-insert-code)
  (register-command! 'markdown-insert-italic cmd-markdown-insert-italic)
  (register-command! 'markdown-insert-image cmd-markdown-insert-image)
  (register-command! 'markdown-image cmd-markdown-image)
  (register-command! 'markdown-heading cmd-markdown-heading)
  (register-command! 'markdown-hr cmd-markdown-hr)
  (register-command! 'markdown-checkbox cmd-markdown-checkbox)
  (register-command! 'markdown-code-block cmd-markdown-code-block)
  (register-command! 'markdown-table cmd-markdown-table)
  (register-command! 'markdown-link cmd-markdown-link)
  (register-command! 'markdown-list-item cmd-markdown-list-item)
  (register-command! 'markdown-insert-list-item cmd-markdown-insert-list-item)
  (register-command! 'markdown-mode cmd-markdown-mode)
  (register-command! 'markdown-preview-outline cmd-markdown-preview-outline)
  (register-command! 'git-blame-line cmd-git-blame-line)
  (register-command! 'eval-region-and-replace cmd-eval-region-and-replace)
  (register-command! 'comment-box cmd-comment-box)
  (register-command! 'goto-matching-bracket cmd-goto-matching-bracket)
  (register-command! 'insert-uuid-v4 cmd-insert-uuid-v4)
  (register-command! 'insert-date-formatted cmd-insert-date-formatted)
  (register-command! 'insert-date-time-stamp cmd-insert-date-time-stamp)
  (register-command! 'insert-char-by-code cmd-insert-char-by-code)
  ;; Parity batch 9: git-gutter, goto-last-edit, highlight, org, misc
  (register-command! 'git-gutter-mode cmd-git-gutter-mode)
  (register-command! 'git-gutter-next-hunk cmd-git-gutter-next-hunk)
  (register-command! 'git-gutter-previous-hunk cmd-git-gutter-previous-hunk)
  (register-command! 'git-gutter-revert-hunk cmd-git-gutter-revert-hunk)
  (register-command! 'git-gutter-stage-hunk cmd-git-gutter-stage-hunk)
  (register-command! 'goto-last-edit cmd-goto-last-edit)
  (register-command! 'highlight-regexp cmd-highlight-regexp)
  (register-command! 'describe-char-at-point cmd-describe-char-at-point)
  (register-command! 'org-link cmd-org-link)
  (register-command! 'org-open-at-point cmd-org-open-at-point)
  (register-command! 'org-cycle cmd-org-cycle)
  (register-command! 'org-shift-tab cmd-org-shift-tab)
  (register-command! 'revert-buffer-no-confirm cmd-revert-buffer-no-confirm)
  (register-command! 'save-buffers-kill-emacs cmd-save-buffers-kill-emacs)
  (register-command! 'kill-compilation cmd-kill-compilation)
  (register-command! 'dired-refresh cmd-dired-refresh))

;;;============================================================================
;;; Alias/parity command registrations (batches 2-11)
;;;============================================================================

(def (qt-register-alias-commands!)
  ;; Parity: new feature commands (from commands-parity.ss)
  (register-command! 'dired-jump cmd-dired-jump)
  (register-command! 'dired-up-directory cmd-dired-up-directory)
  (register-command! 'dired-do-shell-command cmd-dired-do-shell-command)
  (register-command! 'apropos cmd-apropos-emacs)
  (register-command! 'indent-new-comment-line cmd-indent-new-comment-line)
  (register-command! 'isearch-backward-regexp cmd-isearch-backward-regexp)
  (register-command! 'replace-regexp cmd-replace-regexp)
  (register-command! 'org-capture cmd-org-capture)
  (register-command! 'org-refile cmd-org-refile)
  (register-command! 'org-time-stamp cmd-org-time-stamp)
  (register-command! 'org-insert-link cmd-org-insert-link)
  (register-command! 'org-narrow-to-subtree cmd-org-narrow-to-subtree)
  (register-command! 'org-sort cmd-org-sort)
  (register-command! 'project-switch-to-buffer cmd-project-switch-to-buffer)
  (register-command! 'project-kill-buffers cmd-project-kill-buffers)
  (register-command! 'vc-next-action cmd-vc-next-action)
  ;; Parity: canonical Emacs aliases
  (register-command! 'electric-pair-mode cmd-toggle-electric-pair)
  (register-command! 'visual-line-mode cmd-toggle-visual-line-mode)
  (register-command! 'flyspell-mode cmd-flyspell-mode)
  (register-command! 'read-only-mode cmd-toggle-read-only)
  (register-command! 'overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'hl-line-mode cmd-toggle-hl-line)
  (register-command! 'whitespace-cleanup-mode cmd-whitespace-cleanup)
  (register-command! 'line-number-mode cmd-display-line-numbers-mode)
  (register-command! 'column-number-mode cmd-display-line-numbers-mode)
  (register-command! 'comment-or-uncomment-region cmd-toggle-comment)
  (register-command! 'isearch-forward cmd-search-forward)
  (register-command! 'isearch-backward cmd-search-backward)
  ;; Parity batch 2: canonical aliases + new commands
  (register-command! 'keyboard-escape-quit cmd-keyboard-quit)
  (register-command! 'buffer-menu cmd-list-buffers)
  (register-command! 'move-beginning-of-line cmd-beginning-of-line)
  (register-command! 'move-end-of-line cmd-end-of-line)
  (register-command! 'scroll-other-window-down cmd-scroll-other-window-up)
  (register-command! 'kmacro-start-macro cmd-start-kbd-macro)
  (register-command! 'kmacro-end-macro cmd-end-kbd-macro)
  (register-command! 'kmacro-name-last cmd-name-last-kbd-macro)
  (register-command! 'tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'clipboard-yank cmd-yank)
  (register-command! 'clipboard-kill-region cmd-kill-region)
  (register-command! 'comment-line cmd-toggle-comment)
  ;; indent-for-tab-command → registered in facade (cross-cutting)
  (register-command! 'linum-mode cmd-toggle-line-numbers)
  (register-command! 'sort-numeric-fields cmd-sort-numeric-fields)
  (register-command! 'find-dired cmd-find-dired)
  (register-command! 'find-name-dired cmd-find-name-dired)
  (register-command! 'dired-hide-details-mode cmd-dired-hide-details)
  (register-command! 'desktop-save-mode cmd-desktop-save-mode)
  (register-command! 'org-babel-execute-src-block cmd-org-babel-execute-src-block)
  (register-command! 'org-babel-tangle cmd-org-babel-tangle)
  (register-command! 'other-frame cmd-other-frame)
  (register-command! 'register-to-point cmd-point-to-register)
  (register-command! 'winum-mode cmd-winum-mode)
  (register-command! 'help-with-tutorial cmd-help-with-tutorial)
  (register-command! 'flyspell-prog-mode cmd-toggle-flyspell)
  (register-command! 'auto-save-visited-mode cmd-auto-save-mode)
  (register-command! 'cua-mode cmd-cua-mode)
  ;; Batch 3: Package/framework aliases
  (register-command! 'projectile-find-file cmd-project-find-file)
  (register-command! 'projectile-switch-project cmd-project-switch-project)
  (register-command! 'projectile-grep cmd-project-grep)
  (register-command! 'projectile-mode cmd-project-find-file)
  (register-command! 'projectile-run-project cmd-project-compile)
  (register-command! 'projectile-test-project cmd-project-compile)
  (register-command! 'helm-M-x cmd-execute-extended-command)
  ;; helm-find-files → registered in facade (cross-cutting cmd-find-file)
  (register-command! 'helm-recentf cmd-recentf-open)
  (register-command! 'counsel-M-x cmd-execute-extended-command)
  ;; counsel-find-file → registered in facade (cross-cutting cmd-find-file)
  (register-command! 'counsel-grep cmd-grep)
  (register-command! 'counsel-rg cmd-rgrep)
  (register-command! 'counsel-git-grep cmd-project-grep)
  (register-command! 'ivy-switch-buffer cmd-switch-buffer)
  (register-command! 'auto-complete-mode cmd-company-mode)
  (register-command! 'auto-complete cmd-complete-at-point)
  (register-command! 'yasnippet-mode cmd-snippet-expand)
  (register-command! 'yas-expand cmd-snippet-expand)
  (register-command! 'paredit-mode cmd-toggle-auto-pair-mode)
  (register-command! 'smartparens-mode cmd-toggle-auto-pair-mode)
  (register-command! 'undo-tree-mode cmd-undo-history)
  (register-command! 'treemacs-toggle cmd-treemacs)
  (register-command! 'treemacs-select-window cmd-treemacs)
  (register-command! 'magit-branch-create cmd-magit-branch)
  (register-command! 'magit-branch-delete cmd-magit-branch)
  (register-command! 'org-set-tags-command cmd-org-set-tags)
  (register-command! 'multiple-cursors-mode cmd-mc-mark-next)
  (register-command! 'which-key-show-top-level cmd-which-key)
  (register-command! 'window-numbering-mode cmd-winum-mode)
  (register-command! 'select-window-1 cmd-other-window)
  (register-command! 'select-window-2 cmd-other-window)
  (register-command! 'emmet-mode cmd-complete-at-point)
  (register-command! 'emmet-expand-line cmd-complete-at-point)
  (register-command! 'org-archive-subtree cmd-org-archive-subtree)
  (register-command! 'org-toggle-heading cmd-org-toggle-heading)
  (register-command! 'magit-init cmd-magit-init)
  (register-command! 'magit-tag cmd-magit-tag)
  ;; Batch 4: new commands + aliases
  (register-command! 'check-parens cmd-check-parens)
  (register-command! 'count-lines-page cmd-count-lines-page)
  (register-command! 'how-many cmd-how-many)
  (register-command! 'move-to-window-line-top-bottom cmd-move-to-window-line)
  (register-command! 'binary-overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'enriched-mode cmd-text-mode)
  (register-command! 'conf-mode cmd-text-mode)
  (register-command! 'nxml-mode cmd-text-mode)
  (register-command! 'sh-mode cmd-shell-script-mode)
  (register-command! 'markdown-preview-mode cmd-markdown-mode)
  (register-command! 'highlight-symbol-at-point cmd-highlight-symbol)
  (register-command! 'ediff-regions-linewise cmd-ediff-buffers)
  ;; Batch 5: file/dired/ibuffer aliases + new commands
  (register-command! 'rename-file cmd-rename-file-and-buffer)
  (register-command! 'delete-directory cmd-delete-directory)
  (register-command! 'copy-to-buffer cmd-copy-region-as-kill)
  (register-command! 'dired-flag-file-deletion cmd-dired-mark)
  (register-command! 'dired-do-flagged-delete cmd-dired-do-delete)
  (register-command! 'dired-sort-toggle-or-edit cmd-dired-sort-toggle)
  (register-command! 'ibuffer-mark-modified-buffers cmd-ibuffer)
  (register-command! 'ibuffer-mark-unsaved-buffers cmd-ibuffer)
  (register-command! 'ibuffer-do-delete cmd-ibuffer)
  (register-command! 'set-file-modes cmd-set-file-modes)
  (register-command! 'dired-do-chown cmd-dired-do-chown)
  (register-command! 'org-table-sort-lines cmd-org-table-sort)
  (register-command! 'org-export-dispatch cmd-org-export)
  (register-command! 'wdired-change-to-wdired-mode cmd-wdired-mode)
  (register-command! 'info-apropos cmd-apropos-emacs)
  (register-command! 'butterfly cmd-butterfly)
  ;; Batch 6: LSP/eglot aliases + language modes
  (register-command! 'lsp-find-definition cmd-lsp-goto-definition)
  (register-command! 'lsp-describe-thing-at-point cmd-lsp-hover)
  (register-command! 'lsp-execute-code-action cmd-lsp-code-actions)
  (register-command! 'lsp-workspace-restart cmd-lsp-restart)
  (register-command! 'lsp-mode cmd-lsp-restart)
  (register-command! 'lsp-install-server cmd-lsp-restart)
  (register-command! 'eglot cmd-lsp-restart)
  (register-command! 'eglot-rename cmd-lsp-rename)
  (register-command! 'eglot-format cmd-lsp-format-buffer)
  (register-command! 'eglot-code-actions cmd-lsp-code-actions)
  (register-command! 'tab-list cmd-list-buffers)
  (register-command! 'treesit-install-language-grammar cmd-text-mode)
  (register-command! 'json-mode cmd-text-mode)
  (register-command! 'terraform-mode cmd-text-mode)
  (register-command! 'haskell-mode cmd-text-mode)
  (register-command! 'elixir-mode cmd-text-mode)
  (register-command! 'clojure-mode cmd-text-mode)
  (register-command! 'erlang-mode cmd-text-mode)
  (register-command! 'scala-mode cmd-text-mode)
  (register-command! 'kotlin-mode cmd-text-mode)
  (register-command! 'swift-mode cmd-text-mode)
  (register-command! 'zig-mode cmd-text-mode)
  ;; Batch 7: isearch/case/kmacro/debug aliases
  (register-command! 'isearch-forward-symbol-at-point cmd-isearch-forward-symbol)
  (register-command! 'isearch-yank-word-or-char cmd-search-forward)
  (register-command! 'isearch-query-replace cmd-query-replace)
  (register-command! 'capitalize-dwim cmd-capitalize-word)
  (register-command! 'upcase-dwim cmd-upcase-word)
  (register-command! 'downcase-dwim cmd-downcase-word)
  (register-command! 'kmacro-name-last-macro cmd-name-last-kbd-macro)
  (register-command! 'kmacro-edit-macro cmd-name-last-kbd-macro)
  (register-command! 'describe-personal-keybindings cmd-describe-bindings)
  (register-command! 'whitespace-newline-mode cmd-whitespace-mode)
  (register-command! 'highlight-phrase cmd-highlight-regexp)
  (register-command! 'highlight-lines-matching-regexp cmd-highlight-regexp)
  (register-command! 'local-set-key cmd-global-set-key)
  (register-command! 'kmacro-bind-to-key cmd-global-set-key)
  (register-command! 'debug-on-entry cmd-debug-on-entry)
  (register-command! 'cancel-debug-on-entry cmd-cancel-debug-on-entry)
  ;; Batch 8: window/help/rectangle/vc/coding aliases
  ;; Window/frame
  (register-command! 'delete-other-frames cmd-delete-other-windows)
  (register-command! 'make-frame-command cmd-split-window-right)
  (register-command! 'balance-windows-area cmd-balance-windows)
  (register-command! 'window-swap-states cmd-winner-undo)
  (register-command! 'windmove-swap-states-left cmd-windmove-left)
  (register-command! 'windmove-swap-states-right cmd-windmove-right)
  (register-command! 'windmove-swap-states-up cmd-windmove-up)
  (register-command! 'windmove-swap-states-down cmd-windmove-down)
  ;; Buffer
  (register-command! 'view-buffer cmd-list-buffers)
  (register-command! 'display-buffer cmd-list-buffers)
  (register-command! 'pop-to-buffer cmd-list-buffers)
  (register-command! 'switch-to-buffer-other-frame cmd-find-file-other-window)
  ;; Text
  (register-command! 'sort-columns cmd-sort-lines)
  (register-command! 'align cmd-align-regexp)
  ;; Navigation
  (register-command! 'find-file-other-frame cmd-find-file-other-window)
  ;; Help/describe
  (register-command! 'describe-char cmd-what-cursor-position)
  (register-command! 'describe-syntax cmd-describe-mode)
  (register-command! 'describe-categories cmd-describe-mode)
  (register-command! 'describe-current-coding-system cmd-describe-mode)
  (register-command! 'describe-input-method cmd-describe-mode)
  (register-command! 'describe-language-environment cmd-describe-mode)
  (register-command! 'describe-coding-system cmd-describe-mode)
  (register-command! 'command-history cmd-view-lossage)
  (register-command! 'list-command-history cmd-view-lossage)
  (register-command! 'apropos-value cmd-apropos-emacs)
  (register-command! 'apropos-library cmd-apropos-emacs)
  (register-command! 'apropos-user-option cmd-apropos-emacs)
  (register-command! 'list-timers cmd-list-processes)
  (register-command! 'list-faces-display cmd-list-faces-display)
  (register-command! 'list-colors-display cmd-list-faces-display)
  ;; Rectangle/register
  (register-command! 'string-insert-rectangle cmd-string-rectangle)
  (register-command! 'close-rectangle cmd-kill-rectangle)
  (register-command! 'number-to-register cmd-copy-to-register)
  (register-command! 'increment-register cmd-copy-to-register)
  (register-command! 'frameset-to-register cmd-copy-to-register)
  (register-command! 'window-configuration-to-register cmd-copy-to-register)
  (register-command! 'bookmark-write cmd-bookmark-save)
  (register-command! 'bookmark-insert-location cmd-bookmark-jump)
  (register-command! 'bookmark-rename cmd-bookmark-set)
  (register-command! 'bookmark-insert cmd-bookmark-jump)
  (register-command! 'bookmark-bmenu-list cmd-bookmark-list)
  ;; VC
  (register-command! 'vc-create-tag cmd-vc-annotate)
  (register-command! 'vc-retrieve-tag cmd-vc-annotate)
  (register-command! 'vc-root-diff cmd-vc-diff-head)
  (register-command! 'vc-log-incoming cmd-vc-log-file)
  (register-command! 'vc-log-outgoing cmd-vc-log-file)
  (register-command! 'vc-revision-other-window cmd-vc-diff-head)
  (register-command! 'vc-region-history cmd-vc-log-file)
  (register-command! 'vc-ignore cmd-vc-annotate)
  (register-command! 'vc-update cmd-vc-revert)
  (register-command! 'vc-dir-hide-up-to-date cmd-magit-status)
  (register-command! 'vc-dir-mark cmd-magit-status)
  (register-command! 'vc-dir-unmark cmd-magit-status)
  (register-command! 'magit-revert cmd-vc-revert)
  (register-command! 'magit-cherry-pick cmd-magit-commit)
  (register-command! 'magit-bisect cmd-magit-log)
  (register-command! 'magit-submodule cmd-magit-status)
  (register-command! 'magit-worktree cmd-magit-status)
  (register-command! 'magit-blame-echo cmd-magit-blame)
  (register-command! 'magit-section-toggle cmd-magit-status)
  ;; Coding/encoding
  (register-command! 'set-buffer-file-coding-system cmd-set-language-environment)
  (register-command! 'revert-buffer-with-coding-system cmd-revert-buffer)
  (register-command! 'set-terminal-coding-system cmd-set-language-environment)
  (register-command! 'set-keyboard-coding-system cmd-set-language-environment)
  (register-command! 'universal-coding-system-argument cmd-set-language-environment)
  (register-command! 'recode-region cmd-set-language-environment)
  (register-command! 'decode-coding-region cmd-set-language-environment)
  (register-command! 'encode-coding-region cmd-set-language-environment)
  (register-command! 'set-input-method cmd-toggle-input-method)
  ;; Batch 9: org/babel/misc/abbrev aliases
  ;; Org capture
  (register-command! 'org-capture-finalize cmd-org-capture-finalize)
  (register-command! 'org-capture-refile cmd-org-capture)
  (register-command! 'org-capture-abort cmd-org-capture-abort)
  (register-command! 'org-capture-kill cmd-org-capture-abort)
  ;; Org clock
  (register-command! 'org-clock-goto cmd-org-clock-in)
  (register-command! 'org-clock-report cmd-org-clock-in)
  (register-command! 'org-clock-cancel cmd-org-clock-in)
  ;; Org agenda
  (register-command! 'org-agenda-list cmd-org-agenda)
  (register-command! 'org-agenda-day-view cmd-org-agenda)
  (register-command! 'org-agenda-week-view cmd-org-agenda)
  ;; Org heading
  (register-command! 'org-insert-heading-respect-content cmd-org-insert-heading)
  (register-command! 'org-insert-subheading cmd-org-insert-heading)
  (register-command! 'org-insert-todo-heading cmd-org-insert-heading)
  (register-command! 'org-insert-todo-subheading cmd-org-insert-heading)
  ;; Org structure
  (register-command! 'org-do-promote cmd-org-promote)
  (register-command! 'org-do-demote cmd-org-demote)
  (register-command! 'org-metaup cmd-org-promote)
  (register-command! 'org-metadown cmd-org-demote)
  (register-command! 'org-shiftmetaup cmd-org-promote)
  (register-command! 'org-shiftmetadown cmd-org-demote)
  ;; Org babel
  (register-command! 'org-babel-execute-maybe cmd-org-babel-execute-src-block)
  (register-command! 'org-babel-next-src-block cmd-next-error)
  (register-command! 'org-babel-previous-src-block cmd-previous-error)
  (register-command! 'org-babel-mark-block cmd-mark-paragraph)
  ;; Error navigation
  (register-command! 'next-error-no-select cmd-next-error)
  (register-command! 'compilation-minor-mode cmd-compile)
  ;; Search/find
  (register-command! 'lgrep cmd-grep)
  (register-command! 'locate cmd-find-dired)
  (register-command! 'locate-with-filter cmd-find-name-dired)
  ;; Abbrevs
  (register-command! 'dabbrev-completion cmd-dabbrev-expand)
  (register-command! 'edit-abbrevs cmd-list-abbrevs)
  (register-command! 'write-abbrev-file cmd-list-abbrevs)
  (register-command! 'read-abbrev-file cmd-list-abbrevs)
  ;; Minibuffer/escape
  (register-command! 'abort-recursive-edit cmd-keyboard-quit)
  (register-command! 'top-level cmd-keyboard-quit)
  ;; Batch 10: project/diff/calendar/mode aliases
  ;; Project
  (register-command! 'project-compile cmd-project-compile)
  (register-command! 'project-forget-project cmd-project-switch-project)
  (register-command! 'project-remember-project cmd-project-switch-project)
  (register-command! 'project-forget-zombie-projects cmd-project-switch-project)
  (register-command! 'project-async-shell-command cmd-async-shell-command)
  (register-command! 'project-vc-dir cmd-magit-status)
  (register-command! 'project-or-external-find-regexp cmd-project-find-regexp)
  (register-command! 'project-execute-extended-command cmd-execute-extended-command)
  (register-command! 'project-any-command cmd-execute-extended-command)
  (register-command! 'project-list-buffers cmd-list-buffers)
  (register-command! 'project-kill-buffers-confirm cmd-project-kill-buffers)
  ;; Diff/ediff
  (register-command! 'ediff-merge-files cmd-ediff-files)
  (register-command! 'ediff-patch-file cmd-ediff-files)
  (register-command! 'emerge-files cmd-ediff-files)
  (register-command! 'ediff-merge-buffers cmd-ediff-buffers)
  (register-command! 'ediff-revision cmd-vc-diff-head)
  (register-command! 'emerge-buffers cmd-ediff-buffers)
  ;; Smerge aliases
  (register-command! 'smerge-keep-current cmd-smerge-keep-mine)
  (register-command! 'smerge-resolve cmd-smerge-keep-mine)
  ;; Calendar/diary
  (register-command! 'diary-add-entry cmd-calendar)
  (register-command! 'diary-show-all-entries cmd-calendar)
  (register-command! 'diary-insert-entry cmd-calendar)
  (register-command! 'diary-insert-weekly-entry cmd-calendar)
  (register-command! 'diary-insert-monthly-entry cmd-calendar)
  (register-command! 'diary-insert-yearly-entry cmd-calendar)
  (register-command! 'diary-insert-anniversary cmd-calendar)
  (register-command! 'calendar-goto-today cmd-calendar)
  (register-command! 'calendar-mark-holidays cmd-calendar)
  (register-command! 'calendar-list-holidays cmd-calendar)
  (register-command! 'calendar-sunrise-sunset cmd-calendar)
  ;; Batch 11: package/treesit/flymake/mc/helm aliases
  ;; Package
  (register-command! 'use-package cmd-customize-variable)
  (register-command! 'package-autoremove cmd-customize-variable)
  (register-command! 'package-initialize cmd-customize-variable)
  (register-command! 'customize-option cmd-customize-face)
  (register-command! 'customize-save-customized cmd-customize-face)
  ;; Treesitter/eglot
  (register-command! 'treesit-explore-mode cmd-lsp-restart)
  (register-command! 'treesit-inspect-node-at-point cmd-lsp-restart)
  (register-command! 'eglot-ensure cmd-lsp-restart)
  (register-command! 'eglot-shutdown cmd-lsp-restart)
  (register-command! 'eglot-reconnect cmd-lsp-restart)
  ;; Flymake → flycheck/error nav
  (register-command! 'flymake-mode cmd-flycheck-mode)
  (register-command! 'flymake-goto-next-error cmd-next-error)
  (register-command! 'flymake-goto-prev-error cmd-previous-error)
  (register-command! 'flymake-show-diagnostics-buffer cmd-compile)
  (register-command! 'flymake-show-project-diagnostics cmd-compile)
  ;; Multiple cursors
  (register-command! 'mc/mark-next-like-this cmd-mc-mark-next)
  (register-command! 'mc/mark-all-like-this cmd-mc-mark-all)
  (register-command! 'mc/edit-lines cmd-mc-edit-lines)
  ;; Expand region
  (register-command! 'er/expand-region cmd-expand-region)
  (register-command! 'er/contract-region cmd-contract-region)
  ;; Avy/ace/swiper/helm
  (register-command! 'avy-goto-word-1 cmd-avy-goto-char)
  (register-command! 'ace-jump-mode cmd-avy-goto-char)
  (register-command! 'swiper cmd-occur)
  (register-command! 'helm-occur cmd-occur)
  (register-command! 'helm-ag cmd-rgrep)
  (register-command! 'helm-projectile cmd-project-find-file))

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
  "Sort lines in region by a column range."
  (echo-message! (app-state-echo app) "sort-columns: use M-x sort-fields for column sorting"))

(def (cmd-sort-regexp-fields app)
  "Sort lines in region by regex match."
  (echo-message! (app-state-echo app) "sort-regexp-fields: use M-x sort-lines for basic sorting"))

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



