;;; -*- Gerbil -*-
;;; Tests for persist.ss: recentf, savehist, desktop, buffer-local, auto-mode, which-key

(import :std/test
        :std/srfi/13
        :gerbil-scintilla/scintilla
        :gerbil-emacs/core
        :gerbil-emacs/buffer
        :gerbil-emacs/persist
        (only-in :gerbil-emacs/editor register-all-commands!))

(export persist-test)

(def persist-test
  (test-suite "persist"

    (test-case "recent-files-add! adds and deduplicates"
      (let ((saved *recent-files*))
        (set! *recent-files* [])
        (recent-files-add! "/tmp/foo.txt")
        (recent-files-add! "/tmp/bar.txt")
        (recent-files-add! "/tmp/foo.txt")
        (check (length *recent-files*) => 2)
        (check (string-suffix? "foo.txt" (car *recent-files*)) => #t)
        (set! *recent-files* saved)))

    (test-case "recent-files-add! respects max size"
      (let ((saved *recent-files*))
        (set! *recent-files* [])
        (let loop ((i 0))
          (when (< i 55)
            (recent-files-add! (string-append "/tmp/file" (number->string i) ".txt"))
            (loop (+ i 1))))
        (check (<= (length *recent-files*) 50) => #t)
        (set! *recent-files* saved)))

    (test-case "savehist-save! and savehist-load! round-trip"
      (let ((test-history '("command-a" "command-b" "command-c")))
        (savehist-save! test-history)
        (let ((loaded (savehist-load!)))
          (check (length loaded) => 3)
          (check (car loaded) => "command-a")
          (check (cadr loaded) => "command-b")
          (check (caddr loaded) => "command-c"))))

    (test-case "desktop-save! and desktop-load round-trip"
      (let ((entries (list
                       (make-desktop-entry "foo.ss" "/tmp/foo.ss" 42 'gerbil-mode)
                       (make-desktop-entry "bar.py" "/tmp/bar.py" 100 'python-mode))))
        (desktop-save! entries)
        (let ((loaded (desktop-load)))
          (check (length loaded) => 2)
          (check (desktop-entry-buffer-name (car loaded)) => "foo.ss")
          (check (desktop-entry-file-path (car loaded)) => "/tmp/foo.ss")
          (check (desktop-entry-cursor-pos (car loaded)) => 42)
          (check (desktop-entry-major-mode (car loaded)) => 'gerbil-mode)
          (check (desktop-entry-file-path (cadr loaded)) => "/tmp/bar.py")
          (check (desktop-entry-major-mode (cadr loaded)) => 'python-mode))))

    (test-case "buffer-local-get and buffer-local-set!"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "test-local" ed #f)))
        (check (buffer-local-get buf 'tab-width 4) => 4)
        (buffer-local-set! buf 'tab-width 8)
        (check (buffer-local-get buf 'tab-width) => 8)
        (buffer-local-set! buf 'major-mode 'python-mode)
        (check (buffer-local-get buf 'major-mode) => 'python-mode)
        (buffer-local-delete! buf)))

    (test-case "detect-major-mode: common file types"
      (check (detect-major-mode "foo.ss") => 'gerbil-mode)
      (check (detect-major-mode "bar.py") => 'python-mode)
      (check (detect-major-mode "baz.org") => 'org-mode)
      (check (detect-major-mode "readme.md") => 'markdown-mode)
      (check (detect-major-mode "main.rs") => 'rust-mode)
      (check (detect-major-mode "app.js") => 'js-mode)
      (check (detect-major-mode "Makefile") => 'makefile-mode)
      (check (detect-major-mode "style.css") => 'css-mode)
      (check (detect-major-mode "data.json") => 'json-mode)
      (check (detect-major-mode "unknown.xyz") => #f))

    (test-case "which-key-summary: generates key hints"
      (let ((km (make-keymap)))
        (keymap-bind! km "s" 'save-buffer)
        (keymap-bind! km "f" 'find-file)
        (let ((summary (which-key-summary km)))
          ;; string-contains returns index or #f
          (check (not (not (string-contains summary "s:save-buffer"))) => #t)
          (check (not (not (string-contains summary "f:find-file"))) => #t))))

    (test-case "which-key-summary: shows +prefix for nested keymaps"
      (let ((km (make-keymap))
            (sub-km (make-keymap)))
        (keymap-bind! km "r" sub-km)
        (keymap-bind! km "b" 'switch-buffer)
        (let ((summary (which-key-summary km)))
          (check (not (not (string-contains summary "r:+prefix"))) => #t)
          (check (not (not (string-contains summary "b:switch-buffer"))) => #t))))

    (test-case "scroll margin: default value"
      (check (number? *scroll-margin*) => #t)
      (check (>= *scroll-margin* 0) => #t))

    (test-case "scratch-save! and scratch-load! round-trip"
      (scratch-save! ";; test scratch content\n(+ 1 2)")
      (let ((loaded (scratch-load!)))
        (check (not (not loaded)) => #t)
        (check (not (not (string-contains loaded "test scratch"))) => #t)))

    (test-case "init-file-path is a string"
      (check (string? *init-file-path*) => #t)
      (check (not (not (string-contains *init-file-path* ".gerbil-emacs-init"))) => #t))

    (test-case "persist command registration"
      (register-all-commands!)
      (check (procedure? (find-command 'recentf-open)) => #t)
      (check (procedure? (find-command 'desktop-save)) => #t)
      (check (procedure? (find-command 'desktop-read)) => #t)
      (check (procedure? (find-command 'savehist-save)) => #t)
      (check (procedure? (find-command 'savehist-load)) => #t)
      (check (procedure? (find-command 'recentf-cleanup)) => #t)
      ;; New commands
      (check (procedure? (find-command 'set-scroll-margin)) => #t)
      (check (procedure? (find-command 'toggle-scroll-margin)) => #t)
      (check (procedure? (find-command 'load-init-file)) => #t)
      (check (procedure? (find-command 'find-init-file)) => #t)
      (check (procedure? (find-command 'toggle-save-place-mode)) => #t)
      (check (procedure? (find-command 'toggle-delete-trailing-whitespace-on-save)) => #t)
      (check (procedure? (find-command 'toggle-require-final-newline)) => #t)
      (check (procedure? (find-command 'toggle-centered-cursor-mode)) => #t))

    (test-case "save-place round-trip"
      (let ((saved-alist *save-place-alist*)
            (saved-enabled *save-place-enabled*))
        (set! *save-place-enabled* #t)
        (set! *save-place-alist* (make-hash-table))
        (save-place-remember! "/tmp/test-file.txt" 42)
        (check (save-place-restore "/tmp/test-file.txt") => 42)
        (check (save-place-restore "/tmp/nonexistent.txt") => #f)
        (set! *save-place-alist* saved-alist)
        (set! *save-place-enabled* saved-enabled)))

    (test-case "save-place persistence"
      (let ((saved-alist *save-place-alist*)
            (saved-enabled *save-place-enabled*))
        (set! *save-place-enabled* #t)
        (set! *save-place-alist* (make-hash-table))
        (save-place-remember! "/tmp/a.txt" 100)
        (save-place-remember! "/tmp/b.txt" 200)
        (save-place-save!)
        ;; Clear and reload
        (set! *save-place-alist* (make-hash-table))
        (save-place-load!)
        (check (save-place-restore "/tmp/a.txt") => 100)
        (check (save-place-restore "/tmp/b.txt") => 200)
        (set! *save-place-alist* saved-alist)
        (set! *save-place-enabled* saved-enabled)))

    (test-case "clean-on-save defaults"
      (check (boolean? *delete-trailing-whitespace-on-save*) => #t)
      (check (boolean? *require-final-newline*) => #t)
      (check (boolean? *centered-cursor-mode*) => #t))

    ))
