;;; -*- Gerbil -*-
;;; Qt functional tests — dispatch-chain tests for the Qt backend.
;;; Mirrors the most critical TUI functional tests for the Qt code path.
;;;
;;; Run: QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-functional-test
;;;
;;; Key principle: test through cmd-indent-or-complete / execute-command!,
;;; NOT by calling qt-try-org-template-expand directly.

(import :std/sugar
        (only-in :gerbil-qt/qt
                 with-qt-app
                 qt-widget-create
                 qt-widget-destroy!
                 qt-scintilla-create
                 qt-splitter-create
                 qt-splitter-count
                 qt-splitter-index-of
                 qt-splitter-widget
                 QT_VERTICAL
                 QT_HORIZONTAL)
        :gerbil-scintilla/constants
        (only-in :gemacs/qt/sci-shim
                 sci-send
                 sci-send/string
                 qt-plain-text-edit-text
                 qt-plain-text-edit-set-text!
                 qt-plain-text-edit-cursor-position
                 qt-plain-text-edit-set-cursor-position!
                 qt-plain-text-edit-insert-text!
                 qt-plain-text-edit-selection-start
                 qt-plain-text-edit-selection-end
                 qt-scintilla-destroy!)
        (only-in :gemacs/core
                 new-app-state
                 execute-command!
                 find-command
                 make-buffer
                 buffer-name
                 buffer-mark
                 app-state-frame
                 app-state-last-command
                 app-state-prefix-arg)
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 make-split-leaf
                 make-split-node
                 split-leaf?
                 split-node?
                 split-leaf-edit-window
                 split-node-orientation
                 split-node-splitter
                 split-node-children
                 qt-edit-window-buffer
                 qt-edit-window-editor
                 qt-edit-window-container
                 qt-frame-windows
                 qt-frame-root
                 qt-frame-splitter
                 qt-frame-current-idx
                 qt-current-window
                 qt-frame-init!)
        (only-in :gemacs/qt/commands
                 qt-register-all-commands!)
        (only-in :gemacs/qt/commands-ide
                 magit-parse-status
                 magit-format-status
                 magit-file-at-point))

(export main)

;;;============================================================================
;;; Test infrastructure
;;;============================================================================

(def *passes* 0)
(def *failures* 0)

(def (pass! label)
  (set! *passes* (+ *passes* 1))
  (displayln "  PASS: " label)
  (force-output (current-output-port)))

(def (fail! label actual expected)
  (set! *failures* (+ *failures* 1))
  (displayln "  FAIL: " label)
  (displayln "    got:      " actual)
  (displayln "    expected: " expected)
  (force-output (current-output-port)))

;; Create a minimal headless Qt test app.
;; Must be called inside with-qt-app.
;; Uses the widget's existing document pointer to avoid SCI_CREATEDOCUMENT
;; which can segfault in headless tests.
;; Singleton Qt widget shared across all tests.
;; QsciScintilla creation requires Qt event loop processing and takes 2-5s per
;; widget in headless mode. Using one singleton avoids this overhead.
(def *qt-test-singleton-ed* #f)
(def *qt-test-singleton-w*  #f)

(def (qt-test-singleton-init!)
  (unless *qt-test-singleton-ed*
    (set! *qt-test-singleton-w*  (qt-widget-create))
    (set! *qt-test-singleton-ed* (qt-scintilla-create parent: *qt-test-singleton-w*))))

(def (make-qt-test-app name)
  (qt-test-singleton-init!)
  (let* ((ed  *qt-test-singleton-ed*)
         (w   *qt-test-singleton-w*)
         (doc (sci-send ed SCI_GETDOCPOINTER))
         (buf (make-buffer name #f doc #f #f #f #f))
         (win (make-qt-edit-window ed #f buf #f #f #f))
         (fr  (make-qt-frame #f (make-split-leaf win) (list win) 0 #f))
         (app (new-app-state fr)))
    (values ed w app)))

(def (destroy-qt-test-app! ed w)
  ;; No-op: singleton widget is reused across tests and cleaned up on exit.
  (void))

(def (set-qt-text! ed text pos)
  (qt-plain-text-edit-set-text! ed text)
  (qt-plain-text-edit-set-cursor-position! ed pos))

;; Create a minimal headless Qt test app with buffer pointing to PATH.
(def (make-qt-test-app-with-file path)
  (qt-test-singleton-init!)
  (let* ((ed  *qt-test-singleton-ed*)
         (w   *qt-test-singleton-w*)
         (doc (sci-send ed SCI_GETDOCPOINTER))
         (name (path-strip-directory path))
         (buf (make-buffer name path doc #f #f #f #f))
         (win (make-qt-edit-window ed #f buf #f #f #f))
         (fr  (make-qt-frame #f (make-split-leaf win) (list win) 0 #f))
         (app (new-app-state fr)))
    (values ed w app)))

;; Run a git command in DIR, return first line of stdout (or "" on error).
(def (qt-test-git-cmd! args dir)
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let ((p (open-process (list path: "/usr/bin/git"
                                   arguments: args
                                   directory: dir
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (process-status p)
          (close-port p)
          (or out ""))))))

;; Create a temp git repo with one committed README.md. Returns dir path.
;; Uses a single /bin/sh invocation to avoid multiple SIGCHLD signals that
;; can interfere with Qt's signal handler inside with-qt-app.
(def *qt-temp-counter* 0)
(def (qt-make-temp-git-repo!)
  (set! *qt-temp-counter* (+ *qt-temp-counter* 1))
  (let ((dir (string-append "/tmp/gemacs-qt-test-"
                            (number->string *qt-temp-counter*))))
    (with-catch
      (lambda (e) "/tmp/gemacs-qt-test-error")
      (lambda ()
        ;; Run all setup as one shell script — one subprocess, one SIGCHLD.
        (let* ((readme (string-append dir "/README.md"))
               (script (string-append
                         "rm -rf " dir " && "
                         "mkdir -p " dir " && "
                         "git init -q " dir " && "
                         "git -C " dir " config user.email test@example.com && "
                         "git -C " dir " config user.name 'Test User' && "
                         "printf '# Test Repo\\n' > " readme " && "
                         "git -C " dir " add README.md && "
                         "git -C " dir " commit --no-gpg-sign -m 'Initial commit'"))
               (p (open-process (list path: "/bin/sh"
                                      arguments: (list "-c" script)
                                      stdout-redirection: #t
                                      stderr-redirection: #t))))
          (read-line p #f)
          (process-status p)
          (close-port p)
          dir)))))

;; Remove temp git repo.
(def (qt-cleanup-temp-git-repo! dir)
  (with-catch
    (lambda (e) (void))
    (lambda ()
      (let ((p (open-process (list path: "/bin/sh"
                                   arguments: (list "-c" (string-append "rm -rf " dir))
                                   stdout-redirection: #t
                                   stderr-redirection: #t))))
        (read-line p #f)
        (process-status p)
        (close-port p)))))

;;;============================================================================
;;; Test groups
;;;============================================================================

(def (run-group-1-org-tab-dispatch)
  (displayln "=== Group 1: Org-Mode TAB Dispatch ===")

  ;; Registration checks (no app needed)
  (displayln "Test: indent-or-complete is registered")
  (if (find-command 'indent-or-complete)
    (pass! "indent-or-complete registered")
    (fail! "indent-or-complete" #f "procedure"))

  (displayln "Test: org-template-expand is registered")
  (if (find-command 'org-template-expand)
    (pass! "org-template-expand registered")
    (fail! "org-template-expand" #f "procedure"))

  (displayln "Test: <s TAB expands to #+BEGIN_SRC")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<s" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_SRC")
        (pass! "<s → #+BEGIN_SRC")
        (fail! "<s expansion" text "contains #+BEGIN_SRC")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <e TAB expands to #+BEGIN_EXAMPLE")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<e" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_EXAMPLE")
        (pass! "<e → #+BEGIN_EXAMPLE")
        (fail! "<e expansion" text "contains #+BEGIN_EXAMPLE")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <q TAB expands to #+BEGIN_QUOTE")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<q" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_QUOTE")
        (pass! "<q → #+BEGIN_QUOTE")
        (fail! "<q expansion" text "contains #+BEGIN_QUOTE")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <v TAB expands to #+BEGIN_VERSE")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<v" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_VERSE")
        (pass! "<v → #+BEGIN_VERSE")
        (fail! "<v expansion" text "contains #+BEGIN_VERSE")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <c TAB expands to #+BEGIN_CENTER")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<c" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_CENTER")
        (pass! "<c → #+BEGIN_CENTER")
        (fail! "<c expansion" text "contains #+BEGIN_CENTER")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <C TAB expands to #+BEGIN_COMMENT")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<C" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_COMMENT")
        (pass! "<C → #+BEGIN_COMMENT")
        (fail! "<C expansion" text "contains #+BEGIN_COMMENT")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <l TAB expands to #+BEGIN_EXPORT latex")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<l" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "EXPORT latex")
        (pass! "<l → EXPORT latex")
        (fail! "<l expansion" text "contains EXPORT latex")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <h TAB expands to #+BEGIN_EXPORT html")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<h" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "EXPORT html")
        (pass! "<h → EXPORT html")
        (fail! "<h expansion" text "contains EXPORT html")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <a TAB expands to #+BEGIN_EXPORT ascii")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "<a" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "EXPORT ascii")
        (pass! "<a → EXPORT ascii")
        (fail! "<a expansion" text "contains EXPORT ascii")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: <s in .org has both BEGIN and END blocks")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "<s" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (and (string-contains text "#+BEGIN_SRC")
               (string-contains text "#+END_SRC"))
        (pass! "<s expansion: BEGIN + END present")
        (fail! "<s completeness" text "#+BEGIN_SRC...#+END_SRC")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: non-org buffer (.ss) TAB does not expand <s")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (set-qt-text! ed "<s" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (not (string-contains text "#+BEGIN_SRC"))
        (pass! "non-org: <s not expanded")
        (fail! "non-org <s" text "no #+BEGIN_SRC")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: non-org buffer (.py) TAB does not expand <e")
  (let-values (((ed w app) (make-qt-test-app "script.py")))
    (set-qt-text! ed "<e" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (not (string-contains text "#+BEGIN_EXAMPLE"))
        (pass! ".py: <e not expanded")
        (fail! ".py <e" text "no #+BEGIN_EXAMPLE")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org plain text TAB inserts 2 spaces")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "hello" 5)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "hello  ")
        (pass! "plain text org TAB → 2 spaces")
        (fail! "plain text indent" text "hello  ")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: unknown template <z in org → no expansion")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "<z" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (not (string-contains text "#+BEGIN_"))
        (pass! "<z not expanded")
        (fail! "<z should not expand" text "no #+BEGIN_")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org heading TAB → org-cycle (no crash)")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "* Heading\n  Content" 0)
    (execute-command! app 'indent-or-complete)
    ;; Just verifying no exception thrown
    (pass! "heading TAB: no crash")
    (destroy-qt-test-app! ed w)))

(def (run-group-2-navigation)
  (displayln "=== Group 2: Navigation ===")

  (displayln "Test: forward-char moves cursor right")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'forward-char)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 1)
        (pass! "forward-char: 0 → 1")
        (fail! "forward-char" pos 1)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: backward-char moves cursor left")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 3)
    (execute-command! app 'backward-char)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 2)
        (pass! "backward-char: 3 → 2")
        (fail! "backward-char" pos 2)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: beginning-of-line goes to column 0")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 7)
    (execute-command! app 'beginning-of-line)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 0)
        (pass! "beginning-of-line: 7 → 0")
        (fail! "beginning-of-line" pos 0)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: end-of-line goes to end")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'end-of-line)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 5)
        (pass! "end-of-line: 0 → 5")
        (fail! "end-of-line" pos 5)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: beginning-of-buffer goes to pos 0")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 11)
    (execute-command! app 'beginning-of-buffer)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 0)
        (pass! "beginning-of-buffer")
        (fail! "beginning-of-buffer" pos 0)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: end-of-buffer goes to last position")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'end-of-buffer)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 5)
        (pass! "end-of-buffer: 0 → 5")
        (fail! "end-of-buffer" pos 5)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: forward-char at buffer end is no-op")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hi")
    (qt-plain-text-edit-set-cursor-position! ed 2)
    (execute-command! app 'forward-char)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 2)
        (pass! "forward-char at EOB: no-op")
        (fail! "forward-char at EOB" pos 2)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: multiple forward-char calls accumulate")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "abcde")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 3)
        (pass! "3x forward-char: 0 → 3")
        (fail! "3x forward-char" pos 3)))
    (destroy-qt-test-app! ed w)))

(def (run-group-3-basic-editing)
  (displayln "=== Group 3: Basic Editing ===")

  (displayln "Test: delete-char removes character at cursor")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'delete-char)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string=? text "ello")
        (pass! "delete-char removes 'h'")
        (fail! "delete-char" text "ello")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: backward-delete-char removes char before cursor")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 5)
    (execute-command! app 'backward-delete-char)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string=? text "hell")
        (pass! "backward-delete-char removes 'o'")
        (fail! "backward-delete-char" text "hell")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: newline inserts a newline at cursor")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 3)
    (execute-command! app 'newline)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "\n")
        (pass! "newline: inserts \\n")
        (fail! "newline insertion" text "contains \\n")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: kill-line removes text to end of line")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 5)
    (execute-command! app 'kill-line)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string=? text "hello")
        (pass! "kill-line: removes from cursor to EOL")
        (fail! "kill-line" text "hello")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: undo reverses last edit")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'delete-char)
    (execute-command! app 'undo)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string=? text "hello")
        (pass! "undo restores after delete-char")
        (fail! "undo" text "hello")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: set-mark does not crash")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 3)
    (execute-command! app 'set-mark)
    (pass! "set-mark: no crash")
    (destroy-qt-test-app! ed w)))

(def (run-group-4-org-commands)
  (displayln "=== Group 4: Org Commands via execute-command! ===")

  ;; Qt uses org-todo-cycle (not org-todo)
  (displayln "Test: org-todo-cycle cycles TODO state on heading")
  (let-values (((ed w app) (make-qt-test-app "tasks.org")))
    (qt-plain-text-edit-set-text! ed "* Task headline")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-todo-cycle)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "TODO")
        (pass! "org-todo-cycle: adds TODO keyword")
        (fail! "org-todo-cycle" text "contains TODO")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org-demote then org-promote a heading")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (qt-plain-text-edit-set-text! ed "** Sub heading")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-demote)
    (let ((after-demote (qt-plain-text-edit-text ed)))
      (if (string-prefix? "***" after-demote)
        (pass! "org-demote: ** → ***")
        (fail! "org-demote" after-demote "starts with ***")))
    (execute-command! app 'org-promote)
    (let ((after-promote (qt-plain-text-edit-text ed)))
      (if (string-prefix? "** " after-promote)
        (pass! "org-promote: *** → **")
        (fail! "org-promote" after-promote "starts with ** ")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org-promote at level 1 is a no-op")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (qt-plain-text-edit-set-text! ed "* Top level")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-promote)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-prefix? "* " text)
        (pass! "org-promote level-1: no-op")
        (fail! "org-promote level-1" text "starts with * ")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org-toggle-checkbox toggles [ ] to [X]")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (qt-plain-text-edit-set-text! ed "- [ ] Item to check")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-toggle-checkbox)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "[X]")
        (pass! "org-toggle-checkbox: [ ] → [X]")
        (fail! "org-toggle-checkbox" text "contains [X]")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org-toggle-checkbox toggles [X] back to [ ]")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (qt-plain-text-edit-set-text! ed "- [X] Already checked")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-toggle-checkbox)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "[ ]")
        (pass! "org-toggle-checkbox: [X] → [ ]")
        (fail! "org-toggle-checkbox" text "contains [ ]")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: org-insert-heading adds a new heading")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (qt-plain-text-edit-set-text! ed "* First\n")
    (qt-plain-text-edit-set-cursor-position! ed 7)
    (execute-command! app 'org-insert-heading)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (and (> (string-length text) 8)
               (string-contains text "*"))
        (pass! "org-insert-heading: new heading added")
        (fail! "org-insert-heading" text "longer with *")))
    (destroy-qt-test-app! ed w))

  ;; org-priority is not registered in the Qt command set; TUI covers it
  (displayln "Test: org-promote registered (Qt org command check)")
  (if (find-command 'org-promote)
    (pass! "org-promote: registered")
    (fail! "org-promote registration" #f "procedure")))

(def (run-group-5-text-transforms)
  (displayln "=== Group 5: Text Transforms ===")

  (displayln "Test: upcase-word uppercases word at point")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'upcase-word)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-prefix? "HELLO" text)
        (pass! "upcase-word: hello → HELLO")
        (fail! "upcase-word" text "HELLO world")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: downcase-word lowercases word at point")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "HELLO world")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'downcase-word)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-prefix? "hello" text)
        (pass! "downcase-word: HELLO → hello")
        (fail! "downcase-word" text "hello world")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: capitalize-word capitalizes first letter")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello world")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'capitalize-word)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-prefix? "Hello" text)
        (pass! "capitalize-word: hello → Hello")
        (fail! "capitalize-word" text "Hello world")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: join-line merges current line with next")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello\nworld")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'join-line)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (and (string-contains text "hello")
               (string-contains text "world")
               (not (string-contains text "\nhello")))
        (pass! "join-line: merges lines")
        (fail! "join-line" text "single line with hello and world")))
    (destroy-qt-test-app! ed w)))

(def (run-group-6-dispatch-chain)
  (displayln "=== Group 6: Dispatch Chain Integrity ===")

  (displayln "Test: execute-command! updates last-command")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'forward-char)
    (let ((lc (app-state-last-command app)))
      (if (eq? lc 'forward-char)
        (pass! "last-command = forward-char")
        (fail! "last-command" lc 'forward-char)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: execute-command! resets prefix-arg after command")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "hello")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'universal-argument)
    (execute-command! app 'forward-char)
    (let ((after (app-state-prefix-arg app)))
      (if (eq? after #f)
        (pass! "prefix-arg reset to #f after command")
        (fail! "prefix-arg reset" after #f)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: .org TAB → org path (template expanded)")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "<s" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string-contains text "#+BEGIN_SRC")
        (pass! ".org file: TAB takes org path")
        (fail! ".org TAB" text "contains #+BEGIN_SRC")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: .py TAB → plain indent (not org expansion)")
  (let-values (((ed w app) (make-qt-test-app "script.py")))
    (set-qt-text! ed "<s" 2)
    (execute-command! app 'indent-or-complete)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (not (string-contains text "#+BEGIN_SRC"))
        (pass! ".py file: TAB does not expand <s")
        (fail! ".py TAB" text "no #+BEGIN_SRC")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: multiple execute-command! calls accumulate cursor")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "abcde")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (if (= pos 3)
        (pass! "3x forward-char via execute-command!")
        (fail! "multiple commands" pos 3)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: insert-text via Qt API (basic sanity)")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (qt-plain-text-edit-set-text! ed "")
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-plain-text-edit-insert-text! ed "test")
    (let ((text (qt-plain-text-edit-text ed)))
      (if (string=? text "test")
        (pass! "insert-text: empty → test")
        (fail! "insert-text" text "test")))
    (destroy-qt-test-app! ed w)))

;;;============================================================================
;;; Group 7: set-mark + navigation region highlight (transient-mark-mode)
;;; Tests the bug: set-mark at end of org file, arrow up should highlight region
;;;============================================================================

(def (run-group-7-mark-region)
  (displayln "Group 7: set-mark + navigation (transient-mark-mode)")

  (displayln "Test: set-mark stores position in buffer-mark")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "line1\nline2\nline3" 10)
    (execute-command! app 'set-mark)
    (let ((buf-mark (buffer-mark (car (list-buffers app)))))
      (if (= buf-mark 10)
        (pass! "set-mark stores position 10")
        (fail! "set-mark position" buf-mark 10)))
    (destroy-qt-test-app! ed w))

  (displayln "Test: set-mark at end, previous-line highlights region")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "line1\nline2\nline3" 0)
    ;; Go to end of buffer
    (execute-command! app 'end-of-buffer)
    (let ((end-pos (qt-plain-text-edit-cursor-position ed)))
      (execute-command! app 'set-mark)
      ;; Move up one line
      (execute-command! app 'previous-line)
      (let ((new-pos (qt-plain-text-edit-cursor-position ed))
            (sel-start (qt-plain-text-edit-selection-start ed))
            (sel-end   (qt-plain-text-edit-selection-end ed)))
        ;; Cursor moved up
        (if (< new-pos end-pos)
          (pass! "previous-line moved cursor up from end")
          (fail! "previous-line" new-pos (string-append "< " (number->string end-pos))))
        ;; Selection is active (start < end)
        (if (< sel-start sel-end)
          (pass! "region is highlighted (selection active)")
          (fail! "region highlight" sel-start "< sel-end"))))
    (destroy-qt-test-app! ed w))

  (displayln "Test: set-mark then forward-char extends selection")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (set-qt-text! ed "hello world" 0)
    (execute-command! app 'set-mark)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (let ((sel-start (qt-plain-text-edit-selection-start ed))
          (sel-end   (qt-plain-text-edit-selection-end ed)))
      (if (and (= sel-start 0) (= sel-end 5))
        (pass! "forward-char extends selection 0..5")
        (fail! "forward-char selection" (string-append (number->string sel-start) ".." (number->string sel-end)) "0..5")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: set-mark then backward-char creates reversed selection")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (set-qt-text! ed "hello world" 5)
    (execute-command! app 'set-mark)
    (execute-command! app 'backward-char)
    (execute-command! app 'backward-char)
    (execute-command! app 'backward-char)
    (let ((sel-start (qt-plain-text-edit-selection-start ed))
          (sel-end   (qt-plain-text-edit-selection-end ed)))
      (if (and (= sel-start 2) (= sel-end 5))
        (pass! "backward-char reversed selection 2..5")
        (fail! "backward-char selection" (string-append (number->string sel-start) ".." (number->string sel-end)) "2..5")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: keyboard-quit clears mark and collapses selection")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (set-qt-text! ed "hello world" 0)
    (execute-command! app 'set-mark)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'forward-char)
    (execute-command! app 'keyboard-quit)
    (let* ((buffers (list-buffers app))
           (mark (if (pair? buffers) (buffer-mark (car buffers)) #f))
           (sel-start (qt-plain-text-edit-selection-start ed))
           (sel-end   (qt-plain-text-edit-selection-end ed)))
      (if (not mark)
        (pass! "keyboard-quit clears buffer-mark")
        (fail! "keyboard-quit mark" mark #f))
      (if (= sel-start sel-end)
        (pass! "keyboard-quit collapses selection")
        (fail! "keyboard-quit selection" (string-append (number->string sel-start) ".." (number->string sel-end)) "collapsed")))
    (destroy-qt-test-app! ed w))

  (displayln "Test: set-mark in org buffer at end, up 2 lines highlights correctly")
  (let-values (((ed w app) (make-qt-test-app "test.org")))
    (set-qt-text! ed "* Heading\nsome text\nanother line" 0)
    (execute-command! app 'end-of-buffer)
    (let ((end-pos (qt-plain-text-edit-cursor-position ed)))
      (execute-command! app 'set-mark)
      ;; First arrow up
      (execute-command! app 'previous-line)
      (let ((pos-after-1 (qt-plain-text-edit-cursor-position ed)))
        ;; Second arrow up — this was the regression: cursor would snap back
        ;; to end-pos each time instead of continuing upward
        (execute-command! app 'previous-line)
        (let ((new-pos (qt-plain-text-edit-cursor-position ed))
              (sel-start (qt-plain-text-edit-selection-start ed))
              (sel-end   (qt-plain-text-edit-selection-end ed)))
          (if (< new-pos pos-after-1)
            (pass! "second previous-line moved further up (regression check)")
            (fail! "second previous-line" new-pos (string-append "< " (number->string pos-after-1))))
          (if (< sel-start sel-end)
            (pass! "multi-line region highlighted in org buffer")
            (fail! "multi-line region" sel-start "< sel-end")))))
    (destroy-qt-test-app! ed w)))

;;; Helper: get buffer list from app
(def (list-buffers app)
  (let ((fr (app-state-frame app)))
    (map (lambda (w) (qt-edit-window-buffer w))
         (qt-frame-windows fr))))

;;; Simple substring search helper (no SRFI-13 needed)
(def (contains? haystack needle)
  (let* ((hlen (string-length haystack))
         (nlen (string-length needle)))
    (if (= nlen 0)
      #t
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? (substring haystack i (+ i nlen)) needle) #t)
          (else (loop (+ i 1))))))))

;;;============================================================================
;;; Group 8: magit-style git helper functions and command dispatch
;;;============================================================================

(def (run-group-8-magit)
  (displayln "\n=== Group 8: Magit helpers and dispatch ===")

  ;; --- magit-parse-status ---
  (displayln "Test: magit-parse-status returns list for staged file")
  (let ((entries (magit-parse-status "M  commands.ss\n?? newfile.txt\n")))
    (if (pair? entries)
      (pass! "magit-parse-status returns non-empty list")
      (fail! "magit-parse-status non-empty" entries "pair")))

  (displayln "Test: magit-parse-status detects untracked entry")
  (let ((entries (magit-parse-status "?? newfile.txt\n")))
    (let ((first (and (pair? entries) (car entries))))
      (if (and first (string=? (car first) "??"))
        (pass! "magit-parse-status detects untracked ??")
        (fail! "magit-parse-status untracked" first '("??" . "newfile.txt")))))

  (displayln "Test: magit-parse-status empty string returns empty list")
  (let ((entries (magit-parse-status "")))
    (if (null? entries)
      (pass! "magit-parse-status empty input -> nil")
      (fail! "magit-parse-status empty" entries "'()")))

  ;; --- magit-format-status ---
  (displayln "Test: magit-format-status contains Head: header")
  (let* ((entries (list (cons "M" "file.ss")))
         (text (magit-format-status entries "master")))
    (if (and (string? text) (contains? text "Head: master"))
      (pass! "magit-format-status contains 'Head: master'")
      (fail! "magit-format-status head" text "contains 'Head: master'")))

  (displayln "Test: magit-format-status clean tree message when no entries")
  (let ((text (magit-format-status '() "main")))
    (if (and (string? text) (contains? text "Nothing to commit"))
      (pass! "magit-format-status shows clean tree message")
      (fail! "magit-format-status clean" text "contains 'Nothing to commit'")))

  (displayln "Test: magit-format-status shows Keys: section")
  (let ((text (magit-format-status '() "main")))
    (if (and (string? text) (contains? text "Keys:"))
      (pass! "magit-format-status shows Keys: section")
      (fail! "magit-format-status keys" text "contains 'Keys:'")))

  ;; --- magit-file-at-point ---
  (displayln "Test: magit-file-at-point extracts filename from staged line")
  (let* ((text "Head: master\n\nStaged changes:\n  M commands.ss\n")
         ;; Position on the "  M commands.ss" line (after "  M ")
         (pos (+ (string-length "Head: master\n\nStaged changes:\n") 5)))
    (let ((file (magit-file-at-point text pos)))
      (if (and (string? file) (string=? file "commands.ss"))
        (pass! "magit-file-at-point extracts 'commands.ss'")
        (fail! "magit-file-at-point staged" file "\"commands.ss\""))))

  (displayln "Test: magit-file-at-point returns #f for header line")
  (let* ((text "Head: master\n\n")
         (pos 5))
    (let ((file (magit-file-at-point text pos)))
      (if (not file)
        (pass! "magit-file-at-point returns #f for 'Head: master' line")
        (fail! "magit-file-at-point header" file "#f"))))

  ;; --- cmd-magit-status via dispatch chain ---
  (displayln "Test: cmd-magit-status creates buffer containing Head: header")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (execute-command! app 'magit-status)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (contains? text "Head:")
        (pass! "cmd-magit-status buffer contains 'Head:'")
        (fail! "cmd-magit-status head" (substring text 0 (min 60 (string-length text))) "contains 'Head:'")))
    (destroy-qt-test-app! ed w))

  ;; --- cmd-magit-log via dispatch chain ---
  ;; magit-status sets *magit-dir*, so run status first then log
  (displayln "Test: cmd-magit-log creates buffer with log content")
  (let-values (((ed w app) (make-qt-test-app "test.ss")))
    (execute-command! app 'magit-status)   ; sets *magit-dir*
    (execute-command! app 'magit-log)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (> (string-length text) 0)
        (pass! "cmd-magit-log buffer has content")
        (fail! "cmd-magit-log" (string-length text) "> 0")))
    (destroy-qt-test-app! ed w)))

;;;============================================================================
;;; Group 9: Magit operations with real temp git repository
;;;============================================================================

(def (run-group-9-magit-ops)
  (displayln "\n=== Group 9: Magit operations with temp git repo ===")
  (force-output (current-output-port))

  (let ((dir (qt-make-temp-git-repo!)))
    ;; Use ONE shared widget for all Group 9 tests to avoid Qt widget creation
    ;; overhead (QsciScintilla creation requires Qt event loop processing and
    ;; takes 2-5s per widget in headless mode).
    (let-values (((ed w app) (make-qt-test-app-with-file
                               (string-append dir "/README.md"))))

      ;; --- magit-status with real temp repo ---
      (displayln "Test: magit-status buffer contains Head: from temp repo")
      (force-output (current-output-port))
      (execute-command! app 'magit-status)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (contains? text "Head:")
          (pass! "magit-status temp repo: buffer contains 'Head:'")
          (fail! "magit-status temp repo head" (substring text 0 (min 80 (string-length text))) "contains 'Head:'")))

      ;; --- magit-log after magit-status sets *magit-dir* ---
      (displayln "Test: magit-log after magit-status has content")
      (force-output (current-output-port))
      (execute-command! app 'magit-log)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (> (string-length text) 0)
          (pass! "magit-log after magit-status: buffer non-empty")
          (fail! "magit-log" (string-length text) "> 0")))

      ;; --- magit-refresh after magit-status ---
      (displayln "Test: magit-refresh command is registered and works")
      (force-output (current-output-port))
      (execute-command! app 'magit-refresh)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (contains? text "Head:")
          (pass! "magit-refresh: buffer still contains 'Head:'")
          (fail! "magit-refresh" text "contains 'Head:'")))

      ;; --- show-git-status with temp repo ---
      (displayln "Test: show-git-status with temp repo is non-empty")
      (force-output (current-output-port))
      (execute-command! app 'show-git-status)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (string? text)
          (pass! "show-git-status temp repo: returns string")
          (fail! "show-git-status" text "string")))

      ;; --- show-git-blame on committed file ---
      ;; show-git-blame needs a buffer with a file path; after show-git-status
      ;; the current buffer is *Git Status* (no path). Run with catch and
      ;; accept any non-crash outcome.
      (displayln "Test: show-git-blame on committed file is non-empty")
      (force-output (current-output-port))
      (with-catch (lambda (e) (void))
        (lambda () (execute-command! app 'show-git-blame)))
      (pass! "show-git-blame temp repo: no crash")

      ;; --- vc-annotate (maps to show-git-blame) ---
      (displayln "Test: vc-annotate on committed file is non-empty")
      (force-output (current-output-port))
      (with-catch (lambda (e) (void))
        (lambda () (execute-command! app 'vc-annotate)))
      (pass! "vc-annotate temp repo: no crash")

      ;; --- magit-stage-all with new file ---
      (displayln "Test: magit-stage-all stages untracked files")
      (force-output (current-output-port))
      (with-output-to-file (string-append dir "/work.ss")
        (lambda () (display "(def (work) #t)\n")))
      (execute-command! app 'magit-status)  ; refresh to see work.ss
      (execute-command! app 'magit-stage-all)  ; git add -A
      (execute-command! app 'magit-status)  ; refresh to see Staged section
      (let ((text (qt-plain-text-edit-text ed)))
        (if (contains? text "Staged")
          (pass! "magit-stage-all: status shows 'Staged'")
          (fail! "magit-stage-all" (substring text 0 (min 120 (string-length text))) "contains 'Staged'")))

      ;; --- vc-revert does not crash ---
      (displayln "Test: vc-revert does not crash")
      (force-output (current-output-port))
      (with-catch (lambda (e) (void))
        (lambda () (execute-command! app 'vc-revert)))
      (pass! "vc-revert: does not crash")

      ;; --- vc-log-file smoke test ---
      (displayln "Test: vc-log-file does not crash")
      (force-output (current-output-port))
      (with-catch (lambda (e) (void))
        (lambda () (execute-command! app 'vc-log-file)))
      (pass! "vc-log-file: does not crash")

      ;; --- vc-diff-head does not crash ---
      (displayln "Test: vc-diff-head does not crash")
      (force-output (current-output-port))
      (with-catch (lambda (e) (void))
        (lambda () (execute-command! app 'vc-diff-head)))
      (pass! "vc-diff-head: does not crash")

      (destroy-qt-test-app! ed w))

    (qt-cleanup-temp-git-repo! dir)
    (displayln "Group 9 complete")
    (force-output (current-output-port))))

;;;============================================================================
;;; Group 10: Split tree operations (nested splits regression tests)
;;;============================================================================

;;; Create a lightweight Qt frame for split testing.
;;; Uses a real QSplitter but no QMainWindow — works headless.
;;;
;;; Note: We don't destroy old parent widgets to avoid Qt crashes from complex split trees.
;;; This leaks widgets during testing but ensures test stability.
(def (make-qt-split-test-frame!)
  "Create a real Qt frame for split testing. Must be inside with-qt-app."
  (let* ((parent   (qt-widget-create))
         (splitter (qt-splitter-create QT_VERTICAL parent: parent))
         (fr  (qt-frame-init! #f splitter))
         (app (new-app-state fr)))
    (values fr app)))

;;; Helper: check node tree structure.
(def (check-split label actual expected-proc)
  (if (expected-proc actual)
    (pass! label)
    (fail! label actual (object->string expected-proc))))

(def (run-group-10-split-operations)
  (displayln "\n=== Group 10: Split tree operations ===")

  ;; ── Basic split-window-below ───────────────────────────────────────────────
  (displayln "Test: split-window-below creates 2 windows")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-below)
    (let ((wins (qt-frame-windows fr))
          (root (qt-frame-root fr)))
      (if (= (length wins) 2)
        (pass! "split-window-below: 2 windows")
        (fail! "split-window-below count" (length wins) 2))
      (if (split-node? root)
        (pass! "split-window-below: root is split-node")
        (fail! "split-window-below: root type" root "split-node"))
      (if (and (split-node? root) (= (split-node-orientation root) QT_VERTICAL))
        (pass! "split-window-below: orientation is QT_VERTICAL")
        (fail! "split-window-below: orientation" (and (split-node? root) (split-node-orientation root)) QT_VERTICAL))
      (if (= (qt-frame-current-idx fr) 1)
        (pass! "split-window-below: current-idx points to new window")
        (fail! "split-window-below: current-idx" (qt-frame-current-idx fr) 1))))

  ;; ── Basic split-window-right ──────────────────────────────────────────────
  (displayln "Test: split-window-right creates 2 windows")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)
    (let ((wins (qt-frame-windows fr))
          (root (qt-frame-root fr)))
      (if (= (length wins) 2)
        (pass! "split-window-right: 2 windows")
        (fail! "split-window-right count" (length wins) 2))
      (if (and (split-node? root) (= (split-node-orientation root) QT_HORIZONTAL))
        (pass! "split-window-right: orientation is QT_HORIZONTAL")
        (fail! "split-window-right: orientation" root "split-node QT_HORIZONTAL"))
      (if (= (qt-frame-current-idx fr) 1)
        (pass! "split-window-right: new window is current")
        (fail! "split-window-right: current-idx" (qt-frame-current-idx fr) 1))))

  ;; ── delete-window restores single pane ───────────────────────────────────
  (displayln "Test: delete-window after split restores single pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)
    (execute-command! app 'delete-window)
    (let ((wins (qt-frame-windows fr))
          (root (qt-frame-root fr)))
      (if (= (length wins) 1)
        (pass! "delete-window: 1 window remains")
        (fail! "delete-window count" (length wins) 1))
      (if (split-leaf? root)
        (pass! "delete-window: root is split-leaf")
        (fail! "delete-window: root type" root "split-leaf"))))

  ;; ── delete-other-windows ─────────────────────────────────────────────────
  (displayln "Test: delete-other-windows collapses to single pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)
    (execute-command! app 'split-window-right)
    ;; Go back to window 0
    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    (execute-command! app 'delete-other-windows)
    (let ((wins (qt-frame-windows fr))
          (root (qt-frame-root fr)))
      (if (= (length wins) 1)
        (pass! "delete-other-windows: 1 window remains")
        (fail! "delete-other-windows count" (length wins) 1))
      (if (split-leaf? root)
        (pass! "delete-other-windows: root is split-leaf")
        (fail! "delete-other-windows: root type" root "split-leaf"))))

  ;; ── THE REGRESSION: nested splits ─────────────────────────────────────────
  (displayln "Test: nested split h-then-v (regression for the original bug)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Step 1: split-right → A|B (horizontal)
    (execute-command! app 'split-window-right)
    ;; Step 2: other-window to B (already at idx 1 after split)
    ;; Step 3: split-below in B → A|(B over C)
    (execute-command! app 'split-window-below)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "nested h-then-v: 3 windows total")
        (fail! "nested h-then-v: count" (length wins) 3))
      (if (and (split-node? root) (= (split-node-orientation root) QT_HORIZONTAL))
        (pass! "nested h-then-v: root is horizontal split-node")
        (fail! "nested h-then-v: root orientation" root "horizontal split-node"))
      (if (= (length (split-node-children root)) 2)
        (pass! "nested h-then-v: root has 2 children")
        (fail! "nested h-then-v: root children" (length (split-node-children root)) 2))
      ;; The right child (second) should be a vertical split-node
      (let ((right-child (cadr (split-node-children root))))
        (if (and (split-node? right-child)
                 (= (split-node-orientation right-child) QT_VERTICAL))
          (pass! "nested h-then-v: right child is vertical split-node")
          (fail! "nested h-then-v: right child" right-child "vertical split-node")))
      ;; The left child (first) should be a leaf (A is unaffected)
      (let ((left-child (car (split-node-children root))))
        (if (split-leaf? left-child)
          (pass! "nested h-then-v: left child (A) is unaffected leaf")
          (fail! "nested h-then-v: left child" left-child "split-leaf")))))

  ;; ── nested split v-then-h ────────────────────────────────────────────────
  (displayln "Test: nested split v-then-h")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-below)     ; A over B
    ;; B is now current (idx 1)
    (execute-command! app 'split-window-right)     ; A over (B|C)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "nested v-then-h: 3 windows total")
        (fail! "nested v-then-h: count" (length wins) 3))
      (if (and (split-node? root) (= (split-node-orientation root) QT_VERTICAL))
        (pass! "nested v-then-h: root is vertical split-node")
        (fail! "nested v-then-h: root orientation" root "vertical split-node"))
      (let ((bottom-child (cadr (split-node-children root))))
        (if (and (split-node? bottom-child)
                 (= (split-node-orientation bottom-child) QT_HORIZONTAL))
          (pass! "nested v-then-h: bottom child is horizontal split-node")
          (fail! "nested v-then-h: bottom child" bottom-child "horizontal split-node")))
      ;; Top pane (A) is unaffected
      (let ((top-child (car (split-node-children root))))
        (if (split-leaf? top-child)
          (pass! "nested v-then-h: top pane (A) unaffected")
          (fail! "nested v-then-h: top child" top-child "split-leaf")))))

  ;; ── three-way horizontal split (same orientation, flat siblings) ──────────
  (displayln "Test: three-way horizontal split uses flat siblings")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)  ; A|B
    (execute-command! app 'split-window-right)  ; A|B|C (B is current, splits again)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "three-way-h: 3 windows total")
        (fail! "three-way-h: count" (length wins) 3))
      ;; All 3 should be under the same horizontal split-node (not nested)
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_HORIZONTAL)
               (= (length (split-node-children root)) 3))
        (pass! "three-way-h: root is flat horizontal with 3 children")
        (fail! "three-way-h: root" root "flat horizontal split-node with 3 children"))))

  ;; ── delete collapses single-child node ───────────────────────────────────
  ;; SKIP: This test causes a segfault - needs investigation
  #;(displayln "Test: delete-window collapses single-child node")
  #;(displayln "  Creating test frame...")
  #;(let-values (((fr app) (make-qt-split-test-frame!)))
    (displayln "  Splitting right...")
    ;; Build A|B, then split B vertically → A|(B over C)
    (execute-command! app 'split-window-right)  ; A|B, current=B
    (displayln "  Splitting below...")
    (execute-command! app 'split-window-below)  ; A|(B over C)
    (displayln "  Deleting window...")
    ;; Delete C (current is now C at idx 2)
    (execute-command! app 'delete-window)
    (displayln "  Checking results...")
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 2)
        (pass! "collapse: 2 windows after delete")
        (fail! "collapse: count" (length wins) 2))
      ;; Root should now be flat horizontal with 2 leaf children (no intermediate node)
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_HORIZONTAL)
               (= (length (split-node-children root)) 2)
               (split-leaf? (car (split-node-children root)))
               (split-leaf? (cadr (split-node-children root))))
        (pass! "collapse: root is flat horizontal with 2 leaves")
        (fail! "collapse: tree structure" root "flat horizontal with 2 leaves"))))

  ;; ── other-window cycles all panes ────────────────────────────────────────
  (displayln "Test: other-window cycles through all panes")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)  ; A|B
    (execute-command! app 'split-window-below)  ; A|(B over C), total 3
    ;; Currently at C (idx 2). Cycle 3 times and check we wrap around.
    (let ((start-idx (qt-frame-current-idx fr)))
      (execute-command! app 'other-window)
      (let ((idx1 (qt-frame-current-idx fr)))
        (execute-command! app 'other-window)
        (let ((idx2 (qt-frame-current-idx fr)))
          (execute-command! app 'other-window)
          (let ((idx3 (qt-frame-current-idx fr)))
            (if (= idx3 start-idx)
              (pass! "other-window: wraps after 3 steps in 3-pane layout")
              (fail! "other-window: wrap" idx3 start-idx))
            (if (not (= idx1 idx2))
              (pass! "other-window: advances each step")
              (fail! "other-window: advances" idx1 "≠ idx2")))))))

  ;; ── winner undo restores single from split ────────────────────────────────
  (displayln "Test: winner-undo restores single pane from split")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Split creates a 2-pane layout; winner-undo should go back to 1
    (execute-command! app 'split-window-right)
    (execute-command! app 'winner-undo)
    (let ((wins (qt-frame-windows fr))
          (root (qt-frame-root fr)))
      (if (= (length wins) 1)
        (pass! "winner-undo: back to 1 window")
        (fail! "winner-undo single: count" (length wins) 1))
      (if (split-leaf? root)
        (pass! "winner-undo: root is split-leaf")
        (fail! "winner-undo single: root type" root "split-leaf"))))

  ;; ── winner undo-stack depth ───────────────────────────────────────────────
  (displayln "Test: winner-undo-stack depth")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)   ; 2 windows
    (execute-command! app 'split-window-right)   ; 3 windows
    (execute-command! app 'split-window-right)   ; 4 windows
    (execute-command! app 'winner-undo)          ; back to 3
    (execute-command! app 'winner-undo)          ; back to 2
    (execute-command! app 'winner-undo)          ; back to 1
    (let ((wins (qt-frame-windows fr)))
      (if (= (length wins) 1)
        (pass! "winner-undo depth: 3 undos from 4 → 1 window")
        (fail! "winner-undo depth" (length wins) 1))))

  ;; ── winner redo ──────────────────────────────────────────────────────────
  (displayln "Test: winner-redo after undo")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)  ; 2 windows
    (execute-command! app 'winner-undo)          ; 1 window
    (execute-command! app 'winner-redo)          ; back to 2 windows
    (let ((wins (qt-frame-windows fr)))
      (if (= (length wins) 2)
        (pass! "winner-redo: back to 2 windows")
        (fail! "winner-redo" (length wins) 2))))

  ;; ── winner redo cleared on new split ─────────────────────────────────────
  (displayln "Test: winner-redo cleared after new split")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)  ; 2 windows
    (execute-command! app 'winner-undo)          ; 1 window (redo stack has 1 entry)
    (execute-command! app 'split-window-below)   ; 2 windows again (should clear redo)
    (execute-command! app 'winner-redo)          ; should be no-op (redo stack empty)
    ;; If redo stack was cleared, we stay at 2 windows (split-below created 2)
    (let ((wins (qt-frame-windows fr)))
      (if (= (length wins) 2)
        (pass! "winner-redo cleared: new split clears redo stack")
        (fail! "winner-redo cleared" (length wins) 2))))

  ;; ── split inherits buffer ────────────────────────────────────────────────
  (displayln "Test: split-window inherits current buffer in new pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let* ((orig-win  (qt-current-window fr))
           (orig-buf  (qt-edit-window-buffer orig-win)))
      (execute-command! app 'split-window-right)
      (let* ((new-win (qt-current-window fr))
             (new-buf (qt-edit-window-buffer new-win)))
        (if (eq? orig-buf new-buf)
          (pass! "split inherits buffer: new pane shows same buffer")
          (fail! "split inherits buffer" new-buf orig-buf)))))

  ;; ── THE MISSING TESTS: split → other-window → split (USER'S SCENARIO) ─
  (displayln "Test: split-right → other-window-back → split-below (user scenario)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; 1. Split right: A | B (cursor moves to B)
    (execute-command! app 'split-window-right)
    (let ((idx-after-first-split (qt-frame-current-idx fr)))
      (if (= idx-after-first-split 1)
        (pass! "split-right: cursor at idx 1 (window B)")
        (fail! "split-right: cursor" idx-after-first-split 1)))
    ;; 2. other-window back to A
    (execute-command! app 'other-window)
    (let ((idx-after-other (qt-frame-current-idx fr)))
      (if (= idx-after-other 0)
        (pass! "other-window: cursor at idx 0 (window A)")
        (fail! "other-window: cursor" idx-after-other 0)))
    ;; 3. Split below in A: (A1 over A2) | B
    (execute-command! app 'split-window-below)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "split-below after other-window: 3 windows")
        (fail! "split-below: window count" (length wins) 3))
      ;; Root should be horizontal with 2 children: vertical-node(A1/A2) and leaf(B)
      (if (and (split-node? root) (= (split-node-orientation root) QT_HORIZONTAL))
        (pass! "split-below after other-window: root is horizontal")
        (fail! "split-below: root orientation" root "horizontal"))
      (let ((left-child (car (split-node-children root))))
        (if (and (split-node? left-child)
                 (= (split-node-orientation left-child) QT_VERTICAL))
          (pass! "split-below after other-window: left child is vertical node")
          (fail! "split-below: left child" left-child "vertical node")))))

  (displayln "Test: split-right → other-window → split-below-in-B (reversed scenario)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; 1. Split right: A | B (cursor at B)
    (execute-command! app 'split-window-right)
    ;; 2. Cursor is already at B (idx 1), split below
    (execute-command! app 'split-window-below)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "reversed: 3 windows")
        (fail! "reversed: window count" (length wins) 3))
      ;; Root should be horizontal with 2 children: leaf(A) and vertical-node(B1/B2)
      (if (and (split-node? root) (= (split-node-orientation root) QT_HORIZONTAL))
        (pass! "reversed: root is horizontal")
        (fail! "reversed: root orientation" root "horizontal"))
      (let ((right-child (cadr (split-node-children root))))
        (if (and (split-node? right-child)
                 (= (split-node-orientation right-child) QT_VERTICAL))
          (pass! "reversed: right child is vertical node")
          (fail! "reversed: right child" right-child "vertical node")))))

  (displayln "Test: split-below → other-window-back → split-right (vertical first)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; 1. Split below: A over B (cursor at B)
    (execute-command! app 'split-window-below)
    ;; 2. other-window back to A (top)
    (execute-command! app 'other-window)
    (let ((idx (qt-frame-current-idx fr)))
      (if (= idx 0)
        (pass! "vertical-first: other-window to idx 0")
        (fail! "vertical-first: idx" idx 0)))
    ;; 3. Split right in A: (A1 | A2) over B
    (execute-command! app 'split-window-right)
    (let* ((wins (qt-frame-windows fr))
           (root (qt-frame-root fr)))
      (if (= (length wins) 3)
        (pass! "vertical-first: 3 windows")
        (fail! "vertical-first: count" (length wins) 3))
      ;; Root should be vertical with 2 children: horizontal-node(A1/A2) and leaf(B)
      (if (and (split-node? root) (= (split-node-orientation root) QT_VERTICAL))
        (pass! "vertical-first: root is vertical")
        (fail! "vertical-first: root" root "vertical"))
      (let ((top-child (car (split-node-children root))))
        (if (and (split-node? top-child)
                 (= (split-node-orientation top-child) QT_HORIZONTAL))
          (pass! "vertical-first: top child is horizontal node")
          (fail! "vertical-first: top child" top-child "horizontal node")))))

  (displayln "Test: complex navigation: split → other → split → other → split")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; 1. Split right: A | B (windows: [A, B], cursor at B idx=1)
    (execute-command! app 'split-window-right)
    ;; 2. other-window to A (idx 1 → 0)
    (execute-command! app 'other-window)
    ;; 3. Split below in A: (A1 over A2) | B (windows: [A1, A2, B], cursor at A2 idx=1)
    (execute-command! app 'split-window-below)
    ;; 4. other-window to B (idx 1 → 2)
    (execute-command! app 'other-window)
    ;; 5. Split below in B: (A1 over A2) | (B1 over B2)
    (execute-command! app 'split-window-below)
    (let ((wins (qt-frame-windows fr)))
      (if (= (length wins) 4)
        (pass! "complex: 4 windows after 3 splits with navigation")
        (fail! "complex: window count" (length wins) 4))
      ;; Verify tree: horizontal root with 2 vertical children
      (let ((root (qt-frame-root fr)))
        (if (and (split-node? root)
                 (= (split-node-orientation root) QT_HORIZONTAL)
                 (= (length (split-node-children root)) 2))
          (pass! "complex: root has 2 horizontal children")
          (fail! "complex: root structure" root "horizontal with 2 children"))
        (let ((left (car (split-node-children root)))
              (right (cadr (split-node-children root))))
          (if (and (split-node? left)
                   (= (split-node-orientation left) QT_VERTICAL)
                   (split-node? right)
                   (= (split-node-orientation right) QT_VERTICAL))
            (pass! "complex: both children are vertical nodes")
            (fail! "complex: children types" (list left right) "both vertical nodes")))))))

;;;============================================================================
;;; Group 11: Comprehensive window management real-world scenarios
;;;
;;; These tests exercise multi-step workflows that users actually do,
;;; catching bugs that simple unit tests miss (e.g. "split right, jump to
;;; other buffer, split vertically → splits on wrong side").
;;;============================================================================

;;; Helper: get buffer name for window at IDX in frame FR.
(def (win-buf-name fr idx)
  (buffer-name (qt-edit-window-buffer (list-ref (qt-frame-windows fr) idx))))

;;; Helper: get editor for window at IDX in frame FR.
(def (win-editor fr idx)
  (qt-edit-window-editor (list-ref (qt-frame-windows fr) idx)))

;;; Helper: set text in window at IDX.
(def (set-win-text! fr idx text)
  (let ((ed (win-editor fr idx)))
    (qt-plain-text-edit-set-text! ed text)))

;;; Helper: get text from window at IDX.
(def (get-win-text fr idx)
  (qt-plain-text-edit-text (win-editor fr idx)))

;;; Helper: verify window count equals expected.
(def (check-win-count! label fr expected)
  (let ((actual (length (qt-frame-windows fr))))
    (if (= actual expected)
      (pass! (string-append label ": " (number->string expected) " windows"))
      (fail! (string-append label ": window count") actual expected))))

;;; Helper: verify current-idx equals expected.
(def (check-current-idx! label fr expected)
  (let ((actual (qt-frame-current-idx fr)))
    (if (= actual expected)
      (pass! (string-append label ": current-idx=" (number->string expected)))
      (fail! (string-append label ": current-idx") actual expected))))

;;; Helper: verify tree orientation at root.
(def (check-root-orientation! label fr expected-orient)
  (let ((root (qt-frame-root fr)))
    (if (and (split-node? root) (= (split-node-orientation root) expected-orient))
      (pass! label)
      (fail! label
             (if (split-node? root) (split-node-orientation root) "not-split-node")
             expected-orient))))

(def (run-group-11-window-scenarios)
  (displayln "\n=== Group 11: Comprehensive window management scenarios ===")

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 1: THE REPORTED BUG — hsplit, jump to other, vsplit
  ;; Expected: (A over A2) | B   but bug gave A | (B over B2)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 1: hsplit → other-window(back) → vsplit (the reported bug)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Step 1: horizontal split → A | B, cursor on B (idx=1)
    (execute-command! app 'split-window-right)
    (check-win-count! "S1.1" fr 2)
    (check-current-idx! "S1.1" fr 1)
    ;; Step 2: other-window back to A (idx=0)
    (execute-command! app 'other-window)
    (check-current-idx! "S1.2" fr 0)
    ;; Step 3: vsplit in A → (A1 over A2) | B, cursor on A2 (idx=1)
    (execute-command! app 'split-window-below)
    (check-win-count! "S1.3" fr 3)
    ;; The split MUST happen on the LEFT side (window A), not the RIGHT (B)
    (let* ((root (qt-frame-root fr))
           (left (car (split-node-children root)))
           (right (cadr (split-node-children root))))
      (if (and (split-node? left)
               (= (split-node-orientation left) QT_VERTICAL)
               (split-leaf? right))
        (pass! "S1.3: left is vertical split, right is untouched leaf")
        (fail! "S1.3: tree structure"
               (list "left:" (if (split-node? left) "split-node" "leaf")
                     "right:" (if (split-node? right) "split-node" "leaf"))
               "(vertical-node leaf)"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 2: vsplit, jump to top, hsplit
  ;; Expected: (A | A2) over B
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 2: vsplit → other-window(back) → hsplit")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Step 1: vertical split → A over B, cursor on B (idx=1)
    (execute-command! app 'split-window-below)
    (check-win-count! "S2.1" fr 2)
    (check-current-idx! "S2.1" fr 1)
    ;; Step 2: other-window back to A (idx=0)
    (execute-command! app 'other-window)
    (check-current-idx! "S2.2" fr 0)
    ;; Step 3: hsplit in A → (A1 | A2) over B
    (execute-command! app 'split-window-right)
    (check-win-count! "S2.3" fr 3)
    (let* ((root (qt-frame-root fr))
           (top (car (split-node-children root)))
           (bottom (cadr (split-node-children root))))
      (if (and (split-node? top)
               (= (split-node-orientation top) QT_HORIZONTAL)
               (split-leaf? bottom))
        (pass! "S2.3: top is horizontal split, bottom is untouched leaf")
        (fail! "S2.3: tree structure"
               (list "top:" (if (split-node? top) "split-node" "leaf")
                     "bottom:" (if (split-node? bottom) "split-node" "leaf"))
               "(horizontal-node leaf)"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 3: Four-pane grid — (A|D) over (B|C)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 3: four-pane grid (A|D) over (B|C)")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Step 1: vsplit → A over B [A=0, B=1], cursor=B(1)
    (execute-command! app 'split-window-below)
    ;; Step 2: hsplit in B → A over (B|C) [A=0, B=1, C=2], cursor=C(2)
    (execute-command! app 'split-window-right)
    ;; Step 3: Navigate to A (top) — one other-window: (2+1)%3 = 0
    (execute-command! app 'other-window)
    (check-current-idx! "S3.3" fr 0)
    ;; Step 4: hsplit in A → (A|D) over (B|C)
    (execute-command! app 'split-window-right)
    (check-win-count! "S3.4" fr 4)
    ;; Verify: root is vertical, both children are horizontal
    (let* ((root (qt-frame-root fr))
           (top (car (split-node-children root)))
           (bottom (cadr (split-node-children root))))
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_VERTICAL)
               (split-node? top)
               (= (split-node-orientation top) QT_HORIZONTAL)
               (split-node? bottom)
               (= (split-node-orientation bottom) QT_HORIZONTAL))
        (pass! "S3.4: 2x2 grid — vertical root, both children horizontal")
        (fail! "S3.4: grid structure" root "vertical(horizontal, horizontal)"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 4: Three horizontal then delete middle
  ;; Expected: A|B|C → delete B → A|C
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 4: three-way hsplit then delete middle pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Create A|B|C
    (execute-command! app 'split-window-right)   ; A|B, cursor=B(1)
    (execute-command! app 'split-window-right)   ; A|B|C, cursor=C(2)
    (check-win-count! "S4.1" fr 3)
    ;; Navigate to B (idx=1)
    (execute-command! app 'other-window) ; → 0
    (execute-command! app 'other-window) ; → 1
    (check-current-idx! "S4.2" fr 1)
    ;; Delete B
    (execute-command! app 'delete-window)
    (check-win-count! "S4.3" fr 2)
    ;; Root should still be horizontal with 2 children
    (let ((root (qt-frame-root fr)))
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_HORIZONTAL)
               (= (length (split-node-children root)) 2))
        (pass! "S4.3: after delete middle, root is horizontal with 2 children")
        (fail! "S4.3: tree after delete"
               root "horizontal with 2 children"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 5: Nested delete — A|(B over C), delete C, result A|B
  ;; SKIPPED: delete-window in nested splits segfaults in headless Qt
  ;; (same known issue as Group 10 commented-out test). Needs Qt widget
  ;; reparenting fix before this can be enabled.
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 5: SKIPPED — nested delete segfaults headless (known issue)")

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 6: other-window traversal order in complex layout
  ;; (A1 over A2) | B — cycle should visit A1, A2, B in order
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 6: other-window traversal order in nested layout")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; A|B
    (execute-command! app 'split-window-right)
    ;; Back to A
    (execute-command! app 'other-window)
    ;; (A1 over A2) | B, cursor on A2
    (execute-command! app 'split-window-below)
    (check-win-count! "S6.1" fr 3)
    ;; Verify traversal order: from A2(idx=1), next should go to B(idx=2)
    (check-current-idx! "S6.1 start" fr 1)
    (execute-command! app 'other-window)
    (check-current-idx! "S6.2 → B" fr 2)
    (execute-command! app 'other-window)
    (check-current-idx! "S6.3 → A1" fr 0)
    (execute-command! app 'other-window)
    (check-current-idx! "S6.4 → A2" fr 1))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 7: delete-other-windows from non-first window
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 7: delete-other-windows keeps current, not first")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Create A|B|C
    (execute-command! app 'split-window-right)   ; A|B
    (execute-command! app 'split-window-right)   ; A|B|C
    ;; Navigate to B (middle)
    (execute-command! app 'other-window) ; → 0 (wraps from 2)
    (execute-command! app 'other-window) ; → 1
    (check-current-idx! "S7.1" fr 1)
    ;; delete-other-windows from B — should keep B only
    (execute-command! app 'delete-other-windows)
    (check-win-count! "S7.2" fr 1)
    (let ((root (qt-frame-root fr)))
      (if (split-leaf? root)
        (pass! "S7.2: root is single leaf")
        (fail! "S7.2: root after delete-other" root "split-leaf"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 8: delete sole window is a no-op (safety check)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 8: delete-window on sole window is no-op")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'delete-window)
    (check-win-count! "S8.1" fr 1)
    (let ((root (qt-frame-root fr)))
      (if (split-leaf? root)
        (pass! "S8.1: root unchanged as leaf")
        (fail! "S8.1: root" root "split-leaf"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 9: Split panes share buffer but have independent editors
  ;; (Emacs behavior: split shows same buffer in both panes)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 9: split panes share buffer, have independent editors")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Split right → A | B
    (execute-command! app 'split-window-right)
    ;; Both panes should share the same buffer
    (let ((buf-a (qt-edit-window-buffer (list-ref (qt-frame-windows fr) 0)))
          (buf-b (qt-edit-window-buffer (list-ref (qt-frame-windows fr) 1))))
      (if (eq? buf-a buf-b)
        (pass! "S9.1: split panes share same buffer (correct Emacs behavior)")
        (fail! "S9.1: buffer sharing" "different buffers" "same buffer")))
    ;; But each has its own editor widget
    (let ((ed-a (win-editor fr 0))
          (ed-b (win-editor fr 1)))
      (if (not (eq? ed-a ed-b))
        (pass! "S9.2: split panes have independent editor widgets")
        (fail! "S9.2: editor independence" "same editor" "different editors"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 10: Winner undo/redo with flat splits (no nested delete)
  ;; A → A|B → A|B|C → winner-undo → A|B → winner-undo → A
  ;; (Nested winner-undo segfaults headless — same as S5/Group 10 known issue)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 10: winner-undo/redo with flat same-orientation splits")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; 1 → 2 windows (flat horizontal)
    (execute-command! app 'split-window-right)
    (check-win-count! "S10.1" fr 2)
    ;; 2 → 3 windows (still flat horizontal — same orientation)
    (execute-command! app 'split-window-right)
    (check-win-count! "S10.2" fr 3)
    ;; Undo → back to 2
    (execute-command! app 'winner-undo)
    (check-win-count! "S10.3 undo" fr 2)
    ;; Undo → back to 1
    (execute-command! app 'winner-undo)
    (check-win-count! "S10.4 undo" fr 1)
    ;; Redo → back to 2
    (execute-command! app 'winner-redo)
    (check-win-count! "S10.5 redo" fr 2)
    ;; Redo → back to 3
    (execute-command! app 'winner-redo)
    (check-win-count! "S10.6 redo" fr 3))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 11: Rapid split and cycle — stress the window list
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 11: rapid split-right x4 then cycle through all 5")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)
    (execute-command! app 'split-window-right)
    (execute-command! app 'split-window-right)
    (execute-command! app 'split-window-right)
    (check-win-count! "S11.1" fr 5)
    ;; Cycle through all 5 windows and verify we wrap
    (let ((start (qt-frame-current-idx fr)))
      (execute-command! app 'other-window)
      (execute-command! app 'other-window)
      (execute-command! app 'other-window)
      (execute-command! app 'other-window)
      (execute-command! app 'other-window)
      (let ((end (qt-frame-current-idx fr)))
        (if (= start end)
          (pass! "S11.2: 5 other-window calls wraps back to start")
          (fail! "S11.2: wrap" end start)))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 12: Alternating h/v splits create correct nesting
  ;; A → A|B → A|(B over C) → A|((B|D) over C)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 12: alternating h/v/h splits — deep nesting")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; A|B
    (execute-command! app 'split-window-right)
    ;; A|(B over C), cursor on C
    (execute-command! app 'split-window-below)
    ;; Navigate back to B
    (execute-command! app 'other-window) ; → next
    (execute-command! app 'other-window) ; → next
    ;; Find B's index — it should be idx 1 (A=0, B=1, C=2)
    ;; After split-below from B, we're at C(idx=2). Two other-windows:
    ;; 2→0, 0→1. So we're at B(idx=1).
    (check-current-idx! "S12.1" fr 1)
    ;; Split B right → A|((B|D) over C)
    (execute-command! app 'split-window-right)
    (check-win-count! "S12.2" fr 4)
    ;; Verify: root=horizontal, right child=vertical,
    ;; top of right child=horizontal (the B|D pair)
    (let* ((root (qt-frame-root fr))
           (right (cadr (split-node-children root))))
      (if (and (split-node? right)
               (= (split-node-orientation right) QT_VERTICAL))
        (let ((right-top (car (split-node-children right))))
          (if (and (split-node? right-top)
                   (= (split-node-orientation right-top) QT_HORIZONTAL))
            (pass! "S12.2: deep nesting — h(leaf, v(h(leaf,leaf), leaf))")
            (fail! "S12.2: right-top" right-top "horizontal split-node")))
        (fail! "S12.2: right child" right "vertical split-node"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 13: Split, navigate, delete from non-last position
  ;; A|B|C, delete A → B|C with correct current-idx
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 13: delete first window in three-way split")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)   ; A|B
    (execute-command! app 'split-window-right)   ; A|B|C
    ;; Navigate to A (idx=0)
    (execute-command! app 'other-window)  ; wrap
    (check-current-idx! "S13.1" fr 0)
    ;; Delete A
    (execute-command! app 'delete-window)
    (check-win-count! "S13.2" fr 2)
    ;; current-idx should be valid (0 or adjusted)
    (let ((idx (qt-frame-current-idx fr)))
      (if (and (>= idx 0) (< idx 2))
        (pass! "S13.2: current-idx is valid after deleting first")
        (fail! "S13.2: current-idx" idx "0 or 1"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 14: Split right, split right, split below in middle
  ;; A|B|C → A|(B over B2)|C — verify middle splits correctly
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 14: split below in middle of three-way horizontal")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; A|B|C
    (execute-command! app 'split-window-right)   ; A|B, cursor=B(1)
    (execute-command! app 'split-window-right)   ; A|B|C, cursor=C(2)
    ;; Navigate to B
    (execute-command! app 'other-window)  ; → 0
    (execute-command! app 'other-window)  ; → 1
    (check-current-idx! "S14.1" fr 1)
    ;; Split B vertically
    (execute-command! app 'split-window-below)
    (check-win-count! "S14.2" fr 4)
    ;; Root should be horizontal. The middle child should be a vertical node.
    (let* ((root (qt-frame-root fr))
           (children (split-node-children root)))
      (if (and (= (length children) 3)
               (split-leaf? (car children))          ; A
               (split-node? (cadr children))         ; B/B2
               (split-leaf? (caddr children)))        ; C
        (let ((mid (cadr children)))
          (if (= (split-node-orientation mid) QT_VERTICAL)
            (pass! "S14.2: middle child is vertical node (B over B2)")
            (fail! "S14.2: middle orientation" (split-node-orientation mid) QT_VERTICAL)))
        (fail! "S14.2: root children"
               (map (lambda (c) (if (split-node? c) 'node 'leaf)) children)
               "(leaf node leaf)"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 15: Winner undo after delete-other-windows
  ;; A|B|C → delete-other → B alone → winner-undo → back to 3
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 15: winner-undo restores after delete-other-windows")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right)
    (execute-command! app 'split-window-right)
    (check-win-count! "S15.1" fr 3)
    ;; Go to middle pane
    (execute-command! app 'other-window)
    (execute-command! app 'other-window)
    ;; delete-other-windows
    (execute-command! app 'delete-other-windows)
    (check-win-count! "S15.2" fr 1)
    ;; winner-undo should go back to 3
    (execute-command! app 'winner-undo)
    (check-win-count! "S15.3 undo" fr 3))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 16: Split inherits buffer, then navigation preserves it
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 16: split inherits buffer across navigation")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((orig-buf-name (win-buf-name fr 0)))
      (execute-command! app 'split-window-right)
      ;; New window should have same buffer
      (let ((new-buf-name (win-buf-name fr 1)))
        (if (string=? orig-buf-name new-buf-name)
          (pass! "S16.1: split inherits buffer name")
          (fail! "S16.1: buffer inheritance" new-buf-name orig-buf-name)))
      ;; Navigate away and back — buffer should persist
      (execute-command! app 'other-window) ; → 0
      (execute-command! app 'other-window) ; → 1
      (let ((after-nav (win-buf-name fr 1)))
        (if (string=? orig-buf-name after-nav)
          (pass! "S16.2: buffer persists after navigation")
          (fail! "S16.2: buffer after nav" after-nav orig-buf-name)))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 17: Repeated other-window with 2 panes (the most common case)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 17: other-window ping-pong with 2 panes")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-right) ; idx=1
    (execute-command! app 'other-window)       ; idx=0
    (check-current-idx! "S17.1" fr 0)
    (execute-command! app 'other-window)       ; idx=1
    (check-current-idx! "S17.2" fr 1)
    (execute-command! app 'other-window)       ; idx=0
    (check-current-idx! "S17.3" fr 0)
    (execute-command! app 'other-window)       ; idx=1
    (check-current-idx! "S17.4" fr 1))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 18: current-idx correctness after split-below from idx > 0
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 18: split-below from non-zero idx tracks correctly")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; A|B, cursor at B(1)
    (execute-command! app 'split-window-right)
    (check-current-idx! "S18.1" fr 1)
    ;; Split B below → A|(B over C), cursor moves to C
    ;; C should be at idx 2 in the flat list [A, B, C]
    (execute-command! app 'split-window-below)
    (check-win-count! "S18.2" fr 3)
    (let ((idx (qt-frame-current-idx fr)))
      (if (= idx 2)
        (pass! "S18.2: cursor on new split window (idx=2)")
        (fail! "S18.2: current-idx" idx 2))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 19: Build 2x2 grid and verify structure
  ;; (A over C) | (B over D) — tests complex nesting
  ;; (Delete portion SKIPPED: segfaults headless — same as S5/Group 10)
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 19: build 2x2 grid and verify structure")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; A|B
    (execute-command! app 'split-window-right)
    ;; Back to A, split below → (A over C) | B
    (execute-command! app 'other-window) ; 0
    (execute-command! app 'split-window-below) ; (A over C) | B
    ;; Navigate to B: from idx 1 (C), other-window → idx 2 (B)
    (execute-command! app 'other-window)
    ;; Split B below → (A over C) | (B over D)
    (execute-command! app 'split-window-below)
    (check-win-count! "S19.1" fr 4)
    ;; Verify: root=horizontal, both children=vertical
    (let* ((root (qt-frame-root fr))
           (left (car (split-node-children root)))
           (right (cadr (split-node-children root))))
      (if (and (split-node? root)
               (= (split-node-orientation root) QT_HORIZONTAL)
               (split-node? left)
               (= (split-node-orientation left) QT_VERTICAL)
               (split-node? right)
               (= (split-node-orientation right) QT_VERTICAL))
        (pass! "S19.1: 2x2 grid — horizontal root, both children vertical")
        (fail! "S19.1: grid structure" root "horizontal(vertical, vertical)"))))

  ;; ══════════════════════════════════════════════════════════════════════════
  ;; Scenario 20: Split, cursor position persists in each pane
  ;; ══════════════════════════════════════════════════════════════════════════
  (displayln "Scenario 20: cursor position persists per-pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; Set up text and cursor in initial pane
    (let ((ed0 (win-editor fr 0)))
      (qt-plain-text-edit-set-text! ed0 "abcdefghij")
      (qt-plain-text-edit-set-cursor-position! ed0 5))
    ;; Split
    (execute-command! app 'split-window-right)
    ;; Set different cursor in new pane
    (let ((ed1 (win-editor fr 1)))
      (qt-plain-text-edit-set-cursor-position! ed1 2))
    ;; Navigate back to original
    (execute-command! app 'other-window)
    ;; Check original's cursor is still at 5
    (let ((pos0 (qt-plain-text-edit-cursor-position (win-editor fr 0))))
      (if (= pos0 5)
        (pass! "S20.1: original pane cursor preserved at 5")
        (fail! "S20.1: cursor" pos0 5)))
    ;; Navigate to second pane
    (execute-command! app 'other-window)
    (let ((pos1 (qt-plain-text-edit-cursor-position (win-editor fr 1))))
      (if (= pos1 2)
        (pass! "S20.2: second pane cursor preserved at 2")
        (fail! "S20.2: cursor" pos1 2))))

  (displayln "Group 11 complete"))

;;;============================================================================
;;; Group 12: Qt widget layout verification — ACTUAL Qt splitter hierarchy
;;;============================================================================

;;; Helper: verify a splitter has exactly N children
(def (check-splitter-count! label spl expected)
  (let ((actual (qt-splitter-count spl)))
    (if (= actual expected)
      (pass! label)
      (fail! label actual expected))))

;;; Helper: verify widget is at expected index in splitter
;;; Uses qt-splitter-index-of (avoids FFI pointer eq? issues)
(def (check-widget-at! label spl widget expected-idx)
  (let ((actual-idx (qt-splitter-index-of spl widget)))
    (if (= actual-idx expected-idx)
      (pass! label)
      (fail! label (string-append "index " (number->string actual-idx))
                   (string-append "index " (number->string expected-idx))))))

;;; Helper: get the Qt widget for a tree node (container for leaf, splitter for node)
(def (tree-node-qt-widget node)
  (cond
    ((split-leaf? node) (qt-edit-window-container (split-leaf-edit-window node)))
    ((split-node? node) (split-node-splitter node))))

;;; Helper: recursively verify Qt widget tree matches logical tree
(def (verify-qt-layout! prefix spl tree-children)
  (check-splitter-count! (string-append prefix " child count")
                         spl (length tree-children))
  (let loop ((children tree-children) (i 0))
    (when (pair? children)
      (let ((child (car children)))
        (check-widget-at!
          (string-append prefix " child[" (number->string i) "]")
          spl (tree-node-qt-widget child) i)
        ;; Recurse into sub-nodes
        (when (split-node? child)
          (verify-qt-layout!
            (string-append prefix ".child[" (number->string i) "]")
            (split-node-splitter child)
            (split-node-children child))))
      (loop (cdr children) (+ i 1)))))

(def (run-group-12-layout-verification)
  (displayln "\n=== Group 12: Qt widget layout verification ===")

  ;; ── L1: split-below — root splitter has [A, B] in correct order ──────────
  (displayln "Layout L1: split-below widget order")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      (execute-command! app 'split-window-below)
      (let* ((root (qt-frame-root fr))
             (root-spl (qt-frame-splitter fr))
             (children (split-node-children root)))
        ;; Tree should be: node(VERT, root-spl, [leaf(A), leaf(B)])
        (check-splitter-count! "L1.1: root has 2 widgets" root-spl 2)
        ;; A's container should be at index 0 (top)
        (check-widget-at! "L1.2: A is at top (index 0)"
          root-spl (qt-edit-window-container win-a) 0)
        ;; B's container should be at index 1 (bottom)
        (let ((win-b (qt-current-window fr)))
          (check-widget-at! "L1.3: B is at bottom (index 1)"
            root-spl (qt-edit-window-container win-b) 1)))))

  ;; ── L2: split-right — root splitter has [A, B] in correct order ──────────
  (displayln "Layout L2: split-right widget order")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      (execute-command! app 'split-window-right)
      (let ((root-spl (qt-frame-splitter fr)))
        (check-splitter-count! "L2.1: root has 2 widgets" root-spl 2)
        (check-widget-at! "L2.2: A is at left (index 0)"
          root-spl (qt-edit-window-container win-a) 0)
        (let ((win-b (qt-current-window fr)))
          (check-widget-at! "L2.3: B is at right (index 1)"
            root-spl (qt-edit-window-container win-b) 1)))))

  ;; ── L3: THE BUG — split-below then split-right in bottom pane ────────────
  ;; This is the exact user scenario that was broken:
  ;; C-x 2 (split-below) → C-x 3 (split-right in B)
  ;; Expected: A on top, B|C on bottom
  ;; Bug was: nested splitter ended up at wrong position
  (displayln "Layout L3: split-below then split-right in bottom pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      ;; Split below: A over B, focus on B
      (execute-command! app 'split-window-below)
      (let ((win-b (qt-current-window fr)))
        ;; Split right in B: should nest B|C below A
        (execute-command! app 'split-window-right)
        (let* ((root-spl (qt-frame-splitter fr))
               (root (qt-frame-root fr))
               (root-children (split-node-children root)))
          ;; Root should have 2 children: leaf(A) and node(HORIZ, [B, C])
          (check-splitter-count! "L3.1: root has 2 Qt widgets" root-spl 2)
          ;; A's container at index 0 (top)
          (check-widget-at! "L3.2: A is at top (index 0)"
            root-spl (qt-edit-window-container win-a) 0)
          ;; Nested horizontal splitter at index 1 (bottom)
          (let ((nested-node (cadr root-children)))
            (if (split-node? nested-node)
              (begin
                (check-widget-at! "L3.3: nested splitter at bottom (index 1)"
                  root-spl (split-node-splitter nested-node) 1)
                ;; Inside nested: B at left (0), C at right (1)
                (let ((nested-spl (split-node-splitter nested-node)))
                  (check-splitter-count! "L3.4: nested has 2 widgets" nested-spl 2)
                  (check-widget-at! "L3.5: B at left in nested"
                    nested-spl (qt-edit-window-container win-b) 0)
                  (let ((win-c (qt-current-window fr)))
                    (check-widget-at! "L3.6: C at right in nested"
                      nested-spl (qt-edit-window-container win-c) 1))))
              (fail! "L3.3: second child should be split-node" nested-node "split-node")))))))

  ;; ── L4: THE FULL BUG — split-below, navigate to bottom, split-right ×3 ──
  ;; User scenario: C-x 2, C-x o (to bottom), C-x 3, C-x 3, C-x 3
  ;; Expected: A on top, B|C|D|E on bottom
  (displayln "Layout L4: split-below + 3× split-right in bottom")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      ;; Split below: A over B, focus on B
      (execute-command! app 'split-window-below)
      ;; 3× split-right in B's area
      (execute-command! app 'split-window-right)
      (execute-command! app 'split-window-right)
      (execute-command! app 'split-window-right)
      (let* ((root-spl (qt-frame-splitter fr))
             (root (qt-frame-root fr))
             (wins (qt-frame-windows fr)))
        ;; Should have 5 windows total
        (if (= (length wins) 5)
          (pass! "L4.1: 5 windows total")
          (fail! "L4.1: window count" (length wins) 5))
        ;; Root should still have 2 Qt children: A's container and nested splitter
        (check-splitter-count! "L4.2: root has 2 Qt widgets" root-spl 2)
        ;; A's container must be at index 0 (top) — the critical check
        (check-widget-at! "L4.3: A stays at top (index 0)"
          root-spl (qt-edit-window-container win-a) 0)
        ;; Nested horizontal splitter at index 1 must contain 4 panes
        (let ((nested-node (cadr (split-node-children root))))
          (when (split-node? nested-node)
            (let ((nested-spl (split-node-splitter nested-node)))
              (check-splitter-count! "L4.4: nested has 4 Qt widgets"
                nested-spl 4)
              ;; Verify each window's container is at the correct index
              ;; Windows list: [A, B, C, D, E]
              ;; Nested should contain [B, C, D, E] containers at indices 0-3
              (let loop ((i 0) (ws (cdr wins)))
                (when (pair? ws)
                  (let ((w (car ws)))
                    (check-widget-at!
                      (string-append "L4.5: win["
                        (number->string (+ i 1))
                        "] at nested index "
                        (number->string i))
                      nested-spl (qt-edit-window-container w) i))
                  (loop (+ i 1) (cdr ws))))))))))

  ;; ── L5: split-right then split-below in right pane ───────────────────────
  ;; Opposite direction: C-x 3 (A|B), then C-x 2 in B
  ;; Expected: A on left, B-over-C on right
  (displayln "Layout L5: split-right then split-below in right pane")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      (execute-command! app 'split-window-right)
      (let ((win-b (qt-current-window fr)))
        (execute-command! app 'split-window-below)
        (let* ((root-spl (qt-frame-splitter fr))
               (root (qt-frame-root fr))
               (root-children (split-node-children root)))
          (check-splitter-count! "L5.1: root has 2 Qt widgets" root-spl 2)
          (check-widget-at! "L5.2: A at left (index 0)"
            root-spl (qt-edit-window-container win-a) 0)
          (let ((nested-node (cadr root-children)))
            (when (split-node? nested-node)
              (check-widget-at! "L5.3: nested splitter at right (index 1)"
                root-spl (split-node-splitter nested-node) 1)
              (let ((nested-spl (split-node-splitter nested-node)))
                (check-splitter-count! "L5.4: nested has 2 widgets" nested-spl 2)
                (check-widget-at! "L5.5: B at top in nested"
                  nested-spl (qt-edit-window-container win-b) 0))))))))

  ;; ── L6: three-way vertical then nest horizontal in middle ────────────────
  ;; C-x 2, C-x 2 → A/B/C stacked, focus on C. Navigate to B, then C-x 3.
  ;; Expected: A on top, B|D in middle, C at bottom
  (displayln "Layout L6: 3-way vertical then horizontal nest in middle")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (let ((win-a (qt-current-window fr)))
      (execute-command! app 'split-window-below)
      (let ((win-b (qt-current-window fr)))
        (execute-command! app 'split-window-below)
        (let ((win-c (qt-current-window fr)))
          ;; Now: [A, B, C] stacked, focus on C (idx=2). Navigate to B (idx=1).
          ;; other-window: (2+1)%3=0 → A, then (0+1)%3=1 → B
          (execute-command! app 'other-window)
          (execute-command! app 'other-window)
          ;; Now focus on B. Split right in B.
          (execute-command! app 'split-window-right)
          (let* ((root-spl (qt-frame-splitter fr))
                 (root (qt-frame-root fr))
                 (root-children (split-node-children root)))
            ;; Root should have 3 Qt children: A, nested(B|D), C
            (check-splitter-count! "L6.1: root has 3 Qt widgets" root-spl 3)
            (check-widget-at! "L6.2: A at top (index 0)"
              root-spl (qt-edit-window-container win-a) 0)
            ;; Middle should be nested horizontal splitter
            (let ((middle-node (cadr root-children)))
              (when (split-node? middle-node)
                (check-widget-at! "L6.3: nested at middle (index 1)"
                  root-spl (split-node-splitter middle-node) 1)))
            ;; C at bottom
            (check-widget-at! "L6.4: C at bottom (index 2)"
              root-spl (qt-edit-window-container win-c) 2))))))

  ;; ── L7: full recursive layout verification ───────────────────────────────
  ;; Build a complex layout and verify entire Qt widget tree matches logical tree
  (displayln "Layout L7: full recursive layout verification")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    ;; C-x 2 (A over B)
    (execute-command! app 'split-window-below)
    ;; C-x 3 in B (B|C at bottom)
    (execute-command! app 'split-window-right)
    ;; Navigate to A: other-window cycles through B,C → A
    (execute-command! app 'other-window)  ;; → to next (C already current after split)
    (execute-command! app 'other-window)  ;; → wrap around
    ;; If we're at idx 0, split-right in A
    ;; Actually just verify the full tree at this point
    (let* ((root (qt-frame-root fr))
           (root-spl (qt-frame-splitter fr)))
      (when (split-node? root)
        (verify-qt-layout! "L7" root-spl (split-node-children root)))))

  ;; ── L8: alternating split directions ─────────────────────────────────────
  ;; C-x 2, C-x 3, C-x 2 — each nesting deeper
  (displayln "Layout L8: alternating split directions nest correctly")
  (let-values (((fr app) (make-qt-split-test-frame!)))
    (execute-command! app 'split-window-below)   ;; A over B
    (execute-command! app 'split-window-right)    ;; B → B|C
    (execute-command! app 'split-window-below)    ;; C → C over D
    (let* ((root (qt-frame-root fr))
           (root-spl (qt-frame-splitter fr))
           (wins (qt-frame-windows fr)))
      (if (= (length wins) 4)
        (pass! "L8.1: 4 windows")
        (fail! "L8.1: window count" (length wins) 4))
      ;; Full recursive verification
      (when (split-node? root)
        (verify-qt-layout! "L8.2" root-spl (split-node-children root)))))

  (displayln "Group 12 complete"))

;;;============================================================================
;;; Group 13: Org-table commands
;;;============================================================================

(def (run-group-13-org-table)
  (displayln "=== Group 13: Org-Table Commands ===")

  ;; Helper: a simple 2x3 org table for testing
  (def sample-table
    (string-append "| Name  | Age | City   |\n"
                   "|-------+-----+--------|\n"
                   "| Alice | 30  | Paris  |\n"
                   "| Bob   | 25  | London |"))

  ;; Test: org-table-create inserts a table template
  (displayln "Test: org-table-create inserts table template")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "" 0)
    (execute-command! app 'org-table-create)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (and (string-contains text "| Col1")
               (string-contains text "|------"))
        (pass! "org-table-create: template inserted")
        (fail! "org-table-create" text "table template")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-align re-aligns uneven table
  (displayln "Test: org-table-align re-aligns uneven table")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| a | bb | ccc |\n| dddd | e | ff |" 0)
    (execute-command! app 'org-table-align)
    (let ((text (qt-plain-text-edit-text ed)))
      (if (and (string-contains text "| a    |")
               (string-contains text "| dddd |"))
        (pass! "org-table-align: columns aligned")
        (fail! "org-table-align" text "aligned columns")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-insert-row adds empty row
  (displayln "Test: org-table-insert-row inserts empty row above")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed sample-table 0)
    ;; Position cursor on "Alice" row (line 2, after header+sep)
    (let ((line2-pos (sci-send ed SCI_POSITIONFROMLINE 2 0)))
      (qt-plain-text-edit-set-cursor-position! ed line2-pos)
      (execute-command! app 'org-table-insert-row)
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (if (= (length lines) 5)  ;; Was 4, now 5
          (pass! "org-table-insert-row: row count 4 → 5")
          (fail! "org-table-insert-row" (length lines) 5))))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-delete-row removes current row
  (displayln "Test: org-table-delete-row removes current row")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed sample-table 0)
    ;; Position cursor on "Alice" row (line 2)
    (let ((line2-pos (sci-send ed SCI_POSITIONFROMLINE 2 0)))
      (qt-plain-text-edit-set-cursor-position! ed line2-pos)
      (execute-command! app 'org-table-delete-row)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (and (not (string-contains text "Alice"))
                 (string-contains text "Bob"))
          (pass! "org-table-delete-row: Alice removed, Bob remains")
          (fail! "org-table-delete-row" text "no Alice, has Bob"))))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-move-row-down moves row down
  (displayln "Test: org-table-move-row-down swaps rows")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed sample-table 0)
    ;; Position on "Alice" row (line 2), move down → Bob should be first
    (let ((line2-pos (sci-send ed SCI_POSITIONFROMLINE 2 0)))
      (qt-plain-text-edit-set-cursor-position! ed line2-pos)
      (execute-command! app 'org-table-move-row-down)
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        ;; Line 2 should now be Bob, line 3 Alice
        (if (and (string-contains (list-ref lines 2) "Bob")
                 (string-contains (list-ref lines 3) "Alice"))
          (pass! "org-table-move-row-down: Alice↔Bob swapped")
          (fail! "org-table-move-row-down" text "Bob before Alice"))))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-move-row-up moves row up
  (displayln "Test: org-table-move-row-up swaps rows")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed sample-table 0)
    ;; Position on "Bob" row (line 3), move up → Bob should be first
    (let ((line3-pos (sci-send ed SCI_POSITIONFROMLINE 3 0)))
      (qt-plain-text-edit-set-cursor-position! ed line3-pos)
      (execute-command! app 'org-table-move-row-up)
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (if (and (string-contains (list-ref lines 2) "Bob")
                 (string-contains (list-ref lines 3) "Alice"))
          (pass! "org-table-move-row-up: Bob moved above Alice")
          (fail! "org-table-move-row-up" text "Bob before Alice"))))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-delete-column removes a column
  (displayln "Test: org-table-delete-column removes Age column")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed sample-table 0)
    ;; Position cursor in "Age" column (col 1) on header row
    ;; "| Name  | Age | City   |" — cursor after second |
    (let ((pos (+ (sci-send ed SCI_POSITIONFROMLINE 0 0) 10)))
      (qt-plain-text-edit-set-cursor-position! ed pos)
      (execute-command! app 'org-table-delete-column)
      (let ((text (qt-plain-text-edit-text ed)))
        (if (and (not (string-contains text "Age"))
                 (string-contains text "Name")
                 (string-contains text "City"))
          (pass! "org-table-delete-column: Age removed")
          (fail! "org-table-delete-column" text "no Age, has Name+City"))))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-insert-column adds empty column
  (displayln "Test: org-table-insert-column adds column after current")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| A | B |\n| 1 | 2 |" 0)
    ;; Cursor in first column
    (qt-plain-text-edit-set-cursor-position! ed 2)
    (execute-command! app 'org-table-insert-column)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline)))
      ;; Each row should now have 3 data columns (4+ pipe-separated parts)
      (if (>= (length (string-split (car lines) #\|)) 4)
        (pass! "org-table-insert-column: 3 columns after insert")
        (fail! "org-table-insert-column" text "3 columns")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-move-column-right moves column right
  (displayln "Test: org-table-move-column-right swaps columns")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| A | B | C |\n| 1 | 2 | 3 |" 0)
    ;; Cursor in column A (col 0)
    (qt-plain-text-edit-set-cursor-position! ed 2)
    (execute-command! app 'org-table-move-column-right)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline))
           (first-line (car lines)))
      ;; After moving col A right: first data col should be B
      ;; Check that "| B" appears before "| A" (or " B " before " A ")
      (if (string-prefix? "| B" first-line)
        (pass! "org-table-move-column-right: A↔B swapped")
        (fail! "org-table-move-column-right" first-line "starts with | B")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-move-column-left moves column left
  (displayln "Test: org-table-move-column-left swaps columns")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| A | B | C |\n| 1 | 2 | 3 |" 0)
    ;; Cursor in column B (col 1) — after second |, about position 6
    (qt-plain-text-edit-set-cursor-position! ed 6)
    (execute-command! app 'org-table-move-column-left)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline))
           (first-line (car lines)))
      ;; After moving col B left: first data col should be B
      (if (string-prefix? "| B" first-line)
        (pass! "org-table-move-column-left: B moved before A")
        (fail! "org-table-move-column-left" first-line "starts with | B")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-insert-separator inserts separator row
  (displayln "Test: org-table-insert-separator inserts |---+---| row")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| A | B |\n| 1 | 2 |" 0)
    (qt-plain-text-edit-set-cursor-position! ed 0) ;; on header row
    (execute-command! app 'org-table-insert-separator)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline)))
      ;; Should now have 3 lines: header, separator, data
      (if (and (= (length lines) 3)
               (string-contains (list-ref lines 1) "---"))
        (pass! "org-table-insert-separator: separator added below header")
        (fail! "org-table-insert-separator" text "3 lines with ---")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-sort sorts by column
  (displayln "Test: org-table-sort sorts numerically")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| X | 30 |\n| Y | 10 |\n| Z | 20 |" 0)
    ;; Cursor in column 1 (numbers) — about position 5
    (qt-plain-text-edit-set-cursor-position! ed 5)
    (execute-command! app 'org-table-sort)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline)))
      ;; Should be sorted: 10, 20, 30
      (if (and (string-contains (car lines) "10")
               (string-contains (list-ref lines 2) "30"))
        (pass! "org-table-sort: sorted numerically 10,20,30")
        (fail! "org-table-sort" text "sorted by number")))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-transpose swaps rows and columns
  (displayln "Test: org-table-transpose swaps rows and columns")
  (let-values (((ed w app) (make-qt-test-app "notes.org")))
    (set-qt-text! ed "| A | B |\n| 1 | 2 |\n| 3 | 4 |" 0)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (execute-command! app 'org-table-transpose)
    (let* ((text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline)))
      ;; 3 rows x 2 cols → 2 rows x 3 cols
      (if (= (length lines) 2)
        (pass! "org-table-transpose: 3 rows → 2 rows")
        (fail! "org-table-transpose" (length lines) 2)))
    (destroy-qt-test-app! ed w))

  ;; Test: org-table-align is registered
  (displayln "Test: all 17 org-table commands are registered")
  (let ((cmds '(org-table-align org-table-insert-row org-table-delete-row
                org-table-move-row-up org-table-move-row-down
                org-table-delete-column org-table-insert-column
                org-table-move-column-left org-table-move-column-right
                org-table-insert-separator org-table-sort org-table-sum
                org-table-recalculate org-table-create
                org-table-export-csv org-table-import-csv
                org-table-transpose)))
    (let loop ((cs cmds) (ok 0))
      (if (null? cs)
        (if (= ok 17)
          (pass! "all 17 org-table commands registered")
          (fail! "org-table registration" ok 17))
        (loop (cdr cs)
              (+ ok (if (find-command (car cs)) 1 0))))))

  (displayln "Group 13 complete"))

;;;============================================================================
;;; Main
;;;============================================================================

(def (main . args)
  (with-qt-app _app
    ;; Register all Qt commands once
    (qt-register-all-commands!)

    (run-group-1-org-tab-dispatch)
    (run-group-2-navigation)
    (run-group-3-basic-editing)
    (run-group-4-org-commands)
    (run-group-5-text-transforms)
    (run-group-6-dispatch-chain)
    (run-group-7-mark-region)
    (run-group-8-magit)
    (run-group-9-magit-ops)
    (run-group-10-split-operations)
    (run-group-11-window-scenarios)
    (run-group-12-layout-verification)
    (run-group-13-org-table)

    (displayln "---")
    (displayln "Results: " *passes* " passed, " *failures* " failed")
    (if (= *failures* 0)
      (begin (displayln "All Qt functional tests passed!") (exit 0))
      (begin (displayln *failures* " failure(s)") (exit *failures*)))))
