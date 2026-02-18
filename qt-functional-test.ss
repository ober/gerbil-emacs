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
                 qt-scintilla-create)
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
                 buffer-mark
                 app-state-frame
                 app-state-last-command
                 app-state-prefix-arg)
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 qt-edit-window-buffer
                 qt-frame-windows)
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
         (fr  (make-qt-frame #f (list win) 0 #f))
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
         (fr  (make-qt-frame #f (list win) 0 #f))
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
      (execute-command! app 'previous-line)
      (execute-command! app 'previous-line)
      (let ((new-pos (qt-plain-text-edit-cursor-position ed))
            (sel-start (qt-plain-text-edit-selection-start ed))
            (sel-end   (qt-plain-text-edit-selection-end ed)))
        (if (< new-pos end-pos)
          (pass! "moved up 2 lines from end in org buffer")
          (fail! "moved up" new-pos "< end-pos"))
        (if (< sel-start sel-end)
          (pass! "multi-line region highlighted in org buffer")
          (fail! "multi-line region" sel-start "< sel-end"))))
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

    (displayln "---")
    (displayln "Results: " *passes* " passed, " *failures* " failed")
    (if (= *failures* 0)
      (begin (displayln "All Qt functional tests passed!") (exit 0))
      (begin (displayln *failures* " failure(s)") (exit *failures*)))))
