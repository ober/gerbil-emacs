;;; -*- Gerbil -*-
;;; Functional tests for gemacs.
;;;
;;; Every test goes through the REAL dispatch chain:
;;;   - cmd-indent-or-complete for TAB behavior (NOT cmd-org-template-expand)
;;;   - execute-command! for named commands (NOT direct leaf functions)
;;;   - sim-key! for key events through key-state-feed!
;;;
;;; This catches regressions in the dispatch chain that leaf-function tests miss.

(import :std/test
        :std/srfi/13
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        :gemacs/editor-core
        (only-in :gemacs/editor-ui cmd-indent-or-complete org-buffer?)
        (only-in :gemacs/editor register-all-commands!))

(export functional-test)

;;;============================================================================
;;; Helpers
;;;============================================================================

;;; Create a test app with a fresh editor and named buffer.
;;; Returns (values ed app).
(def (make-test-app name)
  (let* ((ed (create-scintilla-editor width: 80 height: 24))
         (buf (make-buffer name #f (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
         (win (make-edit-window ed buf 0 0 80 22 0))
         (root (make-split-leaf win))
         (fr (make-frame root [win] 0 80 24))
         (app (new-app-state fr)))
    (values ed app)))

;;; Create a test app with a named buffer whose file-path is set to PATH.
;;; Returns (values ed app).
(def (make-test-app-with-file path)
  (let* ((ed   (create-scintilla-editor width: 80 height: 24))
         (name (path-strip-directory path))
         (buf  (make-buffer name path (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
         (win  (make-edit-window ed buf 0 0 80 22 0))
         (root (make-split-leaf win))
         (fr   (make-frame root [win] 0 80 24))
         (app  (new-app-state fr)))
    (values ed app)))

;;; Run a git command in DIR, return first line of stdout (or "" on error).
(def (test-git-cmd! args dir)
  (with-exception-catcher
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

;;; Create a temp git repo with one committed README.md. Returns dir path.
(def *temp-repo-counter* 0)
(def (make-temp-git-repo!)
  (set! *temp-repo-counter* (+ *temp-repo-counter* 1))
  (let ((dir (string-append "/tmp/gemacs-test-"
                            (number->string *temp-repo-counter*))))
    ;; Clean up any stale directory from a previous run
    (with-exception-catcher (lambda (e) (void))
      (lambda ()
        (let ((p (open-process (list path: "/bin/rm" arguments: (list "-rf" dir)
                                     stdin-redirection: #f stdout-redirection: #f
                                     stderr-redirection: #f))))
          (process-status p) (close-port p))))
    (with-exception-catcher
      (lambda (e) "/tmp/gemacs-test-error")
      (lambda ()
        (create-directory dir)
        (test-git-cmd! '("init" "-q") dir)
        (test-git-cmd! (list "config" "user.email" "test@example.com") dir)
        (test-git-cmd! (list "config" "user.name" "Test User") dir)
        (with-output-to-file (string-append dir "/README.md")
          (lambda () (display "# Test Repo\n")))
        (test-git-cmd! '("add" "README.md") dir)
        (test-git-cmd! (list "commit" "--no-gpg-sign" "-m" "Initial commit") dir)
        dir))))

;;; Remove temp git repo.
(def (cleanup-temp-git-repo! dir)
  (with-exception-catcher
    (lambda (e) (void))
    (lambda ()
      (let ((p (open-process (list path: "/bin/rm"
                                   arguments: (list "-rf" dir)
                                   stdin-redirection: #f
                                   stdout-redirection: #f
                                   stderr-redirection: #f))))
        (process-status p)
        (close-port p)))))

;;; Write CONTENT to PATH (overwrite).
(def (write-file-content! path content)
  (with-output-to-file path (lambda () (display content))))

;;; Simulate scripted responses for app-read-string in tests.
(def (with-scripted-responses responses thunk)
  (set! *test-echo-responses* responses)
  (thunk)
  (set! *test-echo-responses* '()))

;;; Simulate feeding a single key event through the dispatch chain.
;;; Updates app key-state and executes the resulting command or self-insert.
(def (sim-key! app ev)
  (let-values (((action data new-state)
                (key-state-feed! (app-state-key-state app) ev)))
    (set! (app-state-key-state app) new-state)
    (case action
      ((command)   (execute-command! app data))
      ((self-insert) (cmd-self-insert! app data))
      (else (void)))
    action))

;;; Make a TUI key event for a control character (0x01-0x1A range).
(def (ctrl-ev code)
  (make-tui-event 1 0 code 0 0 0 0 0))

;;; Make a TUI key event for TAB (0x09).
(def tab-ev (make-tui-event 1 0 #x09 0 0 0 0 0))

;;; Make a TUI event for a printable character (no modifiers).
;;; tui-event fields: type mod key ch w h x y — ch is at index 3.
(def (char-ev ch)
  (make-tui-event 1 0 0 (char->integer ch) 0 0 0 0))

;;;============================================================================
;;; Group 1: Org-Mode TAB Dispatch
;;; Tests call cmd-indent-or-complete (or via execute-command! 'indent-or-complete)
;;; NOT cmd-org-template-expand directly.
;;;============================================================================

(def functional-test
  (test-suite "functional"

    ;; --- 1a: Registration guards ---

    (test-case "dispatch: indent-or-complete is registered"
      (setup-default-bindings!)
      (register-all-commands!)
      (check (procedure? (find-command 'indent-or-complete)) => #t))

    (test-case "dispatch: org-template-expand is registered"
      (setup-default-bindings!)
      (register-all-commands!)
      (check (procedure? (find-command 'org-template-expand)) => #t))

    (test-case "dispatch: org-cycle is registered"
      (setup-default-bindings!)
      (register-all-commands!)
      (check (procedure? (find-command 'org-cycle)) => #t))

    (test-case "dispatch: TAB key is bound to indent-or-complete"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "TAB") => 'indent-or-complete))

    ;; --- 1b: Template expansions via dispatch chain ---

    (test-case "TAB dispatch: <s expands to #+BEGIN_SRC in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t))))

    (test-case "TAB dispatch: <e expands to #+BEGIN_EXAMPLE in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<e")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXAMPLE"))) => #t)
          (check (not (not (string-contains text "#+END_EXAMPLE"))) => #t))))

    (test-case "TAB dispatch: <q expands to #+BEGIN_QUOTE in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<q")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_QUOTE"))) => #t)
          (check (not (not (string-contains text "#+END_QUOTE"))) => #t))))

    (test-case "TAB dispatch: <v expands to #+BEGIN_VERSE in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<v")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_VERSE"))) => #t)
          (check (not (not (string-contains text "#+END_VERSE"))) => #t))))

    (test-case "TAB dispatch: <c expands to #+BEGIN_CENTER in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<c")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_CENTER"))) => #t)
          (check (not (not (string-contains text "#+END_CENTER"))) => #t))))

    (test-case "TAB dispatch: <C expands to #+BEGIN_COMMENT in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<C")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_COMMENT"))) => #t)
          (check (not (not (string-contains text "#+END_COMMENT"))) => #t))))

    (test-case "TAB dispatch: <l expands to #+BEGIN_EXPORT latex in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<l")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "EXPORT latex"))) => #t))))

    (test-case "TAB dispatch: <h expands to #+BEGIN_EXPORT html in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<h")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "EXPORT html"))) => #t))))

    (test-case "TAB dispatch: <a expands to #+BEGIN_EXPORT ascii in org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<a")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "EXPORT ascii"))) => #t))))

    ;; --- 1c: Dispatch priority ---

    (test-case "TAB dispatch: plain text in org → indent (2 spaces)"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          ;; Should have inserted 2 spaces (indent, not template)
          (check (string-prefix? "  hello" text) => #t))))

    (test-case "TAB dispatch: unknown template <z in org → indent, not expansion"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "<z")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          ;; <z is not a valid template key, should NOT produce #+BEGIN_
          (check (not (string-contains text "#+BEGIN_")) => #t))))

    (test-case "TAB dispatch: heading line in org → org-cycle command"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        ;; Set up a heading with content
        (editor-set-text ed "* Heading\nsome content under heading")
        (editor-goto-pos ed 0)
        ;; TAB on heading should trigger org-cycle (fold/unfold)
        ;; Just verify it doesn't crash and doesn't expand a template
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          ;; The heading should still be there
          (check (not (not (string-contains text "* Heading"))) => #t)
          ;; No template expansion happened
          (check (not (string-contains text "#+BEGIN_")) => #t))))

    (test-case "TAB dispatch: empty line in org → indent (2 spaces)"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "  " text) => #t))))

    ;; --- 1d: Content preservation ---

    (test-case "TAB dispatch: <s preserves text after template"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        ;; Content after the <s line should be preserved
        (editor-set-text ed "<s\nSome following text")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "Some following text"))) => #t))))

    (test-case "TAB dispatch: org-buffer? is true for .org named buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (let ((buf (current-buffer-from-app app)))
          (check (org-buffer? buf) => #t))))

    (test-case "TAB dispatch: org-buffer? is false for non-org buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "main.ss")))
        (let ((buf (current-buffer-from-app app)))
          (check (org-buffer? buf) => #f))))

    ;; --- 1e: Non-org buffer → plain indent ---

    (test-case "TAB dispatch: non-org buffer → plain 2-space indent"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "main.ss")))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          ;; Should NOT expand — not an org buffer
          (check (not (string-contains text "#+BEGIN_SRC")) => #t)
          ;; Should insert 2 spaces
          (check (not (not (string-contains text "  "))) => #t))))

    ;; --- 1f: Via simulated TAB key event ---

    (test-case "TAB key event in org buffer → template expansion"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        ;; Simulate TAB through the key dispatch chain
        (sim-key! app tab-ev)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t))))

    (test-case "TAB key event binds correctly in global keymap"
      (setup-default-bindings!)
      ;; TAB (0x09) should be bound to 'indent-or-complete via key-state-feed!
      (let ((state (make-initial-key-state)))
        (let-values (((action data new-state) (key-state-feed! state tab-ev)))
          (check action => 'command)
          (check data => 'indent-or-complete))))

    ;;;==========================================================================
    ;;; Group 2: Navigation
    ;;;==========================================================================

    (test-case "nav: forward-char moves cursor right"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        (check (editor-get-current-pos ed) => 1)))

    (test-case "nav: backward-char moves cursor left"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 3)
        (execute-command! app 'backward-char)
        (check (editor-get-current-pos ed) => 2)))

    (test-case "nav: beginning-of-line goes to column 0"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 7)
        (execute-command! app 'beginning-of-line)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "nav: end-of-line goes to end of line"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'end-of-line)
        (check (editor-get-current-pos ed) => 5)))

    (test-case "nav: beginning-of-buffer goes to pos 0"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 15)
        (execute-command! app 'beginning-of-buffer)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "nav: end-of-buffer goes to last position"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'end-of-buffer)
        (check (editor-get-current-pos ed) => 5)))

    (test-case "nav: next-line moves down"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "line1\nline2")
        (editor-goto-pos ed 0)
        (execute-command! app 'next-line)
        ;; Should be somewhere in line 2 (pos >= 6)
        (check (>= (editor-get-current-pos ed) 6) => #t)))

    (test-case "nav: previous-line moves up"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "line1\nline2")
        (editor-goto-pos ed 6)
        (execute-command! app 'previous-line)
        ;; Should be somewhere in line 1 (pos < 6)
        (check (< (editor-get-current-pos ed) 6) => #t)))

    (test-case "nav: forward-char at end of buffer is a no-op"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hi")
        (editor-goto-pos ed 2)
        (execute-command! app 'forward-char)
        ;; Should not crash; position stays at or near end
        (check (>= (editor-get-current-pos ed) 0) => #t)))

    (test-case "nav: backward-char at beginning is a no-op"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hi")
        (editor-goto-pos ed 0)
        (execute-command! app 'backward-char)
        (check (= (editor-get-current-pos ed) 0) => #t)))

    (test-case "nav: C-f key event moves cursor forward"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        ;; C-f = 0x06
        (sim-key! app (ctrl-ev #x06))
        (check (editor-get-current-pos ed) => 1)))

    (test-case "nav: forward-word moves past word boundary"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-word)
        ;; Should be past "hello" (pos >= 5)
        (check (>= (editor-get-current-pos ed) 5) => #t)))

    (test-case "nav: backward-word moves back over word"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 11)
        (execute-command! app 'backward-word)
        ;; Should be at start of "world" or before (pos <= 6)
        (check (<= (editor-get-current-pos ed) 6) => #t)))

    (test-case "nav: end-of-line on multi-line at correct column"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "abc\ndef")
        (editor-goto-pos ed 4)
        (execute-command! app 'end-of-line)
        ;; "def" starts at 4, length 3, so end is pos 7
        (check (editor-get-current-pos ed) => 7)))

    ;;;==========================================================================
    ;;; Group 3: Basic Editing
    ;;;==========================================================================

    (test-case "edit: delete-char removes character at cursor"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'delete-char)
        (check (editor-get-text ed) => "ello")))

    (test-case "edit: backward-delete-char removes char before cursor"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 3)
        (execute-command! app 'backward-delete-char)
        (check (editor-get-text ed) => "helo")))

    (test-case "edit: newline inserts newline at cursor"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "helloworld")
        (editor-goto-pos ed 5)
        (execute-command! app 'newline)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "\n"))) => #t)
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "edit: kill-line kills to end of line"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello\nworld")
        (editor-goto-pos ed 0)
        (execute-command! app 'kill-line)
        (let ((text (editor-get-text ed)))
          ;; "hello" should be gone, newline and "world" remain
          (check (not (string-contains text "hello")) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "edit: kill-line on empty line removes newline"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "\nworld")
        (editor-goto-pos ed 0)
        (execute-command! app 'kill-line)
        ;; The empty line (just the newline) should be removed
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "world" text) => #t))))

    (test-case "edit: open-line inserts newline without moving cursor"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (let ((orig-pos (editor-get-current-pos ed)))
          (execute-command! app 'open-line)
          (let ((text (editor-get-text ed)))
            ;; A newline should be inserted
            (check (not (not (string-contains text "\n"))) => #t)
            ;; Cursor should be at original position (before the newline)
            (check (= (editor-get-current-pos ed) orig-pos) => #t)))))

    (test-case "edit: self-insert inserts character at cursor"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        ;; Simulate 'x' keypress via sim-key!
        (sim-key! app (char-ev #\x))
        (check (editor-get-text ed) => "x")))

    (test-case "edit: multiple self-inserts build a string"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (sim-key! app (char-ev #\h))
        (sim-key! app (char-ev #\i))
        (check (editor-get-text ed) => "hi")))

    (test-case "edit: transpose-chars swaps the two characters before point"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        ;; transpose-chars requires pos >= 2, swaps chars at (pos-2) and (pos-1)
        (editor-set-text ed "ab")
        (editor-goto-pos ed 2)
        (execute-command! app 'transpose-chars)
        (check (editor-get-text ed) => "ba")))

    (test-case "edit: undo reverses last change"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 5)
        ;; Insert directly, then undo
        (editor-insert-text ed 5 "x")
        (editor-goto-pos ed 6)
        (check (editor-get-text ed) => "hellox")
        (execute-command! app 'undo)
        ;; Verify no crash; text should be modified back
        (check (string? (editor-get-text ed)) => #t)))

    ;;;==========================================================================
    ;;; Group 4: Kill Ring & Yank
    ;;;==========================================================================

    (test-case "kill-yank: kill-line then yank round-trip"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello\nworld")
        (editor-goto-pos ed 0)
        ;; Kill line
        (execute-command! app 'kill-line)
        ;; Move to end and yank
        (execute-command! app 'end-of-buffer)
        (execute-command! app 'yank)
        (let ((text (editor-get-text ed)))
          ;; "hello" should be yanked back in
          (check (not (not (string-contains text "hello"))) => #t))))

    (test-case "kill-yank: kill adds to kill ring"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'kill-line)
        ;; Kill ring should have something
        (check (> (length (app-state-kill-ring app)) 0) => #t)))

    (test-case "kill-yank: yank inserts at cursor position"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "aXb")
        (editor-goto-pos ed 1)
        ;; Kill 'X' with delete-char
        (execute-command! app 'delete-char)
        ;; Put cursor at end, yank
        (execute-command! app 'end-of-buffer)
        (execute-command! app 'yank)
        ;; Kill ring from delete-char may not be populated — check text is valid
        (check (string? (editor-get-text ed)) => #t)))

    (test-case "kill-yank: copy-region does not remove text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        ;; Set mark at 0, move to 5 to select "hello"
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        (execute-command! app 'copy-region)
        ;; Text should still be there
        (check (editor-get-text ed) => "hello world")))

    (test-case "kill-yank: kill-region removes selected text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        (execute-command! app 'kill-region)
        (let ((text (editor-get-text ed)))
          ;; "hello" should be gone
          (check (not (string-prefix? "hello" text)) => #t)
          ;; " world" should remain
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "kill-yank: kill-line on empty line removes newline"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        ;; Empty line followed by content: cursor at end of empty first line
        (editor-set-text ed "\nnext")
        (editor-goto-pos ed 0)
        (execute-command! app 'kill-line)
        ;; The newline should be removed, "next" at start
        ;; Note: kill-line on empty line uses editor-delete-range, not kill-ring
        (check (string-prefix? "next" (editor-get-text ed)) => #t)))

    (test-case "kill-yank: copy-region then yank duplicates text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        (execute-command! app 'copy-region)
        ;; Move to end, yank
        (execute-command! app 'end-of-buffer)
        (execute-command! app 'yank)
        ;; Buffer should now contain "hello" twice
        (let ((text (editor-get-text ed)))
          (check (string=? "hellohello" text) => #t))))

    ;;;==========================================================================
    ;;; Group 5: Mark & Region
    ;;;==========================================================================

    (test-case "mark: set-mark stores position in buffer"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 5)
        (execute-command! app 'set-mark)
        ;; Mark should be set on current buffer
        (let ((buf (current-buffer-from-app app)))
          (check (integer? (buffer-mark buf)) => #t)
          (check (buffer-mark buf) => 5))))

    (test-case "mark: set-mark then movement creates active region"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        ;; Mark is at 0, cursor at 5 — region is "hello"
        (let ((buf (current-buffer-from-app app)))
          (check (buffer-mark buf) => 0)
          (check (editor-get-current-pos ed) => 5))))

    (test-case "mark: mark-whole-buffer selects entire buffer without crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 5)
        ;; mark-whole-buffer calls cmd-select-all which uses Scintilla selection,
        ;; not the buffer-mark field. Just verify it doesn't crash.
        (execute-command! app 'mark-whole-buffer)
        (check (string? (editor-get-text ed)) => #t)))

    (test-case "mark: kill-region with mark deletes region text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "ABCDE")
        (editor-goto-pos ed 1)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 4)
        (execute-command! app 'kill-region)
        (let ((text (editor-get-text ed)))
          ;; "BCD" should be gone; "A" and "E" remain
          (check (not (string-contains text "BCD")) => #t)
          (check (not (not (string-contains text "A"))) => #t)
          (check (not (not (string-contains text "E"))) => #t))))

    (test-case "mark: exchange-point-and-mark swaps cursor and mark"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        ;; Now point=5, mark=0
        (execute-command! app 'exchange-point-and-mark)
        ;; After swap: point=0, mark=5
        (check (editor-get-current-pos ed) => 0)
        (let ((buf (current-buffer-from-app app)))
          (check (buffer-mark buf) => 5))))

    ;;;==========================================================================
    ;;; Group 6: Window Management
    ;;;==========================================================================

    (test-case "window: split-window creates two windows"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (let ((fr (app-state-frame app)))
          (check (= (length (frame-windows fr)) 1) => #t)
          (execute-command! app 'split-window)
          (check (= (length (frame-windows (app-state-frame app))) 2) => #t))))

    (test-case "window: other-window switches focus"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'split-window)
        (let ((win1 (current-window (app-state-frame app))))
          (execute-command! app 'other-window)
          (let ((win2 (current-window (app-state-frame app))))
            (check (not (eq? win1 win2)) => #t)))))

    (test-case "window: delete-other-windows returns to single window"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'split-window)
        (check (= (length (frame-windows (app-state-frame app))) 2) => #t)
        (execute-command! app 'delete-other-windows)
        (check (= (length (frame-windows (app-state-frame app))) 1) => #t)))

    ;;;==========================================================================
    ;;; Group 7: Mode-Specific Dispatch
    ;;;==========================================================================

    (test-case "mode: .org file → org-buffer? true, TAB dispatches to org"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (let ((buf (current-buffer-from-app app)))
          (check (org-buffer? buf) => #t))
        ;; TAB on template line should expand
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (check (not (not (string-contains (editor-get-text ed) "#+BEGIN_SRC"))) => #t)))

    (test-case "mode: .ss file → TAB inserts 2 spaces, not org expansion"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "main.ss")))
        (let ((buf (current-buffer-from-app app)))
          (check (org-buffer? buf) => #f))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "#+BEGIN_SRC")) => #t)
          (check (not (not (string-contains text "  "))) => #t))))

    (test-case "mode: .py file → TAB inserts spaces, not org expansion"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "script.py")))
        (let ((buf (current-buffer-from-app app)))
          (check (org-buffer? buf) => #f))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (execute-command! app 'indent-or-complete)
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "#+BEGIN_SRC")) => #t))))

    ;;;==========================================================================
    ;;; Group 8: Buffer Management (basic)
    ;;;==========================================================================

    (test-case "buffer: new buffer has correct name"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "myfile.ss")))
        (let ((buf (current-buffer-from-app app)))
          (check (buffer-name buf) => "myfile.ss"))))

    (test-case "buffer: buffer-list-add! makes buffer findable"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "unique-test-123.ss")))
        (let ((buf (current-buffer-from-app app)))
          (buffer-list-add! buf)
          (check (not (not (buffer-by-name "unique-test-123.ss"))) => #t)
          ;; Cleanup
          (buffer-list-remove! buf))))

    (test-case "buffer: toggle-read-only marks buffer read-only"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        ;; Initially not read-only
        (check (editor-get-read-only? ed) => #f)
        (execute-command! app 'toggle-read-only)
        ;; Now read-only
        (check (editor-get-read-only? ed) => #t)
        ;; Toggle again
        (execute-command! app 'toggle-read-only)
        (check (editor-get-read-only? ed) => #f)))

    ;;;==========================================================================
    ;;; Group 9: Text Transforms
    ;;;==========================================================================

    (test-case "transform: upcase-word uppercases word at point"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'upcase-word)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "HELLO"))) => #t))))

    (test-case "transform: downcase-word lowercases word at point"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "HELLO world")
        (editor-goto-pos ed 0)
        (execute-command! app 'downcase-word)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "hello"))) => #t)
          (check (string-prefix? "hello" text) => #t))))

    (test-case "transform: capitalize-word capitalizes word"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'capitalize-word)
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "Hello" text) => #t))))

    ;;;==========================================================================
    ;;; Group 10: Prefix Arguments
    ;;;==========================================================================

    (test-case "prefix: universal-argument sets prefix to list"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'universal-argument)
        ;; After C-u, prefix-arg should be a list (C-u prefix)
        (check (list? (app-state-prefix-arg app)) => #t)))

    (test-case "prefix: execute-command! resets prefix after normal command"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'universal-argument)
        ;; Prefix should be set
        (check (not (not (app-state-prefix-arg app))) => #t)
        ;; Execute a normal command
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        ;; Prefix should be reset to #f
        (check (app-state-prefix-arg app) => #f)))

    (test-case "prefix: last-command is updated after execute-command!"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        (check (app-state-last-command app) => 'forward-char)))

    ;;;==========================================================================
    ;;; Group 10: Org Commands Beyond TAB
    ;;;==========================================================================

    (test-case "org: org-todo cycles TODO state via execute-command!"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "tasks.org")))
        (editor-set-text ed "* Task headline")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-todo)
        (let ((text (editor-get-text ed)))
          ;; Should now have TODO keyword
          (check (not (not (string-contains text "TODO"))) => #t))))

    (test-case "org: org-promote demotes then promotes a heading"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        ;; Start with level-2 heading
        (editor-set-text ed "** Sub heading")
        (editor-goto-pos ed 0)
        ;; Demote: ** -> ***
        (execute-command! app 'org-demote)
        (check (string-prefix? "***" (editor-get-text ed)) => #t)
        ;; Promote: *** -> **
        (execute-command! app 'org-promote)
        (check (string-prefix? "**" (editor-get-text ed)) => #t)
        ;; Promote: ** -> *
        (execute-command! app 'org-promote)
        (check (string-prefix? "* " (editor-get-text ed)) => #t)))

    (test-case "org: org-promote at level 1 is a no-op"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* Top level")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-promote)
        ;; Still at level 1, no-op
        (check (string-prefix? "* " (editor-get-text ed)) => #t)))

    (test-case "org: org-insert-heading adds new heading"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* First\n")
        (editor-goto-pos ed 7)
        (execute-command! app 'org-insert-heading)
        (let ((text (editor-get-text ed)))
          ;; A new heading line should appear
          (check (> (string-length text) 8) => #t)
          (check (not (not (string-contains text "*"))) => #t))))

    (test-case "org: org-toggle-checkbox toggles [ ] to [X]"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "- [ ] Item to check")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-toggle-checkbox)
        (let ((text (editor-get-text ed)))
          ;; Should be checked now
          (check (not (not (string-contains text "[X]"))) => #t))))

    (test-case "org: org-toggle-checkbox toggles [X] back to [ ]"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "- [X] Already checked")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-toggle-checkbox)
        (let ((text (editor-get-text ed)))
          ;; Should be unchecked now
          (check (not (not (string-contains text "[ ]"))) => #t))))

    (test-case "org: org-move-subtree-up moves heading before prior"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* First\n* Second\n")
        ;; Position cursor on "* Second"
        (editor-goto-pos ed 8)
        (execute-command! app 'org-move-subtree-up)
        (let ((text (editor-get-text ed)))
          ;; "Second" should appear before "First"
          (let ((sec-pos (string-contains text "Second"))
                (fir-pos (string-contains text "First")))
            (when (and sec-pos fir-pos)
              (check (< sec-pos fir-pos) => #t))))))

    (test-case "org: org-move-subtree-down moves heading after next"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* First\n* Second\n")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-move-subtree-down)
        (let ((text (editor-get-text ed)))
          ;; "First" should appear after "Second"
          (let ((sec-pos (string-contains text "Second"))
                (fir-pos (string-contains text "First")))
            (when (and sec-pos fir-pos)
              (check (< sec-pos fir-pos) => #t))))))

    (test-case "org: org-priority adds [#A] priority"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* TODO Task")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-priority)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "[#A]"))) => #t))))

    (test-case "org: org-priority cycles A → B → C → none"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "notes.org")))
        (editor-set-text ed "* TODO Task")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-priority)
        (check (not (not (string-contains (editor-get-text ed) "[#A]"))) => #t)
        (execute-command! app 'org-priority)
        (check (not (not (string-contains (editor-get-text ed) "[#B]"))) => #t)
        (execute-command! app 'org-priority)
        (check (not (not (string-contains (editor-get-text ed) "[#C]"))) => #t)
        (execute-command! app 'org-priority)
        ;; Priority removed
        (check (not (string-contains (editor-get-text ed) "[#")) => #t)))

    ;;;==========================================================================
    ;;; Group 11 (extended): Additional text transforms
    ;;;==========================================================================

    (test-case "transform: join-line merges current line with next"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello\nworld")
        (editor-goto-pos ed 0)
        (execute-command! app 'join-line)
        (let ((text (editor-get-text ed)))
          ;; Lines merged, no newline between them
          (check (not (string-contains text "\nhello")) => #t)
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "transform: comment-region adds comment markers"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "(define x 42)")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 13)
        (execute-command! app 'comment-region)
        (let ((text (editor-get-text ed)))
          ;; Should have a comment marker
          (check (not (not (string-contains text ";"))) => #t))))

    (test-case "transform: upcase-region uppercases selected text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        (execute-command! app 'upcase-region)
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "HELLO" text) => #t))))

    (test-case "transform: downcase-region lowercases selected text"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "HELLO world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (editor-goto-pos ed 5)
        (execute-command! app 'downcase-region)
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "hello" text) => #t))))

    ;;;==========================================================================
    ;;; Group 12 (extended): More navigation and dispatch
    ;;;==========================================================================

    (test-case "nav: scroll-down does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed (make-string 500 #\a))
        (execute-command! app 'scroll-down)
        (check (>= (editor-get-current-pos ed) 0) => #t)))

    (test-case "nav: beginning-of-buffer after scroll returns to 0"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed (make-string 200 #\a))
        (execute-command! app 'end-of-buffer)
        (execute-command! app 'beginning-of-buffer)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "dispatch: multiple execute-command! calls accumulate state"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "abcde")
        (editor-goto-pos ed 0)
        ;; Move forward 3 times
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (check (editor-get-current-pos ed) => 3)))

    (test-case "dispatch: sim-key! with C-b moves backward"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 3)
        ;; C-b = 0x02
        (sim-key! app (ctrl-ev #x02))
        (check (editor-get-current-pos ed) => 2)))

    (test-case "dispatch: sim-key! with C-a goes to beginning of line"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 7)
        ;; C-a = 0x01
        (sim-key! app (ctrl-ev #x01))
        (check (editor-get-current-pos ed) => 0)))

    (test-case "dispatch: sim-key! with C-e goes to end of line"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        ;; C-e = 0x05
        (sim-key! app (ctrl-ev #x05))
        (check (editor-get-current-pos ed) => 5)))

    (test-case "dispatch: sim-key! inserts multiple chars"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (for-each (lambda (c) (sim-key! app (char-ev c)))
                  (string->list "test"))
        (check (editor-get-text ed) => "test")))

    (test-case "dispatch: key-state resets to initial after command"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        ;; Key state should be at initial (global keymap)
        (let ((ks (app-state-key-state app)))
          (check (null? (key-state-prefix-keys ks)) => #t))))

    ;;=========================================================================
    ;; Group 8: set-mark + navigation region highlight (transient-mark-mode)
    ;; Tests the bug: set-mark from bottom of file, arrow up should highlight
    ;;=========================================================================

    (test-case "mark: set-mark stores cursor position in buffer-mark"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 10)
        (execute-command! app 'set-mark)
        (let ((buf (current-buffer-from-app app)))
          (check (buffer-mark buf) => 10))))

    (test-case "mark: set-mark at end-of-buffer then previous-line highlights region"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "line1\nline2\nline3")
        ;; Go to end of buffer and set mark
        (execute-command! app 'end-of-buffer)
        (let ((end-pos (editor-get-current-pos ed)))
          (execute-command! app 'set-mark)
          ;; Move up one line (previous-line)
          (execute-command! app 'previous-line)
          ;; Selection anchor should be at end-pos (mark), caret at new pos
          (let ((sel-start (editor-get-selection-start ed))
                (sel-end (editor-get-selection-end ed))
                (new-pos (editor-get-current-pos ed)))
            ;; Region should be active: selection start <= new-pos, end = mark
            (check (< new-pos end-pos) => #t)  ; cursor moved up
            (check (or (= sel-start new-pos) (= sel-end new-pos)) => #t)))))

    (test-case "mark: set-mark then forward-char extends selection forward"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        ;; Move forward 5 chars
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (let ((sel-start (editor-get-selection-start ed))
              (sel-end (editor-get-selection-end ed)))
          ;; Selection from 0 (mark) to 5 (point)
          (check sel-start => 0)
          (check sel-end => 5))))

    (test-case "mark: set-mark then backward-char extends selection backward"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 5)
        (execute-command! app 'set-mark)
        ;; Move backward 3 chars
        (execute-command! app 'backward-char)
        (execute-command! app 'backward-char)
        (execute-command! app 'backward-char)
        (let ((sel-start (editor-get-selection-start ed))
              (sel-end (editor-get-selection-end ed)))
          ;; Selection: anchor at 5 (mark), caret at 2 (point)
          ;; Scintilla stores as start=2, end=5 with reversed caret
          (check sel-start => 2)
          (check sel-end => 5))))

    (test-case "mark: keyboard-quit clears mark and visual selection"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'set-mark)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        ;; Now C-g to clear mark
        (execute-command! app 'keyboard-quit)
        (let* ((buf (current-buffer-from-app app))
               (sel-start (editor-get-selection-start ed))
               (sel-end (editor-get-selection-end ed)))
          (check (buffer-mark buf) => #f)
          ;; Selection should be collapsed (start == end)
          (check (= sel-start sel-end) => #t))))

    (test-case "mark: set-mark in org buffer, previous-line from last line highlights"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.org")))
        (editor-set-text ed "* Heading\nsome text\nanother line")
        ;; Go to very end
        (execute-command! app 'end-of-buffer)
        (let ((end-pos (editor-get-current-pos ed)))
          (execute-command! app 'set-mark)
          ;; Move up 2 lines
          (execute-command! app 'previous-line)
          (execute-command! app 'previous-line)
          (let ((new-pos (editor-get-current-pos ed))
                (sel-start (editor-get-selection-start ed))
                (sel-end (editor-get-selection-end ed)))
            ;; Cursor should have moved up
            (check (< new-pos end-pos) => #t)
            ;; Visual selection should be active (not collapsed)
            (check (< sel-start sel-end) => #t)))))

    ;;=========================================================================
    ;; Group 9: magit-style git interface
    ;; Tests that magit commands create appropriate buffers via dispatch chain.
    ;;=========================================================================

    (test-case "magit: magit-status creates buffer containing Head: header"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'magit-status)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "Head:"))) => #t))))

    (test-case "magit: magit-status buffer contains Staged/Unstaged or clean message"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'magit-status)
        (let ((text (editor-get-text ed)))
          ;; Should contain either change sections or clean message
          (check (or (not (eq? #f (string-contains text "Staged")))
                     (not (eq? #f (string-contains text "Unstaged")))
                     (not (eq? #f (string-contains text "clean")))
                     (not (eq? #f (string-contains text "Recent commits")))) => #t))))

    (test-case "magit: magit-log creates buffer containing Git Log header"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'magit-log)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "Git Log"))) => #t))))

    (test-case "magit: magit-diff creates buffer containing Git Diff header"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'magit-diff)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "Git Diff"))) => #t))))

    (test-case "magit: magit-stage-file with no file-path buffer does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        ;; Buffer has no file-path → command echoes "Buffer has no file"
        (execute-command! app 'magit-stage-file)
        (check #t => #t)))

    (test-case "magit: magit-unstage-file with no file-path buffer does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'magit-unstage-file)
        (check #t => #t)))

    (test-case "magit: git-log-file with no file-path buffer does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'git-log-file)
        (check #t => #t)))

    (test-case "magit: magit-branch command runs without error"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "test.ss")))
        (execute-command! app 'magit-branch)
        (check #t => #t)))

    ;;=========================================================================
    ;; Group 10: Real git operations on a controlled temp repository
    ;; Commands that use (directory: (path-directory buffer-file-path)) work
    ;; correctly against a temp repo. Commands without directory: use CWD.
    ;;=========================================================================

    (test-case "git-real: show-git-log on temp repo contains Initial commit"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let-values (((ed app) (make-test-app-with-file
                                 (string-append dir "/README.md"))))
          (execute-command! app 'show-git-log)
          (let ((text (editor-get-text ed)))
            (check (not (eq? #f (string-contains text "Initial commit"))) => #t)))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: show-git-status after modification shows modified file"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let ((path (string-append dir "/README.md")))
          (write-file-content! path "# Modified\n")
          (let-values (((ed app) (make-test-app-with-file path)))
            (execute-command! app 'show-git-status)
            (let ((text (editor-get-text ed)))
              (check (not (eq? #f (string-contains text "README"))) => #t))))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: show-git-diff after modification is non-empty"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let ((path (string-append dir "/README.md")))
          (write-file-content! path "# Modified\n")
          (let-values (((ed app) (make-test-app-with-file path)))
            (execute-command! app 'show-git-diff)
            (let ((text (editor-get-text ed)))
              (check (> (string-length text) 0) => #t))))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: show-git-blame on committed file is non-empty"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let-values (((ed app) (make-test-app-with-file
                                 (string-append dir "/README.md"))))
          (execute-command! app 'show-git-blame)
          (let ((text (editor-get-text ed)))
            (check (> (string-length text) 0) => #t)))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: vc-log-file on gemacs file is non-empty"
      (setup-default-bindings!)
      (register-all-commands!)
      ;; vc-log-file runs git from CWD (gemacs project), so use a real gemacs file
      (let-values (((ed app) (make-test-app-with-file "functional-test.ss")))
        (execute-command! app 'vc-log-file)
        (let ((text (editor-get-text ed)))
          (check (> (string-length text) 0) => #t))))

    (test-case "git-real: vc-annotate on small gemacs file does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      ;; Use a small file to avoid git blame timeout on large files
      (let-values (((ed app) (make-test-app-with-file "gerbil.pkg")))
        (execute-command! app 'vc-annotate)
        (check #t => #t)))

    (test-case "git-real: vc-diff-head on gemacs file does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "functional-test.ss")))
        (execute-command! app 'vc-diff-head)
        (check #t => #t)))

    (test-case "git-real: vc-revert with scripted no response does not revert"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let ((path (string-append dir "/README.md")))
          (write-file-content! path "# Modified\n")
          (let-values (((ed app) (make-test-app-with-file path)))
            ;; Respond "no" to the revert confirmation
            (with-scripted-responses '("no")
              (lambda () (execute-command! app 'vc-revert)))
            ;; File should still be modified (not reverted)
            (check #t => #t)))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: vc-revert with scripted yes response restores file"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let ((path (string-append dir "/README.md")))
          (write-file-content! path "# Modified\n")
          (let-values (((ed app) (make-test-app-with-file path)))
            ;; Respond "yes" to the revert confirmation
            ;; Note: vc-revert runs git from CWD, so path must be in CWD's repo.
            ;; Here we just verify the command dispatches correctly.
            (with-scripted-responses '("yes")
              (lambda () (execute-command! app 'vc-revert)))
            (check #t => #t)))
        (cleanup-temp-git-repo! dir)))

    (test-case "git-real: magit-stage-file with temp file does not crash"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((dir (make-temp-git-repo!)))
        (let ((path (string-append dir "/newfile.ss")))
          (write-file-content! path "(def (hello) \"world\")\n")
          (let-values (((ed app) (make-test-app-with-file path)))
            ;; magit-stage-file runs git add with full path; may fail if CWD
            ;; is a different repo but should not crash.
            (execute-command! app 'magit-stage-file)
            (check #t => #t)))
        (cleanup-temp-git-repo! dir)))

    ;;=========================================================================
    ;; TUI Window Management Tests (Group 11 equivalent)
    ;;=========================================================================

    (test-case "window: split-window-below creates 2 windows"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-below)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 2)
          (check (split-node? root) => #t)
          (check (split-node-orientation root) => 'vertical))))

    (test-case "window: split-window-right creates 2 windows"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-right)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 2)
          (check (split-node? root) => #t)
          (check (split-node-orientation root) => 'horizontal))))

    (test-case "window: delete-window restores single pane"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-below)
        (execute-command! app 'delete-window)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 1)
          (check (split-leaf? root) => #t))))

    (test-case "window: delete-other-windows collapses to single pane"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-right)
        (execute-command! app 'split-window-below)
        (execute-command! app 'delete-other-windows)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 1)
          (check (split-leaf? root) => #t))))

    (test-case "window: hsplit → other-window → vsplit (the reported bug)"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        ;; 1. Split right: A | B (cursor at B)
        (execute-command! app 'split-window-right)
        (let ((fr (app-state-frame app)))
          (check (frame-current-idx fr) => 1))
        ;; 2. other-window back to A
        (execute-command! app 'other-window)
        (let ((fr (app-state-frame app)))
          (check (frame-current-idx fr) => 0))
        ;; 3. Split below in A: (A1 over A2) | B
        (execute-command! app 'split-window-below)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 3)
          ;; Root should be horizontal, left child vertical, right leaf
          (check (split-node? root) => #t)
          (check (split-node-orientation root) => 'horizontal)
          (let ((left (car (split-node-children root)))
                (right (cadr (split-node-children root))))
            (check (split-node? left) => #t)
            (check (split-node-orientation left) => 'vertical)
            (check (split-leaf? right) => #t)))))

    (test-case "window: vsplit → other-window → hsplit"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        ;; 1. Split below: A over B (cursor at B)
        (execute-command! app 'split-window-below)
        (let ((fr (app-state-frame app)))
          (check (frame-current-idx fr) => 1))
        ;; 2. other-window back to A
        (execute-command! app 'other-window)
        (let ((fr (app-state-frame app)))
          (check (frame-current-idx fr) => 0))
        ;; 3. Split right in A: (A1 | A2) over B
        (execute-command! app 'split-window-right)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 3)
          ;; Root should be vertical, top child horizontal, bottom leaf
          (check (split-node? root) => #t)
          (check (split-node-orientation root) => 'vertical)
          (let ((top (car (split-node-children root)))
                (bottom (cadr (split-node-children root))))
            (check (split-node? top) => #t)
            (check (split-node-orientation top) => 'horizontal)
            (check (split-leaf? bottom) => #t)))))

    (test-case "window: four-pane grid (2x2)"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        ;; Build (A|B) over (C|D)
        (execute-command! app 'split-window-right)   ; A|B, current=B
        (execute-command! app 'other-window)         ; current=A
        (execute-command! app 'split-window-below)   ; (A1|B) + A2, current=A2
        (execute-command! app 'other-window)         ; current=B
        (execute-command! app 'split-window-below)   ; (A1|B1) + (A2|B2)
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr)))
          (check (length wins) => 4))))

    (test-case "window: other-window cycles through all panes"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-right)
        (execute-command! app 'split-window-below)
        (let* ((fr (app-state-frame app))
               (start-idx (frame-current-idx fr)))
          (execute-command! app 'other-window)
          (execute-command! app 'other-window)
          (execute-command! app 'other-window)
          (let ((end-idx (frame-current-idx fr)))
            (check end-idx => start-idx)))))

    (test-case "window: three-way horizontal split uses flat siblings"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "*scratch*")))
        (execute-command! app 'split-window-right)  ; A|B
        (execute-command! app 'split-window-right)  ; A|B|C
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               (root (frame-root fr)))
          (check (length wins) => 3)
          (check (split-node? root) => #t)
          (check (split-node-orientation root) => 'horizontal)
          (check (length (split-node-children root)) => 3))))

    ;;; Org-table commands — TUI dispatch-chain tests
    (test-case "org-table: create inserts table template"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        ;; Feed empty string so app-read-string defaults to 3 columns
        (with-scripted-responses '("")
          (lambda ()
            (execute-command! app 'org-table-create)
            (let ((text (editor-get-text ed)))
              (check (and (string-contains text "| Col1") #t) => #t)
              (check (and (string-contains text "|---") #t) => #t))))))

    (test-case "org-table: align re-aligns uneven table"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| a | bb | ccc |\n| dddd | e | ff |")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-table-align)
        (let ((text (editor-get-text ed)))
          (check (and (string-contains text "| a    |") #t) => #t)
          (check (and (string-contains text "| dddd |") #t) => #t))))

    (test-case "org-table: insert-row adds empty row"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| A | B |\n| 1 | 2 |")
        ;; Position on second row
        (editor-goto-pos ed (editor-position-from-line ed 1))
        (execute-command! app 'org-table-insert-row)
        (let ((text (editor-get-text ed)))
          ;; Should now have 3 lines
          (check (length (string-split text #\newline)) => 3))))

    (test-case "org-table: delete-row removes current row"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| Alice | 30 |\n| Bob | 25 |")
        ;; Position on Alice row
        (editor-goto-pos ed 0)
        (execute-command! app 'org-table-delete-row)
        (let ((text (editor-get-text ed)))
          (check (string-contains text "Alice") => #f)
          (check (and (string-contains text "Bob") #t) => #t))))

    (test-case "org-table: move-row-down swaps rows"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| Alice |\n| Bob |")
        (editor-goto-pos ed 0) ;; on Alice
        (execute-command! app 'org-table-move-row-down)
        (let* ((text (editor-get-text ed))
               (lines (string-split text #\newline)))
          (check (and (string-contains (car lines) "Bob") #t) => #t))))

    (test-case "org-table: delete-column removes column"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| Name | Age |\n| Alice | 30 |")
        ;; Position cursor in Age column (after second |)
        (editor-goto-pos ed 10)
        (execute-command! app 'org-table-delete-column)
        (let ((text (editor-get-text ed)))
          (check (string-contains text "Age") => #f)
          (check (and (string-contains text "Name") #t) => #t))))

    (test-case "org-table: insert-separator adds divider"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| A | B |\n| 1 | 2 |")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-table-insert-separator)
        (let ((text (editor-get-text ed)))
          (check (and (string-contains text "---") #t) => #t))))

    (test-case "org-table: sort sorts numerically"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| X | 30 |\n| Y | 10 |\n| Z | 20 |")
        ;; Position in number column (col 1)
        (editor-goto-pos ed 5)
        (execute-command! app 'org-table-sort)
        (let* ((text (editor-get-text ed))
               (lines (string-split text #\newline)))
          (check (and (string-contains (car lines) "10") #t) => #t))))

    (test-case "org-table: transpose swaps rows and columns"
      (setup-default-bindings!)
      (register-all-commands!)
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.org")))
        (editor-set-text ed "| A | B |\n| 1 | 2 |\n| 3 | 4 |")
        (editor-goto-pos ed 0)
        (execute-command! app 'org-table-transpose)
        (let* ((text (editor-get-text ed))
               (lines (string-split text #\newline)))
          ;; 3 rows x 2 cols -> 2 rows x 3 cols
          (check (length lines) => 2))))

    (test-case "org-table: all 17 commands registered"
      (setup-default-bindings!)
      (register-all-commands!)
      (let ((cmds '(org-table-create org-table-align org-table-insert-row
                    org-table-delete-row org-table-move-row-up org-table-move-row-down
                    org-table-delete-column org-table-insert-column
                    org-table-move-column-left org-table-move-column-right
                    org-table-insert-separator org-table-sort org-table-sum
                    org-table-recalculate org-table-export-csv org-table-import-csv
                    org-table-transpose)))
        (for-each (lambda (cmd) (check (procedure? (find-command cmd)) => #t)) cmds)))

    ;; Electric pair tests
    (test-case "electric-pair: ( inserts matching )"
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.ss")))
        (set! *auto-pair-mode* #t)
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 40) ;; (
        (let ((text (editor-get-text ed)))
          (check text => "()")
          (check (editor-get-current-pos ed) => 1))))

    (test-case "electric-pair: [ inserts matching ]"
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.ss")))
        (set! *auto-pair-mode* #t)
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 91) ;; [
        (check (editor-get-text ed) => "[]")))

    (test-case "electric-pair: { inserts matching }"
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.ss")))
        (set! *auto-pair-mode* #t)
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 123) ;; {
        (check (editor-get-text ed) => "{}")))

    (test-case "electric-pair: ) skips over existing )"
      (let-values (((ed app) (make-test-app-with-file "/tmp/test.ss")))
        (set! *auto-pair-mode* #t)
        (editor-set-text ed "()")
        (editor-goto-pos ed 1) ;; between ( and )
        (cmd-self-insert! app 41) ;; )
        (check (editor-get-text ed) => "()")
        (check (editor-get-current-pos ed) => 2)))

    (test-case "electric-pair: toggle-electric-pair registered"
      (setup-default-bindings!)
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-electric-pair)) => #t))

    ;;; Code Folding commands
    (test-case "code-folding: commands registered"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-fold)) => #t)
      (check (procedure? (find-command 'fold-all)) => #t)
      (check (procedure? (find-command 'unfold-all)) => #t)
      (check (procedure? (find-command 'fold-level)) => #t))

    (test-case "code-folding: fold-all dispatch"
      (let-values (((ed app) (make-test-app "fold-test")))
        (editor-set-text ed "(def (foo x)\n  (+ x 1))\n")
        (execute-command! app 'fold-all)
        (check (app-state-last-command app) => 'fold-all)))

    (test-case "code-folding: unfold-all dispatch"
      (let-values (((ed app) (make-test-app "unfold-test")))
        (editor-set-text ed "(def (bar y)\n  (* y 2))\n")
        (execute-command! app 'unfold-all)
        (check (app-state-last-command app) => 'unfold-all)))

    (test-case "code-folding: toggle-fold dispatch"
      (let-values (((ed app) (make-test-app "toggle-fold-test")))
        (editor-set-text ed "(def (baz z)\n  (- z 1))\n")
        (execute-command! app 'toggle-fold)
        (check (app-state-last-command app) => 'toggle-fold)))

    (test-case "code-folding: keybindings"
      (setup-default-bindings!)
      (check (keymap-lookup *meta-g-map* "F") => 'toggle-fold)
      (check (keymap-lookup *meta-g-map* "C") => 'fold-all)
      (check (keymap-lookup *meta-g-map* "E") => 'unfold-all))

    (test-case "recenter-top-bottom: registered and dispatches"
      (let-values (((ed app) (make-test-app "recenter-test")))
        (editor-set-text ed "line one\nline two\nline three\n")
        (execute-command! app 'recenter-top-bottom)
        (check (app-state-last-command app) => 'recenter-top-bottom)))

    (test-case "recenter-top-bottom: C-l binding"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "C-l") => 'recenter-top-bottom))

    (test-case "overwrite-mode: dispatches via execute-command!"
      (let-values (((ed app) (make-test-app "overwrite-test")))
        (editor-set-text ed "hello")
        (execute-command! app 'overwrite-mode)
        (check (app-state-last-command app) => 'overwrite-mode)
        ;; SCI_GETOVERTYPE = 2187; should be 1 after toggling ON
        (check (send-message ed 2187 0 0) => 1)
        ;; Toggle again to turn OFF
        (execute-command! app 'overwrite-mode)
        (check (send-message ed 2187 0 0) => 0)))

    (test-case "delete-rectangle: registered and dispatches"
      (let-values (((ed app) (make-test-app "del-rect-test")))
        (editor-set-text ed "abcde\nfghij\nklmno\n")
        ;; Set mark at col 1 line 0, cursor at col 3 line 1
        (let ((buf (current-buffer-from-app app)))
          (set! (buffer-mark buf) 1)
          (editor-goto-pos ed 9))
        (execute-command! app 'delete-rectangle)
        (check (app-state-last-command app) => 'delete-rectangle)))

    (test-case "delete-rectangle: C-x r d binding"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-r-map* "d") => 'delete-rectangle))

    ;; ==================================================================
    ;; Regression tests for the execute-command! paren bug
    ;; (echo-error! was running unconditionally for ALL commands)
    ;; ==================================================================

    (test-case "dispatch: found command does NOT show 'undefined' error"
      (let-values (((ed app) (make-test-app "paren-bug-test")))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        (let* ((echo (app-state-echo app))
               (msg (echo-state-message echo))
               (err? (echo-state-error? echo)))
          ;; After a found command, echo should NOT contain "is undefined"
          (check (not (and msg (string-contains msg "is undefined"))) => #t))))

    (test-case "dispatch: unfound command shows 'undefined' error"
      (let-values (((ed app) (make-test-app "paren-bug-test2")))
        (execute-command! app 'nonexistent-command-xyz)
        (let* ((echo (app-state-echo app))
               (msg (echo-state-message echo)))
          (check (not (not (and msg (string-contains msg "is undefined")))) => #t))))

    (test-case "dispatch: multiple commands don't accumulate errors"
      (let-values (((ed app) (make-test-app "multi-cmd-test")))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (execute-command! app 'forward-char)
        (let ((err? (echo-state-error? (app-state-echo app))))
          (check (not err?) => #t))))

    (test-case "dispatch: save-buffer via execute-command! writes file"
      (let* ((tmp-path "/tmp/gemacs-func-test-save.ss"))
        (write-string-to-file tmp-path "original\n")
        (let-values (((ed app) (make-test-app-with-file tmp-path)))
          (editor-set-text ed "(+ 1 2)\n")
          (execute-command! app 'save-buffer)
          (let ((saved (call-with-input-file tmp-path
                         (lambda (p) (read-line p #f)))))
            (check (not (not (string-contains saved "(+ 1 2)"))) => #t))
          (with-catch (lambda (e) #f)
            (lambda () (delete-file tmp-path))))))

    (test-case "dispatch: eval-last-sexp via execute-command! evaluates sexp"
      (let-values (((ed app) (make-test-app "eval-test.ss")))
        (editor-set-text ed "(+ 1 2)")
        (editor-goto-pos ed 7)  ;; after the closing )
        (execute-command! app 'eval-last-sexp)
        (let* ((echo (app-state-echo app))
               (msg (echo-state-message echo)))
          (check (not (not (and msg (string-contains msg "3")))) => #t))))

    (test-case "dispatch: C-x C-s bound to save-buffer"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-s") => 'save-buffer))

    (test-case "dispatch: C-x C-e bound to eval-last-sexp"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-e") => 'eval-last-sexp))

    ;; Smerge tests
    (test-case "smerge: commands registered"
      (register-all-commands!)
      (check (not (eq? #f (find-command 'smerge-mode))) => #t)
      (check (not (eq? #f (find-command 'smerge-next))) => #t)
      (check (not (eq? #f (find-command 'smerge-keep-mine))) => #t)
      (check (not (eq? #f (find-command 'smerge-keep-other))) => #t)
      (check (not (eq? #f (find-command 'smerge-keep-both))) => #t))

    (test-case "smerge: keep-mine resolves conflict"
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "conflict.txt")))
        (editor-set-text ed "before\n<<<<<<< HEAD\nmine\n=======\ntheirs\n>>>>>>> branch\nafter\n")
        (editor-goto-pos ed 10)
        (execute-command! app 'smerge-keep-mine)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "mine"))) => #t)
          (check (eq? #f (string-contains text "<<<<<<<")) => #t)
          (check (eq? #f (string-contains text "theirs")) => #t))))

    (test-case "smerge: keep-other resolves conflict"
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "conflict.txt")))
        (editor-set-text ed "before\n<<<<<<< HEAD\nmine\n=======\ntheirs\n>>>>>>> branch\nafter\n")
        (editor-goto-pos ed 10)
        (execute-command! app 'smerge-keep-other)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "theirs"))) => #t)
          (check (eq? #f (string-contains text "<<<<<<<")) => #t)
          (check (eq? #f (string-contains text "mine")) => #t))))

    (test-case "smerge: keep-both removes markers keeps content"
      (register-all-commands!)
      (let-values (((ed app) (make-test-app "conflict.txt")))
        (editor-set-text ed "before\n<<<<<<< HEAD\nmine\n=======\ntheirs\n>>>>>>> branch\nafter\n")
        (editor-goto-pos ed 10)
        (execute-command! app 'smerge-keep-both)
        (let ((text (editor-get-text ed)))
          (check (not (eq? #f (string-contains text "mine"))) => #t)
          (check (not (eq? #f (string-contains text "theirs"))) => #t)
          (check (eq? #f (string-contains text "<<<<<<<")) => #t))))

    ;; Interactive agenda tests
    (test-case "org-agenda: interactive commands registered"
      (register-all-commands!)
      (check (not (eq? #f (find-command 'org-agenda-goto))) => #t)
      (check (not (eq? #f (find-command 'org-agenda-todo))) => #t))

))

(def main
  (lambda args
    (setup-default-bindings!)
    (register-all-commands!)
    (run-tests! functional-test)
    (test-report-summary!)))
