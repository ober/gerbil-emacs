;;; -*- Gerbil -*-
;;; LSP functional tests for gemacs (Qt exe).
;;; Tests command registration, workspace edit application, and error resilience
;;; through the Qt dispatch chain. These tests do NOT need a live LSP server.
;;;
;;; Run: QT_QPA_PLATFORM=offscreen .gerbil/bin/lsp-functional-test
;;;
;;; For LSP protocol tests (lifecycle, doc sync, diagnostics, requests),
;;; see lsp-protocol-test.ss which runs in the interpreter.

(import :std/sugar
        :std/text/json
        (only-in :gerbil-qt/qt
                 with-qt-app
                 qt-widget-create
                 qt-scintilla-create)
        :gerbil-scintilla/constants
        (only-in :gemacs/qt/sci-shim
                 sci-send
                 qt-plain-text-edit-text
                 qt-plain-text-edit-set-text!
                 qt-plain-text-edit-cursor-position
                 qt-plain-text-edit-set-cursor-position!)
        :gemacs/core
        (only-in :gemacs/qt/window
                 make-qt-edit-window
                 make-qt-frame
                 make-split-leaf
                 qt-edit-window-buffer)
        (only-in :gemacs/qt/commands
                 qt-register-all-commands!)
        (only-in :gemacs/qt/lsp-client
                 lsp-stop!
                 lsp-running?
                 lsp-poll-ui-actions!
                 *lsp-initialized*
                 *lsp-initializing*
                 *lsp-process*)
        (only-in :gemacs/qt/commands-lsp
                 lsp-apply-workspace-edit!
                 lsp-apply-text-edits!))

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

(def (check! label val expected)
  (if (equal? val expected)
    (pass! label)
    (fail! label val expected)))

(def (check-true! label val)
  (if val
    (pass! label)
    (fail! label val #t)))

(def (check-false! label val)
  (if (not val)
    (pass! label)
    (fail! label val #f)))

(def (check-string-contains! label haystack needle)
  (if (and (string? haystack) (string-contains haystack needle))
    (pass! label)
    (fail! label haystack (string-append "string containing '" needle "'"))))

;;;============================================================================
;;; Singleton Qt widget (same pattern as qt-functional-test.ss)
;;;============================================================================

(def *lsp-test-singleton-ed* #f)
(def *lsp-test-singleton-w*  #f)

(def (lsp-test-singleton-init!)
  (unless *lsp-test-singleton-ed*
    (set! *lsp-test-singleton-w*  (qt-widget-create))
    (set! *lsp-test-singleton-ed* (qt-scintilla-create parent: *lsp-test-singleton-w*))))

(def (make-lsp-test-app path)
  "Create a headless Qt test app with a buffer pointing to PATH."
  (lsp-test-singleton-init!)
  (let* ((ed  *lsp-test-singleton-ed*)
         (w   *lsp-test-singleton-w*)
         (doc (sci-send ed SCI_GETDOCPOINTER))
         (name (if path (path-strip-directory path) "*lsp-test*"))
         (buf (make-buffer name path doc #f #f #f #f))
         (win (make-qt-edit-window ed #f buf #f #f #f))
         (fr  (make-qt-frame #f (make-split-leaf win) (list win) 0 #f))
         (app (new-app-state fr)))
    (values ed app buf)))

(def (lsp-get-echo app)
  "Return the current echo message from app (or #f if none)."
  (echo-state-message (app-state-echo app)))

;;;============================================================================
;;; Group 1: Command Registration
;;;============================================================================

(def (run-group-1-command-registration)
  (displayln "=== Group 1: LSP Command Registration ===")
  (for-each
    (lambda (cmd)
      (if (find-command cmd)
        (pass! (string-append (symbol->string cmd) " registered"))
        (fail! (string-append (symbol->string cmd) " registered") #f "procedure")))
    '(toggle-lsp lsp
      lsp-goto-definition lsp-declaration lsp-type-definition lsp-implementation
      lsp-hover lsp-completion lsp-rename
      lsp-code-actions lsp-find-references lsp-document-symbols
      lsp-workspace-symbol lsp-format-buffer lsp-restart lsp-stop
      lsp-smart-goto-definition)))

;;;============================================================================
;;; Group 9: Workspace Edit Application
;;;============================================================================

(def (run-group-9-workspace-edit)
  (displayln "=== Group 9: Workspace Edit Application ===")

  (let-values (((ed app buf) (make-lsp-test-app "/tmp/test-edit.ss")))

    ;; 9.1: lsp-apply-text-edits! — replace range with new text
    (displayln "  Test: lsp-apply-text-edits! replaces range")
    (let ((original "(def (hello) \"world\")"))
      (qt-plain-text-edit-set-text! ed original)
      ;; Build a TextEdit that replaces "hello" with "hi"
      ;; "(def (hello) ...)" — "hello" starts at char 6, ends at char 11
      ;; Positions: ( = 0, d = 1, e = 2, f = 3, space = 4, ( = 5, h = 6
      (let* ((range (make-hash-table))
             (start (make-hash-table))
             (end   (make-hash-table))
             (edit  (make-hash-table)))
        (hash-put! start "line" 0)
        (hash-put! start "character" 6)
        (hash-put! end   "line" 0)
        (hash-put! end   "character" 11)
        (hash-put! range "start" start)
        (hash-put! range "end"   end)
        (hash-put! edit  "range" range)
        (hash-put! edit  "newText" "hi")
        (lsp-apply-text-edits! app ed (list edit))
        (let ((result (qt-plain-text-edit-text ed)))
          (check! "text edit replaced 'hello' with 'hi'"
            result "(def (hi) \"world\")"))))

    ;; 9.2: Multiple edits applied in reverse order
    (displayln "  Test: multiple text edits applied in reverse document order")
    (let ((text "aaa bbb ccc"))
      (qt-plain-text-edit-set-text! ed text)
      ;; Edit 1: replace "aaa" (pos 0-3) with "AAA"
      ;; Edit 2: replace "ccc" (pos 8-11) with "CCC"
      (let* ((make-edit (lambda (l0 c0 l1 c1 new)
                          (let ((r (make-hash-table))
                                (s (make-hash-table))
                                (e (make-hash-table))
                                (h (make-hash-table)))
                            (hash-put! s "line" l0) (hash-put! s "character" c0)
                            (hash-put! e "line" l1) (hash-put! e "character" c1)
                            (hash-put! r "start" s) (hash-put! r "end" e)
                            (hash-put! h "range" r) (hash-put! h "newText" new)
                            h)))
             (edit1 (make-edit 0 0 0 3 "AAA"))
             (edit2 (make-edit 0 8 0 11 "CCC")))
        ;; apply-text-edits! sorts in reverse order, so edit2 first then edit1
        (lsp-apply-text-edits! app ed (list edit1 edit2))
        (let ((result (qt-plain-text-edit-text ed)))
          (check! "multiple edits applied correctly"
            result "AAA bbb CCC"))))

    ;; 9.3: lsp-apply-workspace-edit! with changes map
    (displayln "  Test: lsp-apply-workspace-edit! with changes")
    (qt-plain-text-edit-set-text! ed "original text")
    (let* ((range (make-hash-table))
           (start (make-hash-table))
           (end   (make-hash-table))
           (edit  (make-hash-table))
           (changes (make-hash-table))
           (workspace-edit (make-hash-table))
           (uri "file:///tmp/test-edit.ss"))
      (hash-put! start "line" 0) (hash-put! start "character" 0)
      (hash-put! end   "line" 0) (hash-put! end   "character" 8)
      (hash-put! range "start" start) (hash-put! range "end" end)
      (hash-put! edit "range" range) (hash-put! edit "newText" "modified")
      (hash-put! changes uri (list edit))
      (hash-put! workspace-edit "changes" changes)
      (lsp-apply-workspace-edit! app workspace-edit)
      (let ((result (qt-plain-text-edit-text ed)))
        (check! "workspace-edit changes applied to buffer"
          result "modified text")))))

;;;============================================================================
;;; Group 10: Error Resilience
;;;============================================================================

(def (run-group-10-resilience)
  (displayln "=== Group 10: Error Resilience ===")

  ;; Ensure LSP is not running
  (when (lsp-running?) (lsp-stop!))

  (let-values (((ed app buf) (make-lsp-test-app "/tmp/test-resilience.ss")))
    (buffer-list-add! buf)

    ;; Helper: call command by name and return echo message.
    ;; Use find-command + direct call instead of execute-command!
    ;; (execute-command! has a known issue in compiled exes where find-command
    ;; returns #f internally — likely a Gambit closure/hash-table identity issue).
    (def (call-cmd! name)
      (let ((cmd (find-command name)))
        (if cmd
          (cmd app)
          (fail! (string-append (symbol->string name) " found via find-command") #f "procedure")))
      (lsp-get-echo app))

    ;; 10.1: Every command that guards on lsp-running? must echo "not running"
    ;; when called with no server active. This catches runtime unbound-variable
    ;; crashes (silent before the guard is reached) as well as missing guards.
    (displayln "  Test: all LSP commands produce 'not running' error when server is off")
    (for-each
      (lambda (name)
        (let ((msg (call-cmd! name)))
          (check-string-contains!
            (string-append (symbol->string name) " shows 'not running' error")
            msg "not running")))
      '(lsp-goto-definition lsp-declaration lsp-type-definition lsp-implementation
        lsp-hover lsp-completion lsp-rename lsp-code-actions
        lsp-find-references lsp-document-symbols lsp-workspace-symbol
        lsp-format-buffer))

    ;; 10.2: lsp-stop command echoes "stopped" even when already stopped (idempotent)
    (displayln "  Test: lsp-stop command echoes and is idempotent")
    (let ((msg (call-cmd! 'lsp-stop)))
      (check-string-contains! "lsp-stop echoes 'stopped'" msg "stopped"))
    (let ((msg (call-cmd! 'lsp-stop)))
      (check-string-contains! "lsp-stop idempotent second call" msg "stopped"))
    (check-false! "lsp-running? after lsp-stop commands" (lsp-running?))

    ;; 10.3: lsp-restart echoes something visible (project root or restart message)
    (displayln "  Test: lsp-restart command echoes a visible message")
    (let ((msg (call-cmd! 'lsp-restart)))
      (check-true! "lsp-restart produces a non-empty echo message"
        (and (string? msg) (> (string-length msg) 0))))

    ;; 10.4: lsp-smart-goto-definition falls back and echoes something
    (displayln "  Test: lsp-smart-goto-definition fallback echoes something")
    (let ((msg (call-cmd! 'lsp-smart-goto-definition)))
      (check-true! "lsp-smart-goto-definition produces a non-empty echo message"
        (and (string? msg) (> (string-length msg) 0))))

    ;; 10.5: toggle-lsp and lsp alias execute and produce visible echo messages.
    ;; This is the regression test for the bug where cmd-toggle-lsp called
    ;; lsp-find-project-root/lsp-install-handlers! from the wrong module scope,
    ;; silently crashing before any echo-message! was reached.
    (displayln "  Test: toggle-lsp and lsp alias execute (not silent no-ops)")
    (let ((msg (call-cmd! 'toggle-lsp)))
      (check-true! "toggle-lsp produces a non-empty echo message"
        (and (string? msg) (> (string-length msg) 0))))
    (let ((msg (call-cmd! 'lsp)))
      (check-true! "'lsp' alias produces a non-empty echo message"
        (and (string? msg) (> (string-length msg) 0))))

    (buffer-list-remove! buf)))

;;;============================================================================
;;; Main
;;;============================================================================

(def (main . args)
  (displayln "LSP Functional Tests for gemacs (Qt exe)")
  (displayln "=========================================")
  (force-output (current-output-port))

  (with-qt-app _app
    ;; Register all commands (required for execute-command! dispatch)
    (qt-register-all-commands!)

    ;; Run test groups with per-group error isolation so one crashed group
    ;; does not prevent the remaining groups from running.
    (def (run-group! name thunk)
      (with-catch
        (lambda (e)
          (set! *failures* (+ *failures* 1))
          (displayln "  CRASH in " name ": " e)
          (force-output (current-output-port)))
        thunk))

    (run-group! "Group 1" run-group-1-command-registration)
    (run-group! "Group 9" run-group-9-workspace-edit)
    (run-group! "Group 10" run-group-10-resilience)

    ;; Report results
    (displayln "")
    (displayln "=========================================")
    (displayln "Results: "
               *passes* " passed, "
               *failures* " failed")
    (force-output (current-output-port))

    (when (> *failures* 0)
      (exit 1))))
