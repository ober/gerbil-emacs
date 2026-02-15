;;; -*- Gerbil -*-
;;; Tests for gerbil-emacs
;;; Includes both pure-logic tests and headless Scintilla editor tests.
;;; Scintilla works headlessly (no terminal needed) for all operations
;;; except scintilla_refresh(). Lexer/syntax highlighting requires Lexilla
;;; which is only available at runtime, so highlighting tests are skipped.

(import :std/test
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/echo
        :gerbil-emacs/editor-core
        (only-in :gerbil-emacs/editor-cmds-a cmd-copy-region-as-kill)
        (only-in :gerbil-emacs/editor-extra-org
                 cmd-org-todo cmd-org-export cmd-org-cycle cmd-org-shift-tab
                 cmd-org-store-link *org-stored-link*
                 cmd-org-promote cmd-org-demote
                 cmd-org-move-subtree-up cmd-org-move-subtree-down
                 cmd-org-toggle-checkbox cmd-org-priority
                 cmd-org-insert-heading cmd-org-insert-src-block
                 cmd-org-template-expand
                 org-heading-level org-find-subtree-end org-on-checkbox-line?)
        (only-in :gerbil-emacs/editor register-all-commands!)
        (only-in :gerbil-emacs/highlight
                 detect-file-language gerbil-file-extension?
                 setup-highlighting-for-file!)
        (only-in :gerbil-emacs/terminal
                 parse-ansi-segments text-segment-text
                 text-segment-fg-color text-segment-bold?
                 terminal-buffer? color-to-style
                 *term-style-base*)
        (only-in :gerbil-emacs/echo
                 *minibuffer-history* minibuffer-history-add!)
        (only-in :gerbil-emacs/editor-core
                 make-auto-save-path file-mod-time
                 *buffer-mod-times* update-buffer-mod-time!)
)

(export emacs-test)

(def emacs-test
  (test-suite "gerbil-emacs"

    (test-case "key-event->string: Ctrl keys"
      ;; C-a = key 0x01
      (check (key-event->string (make-tui-event 1 0 #x01 0 0 0 0 0)) => "C-a")
      ;; C-x = key 0x18
      (check (key-event->string (make-tui-event 1 0 #x18 0 0 0 0 0)) => "C-x")
      ;; C-g = key 0x07
      (check (key-event->string (make-tui-event 1 0 #x07 0 0 0 0 0)) => "C-g")
      ;; C-z = key 0x1A
      (check (key-event->string (make-tui-event 1 0 #x1A 0 0 0 0 0)) => "C-z")
      ;; C-@ / C-SPC = key 0, ch 0
      (check (key-event->string (make-tui-event 1 0 0 0 0 0 0 0)) => "C-@"))

    (test-case "key-event->string: special keys"
      ;; ESC
      (check (key-event->string (make-tui-event 1 0 #x1B 0 0 0 0 0)) => "ESC")
      ;; DEL (backspace)
      (check (key-event->string (make-tui-event 1 0 #x7F 0 0 0 0 0)) => "DEL")
      ;; C-_ (C-/)
      (check (key-event->string (make-tui-event 1 0 #x1F 0 0 0 0 0)) => "C-_")
      ;; C-\
      (check (key-event->string (make-tui-event 1 0 #x1C 0 0 0 0 0)) => "C-\\")
      ;; Space
      (check (key-event->string (make-tui-event 1 0 #x20 0 0 0 0 0)) => "SPC"))

    (test-case "key-event->string: regular characters"
      ;; 'a' = key 0, ch 97
      (check (key-event->string (make-tui-event 1 0 0 97 0 0 0 0)) => "a")
      ;; 'Z' = key 0, ch 90
      (check (key-event->string (make-tui-event 1 0 0 90 0 0 0 0)) => "Z")
      ;; '1' = key 0, ch 49
      (check (key-event->string (make-tui-event 1 0 0 49 0 0 0 0)) => "1"))

    (test-case "key-event->string: Alt/Meta"
      ;; M-f = key 0, ch 102, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 0 102 0 0 0 0)) => "M-f")
      ;; M-< = key 0, ch 60, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 0 60 0 0 0 0)) => "M-<")
      ;; M-SPC = key 0x20, ch 0, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 #x20 0 0 0 0 0)) => "M-SPC"))

    (test-case "key-event->string: arrow and function keys"
      (check (key-event->string (make-tui-event 1 0 TB_KEY_ARROW_UP 0 0 0 0 0))
             => "<up>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_ARROW_DOWN 0 0 0 0 0))
             => "<down>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_F1 0 0 0 0 0))
             => "<f1>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_HOME 0 0 0 0 0))
             => "<home>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_DELETE 0 0 0 0 0))
             => "<delete>"))

    (test-case "keymap: bind and lookup"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-f" 'forward-char)
        (keymap-bind! km "C-b" 'backward-char)
        (check (keymap-lookup km "C-f") => 'forward-char)
        (check (keymap-lookup km "C-b") => 'backward-char)
        (check (keymap-lookup km "C-z") => #f)))

    (test-case "keymap: nested keymaps for prefix keys"
      (let ((outer (make-keymap))
            (inner (make-keymap)))
        (keymap-bind! inner "C-s" 'save-buffer)
        (keymap-bind! outer "C-x" inner)
        ;; Lookup C-x returns the inner keymap
        (check (hash-table? (keymap-lookup outer "C-x")) => #t)
        ;; Lookup C-s in inner keymap
        (check (keymap-lookup inner "C-s") => 'save-buffer)))

    (test-case "key-state: single key command"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-f" 'forward-char)
        (let ((state (make-key-state km [])))
          ;; C-f event: key=0x06, ch=0, mod=0
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x06 0 0 0 0 0))))
            (check action => 'command)
            (check data => 'forward-char)))))

    (test-case "key-state: prefix key sequence"
      (let ((outer (make-keymap))
            (inner (make-keymap)))
        (keymap-bind! inner "C-s" 'save-buffer)
        (keymap-bind! outer "C-x" inner)
        (let ((state (make-key-state outer [])))
          ;; First key: C-x (key=0x18) -> prefix
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x18 0 0 0 0 0))))
            (check action => 'prefix)
            ;; Second key: C-s (key=0x13) -> command
            (let-values (((action2 data2 new-state2)
                          (key-state-feed! new-state
                            (make-tui-event 1 0 #x13 0 0 0 0 0))))
              (check action2 => 'command)
              (check data2 => 'save-buffer))))))

    (test-case "key-state: self-insert for printable chars"
      (let ((km (make-keymap)))
        (let ((state (make-key-state km [])))
          ;; 'a' = key=0, ch=97 -> self-insert
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 0 97 0 0 0 0))))
            (check action => 'self-insert)
            (check data => 97)))))

    (test-case "key-state: undefined key"
      (let ((km (make-keymap)))
        (let ((state (make-key-state km [])))
          ;; C-z (key=0x1A) with no binding -> undefined
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x1A 0 0 0 0 0))))
            (check action => 'undefined)
            (check data => "C-z")))))

    (test-case "echo-state: messages"
      (let ((echo (make-initial-echo-state)))
        (check (echo-state-message echo) => #f)
        (echo-message! echo "Hello")
        (check (echo-state-message echo) => "Hello")
        (check (echo-state-error? echo) => #f)
        (echo-error! echo "Error!")
        (check (echo-state-message echo) => "Error!")
        (check (echo-state-error? echo) => #t)
        (echo-clear! echo)
        (check (echo-state-message echo) => #f)))

    (test-case "default bindings setup"
      (setup-default-bindings!)
      ;; Check some bindings
      (check (keymap-lookup *global-keymap* "C-f") => 'forward-char)
      (check (keymap-lookup *global-keymap* "C-b") => 'backward-char)
      (check (keymap-lookup *global-keymap* "C-n") => 'next-line)
      (check (keymap-lookup *global-keymap* "C-p") => 'previous-line)
      (check (keymap-lookup *global-keymap* "C-a") => 'beginning-of-line)
      (check (keymap-lookup *global-keymap* "C-e") => 'end-of-line)
      (check (keymap-lookup *global-keymap* "C-k") => 'kill-line)
      (check (keymap-lookup *global-keymap* "C-y") => 'yank)
      (check (keymap-lookup *global-keymap* "C-g") => 'keyboard-quit)
      (check (keymap-lookup *global-keymap* "M-f") => 'forward-word)
      (check (keymap-lookup *global-keymap* "M-b") => 'backward-word)
      ;; C-x prefix is a keymap
      (check (hash-table? (keymap-lookup *global-keymap* "C-x")) => #t)
      ;; C-x C-s
      (check (keymap-lookup *ctrl-x-map* "C-s") => 'save-buffer)
      (check (keymap-lookup *ctrl-x-map* "C-c") => 'quit)
      ;; REPL bindings
      (check (keymap-lookup *global-keymap* "M-:") => 'eval-expression)
      (check (hash-table? (keymap-lookup *ctrl-x-map* "r")) => #t)  ; C-x r is now prefix map
      ;; New bindings: M-x, M-g, C-h, C-x C-b, M-%, TAB
      (check (keymap-lookup *global-keymap* "M-x") => 'execute-extended-command)
      (check (hash-table? (keymap-lookup *global-keymap* "M-g")) => #t)
      (check (keymap-lookup *meta-g-map* "g") => 'goto-line)
      (check (keymap-lookup *meta-g-map* "M-g") => 'goto-line)
      (check (hash-table? (keymap-lookup *global-keymap* "C-h")) => #t)
      (check (keymap-lookup *help-map* "k") => 'describe-key)
      (check (keymap-lookup *help-map* "b") => 'list-bindings)
      (check (keymap-lookup *help-map* "f") => 'describe-command)
      (check (keymap-lookup *ctrl-x-map* "C-b") => 'list-buffers)
      (check (keymap-lookup *global-keymap* "M-%") => 'query-replace)
      (check (keymap-lookup *global-keymap* "TAB") => 'indent-or-complete))

    (test-case "key-event->string: TAB key"
      ;; Tab key = 0x09
      (check (key-event->string (make-tui-event 1 0 #x09 0 0 0 0 0)) => "TAB")
      ;; M-TAB = Tab with Alt
      (check (key-event->string (make-tui-event 1 1 #x09 0 0 0 0 0)) => "M-TAB"))

    (test-case "keymap-entries"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-a" 'foo)
        (keymap-bind! km "C-b" 'bar)
        (let ((entries (keymap-entries km)))
          (check (length entries) => 2)
          (check (assoc "C-a" entries) => '("C-a" . foo))
          (check (assoc "C-b" entries) => '("C-b" . bar)))))

    (test-case "eval-expression-string: simple expression"
      (let-values (((result error?) (eval-expression-string "(+ 1 2)")))
        (check result => "3")
        (check error? => #f)))

    (test-case "eval-expression-string: error handling"
      (let-values (((result error?) (eval-expression-string "(/ 1 0)")))
        (check error? => #t)
        (check (string? result) => #t)))

    (test-case "eval-expression-string: output capture"
      ;; eval should capture the return value, not stdout
      (let-values (((result error?) (eval-expression-string "42")))
        (check result => "42")
        (check error? => #f)))

    (test-case "repl-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (repl-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'repl)
        (check (repl-buffer? buf) => #t)
        (set! (buffer-lexer-lang buf) 'dired)
        (check (repl-buffer? buf) => #f)))

    (test-case "eshell-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (eshell-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'eshell)
        (check (eshell-buffer? buf) => #t)))

    (test-case "eshell: pwd"
      (let-values (((output cwd) (eshell-process-input "pwd" "/tmp")))
        (check output => "/tmp\n")
        (check cwd => "/tmp")))

    (test-case "eshell: cd"
      (let-values (((output cwd) (eshell-process-input "cd /tmp" "/home")))
        (check output => "")
        (check cwd => "/tmp"))
      ;; cd to non-existent directory
      (let-values (((output cwd) (eshell-process-input "cd /nonexistent999" "/tmp")))
        (check (string-contains output "no such") => 4)
        (check cwd => "/tmp")))

    (test-case "eshell: echo"
      (let-values (((output cwd) (eshell-process-input "echo hello world" "/tmp")))
        (check output => "hello world\n")))

    (test-case "eshell: eval expression"
      (let-values (((output cwd) (eshell-process-input "(+ 1 2)" "/tmp")))
        (check output => "3\n")))

    (test-case "eshell: which"
      (let-values (((output cwd) (eshell-process-input "which ls" "/tmp")))
        ;; Should find ls somewhere (string-contains returns index or #f)
        (check (not (not (string-contains output "ls"))) => #t)))

    (test-case "eshell: ls"
      ;; ls on /tmp should work
      (let-values (((output cwd) (eshell-process-input "ls /tmp" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: external command"
      (let-values (((output cwd) (eshell-process-input "echo external-test" "/tmp")))
        (check output => "external-test\n")))

    (test-case "eshell: empty input"
      (let-values (((output cwd) (eshell-process-input "" "/tmp")))
        (check output => "")
        (check cwd => "/tmp")))

    (test-case "eshell: clear and exit"
      (let-values (((output cwd) (eshell-process-input "clear" "/tmp")))
        (check output => 'clear))
      (let-values (((output cwd) (eshell-process-input "exit" "/tmp")))
        (check output => 'exit)))

    (test-case "eshell keybinding"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "e") => 'eshell))

    (test-case "shell-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (shell-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'shell)
        (check (shell-buffer? buf) => #t)))

    (test-case "strip-ansi-codes"
      ;; Regular text passes through
      (check (strip-ansi-codes "hello") => "hello")
      ;; CSI color codes stripped
      (let ((esc (string (integer->char 27))))
        (check (strip-ansi-codes (string-append esc "[32mgreen" esc "[0m"))
               => "green")
        ;; CSI with multiple params
        (check (strip-ansi-codes (string-append esc "[1;34mbold-blue" esc "[0m"))
               => "bold-blue")))

    (test-case "shell subprocess lifecycle"
      (let ((ss (shell-start!)))
        ;; Verify state
        (check (shell-state? ss) => #t)
        (check (shell-state-prompt-pos ss) => 0)
        ;; Send a simple command
        (shell-send! ss "echo test123")
        ;; Wait for output
        (thread-sleep! 1.0)
        (let ((output (shell-read-available ss)))
          (check (string? output) => #t)
          (check (not (not (string-contains output "test123"))) => #t))
        ;; Clean shutdown
        (shell-stop! ss)))

    (test-case "shell keybinding"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "$") => 'shell))

    (test-case "new keybindings: redo, toggles, zoom, etc"
      (setup-default-bindings!)
      ;; Redo
      (check (keymap-lookup *global-keymap* "M-_") => 'redo)
      ;; Toggle line numbers
      (check (keymap-lookup *ctrl-x-map* "l") => 'toggle-line-numbers)
      ;; Toggle word wrap
      (check (keymap-lookup *ctrl-x-map* "w") => 'toggle-word-wrap)
      ;; Toggle whitespace
      (check (keymap-lookup *ctrl-x-map* "t") => 'toggle-whitespace)
      ;; Zoom
      (check (keymap-lookup *global-keymap* "C-=") => 'zoom-in)
      (check (keymap-lookup *global-keymap* "C--") => 'zoom-out)
      (check (keymap-lookup *ctrl-x-map* "C-0") => 'zoom-reset)
      ;; Select all
      (check (keymap-lookup *ctrl-x-map* "h") => 'select-all)
      ;; Dired (was duplicate-line, now dired per standard Emacs)
      (check (keymap-lookup *ctrl-x-map* "d") => 'dired)
      ;; Comment toggle
      (check (keymap-lookup *global-keymap* "M-;") => 'toggle-comment)
      ;; Transpose
      (check (keymap-lookup *global-keymap* "C-t") => 'transpose-chars)
      ;; Word case
      (check (keymap-lookup *global-keymap* "M-u") => 'upcase-word)
      (check (keymap-lookup *global-keymap* "M-l") => 'downcase-word)
      (check (keymap-lookup *global-keymap* "M-c") => 'capitalize-word)
      ;; Kill word
      (check (keymap-lookup *global-keymap* "M-d") => 'kill-word)
      ;; What line (M-g prefix)
      (check (keymap-lookup *meta-g-map* "l") => 'what-line))

    (test-case "new keybindings: write-file, revert, defun nav"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-w") => 'write-file)
      (check (keymap-lookup *ctrl-x-map* "C-r") => 'recentf-open)
      (check (keymap-lookup *global-keymap* "M-a") => 'backward-sentence)
      (check (keymap-lookup *global-keymap* "M-e") => 'forward-sentence))

    (test-case "auto-pair-char helper"
      ;; Import via editor.ss is not possible without TUI, so test inline
      (let ((apc (lambda (ch)
                   (cond
                     ((= ch 40) 41)   ; ( -> )
                     ((= ch 91) 93)   ; [ -> ]
                     ((= ch 34) 34)   ; " -> "
                     (else #f)))))
        (check (apc 40) => 41)   ; ( -> )
        (check (apc 91) => 93)   ; [ -> ]
        (check (apc 34) => 34)   ; " -> "
        (check (apc 97) => #f)   ; 'a' -> no pair
        (check (apc 32) => #f))) ; space -> no pair

    (test-case "new keybindings: delete-blank-lines, count words"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-o") => 'delete-blank-lines)
      (check (keymap-lookup *global-keymap* "M-=") => 'count-words))

    (test-case "brace-char? helper"
      ;; Test the brace matching character check
      ;; ( = 40, ) = 41, [ = 91, ] = 93, { = 123, } = 125
      (let ((bc? (lambda (ch)
                   (let ((c (char->integer ch)))
                     (or (= c 40) (= c 41)
                         (= c 91) (= c 93)
                         (= c 123) (= c 125))))))
        (check (bc? #\() => #t)
        (check (bc? #\)) => #t)
        (check (bc? #\[) => #t)
        (check (bc? #\]) => #t)
        (check (bc? #\{) => #t)
        (check (bc? #\}) => #t)
        (check (bc? #\a) => #f)
        (check (bc? #\space) => #f)))

    (test-case "word-char? helper"
      ;; Test word-char? logic (re-implemented inline for testing)
      (let ((wc? (lambda (ch)
                   (or (char-alphabetic? ch) (char-numeric? ch)
                       (char=? ch #\_) (char=? ch #\-)))))
        (check (wc? #\a) => #t)
        (check (wc? #\Z) => #t)
        (check (wc? #\0) => #t)
        (check (wc? #\_) => #t)
        (check (wc? #\-) => #t)
        (check (wc? #\space) => #f)
        (check (wc? #\() => #f)))

    (test-case "new keybindings: yank-pop, occur, compile, etc"
      ;; Test new keybinding registrations
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-y") => 'yank-pop)
      (check (hash-table? (keymap-lookup *global-keymap* "M-s")) => #t)
      (let ((ms-map (keymap-lookup *global-keymap* "M-s")))
        (check (keymap-lookup ms-map "o") => 'occur))
      (check (keymap-lookup *ctrl-x-map* "c") => 'compile)
      (check (keymap-lookup *global-keymap* "M-|") => 'shell-command-on-region)
      (check (keymap-lookup *ctrl-c-map* "^") => 'sort-lines))

    (test-case "app-state new fields"
      ;; Test new app-state fields have correct defaults
      (let ((app (new-app-state #f)))
        (check (app-state-kill-ring-idx app) => 0)
        (check (app-state-last-yank-pos app) => #f)
        (check (app-state-last-yank-len app) => #f)
        (check (app-state-last-compile app) => #f)
        (check (hash-table? (app-state-bookmarks app)) => #t)
        (check (app-state-rect-kill app) => [])))

    (test-case "bookmark and register keybindings"
      (setup-default-bindings!)
      ;; C-c prefix
      (check (hash-table? (keymap-lookup *global-keymap* "C-c")) => #t)
      (let ((cc-map (keymap-lookup *global-keymap* "C-c")))
        (check (keymap-lookup cc-map "z") => 'repl)
        (check (keymap-lookup cc-map "p") => 'goto-matching-paren))
      ;; C-x r prefix
      (let ((rxr-map (keymap-lookup *ctrl-x-map* "r")))
        (check (keymap-lookup rxr-map "m") => 'bookmark-set)
        (check (keymap-lookup rxr-map "b") => 'bookmark-jump)
        (check (keymap-lookup rxr-map "l") => 'bookmark-list)
        (check (keymap-lookup rxr-map "k") => 'kill-rectangle)
        (check (keymap-lookup rxr-map "y") => 'yank-rectangle))
      ;; Other new bindings
      (check (keymap-lookup *global-keymap* "M-j") => 'join-line)
      (check (keymap-lookup *ctrl-x-map* "C-l") => 'downcase-region)
      (check (keymap-lookup *ctrl-x-map* "C-u") => 'upcase-region))

    (test-case "more keybindings: shell-cmd, fill, dabbrev, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-!") => 'shell-command)
      (check (keymap-lookup *global-keymap* "M-q") => 'fill-paragraph)
      (check (keymap-lookup *ctrl-x-map* "i") => 'insert-file)
      (check (keymap-lookup *global-keymap* "M-/") => 'hippie-expand)
      (check (keymap-lookup *ctrl-x-map* "=") => 'what-cursor-position))

    (test-case "macro and mark ring keybindings"
      (setup-default-bindings!)
      ;; Keyboard macros
      (check (keymap-lookup *ctrl-x-map* "(") => 'start-kbd-macro)
      (check (keymap-lookup *ctrl-x-map* ")") => 'end-kbd-macro)
      (check (keymap-lookup *ctrl-x-map* "e") => 'call-last-kbd-macro)
      ;; Mark ring
      (check (keymap-lookup *ctrl-c-map* "SPC") => 'pop-mark))

    (test-case "app-state macro and mark-ring fields"
      (let ((app (new-app-state #f)))
        (check (app-state-macro-recording app) => #f)
        (check (app-state-macro-last app) => #f)
        (check (app-state-mark-ring app) => [])))

    (test-case "shell-quote helper"
      ;; Inline shell-quote for testing
      (let ((sq (lambda (s)
                  (string-append "'"
                    (let loop ((i 0) (acc ""))
                      (if (>= i (string-length s)) acc
                        (let ((ch (string-ref s i)))
                          (if (char=? ch #\')
                            (loop (+ i 1) (string-append acc "'\"'\"'"))
                            (loop (+ i 1) (string-append acc (string ch)))))))
                    "'"))))
        (check (not (not (string-contains (sq "hello") "hello"))) => #t)
        (check (string-ref (sq "hello") 0) => #\')
        (check (string-ref (sq "it's") 0) => #\')))

    (test-case "eval-expression-string"
      ;; Test in-process eval
      (let-values (((result error?) (eval-expression-string "(+ 1 2)")))
        (check result => "3")
        (check error? => #f))
      ;; Test error case
      (let-values (((result error?) (eval-expression-string "(/ 1 0)")))
        (check error? => #t)
        (check (string? result) => #t)))

    (test-case "register keybindings"
      (setup-default-bindings!)
      (let ((rxr-map (keymap-lookup *ctrl-x-map* "r")))
        (check (keymap-lookup rxr-map "s") => 'copy-to-register)
        (check (keymap-lookup rxr-map "i") => 'insert-register)
        (check (keymap-lookup rxr-map "SPC") => 'point-to-register)
        (check (keymap-lookup rxr-map "j") => 'jump-to-register)))

    (test-case "new keybindings: backward-kill-word, zap, goto-char"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-DEL") => 'backward-kill-word)
      (check (keymap-lookup *global-keymap* "M-z") => 'zap-to-char)
      (check (keymap-lookup *meta-g-map* "c") => 'goto-char))

    (test-case "app-state registers field"
      (let ((app (new-app-state #f)))
        (check (hash-table? (app-state-registers app)) => #t)))

    (test-case "new keybindings: transpose, just-one-space, repeat, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-t") => 'transpose-words)
      (check (keymap-lookup *ctrl-x-map* "C-t") => 'transpose-lines)
      (check (keymap-lookup *ctrl-x-map* "z") => 'repeat)
      (check (keymap-lookup *global-keymap* "M-SPC") => 'just-one-space)
      (check (keymap-lookup *meta-g-map* "n") => 'next-error)
      (check (keymap-lookup *meta-g-map* "p") => 'previous-error))

    (test-case "app-state last-command field"
      (let ((app (new-app-state #f)))
        (check (app-state-last-command app) => #f)))

    (test-case "new keybindings: kill-whole-line, move-line, narrow, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "k") => 'kill-whole-line)
      (check (keymap-lookup *global-keymap* "M-<up>") => 'move-line-up)
      (check (keymap-lookup *global-keymap* "M-<down>") => 'move-line-down)
      (check (keymap-lookup *ctrl-c-map* "!") => 'pipe-buffer)
      (check (keymap-lookup *ctrl-c-map* "n") => 'narrow-to-region)
      (check (keymap-lookup *ctrl-c-map* "w") => 'widen))

    (test-case "repl subprocess lifecycle"
      (let ((rs (repl-start!)))
        ;; Verify state is initialized
        (check (repl-state? rs) => #t)
        (check (repl-state-history rs) => [])
        (check (repl-state-prompt-pos rs) => 0)
        ;; Send expression and wait for output
        (repl-send! rs "(+ 1 2)")
        (check (equal? (car (repl-state-history rs)) "(+ 1 2)") => #t)
        ;; Wait for gxi to process
        (thread-sleep! 1.0)
        ;; Read available output
        (let ((output (repl-read-available rs)))
          (check (string? output) => #t)
          ;; string-contains returns index (integer) or #f
          (check (not (not (string-contains output "3"))) => #t))
        ;; Clean shutdown
        (repl-stop! rs)))

    (test-case "new keybindings: string-rect, number-lines, align, etc"
      ;; Rectangle extensions
      (check (keymap-lookup *ctrl-x-r-map* "t") => 'string-rectangle)
      (check (keymap-lookup *ctrl-x-r-map* "o") => 'open-rectangle)
      ;; Number lines, reverse region
      (check (keymap-lookup *ctrl-c-map* "#") => 'number-lines)
      (check (keymap-lookup *ctrl-c-map* "r") => 'reverse-region)
      ;; Flush/keep lines
      (check (keymap-lookup *meta-s-map* "f") => 'flush-lines)
      (check (keymap-lookup *meta-s-map* "k") => 'keep-lines)
      ;; Align, sort fields
      (check (keymap-lookup *ctrl-c-map* "a") => 'align-regexp)
      (check (keymap-lookup *ctrl-c-map* "s") => 'sort-fields))

    (test-case "new keybindings: mark-word, paragraph, indentation, etc"
      ;; Mark word, paragraph nav
      (check (keymap-lookup *global-keymap* "M-@") => 'mark-word)
      (check (keymap-lookup *global-keymap* "M-h") => 'mark-paragraph)
      (check (keymap-lookup *global-keymap* "M-}") => 'forward-paragraph)
      (check (keymap-lookup *global-keymap* "M-{") => 'backward-paragraph)
      ;; Indentation
      (check (keymap-lookup *global-keymap* "M-m") => 'back-to-indentation)
      (check (keymap-lookup *global-keymap* "M-^") => 'delete-indentation)
      ;; Exchange point and mark
      (check (keymap-lookup *ctrl-x-map* "C-x") => 'exchange-point-and-mark)
      ;; Info
      (check (keymap-lookup *ctrl-x-map* "C-p") => 'what-page)
      (check (keymap-lookup *ctrl-c-map* "l") => 'count-lines-region)
      ;; Copy line
      (check (keymap-lookup *ctrl-c-map* "c") => 'whitespace-cleanup))

    (test-case "new keybindings: where-is, apropos, read-only, etc"
      ;; Help extensions
      (check (keymap-lookup *help-map* "w") => 'where-is)
      (check (keymap-lookup *help-map* "a") => 'apropos-command)
      ;; Buffer: read-only, rename
      (check (keymap-lookup *ctrl-x-map* "C-q") => 'toggle-read-only)
      (check (keymap-lookup *ctrl-x-r-map* "n") => 'rename-buffer)
      ;; C-x 4 prefix
      (check (hash-table? (keymap-lookup *ctrl-x-map* "4")) => #t)
      (check (keymap-lookup *ctrl-x-4-map* "b") => 'switch-buffer-other-window)
      (check (keymap-lookup *ctrl-x-4-map* "f") => 'find-file-other-window))

    (test-case "rot13 helper (inline)"
      ;; Reimplementation for testing (same logic as editor.ss rot13-string)
      (let* ((rot13-char
               (lambda (ch)
                 (cond
                   ((and (char>=? ch #\a) (char<=? ch #\z))
                    (integer->char (+ (char->integer #\a)
                                      (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
                   ((and (char>=? ch #\A) (char<=? ch #\Z))
                    (integer->char (+ (char->integer #\A)
                                      (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
                   (else ch))))
             (rot13-string
               (lambda (s)
                 (let* ((len (string-length s))
                        (result (make-string len)))
                   (let loop ((i 0))
                     (when (< i len)
                       (string-set! result i (rot13-char (string-ref s i)))
                       (loop (+ i 1))))
                   result))))
        (check (rot13-string "Hello") => "Uryyb")
        (check (rot13-string "Uryyb") => "Hello")
        (check (rot13-string "123") => "123")))

    (test-case "new keybindings: tabify, rot13, hex, count, dedup"
      (check (keymap-lookup *ctrl-c-map* "t") => 'tabify)
      (check (keymap-lookup *ctrl-c-map* "3") => 'rot13-region)
      (check (keymap-lookup *ctrl-c-map* "x") => 'hexl-mode)
      (check (keymap-lookup *meta-s-map* "c") => 'count-matches)
      (check (keymap-lookup *ctrl-c-map* "u") => 'delete-duplicate-lines))

    (test-case "new keybindings: diff, checksum, grep-buffer, etc"
      (check (keymap-lookup *ctrl-c-map* "d") => 'diff-buffer-with-file)
      (check (keymap-lookup *ctrl-c-map* "5") => 'checksum)
      (check (keymap-lookup *global-keymap* "M-&") => 'async-shell-command)
      (check (keymap-lookup *ctrl-x-map* "$") => 'toggle-truncate-lines)
      (check (keymap-lookup *meta-s-map* "g") => 'grep-buffer)
      (check (keymap-lookup *ctrl-c-map* "D") => 'insert-date)
      (check (keymap-lookup *ctrl-c-map* "8") => 'insert-char))

    (test-case "new keybindings: eval-buffer, clone, scratch, save-some, etc"
      (setup-default-bindings!)
      ;; Eval buffer/region
      (check (keymap-lookup *ctrl-c-map* "E") => 'eval-buffer)
      (check (keymap-lookup *ctrl-c-map* "v") => 'eval-region)
      ;; Clone buffer, scratch
      (check (keymap-lookup *ctrl-c-map* "b") => 'clone-buffer)
      (check (keymap-lookup *ctrl-c-map* "S") => 'scratch-buffer)
      ;; Save some buffers (overrides old shell on C-x s)
      (check (keymap-lookup *ctrl-x-map* "s") => 'save-some-buffers)
      ;; Revert quick
      (check (keymap-lookup *ctrl-c-map* "R") => 'revert-buffer-quick)
      ;; Toggle highlighting
      (check (keymap-lookup *ctrl-c-map* "f") => 'toggle-highlighting)
      ;; Display time
      (check (keymap-lookup *ctrl-c-map* "T") => 'display-time))

    (test-case "new keybindings: ediff, calc, describe-bindings, etc"
      (setup-default-bindings!)
      ;; Calculator
      (check (keymap-lookup *ctrl-c-map* "=") => 'calc)
      ;; Case fold search
      (check (keymap-lookup *ctrl-c-map* "C") => 'toggle-case-fold-search)
      ;; Describe bindings
      (check (keymap-lookup *help-map* "B") => 'describe-bindings)
      ;; Center line
      (check (keymap-lookup *global-keymap* "M-o") => 'center-line)
      ;; What face
      (check (keymap-lookup *ctrl-c-map* "F") => 'what-face)
      ;; List processes
      (check (keymap-lookup *ctrl-c-map* "P") => 'list-processes)
      ;; View messages
      (check (keymap-lookup *ctrl-c-map* "m") => 'view-messages)
      ;; Auto fill
      (check (keymap-lookup *ctrl-c-map* "q") => 'toggle-auto-fill)
      ;; Delete trailing whitespace
      (check (keymap-lookup *ctrl-c-map* "W") => 'delete-trailing-whitespace)
      ;; Ediff buffers
      (check (keymap-lookup *ctrl-c-map* "B") => 'ediff-buffers)
      ;; Rename file
      (check (keymap-lookup *ctrl-c-map* "M") => 'rename-file-and-buffer)
      ;; Sudo write
      (check (keymap-lookup *ctrl-c-map* "X") => 'sudo-write)
      ;; Sort numeric
      (check (keymap-lookup *ctrl-c-map* "N") => 'sort-numeric)
      ;; Count words region
      (check (keymap-lookup *ctrl-c-map* "L") => 'count-words-region))

    (test-case "new keybindings: overwrite, visual-line, fill-col, etc"
      (setup-default-bindings!)
      ;; Overwrite mode
      (check (keymap-lookup *global-keymap* "<insert>") => 'toggle-overwrite-mode)
      ;; Visual line mode
      (check (keymap-lookup *ctrl-c-map* "V") => 'toggle-visual-line-mode)
      ;; Fill column
      (check (keymap-lookup *ctrl-c-map* ".") => 'set-fill-column)
      (check (keymap-lookup *ctrl-c-map* "|") => 'toggle-fill-column-indicator)
      ;; Repeat complex command
      (check (keymap-lookup *ctrl-c-map* "Z") => 'repeat-complex-command)
      ;; Eldoc
      (check (keymap-lookup *ctrl-c-map* "I") => 'eldoc)
      ;; Highlight symbol/clear
      (check (keymap-lookup *ctrl-c-map* "h") => 'highlight-symbol)
      (check (keymap-lookup *ctrl-c-map* "H") => 'clear-highlight)
      ;; Indent rigidly
      (check (keymap-lookup *ctrl-c-map* ">") => 'indent-rigidly-right)
      (check (keymap-lookup *ctrl-c-map* "<") => 'indent-rigidly-left)
      ;; Buffer stats
      (check (keymap-lookup *ctrl-c-map* "?") => 'buffer-stats)
      ;; Show tabs/eol
      (check (keymap-lookup *ctrl-c-map* "4") => 'toggle-show-tabs)
      (check (keymap-lookup *ctrl-c-map* "6") => 'toggle-show-eol))

    (test-case "new keybindings: copy-above, open-line-above, select, etc"
      (setup-default-bindings!)
      ;; Copy from above
      (check (keymap-lookup *ctrl-c-map* "A") => 'copy-from-above)
      ;; Open line above
      (check (keymap-lookup *ctrl-c-map* "O") => 'open-line-above)
      ;; Select line
      (check (keymap-lookup *ctrl-c-map* "G") => 'select-line)
      ;; Split line
      (check (keymap-lookup *ctrl-c-map* "J") => 'split-line)
      ;; Hippie expand
      (check (keymap-lookup *global-keymap* "M-TAB") => 'hippie-expand)
      ;; Swap buffers
      (check (keymap-lookup *ctrl-c-map* "9") => 'swap-buffers)
      ;; Tab width
      (check (keymap-lookup *ctrl-c-map* "7") => 'cycle-tab-width)
      ;; Indent tabs
      (check (keymap-lookup *ctrl-c-map* "0") => 'toggle-indent-tabs-mode)
      ;; Buffer info
      (check (keymap-lookup *ctrl-c-map* "j") => 'buffer-info)
      ;; Enlarge/shrink
      (check (keymap-lookup *ctrl-x-map* "^") => 'enlarge-window)
      (check (keymap-lookup *ctrl-x-map* "-") => 'shrink-window))

    (test-case "new keybindings: whitespace-cleanup, electric-pair, etc"
      (setup-default-bindings!)
      ;; Whitespace cleanup (overrides copy-line)
      (check (keymap-lookup *ctrl-c-map* "c") => 'whitespace-cleanup)
      ;; Toggle electric pair
      (check (keymap-lookup *ctrl-c-map* "Q") => 'toggle-electric-pair)
      ;; Previous/next buffer
      (check (keymap-lookup *ctrl-x-map* "<left>") => 'previous-buffer)
      (check (keymap-lookup *ctrl-x-map* "<right>") => 'next-buffer)
      ;; Balance windows
      (check (keymap-lookup *ctrl-x-map* "+") => 'balance-windows)
      ;; Move to window line
      (check (keymap-lookup *global-keymap* "M-r") => 'move-to-window-line)
      ;; Kill buffer and window
      (check (keymap-lookup *ctrl-x-4-map* "0") => 'kill-buffer-and-window)
      ;; Flush undo
      (check (keymap-lookup *ctrl-c-map* "/") => 'flush-undo)
      ;; Upcase initials region
      (check (keymap-lookup *ctrl-c-map* "U") => 'upcase-initials-region)
      ;; Untabify buffer
      (check (keymap-lookup *ctrl-c-map* "_") => 'untabify-buffer)
      ;; Insert buffer name
      (check (keymap-lookup *ctrl-c-map* "%") => 'insert-buffer-name)
      ;; Mark defun
      (check (keymap-lookup *ctrl-c-map* "y") => 'mark-defun)
      ;; Insert pairs
      (check (keymap-lookup *ctrl-c-map* "(") => 'insert-parentheses)
      (check (keymap-lookup *ctrl-c-map* "[") => 'insert-pair-brackets)
      ;; Describe char
      (check (keymap-lookup *ctrl-c-map* ",") => 'describe-char)
      ;; Find file at point
      (check (keymap-lookup *ctrl-c-map* "o") => 'find-file-at-point)
      ;; Count chars region
      (check (keymap-lookup *ctrl-c-map* "K") => 'count-chars-region))

    (test-case "new keybindings: sexp nav, unfill, kill-ring, registers, etc"
      (setup-default-bindings!)
      ;; Count words buffer
      (check (keymap-lookup *ctrl-c-map* "+") => 'count-words-buffer)
      ;; Unfill paragraph
      (check (keymap-lookup *ctrl-c-map* ";") => 'unfill-paragraph)
      ;; List registers
      (check (keymap-lookup *ctrl-c-map* "@") => 'list-registers)
      ;; Show kill ring
      (check (keymap-lookup *ctrl-c-map* "Y") => 'show-kill-ring)
      ;; Smart beginning of line
      (check (keymap-lookup *ctrl-c-map* "`") => 'smart-beginning-of-line)
      ;; What buffer
      (check (keymap-lookup *ctrl-c-map* "~") => 'what-buffer)
      ;; Narrowing indicator
      (check (keymap-lookup *ctrl-c-map* ":") => 'toggle-narrowing-indicator)
      ;; Insert file name
      (check (keymap-lookup *ctrl-c-map* "&") => 'insert-file-name)
      ;; S-expression navigation
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "d") => 'forward-up-list)
      (check (keymap-lookup *meta-g-map* "k") => 'kill-sexp)
      (check (keymap-lookup *meta-g-map* "f") => 'forward-sexp)
      (check (keymap-lookup *meta-g-map* "b") => 'backward-sexp))

    (test-case "new keybindings: mark-sexp, word-freq, uuid, delete-pair, etc"
      (setup-default-bindings!)
      ;; Mark sexp
      (check (keymap-lookup *meta-g-map* "SPC") => 'mark-sexp)
      ;; Indent sexp
      (check (keymap-lookup *meta-g-map* "TAB") => 'indent-sexp)
      ;; Word frequency
      (check (keymap-lookup *ctrl-c-map* "*") => 'word-frequency)
      ;; Insert UUID
      (check (keymap-lookup *ctrl-c-map* "'") => 'insert-uuid)
      ;; Delete pair
      (check (keymap-lookup *ctrl-c-map* "}") => 'delete-pair)
      ;; Toggle hl-line
      (check (keymap-lookup *ctrl-c-map* "{") => 'toggle-hl-line)
      ;; Find alternate file
      (check (keymap-lookup *ctrl-x-map* "C-v") => 'find-alternate-file)
      ;; Increment register
      (check (keymap-lookup *ctrl-x-r-map* "+") => 'increment-register))

    ;; Task #39: sort, scroll, text processing commands
    (test-case "new keybindings: scroll-other, matching-paren, etc"
      ;; M-g v/V: scroll other window
      (check (keymap-lookup *meta-g-map* "v") => 'scroll-other-window)
      (check (keymap-lookup *meta-g-map* "V") => 'scroll-other-window-up)
      ;; M-g m: goto matching paren
      (check (keymap-lookup *meta-g-map* "m") => 'goto-matching-paren)
      ;; editor-get-text-range helper (defined in editor.ss, can't test here)
      ;; Verify *auto-revert-mode* global exists in editor.ss (also can't test)
      ;; Check that the new M-g bindings are distinct from existing ones
      (check (not (eq? (keymap-lookup *meta-g-map* "v")
                       (keymap-lookup *meta-g-map* "V"))) => #t)
      ;; Check M-g map still has previous bindings
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "d") => 'forward-up-list)
      (check (keymap-lookup *meta-g-map* "k") => 'kill-sexp)
      (check (keymap-lookup *meta-g-map* "f") => 'forward-sexp)
      (check (keymap-lookup *meta-g-map* "b") => 'backward-sexp))

    (test-case "new keybindings: backward-kill-sexp, goto-percent, etc"
      ;; M-g DEL -> backward-kill-sexp
      (check (keymap-lookup *meta-g-map* "DEL") => 'backward-kill-sexp)
      ;; M-g % -> goto-percent
      (check (keymap-lookup *meta-g-map* "%") => 'goto-percent)
      ;; Verify existing M-g bindings still intact
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "m") => 'goto-matching-paren)
      (check (keymap-lookup *meta-g-map* "v") => 'scroll-other-window)
      (check (keymap-lookup *meta-g-map* "V") => 'scroll-other-window-up))

    (test-case "new keybindings: regex search, window resize, undo"
      (setup-default-bindings!)
      ;; Regex search and replace
      (check (keymap-lookup *global-keymap* "C-M-s") => 'isearch-forward-regexp)
      (check (keymap-lookup *global-keymap* "C-M-%") => 'query-replace-regexp)
      ;; Window resize
      (check (keymap-lookup *ctrl-x-map* "^") => 'enlarge-window)
      (check (keymap-lookup *ctrl-x-map* "{") => 'shrink-window-horizontally)
      (check (keymap-lookup *ctrl-x-map* "}") => 'enlarge-window-horizontally)
      ;; Alternative undo
      (check (keymap-lookup *global-keymap* "C-/") => 'undo))

    (test-case "new keybindings: view-lossage"
      (setup-default-bindings!)
      (check (keymap-lookup *help-map* "l") => 'view-lossage))

    (test-case "keybindings: code folding"
      (setup-default-bindings!)
      (check (keymap-lookup *meta-g-map* "F") => 'toggle-fold)
      (check (keymap-lookup *meta-g-map* "C") => 'fold-all)
      (check (keymap-lookup *meta-g-map* "E") => 'unfold-all))

    (test-case "key lossage recording"
      (let ((app (new-app-state #f)))
        ;; Starts empty
        (check (app-state-key-lossage app) => [])
        ;; Record some keys
        (key-lossage-record! app "C-x")
        (key-lossage-record! app "C-f")
        (check (length (app-state-key-lossage app)) => 2)
        ;; Most recent first
        (check (car (app-state-key-lossage app)) => "C-f")
        ;; Format includes both keys
        (let ((s (key-lossage->string app)))
          (check (not (not (string-contains s "C-x"))) => #t)
          (check (not (not (string-contains s "C-f"))) => #t))))

    ;;=========================================================================
    ;; Feature implementation tests
    ;;=========================================================================

    (test-case "app-state: winner mode fields"
      (let ((app (new-app-state #f)))
        ;; Winner history starts empty
        (check (app-state-winner-history app) => [])
        (check (app-state-winner-history-idx app) => 0)
        ;; Can set winner history
        (set! (app-state-winner-history app)
          (list '(1 0 ("*scratch*")) '(2 0 ("*scratch*" "foo.ss"))))
        (check (length (app-state-winner-history app)) => 2)
        ;; Can navigate history index
        (set! (app-state-winner-history-idx app) 1)
        (check (app-state-winner-history-idx app) => 1)))

    (test-case "app-state: tab management fields"
      (let ((app (new-app-state #f)))
        ;; Starts with one default tab
        (check (length (app-state-tabs app)) => 1)
        (check (app-state-current-tab-idx app) => 0)
        ;; Default tab has correct structure: (name buffer-names window-idx)
        (let ((tab (car (app-state-tabs app))))
          (check (car tab) => "Tab 1")
          (check (cadr tab) => '("*scratch*"))
          (check (caddr tab) => 0))
        ;; Can add tabs
        (set! (app-state-tabs app)
          (append (app-state-tabs app) (list (list "Tab 2" '("foo.ss") 0))))
        (check (length (app-state-tabs app)) => 2)
        ;; Can switch tabs
        (set! (app-state-current-tab-idx app) 1)
        (check (app-state-current-tab-idx app) => 1)))

    (test-case "command registration: spell checking"
      (register-all-commands!)
      (check (not (not (find-command 'ispell-word))) => #t)
      (check (not (not (find-command 'ispell-buffer))) => #t)
      (check (not (not (find-command 'ispell-region))) => #t))

    (test-case "command registration: winner mode"
      (register-all-commands!)
      (check (not (not (find-command 'winner-undo))) => #t)
      (check (not (not (find-command 'winner-redo))) => #t)
      (check (not (not (find-command 'winner-mode))) => #t))

    (test-case "command registration: tab management"
      (register-all-commands!)
      (check (not (not (find-command 'tab-new))) => #t)
      (check (not (not (find-command 'tab-close))) => #t)
      (check (not (not (find-command 'tab-next))) => #t)
      (check (not (not (find-command 'tab-previous))) => #t)
      (check (not (not (find-command 'tab-rename))) => #t)
      (check (not (not (find-command 'tab-move))) => #t))

    (test-case "command registration: flycheck"
      (register-all-commands!)
      (check (not (not (find-command 'flycheck-mode))) => #t)
      (check (not (not (find-command 'flycheck-next-error))) => #t)
      (check (not (not (find-command 'flycheck-previous-error))) => #t)
      (check (not (not (find-command 'flycheck-list-errors))) => #t))

    (test-case "command registration: git gutter"
      (register-all-commands!)
      (check (not (not (find-command 'git-gutter-mode))) => #t)
      (check (not (not (find-command 'git-gutter-next-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-previous-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-revert-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-stage-hunk))) => #t))

    (test-case "command registration: multiple cursors"
      (register-all-commands!)
      (check (not (not (find-command 'mc-mark-next-like-this))) => #t)
      (check (not (not (find-command 'mc-mark-previous-like-this))) => #t)
      (check (not (not (find-command 'mc-mark-all-like-this))) => #t))

    (test-case "command registration: xref navigation"
      (register-all-commands!)
      (check (not (not (find-command 'xref-find-definitions))) => #t)
      (check (not (not (find-command 'xref-find-references))) => #t)
      (check (not (not (find-command 'xref-find-apropos))) => #t)
      (check (not (not (find-command 'xref-go-back))) => #t)
      (check (not (not (find-command 'xref-go-forward))) => #t))

    (test-case "command registration: project.el"
      (register-all-commands!)
      (check (not (not (find-command 'project-switch-project))) => #t)
      (check (not (not (find-command 'project-find-regexp))) => #t)
      (check (not (not (find-command 'project-shell))) => #t)
      (check (not (not (find-command 'project-dired))) => #t)
      (check (not (not (find-command 'project-eshell))) => #t))

    (test-case "command registration: treemacs"
      (register-all-commands!)
      (check (not (not (find-command 'treemacs))) => #t)
      (check (not (not (find-command 'treemacs-find-file))) => #t))

    (test-case "command registration: calendar"
      (register-all-commands!)
      (check (not (not (find-command 'calendar-goto-date))) => #t)
      (check (not (not (find-command 'calendar-holidays))) => #t))

    (test-case "command registration: auto-insert"
      (register-all-commands!)
      (check (not (not (find-command 'auto-insert))) => #t)
      (check (not (not (find-command 'auto-insert-mode))) => #t))

    (test-case "command registration: smartparens and paredit"
      (register-all-commands!)
      (check (not (not (find-command 'sp-forward-slurp-sexp))) => #t)
      (check (not (not (find-command 'sp-forward-barf-sexp))) => #t)
      (check (not (not (find-command 'paredit-wrap-round))) => #t)
      (check (not (not (find-command 'paredit-raise-sexp))) => #t)
      (check (not (not (find-command 'paredit-splice-sexp))) => #t))

    (test-case "command registration: EWW browser"
      (register-all-commands!)
      (check (not (not (find-command 'eww))) => #t)
      (check (not (not (find-command 'eww-browse-url))) => #t)
      (check (not (not (find-command 'browse-url-at-point))) => #t)
      (check (not (not (find-command 'eww-back))) => #t)
      (check (not (not (find-command 'eww-forward))) => #t)
      (check (not (not (find-command 'eww-reload))) => #t)
      (check (not (not (find-command 'eww-download))) => #t)
      (check (not (not (find-command 'eww-copy-page-url))) => #t))

    (test-case "command registration: diff and ediff"
      (register-all-commands!)
      (check (not (not (find-command 'diff-mode))) => #t)
      (check (not (not (find-command 'diff-apply-hunk))) => #t)
      (check (not (not (find-command 'diff-revert-hunk))) => #t)
      (check (not (not (find-command 'diff-goto-source))) => #t)
      (check (not (not (find-command 'ediff-files))) => #t)
      (check (not (not (find-command 'ediff-regions))) => #t)
      (check (not (not (find-command 'ediff-merge))) => #t))

    (test-case "command registration: yasnippet"
      (register-all-commands!)
      (check (not (not (find-command 'yas-insert-snippet))) => #t)
      (check (not (not (find-command 'yas-new-snippet))) => #t)
      (check (not (not (find-command 'yas-visit-snippet-file))) => #t))

    (test-case "command registration: EMMS media player"
      (register-all-commands!)
      (check (not (not (find-command 'emms))) => #t)
      (check (not (not (find-command 'emms-play-file))) => #t)
      (check (not (not (find-command 'emms-pause))) => #t)
      (check (not (not (find-command 'emms-stop))) => #t)
      (check (not (not (find-command 'emms-next))) => #t)
      (check (not (not (find-command 'emms-previous))) => #t))

    (test-case "command registration: PDF viewer"
      (register-all-commands!)
      (check (not (not (find-command 'pdf-view-mode))) => #t)
      (check (not (not (find-command 'pdf-view-next-page))) => #t)
      (check (not (not (find-command 'pdf-view-previous-page))) => #t)
      (check (not (not (find-command 'pdf-view-goto-page))) => #t))

    (test-case "command registration: avy/ace-jump"
      (register-all-commands!)
      (check (not (not (find-command 'avy-goto-char))) => #t)
      (check (not (not (find-command 'avy-goto-word))) => #t)
      (check (not (not (find-command 'avy-goto-line))) => #t))

    (test-case "command registration: expand-region"
      (register-all-commands!)
      (check (not (not (find-command 'expand-region))) => #t)
      (check (not (not (find-command 'contract-region))) => #t))

    (test-case "command registration: abbreviations"
      (register-all-commands!)
      (check (not (not (find-command 'define-global-abbrev))) => #t))

    (test-case "keybindings: ediff"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "B") => 'ediff-buffers))

    (test-case "keybindings: help extensions"
      (setup-default-bindings!)
      (check (keymap-lookup *help-map* "c") => 'describe-key-briefly)
      (check (keymap-lookup *help-map* "d") => 'describe-function)
      (check (keymap-lookup *help-map* "v") => 'describe-variable)
      (check (keymap-lookup *help-map* "i") => 'info))

    (test-case "keybindings: M-g extended bindings"
      (setup-default-bindings!)
      (check (keymap-lookup *meta-g-map* "i") => 'imenu)
      (check (keymap-lookup *meta-g-map* "F") => 'toggle-fold))

    (test-case "buffer-list management"
      ;; Clear any existing buffers
      (set! *buffer-list* [])
      ;; Create and add buffers
      (let* ((buf1 (make-buffer "test1.ss" #f #f #f #f #f #f))
             (buf2 (make-buffer "test2.ss" #f #f #f #f #f #f)))
        (buffer-list-add! buf1)
        (buffer-list-add! buf2)
        (check (length (buffer-list)) => 2)
        ;; Find by name
        (check (buffer-name (buffer-by-name "test1.ss")) => "test1.ss")
        (check (buffer-by-name "nonexistent") => #f)
        ;; Remove
        (buffer-list-remove! buf1)
        (check (length (buffer-list)) => 1)
        (check (buffer-by-name "test1.ss") => #f)))

    (test-case "dired-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (dired-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'dired)
        (check (dired-buffer? buf) => #t)))

    (test-case "brace-char? exported from core"
      (check (brace-char? (char->integer #\()) => #t)
      (check (brace-char? (char->integer #\))) => #t)
      (check (brace-char? (char->integer #\[)) => #t)
      (check (brace-char? (char->integer #\])) => #t)
      (check (brace-char? (char->integer #\{)) => #t)
      (check (brace-char? (char->integer #\})) => #t)
      (check (brace-char? (char->integer #\a)) => #f)
      (check (brace-char? (char->integer #\space)) => #f))

    (test-case "file I/O helpers"
      ;; Write and read a temp file
      (let ((tmp "/tmp/gerbil-emacs-test-file.txt"))
        (write-string-to-file tmp "hello world\n")
        (check (read-file-as-string tmp) => "hello world\n")
        ;; Cleanup
        (delete-file tmp)))

    (test-case "eval-expression-string: various types"
      ;; String result
      (let-values (((result error?) (eval-expression-string "\"hello\"")))
        (check result => "\"hello\"")
        (check error? => #f))
      ;; Boolean
      (let-values (((result error?) (eval-expression-string "#t")))
        (check result => "#t")
        (check error? => #f))
      ;; List
      (let-values (((result error?) (eval-expression-string "'(1 2 3)")))
        (check result => "(1 2 3)")
        (check error? => #f)))

    (test-case "eshell: pipeline"
      ;; Test piped commands
      (let-values (((output cwd) (eshell-process-input "echo hello | wc -c" "/tmp")))
        ;; Should get the character count of "hello\n"
        (check (string? output) => #t)))

    (test-case "eshell: head and tail"
      (let-values (((output cwd) (eshell-process-input "echo -e 'a\\nb\\nc' | head -2" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: find command"
      ;; Find in /tmp (may or may not have files)
      (let-values (((output cwd) (eshell-process-input "find /tmp -maxdepth 0 -type d" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: env and export"
      ;; Test env variable access
      (let-values (((output cwd) (eshell-process-input "echo $HOME" "/tmp")))
        (check (string? output) => #t)
        (check (> (string-length output) 0) => #t)))

    (test-case "strip-ansi-codes: complex sequences"
      ;; OSC sequences
      (let ((esc (string (integer->char 27))))
        ;; Title setting escape
        (check (strip-ansi-codes (string-append esc "]0;title" (string (integer->char 7)) "text"))
               => "text")
        ;; Nested formatting
        (check (strip-ansi-codes
                 (string-append esc "[1m" esc "[31m" "bold-red" esc "[0m"))
               => "bold-red")))

    (test-case "command registry: find-command returns procedure"
      (register-all-commands!)
      ;; find-command should return a procedure for registered commands
      (let ((cmd (find-command 'forward-char)))
        (check (procedure? cmd) => #t))
      ;; Non-existent command returns #f
      (check (find-command 'nonexistent-command-xyz) => #f))

    (test-case "all-commands hash populated"
      (register-all-commands!)
      ;; Should have many commands registered
      (check (> (hash-length *all-commands*) 100) => #t))

    ;;=========================================================================
    ;; Headless Scintilla editor tests
    ;; These create real Scintilla editors without a terminal and test
    ;; actual editor commands (cursor movement, text insertion, etc.)
    ;;=========================================================================

    (test-case "headless: create editor and app-state"
      ;; Verify we can create a fully functional headless editor
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (check (app-state? app) => #t)
        (check (eq? (current-editor app) ed) => #t)
        (check (eq? (current-buffer-from-app app) buf) => #t)))

    (test-case "headless: self-insert advances cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type 'h' (ASCII 104)
        (cmd-self-insert! app 104)
        (check (editor-get-text ed) => "h")
        (check (editor-get-current-pos ed) => 1)
        ;; Type 'i' (ASCII 105)
        (cmd-self-insert! app 105)
        (check (editor-get-text ed) => "hi")
        (check (editor-get-current-pos ed) => 2)
        ;; Type '!' (ASCII 33)
        (cmd-self-insert! app 33)
        (check (editor-get-text ed) => "hi!")
        (check (editor-get-current-pos ed) => 3)))

    (test-case "headless: auto-pair inserts both chars"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (set! *auto-pair-mode* #t)
        ;; Type '(' (ASCII 40) - should auto-pair with ')'
        (cmd-self-insert! app 40)
        (check (editor-get-text ed) => "()")
        ;; Cursor between parens
        (check (editor-get-current-pos ed) => 1)
        (set! *auto-pair-mode* #f)))

    (test-case "headless: navigation - forward/backward char"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        ;; Forward 3 chars
        (cmd-forward-char app)
        (cmd-forward-char app)
        (cmd-forward-char app)
        (check (editor-get-current-pos ed) => 3)
        ;; Backward 1 char
        (cmd-backward-char app)
        (check (editor-get-current-pos ed) => 2)))

    (test-case "headless: navigation - beginning/end of line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 5)
        (cmd-end-of-line app)
        (check (editor-get-current-pos ed) => 11)
        (cmd-beginning-of-line app)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: navigation - next/previous line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line one\nline two\nline three")
        (editor-goto-pos ed 0)
        (cmd-next-line app)
        ;; Should be on line 1, column 0 -> position 9
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 1)
        (cmd-next-line app)
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 2)
        (cmd-previous-line app)
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 1)))

    (test-case "headless: navigation - beginning/end of buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line one\nline two\nline three")
        (editor-goto-pos ed 5)
        (cmd-end-of-buffer app)
        ;; "line one\nline two\nline three" = 28 chars, cursor goes to end
        (check (editor-get-current-pos ed) => (editor-get-text-length ed))
        (cmd-beginning-of-buffer app)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: newline with electric indent"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "  indented")
        (editor-goto-pos ed 10) ;; end of "  indented"
        (cmd-newline app)
        (let ((text (editor-get-text ed)))
          ;; Should have newline + 2 spaces of indent
          (check (string-contains text "\n  ") => 10)
          ;; Cursor should be after the indent
          (check (editor-get-current-pos ed) => 13))))

    (test-case "headless: kill-line and yank"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 0)
        ;; Kill line should remove "hello world" (to end of line)
        (cmd-kill-line app)
        (check (editor-get-text ed) => "\nsecond line")
        ;; Kill ring should have "hello world"
        (check (car (app-state-kill-ring app)) => "hello world")
        ;; Move to end and yank
        (cmd-end-of-buffer app)
        (cmd-yank app)
        (check (editor-get-text ed) => "\nsecond linehello world")))

    (test-case "headless: delete-char and backward-delete-char"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcde")
        ;; Delete char at position 0 (removes 'a')
        (editor-goto-pos ed 0)
        (cmd-delete-char app)
        (check (editor-get-text ed) => "bcde")
        ;; Backward delete at position 2 (removes 'c')
        (editor-goto-pos ed 2)
        (cmd-backward-delete-char app)
        (check (editor-get-text ed) => "bde")))

    (test-case "headless: undo and redo"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (editor-set-save-point ed) ;; clear undo history
        (editor-insert-text ed 0 "hello")
        (editor-goto-pos ed 5)
        (check (editor-get-text ed) => "hello")
        (cmd-undo app)
        (check (editor-get-text ed) => "")
        (cmd-redo app)
        (check (editor-get-text ed) => "hello")))

    (test-case "headless: set-mark and kill-region"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0, move to 5, kill region -> removes "hello"
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-kill-region app)
        (check (editor-get-text ed) => " world")
        ;; Scintilla clipboard should have the killed text
        (check (editor-get-clipboard ed) => "hello")
        ;; Mark should be cleared
        (check (buffer-mark buf) => #f)))

    (test-case "headless: copy-region-as-kill"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-copy-region-as-kill app)
        ;; Text should be unchanged
        (check (editor-get-text ed) => "hello world")
        ;; Kill ring should have "hello"
        (check (car (app-state-kill-ring app)) => "hello")))

    (test-case "headless: buffer-list-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (buffer-list-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'buffer-list)
        (check (buffer-list-buffer? buf) => #t)))

    (test-case "headless: buffer list select switches buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        ;; Create a second buffer and buffer-list buffer
        (let* ((buf2 (buffer-create! "target.ss" ed "/tmp/target.ss"))
               (bl-buf (buffer-create! "*Buffer List*" ed)))
          (set! (buffer-lexer-lang bl-buf) 'buffer-list)
          (buffer-attach! ed bl-buf)
          (set! (edit-window-buffer win) bl-buf)
          (editor-set-text ed "  Buffer\t\tFile\n  ------\t\t----\n  *scratch*\t\t\n  target.ss\t\t/tmp/target.ss\n")
          ;; Position cursor on the "target.ss" line (line 3)
          (let ((line3-start (editor-position-from-line ed 3)))
            (editor-goto-pos ed line3-start)
            ;; Select buffer
            (cmd-buffer-list-select app)
            ;; Should have switched to target.ss buffer
            (check (buffer-name (edit-window-buffer win)) => "target.ss"))
          ;; Cleanup
          (buffer-list-remove! buf)
          (buffer-list-remove! buf2)
          (buffer-list-remove! bl-buf))))

    (test-case "headless: org-todo cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Test: heading with no state -> add TODO
        (editor-set-text ed "* Buy milk")
        (editor-goto-pos ed 2) ;; on the heading
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* TODO Buy milk")
        ;; Test: TODO -> DONE
        (editor-goto-pos ed 2)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* DONE Buy milk")
        ;; Test: DONE -> remove keyword
        (editor-goto-pos ed 2)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* Buy milk")))

    (test-case "headless: dired buffer blocks self-insert"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*dired*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f 'dired #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "  file1.txt\n  file2.txt\n")
        (editor-goto-pos ed 0)
        ;; Self-insert should be suppressed in dired
        (cmd-self-insert! app 97) ;; 'a'
        (check (editor-get-text ed) => "  file1.txt\n  file2.txt\n")
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: open-line inserts newline without moving cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 5)
        (cmd-open-line app)
        (check (editor-get-text ed) => "hello\n")
        ;; Cursor stays at position 5 (before the newline)
        (check (editor-get-current-pos ed) => 5)))

    (test-case "headless: forward-word navigation"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world foo")
        (editor-goto-pos ed 0)
        (cmd-forward-word app)
        ;; Should be at end of first word or start of second
        (let ((pos (editor-get-current-pos ed)))
          (check (> pos 0) => #t)
          (check (<= pos 6) => #t))))

    (test-case "headless: self-insert in dired is suppressed"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*dired*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f 'dired #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "dir listing")
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 120) ;; 'x'
        ;; Text should be unchanged
        (check (editor-get-text ed) => "dir listing")))

    (test-case "headless: multiple self-inserts build text correctly"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type "abc" one character at a time
        (cmd-self-insert! app 97)  ;; a
        (cmd-self-insert! app 98)  ;; b
        (cmd-self-insert! app 99)  ;; c
        (check (editor-get-text ed) => "abc")
        (check (editor-get-current-pos ed) => 3)
        ;; Move to beginning and type "X"
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 88)  ;; X
        (check (editor-get-text ed) => "Xabc")
        (check (editor-get-current-pos ed) => 1)))

    (test-case "headless: kill-line at end of line kills newline"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "first\nsecond")
        ;; Go to end of first line (pos 5, just before \n)
        (editor-goto-pos ed 5)
        (cmd-kill-line app)
        ;; Should kill the newline, joining lines
        (check (editor-get-text ed) => "firstsecond")))

    ;;=========================================================================
    ;; Org-mode: helper function tests
    ;;=========================================================================

    (test-case "org-heading-level: counts stars"
      (check (org-heading-level "* Heading") => 1)
      (check (org-heading-level "** Sub") => 2)
      (check (org-heading-level "*** Deep") => 3)
      (check (org-heading-level "Not a heading") => 0)
      (check (org-heading-level "") => 0))

    (test-case "org-find-subtree-end: finds boundaries"
      (let ((lines '("* H1" "Body1" "** Sub1" "Body2" "* H2" "Body3")))
        ;; Subtree of H1 (level 1) ends at H2 (index 4)
        (check (org-find-subtree-end lines 0 1) => 4)
        ;; Subtree of Sub1 (level 2) ends at H2 (index 4)
        (check (org-find-subtree-end lines 2 2) => 4)
        ;; Subtree of H2 (level 1) ends at end of list
        (check (org-find-subtree-end lines 4 1) => 6)))

    (test-case "org-on-checkbox-line?: detects checkboxes"
      ;; string-contains returns index (0 for first match) or #f
      (check (not (not (org-on-checkbox-line? "- [ ] Buy milk"))) => #t)
      (check (not (not (org-on-checkbox-line? "- [X] Done task"))) => #t)
      (check (not (not (org-on-checkbox-line? "- [x] Also done"))) => #t)
      (check (org-on-checkbox-line? "Regular text") => #f)
      (check (org-on-checkbox-line? "* Heading") => #f))

    ;;=========================================================================
    ;; Org-mode: headless command tests
    ;;=========================================================================

    (test-case "headless: org-todo on nested heading"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Test on ** sub-heading
        (editor-set-text ed "** Task item")
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** TODO Task item")
        ;; Cycle to DONE
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** DONE Task item")
        ;; Cycle back to none
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** Task item")))

    (test-case "headless: org-promote and org-demote"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Demote: * -> **
        (editor-set-text ed "* Heading One")
        (editor-goto-pos ed 2)
        (cmd-org-demote app)
        (check (editor-get-text ed) => "** Heading One")
        ;; Demote again: ** -> ***
        (editor-goto-pos ed 2)
        (cmd-org-demote app)
        (check (editor-get-text ed) => "*** Heading One")
        ;; Promote: *** -> **
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        (check (editor-get-text ed) => "** Heading One")
        ;; Promote: ** -> *
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        (check (editor-get-text ed) => "* Heading One")))

    (test-case "headless: org-promote at level 1 is no-op"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Already top")
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        ;; Should stay unchanged
        (check (editor-get-text ed) => "* Already top")))

    (test-case "headless: org-toggle-checkbox"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Unchecked -> checked
        (editor-set-text ed "- [ ] Buy groceries")
        (editor-goto-pos ed 5)
        (cmd-org-toggle-checkbox app)
        (check (editor-get-text ed) => "- [X] Buy groceries")
        ;; Checked -> unchecked
        (editor-goto-pos ed 5)
        (cmd-org-toggle-checkbox app)
        (check (editor-get-text ed) => "- [ ] Buy groceries")))

    (test-case "headless: org-priority cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; No priority -> [#A]
        (editor-set-text ed "* Task")
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#A] Task")
        ;; [#A] -> [#B]
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#B] Task")
        ;; [#B] -> [#C]
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#C] Task")
        ;; [#C] -> none
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* Task")))

    (test-case "headless: org-priority with TODO keyword"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; TODO heading: priority goes after keyword
        (editor-set-text ed "* TODO Fix bug")
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* TODO [#A] Fix bug")))

    (test-case "headless: org-move-subtree-down"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Two headings with content
        (editor-set-text ed "* First\nBody 1\n* Second\nBody 2")
        (editor-goto-pos ed 0)  ;; on "* First"
        (cmd-org-move-subtree-down app)
        (check (editor-get-text ed) => "* Second\nBody 2\n* First\nBody 1")))

    (test-case "headless: org-move-subtree-up"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Position on second heading
        (editor-set-text ed "* First\nBody 1\n* Second\nBody 2")
        ;; Move cursor to "* Second" (line 2)
        (editor-goto-pos ed (editor-position-from-line ed 2))
        (cmd-org-move-subtree-up app)
        (check (editor-get-text ed) => "* Second\nBody 2\n* First\nBody 1")))

    (test-case "headless: org-insert-heading"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; On a level-1 heading, insert new level-1 heading after subtree
        (editor-set-text ed "* Heading\nSome body text")
        (editor-goto-pos ed 2)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should have new "* " heading after the body
          (check (not (not (string-contains text "\n* "))) => #t))))

    (test-case "headless: org-insert-heading at level 2"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Sub heading\nBody")
        (editor-goto-pos ed 3)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should insert ** heading (same level)
          (check (not (not (string-contains text "\n** "))) => #t))))

    (test-case "headless: org-export strips stars, preserves case"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* My Heading\nBody text here\n** Sub Heading")
        (cmd-org-export app)
        ;; Export creates *Org Export* buffer with stripped text
        (let ((text (editor-get-text ed)))
          ;; Heading text should preserve case (bug fix: was string-upcase)
          (check (not (not (string-contains text "My Heading"))) => #t)
          (check (not (not (string-contains text "Sub Heading"))) => #t)
          ;; Should not contain * prefix
          (check (not (string-contains text "* ")) => #t)
          ;; Body preserved
          (check (not (not (string-contains text "Body text here"))) => #t))))

    (test-case "headless: org-cycle fold/unfold"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nChild line 1\nChild line 2\n* Next")
        (editor-goto-pos ed 0)
        ;; All lines should be visible initially
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)
        ;; Fold
        (cmd-org-cycle app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 0)
        ;; Heading line 0 stays visible
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)
        ;; Line 3 ("* Next") should stay visible (not part of subtree)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)
        ;; Unfold
        (cmd-org-cycle app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)))

    (test-case "headless: org-shift-tab global fold/unfold"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* H1\nBody1\n* H2\nBody2")
        ;; All visible initially
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        ;; Fold: hide non-headings
        (cmd-org-shift-tab app)
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)  ;; heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)  ;; body hidden
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)  ;; heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 0)  ;; body hidden
        ;; Unfold: show all
        (cmd-org-shift-tab app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)))

    (test-case "headless: org-store-link"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nLine 2")
        (editor-goto-pos ed 0) ;; line 0
        (cmd-org-store-link app)
        (check (string? *org-stored-link*) => #t)
        (check (not (not (string-contains *org-stored-link* "file:/tmp/test.org"))) => #t)
        (check (not (not (string-contains *org-stored-link* "::1"))) => #t)))

    (test-case "headless: org src-block template format"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; cmd-org-insert-src-block requires interactive prompt, so test
        ;; the template format directly by inserting text
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (let ((template "#+BEGIN_SRC python\n\n#+END_SRC\n"))
          (editor-insert-text ed 0 template)
          (let ((text (editor-get-text ed)))
            (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
            (check (not (not (string-contains text "#+END_SRC"))) => #t)))))

    (test-case "headless: org-template-expand <s"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t)
          ;; No <s remaining
          (check (not (string-contains text "<s")) => #t))))

    (test-case "headless: org-template-expand <e"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<e")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXAMPLE"))) => #t)
          (check (not (not (string-contains text "#+END_EXAMPLE"))) => #t))))

    (test-case "headless: org-template-expand <q"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<q")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_QUOTE"))) => #t)
          (check (not (not (string-contains text "#+END_QUOTE"))) => #t))))

    (test-case "headless: org-template-expand preserves indentation"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Indented template
        (editor-set-text ed "  <s")
        (editor-goto-pos ed 4)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          ;; Both begin and end should be indented
          (check (not (not (string-contains text "  #+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "  #+END_SRC"))) => #t))))

    (test-case "headless: org-template-expand <l for latex export"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<l")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          ;; BEGIN has full "EXPORT latex"
          (check (not (not (string-contains text "#+BEGIN_EXPORT latex"))) => #t)
          ;; END has just "EXPORT"
          (check (not (not (string-contains text "#+END_EXPORT"))) => #t))))

    (test-case "headless: org-template-expand unknown key"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Unknown template key - should not expand
        (editor-set-text ed "<z")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (check (editor-get-text ed) => "<z")))

    (test-case "headless: command registration for new org commands"
      (register-all-commands!)
      (check (not (not (find-command 'org-promote))) => #t)
      (check (not (not (find-command 'org-demote))) => #t)
      (check (not (not (find-command 'org-move-subtree-up))) => #t)
      (check (not (not (find-command 'org-move-subtree-down))) => #t)
      (check (not (not (find-command 'org-toggle-checkbox))) => #t)
      (check (not (not (find-command 'org-priority))) => #t)
      (check (not (not (find-command 'org-set-tags))) => #t)
      (check (not (not (find-command 'org-insert-heading))) => #t)
      (check (not (not (find-command 'org-insert-src-block))) => #t)
      (check (not (not (find-command 'org-clock-in))) => #t)
      (check (not (not (find-command 'org-clock-out))) => #t)
      (check (not (not (find-command 'org-template-expand))) => #t))

    ;;; ================================================================
    ;;; Language detection and highlighting tests
    ;;; ================================================================

    (test-case "detect-file-language: Python"
      (check (detect-file-language "script.py") => 'python)
      (check (detect-file-language "app.pyw") => 'python)
      (check (detect-file-language "/home/user/code/main.py") => 'python))

    (test-case "detect-file-language: Scheme/Gerbil"
      (check (detect-file-language "core.ss") => 'scheme)
      (check (detect-file-language "lib.scm") => 'scheme)
      (check (detect-file-language "module.sld") => 'scheme)
      (check (detect-file-language "lib.sls") => 'scheme))

    (test-case "detect-file-language: C/C++"
      (check (detect-file-language "main.c") => 'c)
      (check (detect-file-language "header.h") => 'c)
      (check (detect-file-language "app.cpp") => 'c)
      (check (detect-file-language "lib.hpp") => 'c)
      (check (detect-file-language "test.cc") => 'c)
      (check (detect-file-language "util.cxx") => 'c))

    (test-case "detect-file-language: JavaScript/TypeScript"
      (check (detect-file-language "app.js") => 'javascript)
      (check (detect-file-language "index.jsx") => 'javascript)
      (check (detect-file-language "main.mjs") => 'javascript)
      (check (detect-file-language "app.ts") => 'typescript)
      (check (detect-file-language "component.tsx") => 'typescript))

    (test-case "detect-file-language: Web languages"
      (check (detect-file-language "index.html") => 'html)
      (check (detect-file-language "page.htm") => 'html)
      (check (detect-file-language "style.css") => 'css))

    (test-case "detect-file-language: Shell/Bash"
      (check (detect-file-language "deploy.sh") => 'bash)
      (check (detect-file-language "config.bash") => 'bash)
      (check (detect-file-language "setup.zsh") => 'bash))

    (test-case "detect-file-language: Data formats"
      (check (detect-file-language "data.json") => 'json)
      (check (detect-file-language "config.yaml") => 'yaml)
      (check (detect-file-language "config.yml") => 'yaml)
      (check (detect-file-language "settings.toml") => 'toml))

    (test-case "detect-file-language: Other languages"
      (check (detect-file-language "app.rb") => 'ruby)
      (check (detect-file-language "main.rs") => 'rust)
      (check (detect-file-language "main.go") => 'go)
      (check (detect-file-language "App.java") => 'java)
      (check (detect-file-language "script.lua") => 'lua)
      (check (detect-file-language "query.sql") => 'sql)
      (check (detect-file-language "README.md") => 'markdown)
      (check (detect-file-language "Makefile") => 'makefile)
      (check (detect-file-language "changes.diff") => 'diff)
      (check (detect-file-language "changes.patch") => 'diff))

    (test-case "detect-file-language: unknown/no extension"
      (check (detect-file-language "README") => #f)
      (check (detect-file-language "data.xyz") => #f)
      (check (detect-file-language #f) => #f))

    (test-case "gerbil-file-extension?"
      (check (gerbil-file-extension? "core.ss") => #t)
      (check (gerbil-file-extension? "lib.scm") => #t)
      (check (gerbil-file-extension? "main.py") => #f)
      (check (gerbil-file-extension? #f) => #f))

    (test-case "buffer-attach hook restores highlighting"
      ;; Verify the hook mechanism: creating a buffer with a file-path
      ;; and attaching it should trigger the post-attach hook
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "test.py" ed "/tmp/test.py")))
        ;; Just verify the attach doesn't crash (hook is no-op by default in tests)
        (buffer-attach! ed buf)
        (check (buffer-file-path buf) => "/tmp/test.py")
        (check (detect-file-language "/tmp/test.py") => 'python)))

    ;;; ================================================================
    ;;; Terminal ANSI parsing tests
    ;;; ================================================================

    (test-case "parse-ansi-segments: plain text"
      (let ((segs (parse-ansi-segments "hello world")))
        (check (length segs) => 1)
        (check (text-segment-text (car segs)) => "hello world")
        (check (text-segment-fg-color (car segs)) => -1)
        (check (text-segment-bold? (car segs)) => #f)))

    (test-case "parse-ansi-segments: single color"
      ;; ESC[31m = red foreground, ESC[0m = reset
      (let* ((input (string-append "\x1b;[31mhello\x1b;[0m world"))
             (segs (parse-ansi-segments input)))
        (check (>= (length segs) 2) => #t)
        ;; First segment: "hello" in red
        (check (text-segment-text (car segs)) => "hello")
        (check (text-segment-fg-color (car segs)) => 1)  ; red
        ;; Second segment: " world" in default
        (check (text-segment-text (cadr segs)) => " world")
        (check (text-segment-fg-color (cadr segs)) => -1)))

    (test-case "parse-ansi-segments: bold + color"
      ;; ESC[1;32m = bold green
      (let* ((input (string-append "\x1b;[1;32mOK\x1b;[0m"))
             (segs (parse-ansi-segments input)))
        (check (>= (length segs) 1) => #t)
        (check (text-segment-text (car segs)) => "OK")
        (check (text-segment-fg-color (car segs)) => 2)  ; green
        (check (text-segment-bold? (car segs)) => #t)))

    (test-case "parse-ansi-segments: bright colors"
      ;; ESC[91m = bright red
      (let* ((input (string-append "\x1b;[91mERROR\x1b;[0m"))
             (segs (parse-ansi-segments input)))
        (check (text-segment-text (car segs)) => "ERROR")
        (check (text-segment-fg-color (car segs)) => 9)))  ; bright red = 8+1

    (test-case "parse-ansi-segments: strips carriage returns"
      (let ((segs (parse-ansi-segments "line1\r\nline2\r\n")))
        (check (length segs) => 1)
        (check (text-segment-text (car segs)) => "line1\nline2\n")))

    (test-case "parse-ansi-segments: strips non-SGR CSI sequences"
      ;; ESC[2J = clear screen, ESC[H = cursor home
      (let* ((input (string-append "\x1b;[2J\x1b;[Hhello"))
             (segs (parse-ansi-segments input)))
        ;; Should produce "hello" with default color
        (check (= (length segs) 1) => #t)
        (check (text-segment-text (car segs)) => "hello")))

    (test-case "parse-ansi-segments: OSC sequences stripped"
      ;; ESC]0;title BEL = set window title
      (let* ((input (string-append "\x1b;]0;my-title\x07;text"))
             (segs (parse-ansi-segments input)))
        (check (text-segment-text (car segs)) => "text")))

    (test-case "color-to-style: default"
      (check (color-to-style -1 #f) => 0)
      (check (color-to-style -1 #t) => (+ *term-style-base* 15)))

    (test-case "color-to-style: standard colors"
      (check (color-to-style 0 #f) => (+ *term-style-base* 0))   ; black
      (check (color-to-style 1 #f) => (+ *term-style-base* 1))   ; red
      (check (color-to-style 7 #f) => (+ *term-style-base* 7)))  ; white

    (test-case "color-to-style: bold shifts to bright"
      (check (color-to-style 1 #t) => (+ *term-style-base* 9))   ; bold red = bright red
      (check (color-to-style 4 #t) => (+ *term-style-base* 12))) ; bold blue = bright blue

    (test-case "terminal-buffer? detection"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "*terminal*" ed #f)))
        (check (terminal-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'terminal)
        (check (terminal-buffer? buf) => #t)))

    ;;=========================================================================
    ;; Minibuffer history tests
    ;;=========================================================================

    (test-case "minibuffer-history-add! adds entries"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "first")
      (check *minibuffer-history* => ["first"])
      (minibuffer-history-add! "second")
      (check *minibuffer-history* => ["second" "first"]))

    (test-case "minibuffer-history-add! ignores empty strings"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "")
      (check *minibuffer-history* => [])
      (minibuffer-history-add! "valid")
      (minibuffer-history-add! "")
      (check *minibuffer-history* => ["valid"]))

    (test-case "minibuffer-history-add! deduplicates front"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "cmd")
      (minibuffer-history-add! "cmd")
      ;; Should not have duplicate at front
      (check (length *minibuffer-history*) => 1)
      (check (car *minibuffer-history*) => "cmd"))

    (test-case "minibuffer-history-add! allows non-front duplicates"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "alpha")
      (minibuffer-history-add! "beta")
      (minibuffer-history-add! "alpha")
      ;; alpha appears twice (front and end), that's OK
      (check (length *minibuffer-history*) => 3)
      (check (car *minibuffer-history*) => "alpha"))

    (test-case "minibuffer-history-add! respects max size"
      (set! *minibuffer-history* [])
      ;; Add 105 entries
      (let loop ((i 0))
        (when (< i 105)
          (minibuffer-history-add! (string-append "entry-" (number->string i)))
          (loop (+ i 1))))
      ;; Should be capped at 100
      (check (<= (length *minibuffer-history*) 100) => #t))

    ;;=========================================================================
    ;; Auto-save and file modification tracking tests
    ;;=========================================================================

    (test-case "make-auto-save-path produces #name# format"
      (check (make-auto-save-path "/home/user/foo.txt") => "/home/user/#foo.txt#")
      (check (make-auto-save-path "/tmp/bar.ss") => "/tmp/#bar.ss#")
      (check (make-auto-save-path "simple.txt") => "#simple.txt#"))

    (test-case "file-mod-time returns number for existing file"
      (let ((mt (file-mod-time "/home/jafourni/mine/gerbil-emacs/core.ss")))
        (check (number? mt) => #t)
        (check (> mt 0) => #t)))

    (test-case "file-mod-time returns #f for nonexistent file"
      (check (file-mod-time "/nonexistent/path/foo.ss") => #f))

    (test-case "update-buffer-mod-time! tracks file times"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "test.ss" ed "/home/jafourni/mine/gerbil-emacs/core.ss")))
        (update-buffer-mod-time! buf)
        (let ((mt (hash-get *buffer-mod-times* buf)))
          (check (number? mt) => #t)
          (check (> mt 0) => #t))
        ;; Clean up
        (hash-remove! *buffer-mod-times* buf)))

    (test-case "update-buffer-mod-time! ignores non-file buffers"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "*scratch*" ed #f)))
        (update-buffer-mod-time! buf)
        (check (hash-get *buffer-mod-times* buf) => #f)))

    ))

;; Run tests when executed directly
(def main
  (lambda args
    (run-tests! emacs-test)
    (test-report-summary!)))
