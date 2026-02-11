;;; -*- Gerbil -*-
;;; Tests for gerbil-emacs
;;; Focus on pure logic (keymap, constants, helpers) since
;;; editor operations require a live terminal.

(import :std/test
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/echo)

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
      (let ((buf (make-buffer "*test*" #f #f #f #f #f)))
        (check (repl-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'repl)
        (check (repl-buffer? buf) => #t)
        (set! (buffer-lexer-lang buf) 'dired)
        (check (repl-buffer? buf) => #f)))

    (test-case "eshell-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f)))
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
      (let ((buf (make-buffer "*test*" #f #f #f #f #f)))
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
      ;; Duplicate line
      (check (keymap-lookup *ctrl-x-map* "d") => 'duplicate-line)
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
      (check (keymap-lookup *ctrl-x-map* "C-r") => 'revert-buffer)
      (check (keymap-lookup *global-keymap* "M-a") => 'beginning-of-defun)
      (check (keymap-lookup *global-keymap* "M-e") => 'end-of-defun))

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
      (check (keymap-lookup *global-keymap* "M-/") => 'dabbrev-expand)
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

    ))

;; Run tests when executed directly
(def main
  (lambda args
    (run-tests! emacs-test)
    (test-report-summary!)))
