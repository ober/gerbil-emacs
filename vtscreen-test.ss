;;; -*- Gerbil -*-
;;; Tests for vtscreen VT100 terminal emulator and gsh-eshell ANSI stripping

(export vtscreen-test)

(import :std/test
        :std/format
        :std/misc/string
        :std/srfi/13
        :gemacs/vtscreen
        :gemacs/gsh-eshell)

(def (feed-string! vt str)
  "Feed a string to vtscreen."
  (vtscreen-feed! vt str))

(def (vt-row-text vt row)
  "Get the text content of a vtscreen row (trimmed)."
  (let* ((rendered (vtscreen-render vt))
         (lines (string-split rendered #\newline)))
    (if (< row (length lines))
      (list-ref lines row)
      "")))

(def vtscreen-test
  (test-suite "vtscreen and eshell"

    ;; =========================================================================
    ;; vtscreen: basic text rendering
    ;; =========================================================================

    (test-case "vtscreen: basic text output"
      (let ((vt (new-vtscreen 24 80)))
        (feed-string! vt "Hello, World!")
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "Hello, World!") => 0))))

    (test-case "vtscreen: newline moves to next row"
      ;; LF (\n) moves cursor down but NOT to column 0 (VT100 behavior).
      ;; CR+LF (\r\n) would move to column 0.
      (let ((vt (new-vtscreen 24 80)))
        (feed-string! vt "Line1\r\nLine2")
        (let ((row0 (vt-row-text vt 0))
              (row1 (vt-row-text vt 1)))
          (check (string-contains row0 "Line1") => 0)
          (check (string-contains row1 "Line2") => 0))))

    ;; =========================================================================
    ;; vtscreen: charset designation (ESC(B) -- the root cause of top ugliness
    ;; =========================================================================

    (test-case "vtscreen: ESC(B charset designation is consumed silently"
      ;; ESC(B is a 3-byte charset designation sequence.
      ;; The 'B' must NOT appear in the grid output.
      (let ((vt (new-vtscreen 24 80)))
        (feed-string! vt (string (integer->char 27) #\( #\B))
        (feed-string! vt "Hello")
        (let ((row0 (vt-row-text vt 0)))
          ;; Should show "Hello" without any stray "B"
          (check (string-contains row0 "Hello") => 0)
          (check (string-contains row0 "BHello") => #f))))

    (test-case "vtscreen: multiple ESC(B sequences between text"
      ;; This is what top actually does: ESC(B before almost every text chunk
      (let ((vt (new-vtscreen 24 80))
            (esc-b (string (integer->char 27) #\( #\B)))
        (feed-string! vt esc-b)
        (feed-string! vt "top")
        (feed-string! vt esc-b)
        (feed-string! vt " - ")
        (feed-string! vt esc-b)
        (feed-string! vt "12:00")
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "top - 12:00") => 0)
          ;; No stray B characters
          (check (string-contains row0 "B") => #f))))

    (test-case "vtscreen: ESC)B and ESC*B charset variants consumed"
      ;; Other charset designation variants: ESC)X, ESC*X, ESC+X
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt (string esc #\) #\B))
        (feed-string! vt (string esc #\* #\0))
        (feed-string! vt (string esc #\+ #\A))
        (feed-string! vt "OK")
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "OK") => 0)
          ;; None of B, 0, A designator chars should appear
          (check (string-contains row0 "BOK") => #f)
          (check (string-contains row0 "0OK") => #f)
          (check (string-contains row0 "AOK") => #f))))

    ;; =========================================================================
    ;; vtscreen: SGR (color) sequences should not produce visible text
    ;; =========================================================================

    (test-case "vtscreen: SGR color sequences are invisible"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        ;; ESC[38;5;33m = set foreground to color 33
        (feed-string! vt (string-append (string esc) "[38;5;33m"))
        (feed-string! vt "colored")
        ;; ESC[0m = reset
        (feed-string! vt (string-append (string esc) "[0m"))
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "colored") => 0)
          ;; No escape junk in the output
          (check (string-contains row0 "[") => #f))))

    ;; =========================================================================
    ;; vtscreen: alt-screen detection
    ;; =========================================================================

    (test-case "vtscreen: alt-screen? starts false"
      (let ((vt (new-vtscreen 24 80)))
        (check (vtscreen-alt-screen? vt) => #f)))

    (test-case "vtscreen: CSI 2J sets alt-screen"
      ;; Full-screen programs like top use ESC[2J (clear entire screen)
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt (string-append (string esc) "[2J"))
        (check (vtscreen-alt-screen? vt) => #t)))

    (test-case "vtscreen: CSI ?1049h sets alt-screen"
      ;; Programs like vim use ESC[?1049h (switch to alt screen buffer)
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt (string-append (string esc) "[?1049h"))
        (check (vtscreen-alt-screen? vt) => #t)))

    ;; =========================================================================
    ;; vtscreen: cursor movement
    ;; =========================================================================

    (test-case "vtscreen: CSI H moves cursor to home"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt "XXXX")
        ;; ESC[H = cursor home (row 1, col 1 in 1-based)
        (feed-string! vt (string-append (string esc) "[H"))
        (feed-string! vt "YY")
        (let ((row0 (vt-row-text vt 0)))
          ;; "YY" overwrites the first two X's
          (check (string-contains row0 "YYXX") => 0))))

    ;; =========================================================================
    ;; vtscreen: LF implies CR (newline mode — fixes dmesg scrolling)
    ;; =========================================================================

    (test-case "vtscreen: LF implies CR (dmesg fix)"
      ;; LF alone should move to column 0 (newline mode)
      (let ((vt (new-vtscreen 24 80)))
        (feed-string! vt "AAAA\nBBBB")
        (let ((row0 (vt-row-text vt 0))
              (row1 (vt-row-text vt 1)))
          (check (string-contains row0 "AAAA") => 0)
          ;; BBBB should be at column 0, not column 4
          (check (string-contains row1 "BBBB") => 0))))

    ;; =========================================================================
    ;; vtscreen: VT220 features
    ;; =========================================================================

    (test-case "vtscreen: CSI b (REP) repeats previous character"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt "X")
        ;; CSI 5 b = repeat 'X' 5 more times
        (feed-string! vt (string-append (string esc) "[5b"))
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "XXXXXX") => 0))))

    (test-case "vtscreen: CSI E (Cursor Next Line)"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt "    Hello")  ; cursor at col 9
        ;; CSI 1 E = next line, go to column 0
        (feed-string! vt (string-append (string esc) "[1E"))
        (feed-string! vt "World")
        (let ((row1 (vt-row-text vt 1)))
          (check (string-contains row1 "World") => 0))))

    (test-case "vtscreen: DCS consumed silently"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        ;; ESC P ... ESC \ (DCS with string terminator)
        (feed-string! vt (string-append (string esc) "P" "junk data" (string esc) "\\"))
        (feed-string! vt "OK")
        (let ((row0 (vt-row-text vt 0)))
          ;; Only "OK" should appear, not the DCS payload
          (check (string-contains row0 "OK") => 0)
          (check (string-contains row0 "junk") => #f))))

    (test-case "vtscreen: ESC c (Full Reset)"
      (let ((vt (new-vtscreen 24 80))
            (esc (integer->char 27)))
        (feed-string! vt "Hello World")
        ;; ESC c = full reset
        (feed-string! vt (string-append (string esc) "c"))
        (feed-string! vt "Reset")
        (let ((row0 (vt-row-text vt 0)))
          ;; Screen should be cleared, only "Reset" visible
          (check (string-contains row0 "Reset") => 0)
          (check (string-contains row0 "Hello") => #f))))

    (test-case "vtscreen: 8-bit CSI (0x9B) works like ESC ["
      (let ((vt (new-vtscreen 24 80)))
        (feed-string! vt "ABCDE")
        ;; 8-bit CSI followed by "2D" = cursor back 2 (col 5 -> col 3)
        (feed-string! vt (string (integer->char #x9B) #\2 #\D))
        (feed-string! vt "XY")
        (let ((row0 (vt-row-text vt 0)))
          (check (string-contains row0 "ABCXY") => 0))))

    ;; =========================================================================
    ;; gsh-eshell-strip-ansi: ANSI escape removal
    ;; =========================================================================

    (test-case "strip-ansi: plain text unchanged"
      (check (gsh-eshell-strip-ansi "hello world") => "hello world"))

    (test-case "strip-ansi: removes CSI SGR sequences"
      ;; ESC[38;5;33m ... ESC[0m
      (let ((esc (integer->char 27)))
        (check (gsh-eshell-strip-ansi
                 (string-append (string esc) "[38;5;33m" "user"
                                (string esc) "[0m"))
               => "user")))

    (test-case "strip-ansi: removes ESC(B charset designation"
      (let ((esc (integer->char 27)))
        (check (gsh-eshell-strip-ansi
                 (string-append (string esc) "(B" "text"))
               => "text")))

    (test-case "strip-ansi: removes OSC sequences (BEL terminated)"
      ;; ESC]0;title BEL
      (let ((esc (integer->char 27))
            (bel (integer->char 7)))
        (check (gsh-eshell-strip-ansi
                 (string-append (string esc) "]0;window title" (string bel) "visible"))
               => "visible")))

    (test-case "strip-ansi: removes readline prompt ignore markers"
      ;; SOH (^A) and STX (^B) are used by readline for prompt width calculation
      (let ((soh (integer->char 1))
            (stx (integer->char 2)))
        (check (gsh-eshell-strip-ansi
                 (string-append (string soh) "hidden" (string stx) "visible"))
               => "hiddenvisible")))

    (test-case "strip-ansi: removes carriage return"
      (check (gsh-eshell-strip-ansi "hello\rworld") => "helloworld"))

    (test-case "strip-ansi: complex prompt with colors and ESC(B"
      ;; Simulates a real PS1 like: ESC[38;5;33m user ESC(B ESC[m @ ESC[38;5;208m host ESC(B ESC[m
      (let ((esc (integer->char 27)))
        (let ((prompt (string-append
                        (string esc) "[38;5;33m" "user"
                        (string esc) "(B" (string esc) "[m"
                        "@"
                        (string esc) "[38;5;208m" "host"
                        (string esc) "(B" (string esc) "[m")))
          (check (gsh-eshell-strip-ansi prompt) => "user@host"))))

    ;; =========================================================================
    ;; interactive-command? detection
    ;; =========================================================================

    (test-case "interactive-command?: detects top"
      (check (not (not (interactive-command? "top"))) => #t))

    (test-case "interactive-command?: detects vim"
      (check (not (not (interactive-command? "vim file.txt"))) => #t))

    (test-case "interactive-command?: detects htop"
      (check (not (not (interactive-command? "htop"))) => #t))

    (test-case "interactive-command?: allows ls"
      (check (interactive-command? "ls -la") => #f))

    (test-case "interactive-command?: allows grep"
      (check (interactive-command? "grep pattern file") => #f))

    (test-case "interactive-command?: handles leading spaces"
      (check (not (not (interactive-command? "  top"))) => #t))

    (test-case "interactive-command?: allows empty input"
      (check (interactive-command? "") => #f))

))

(def main
  (lambda args
    (run-tests! vtscreen-test)))
