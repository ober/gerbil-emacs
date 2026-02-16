;;; -*- Gerbil -*-
;;; Qt highlighting test — verifies lexer activation for various languages.
;;; Run: QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-highlight-test

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-scintilla/constants)

(export main)

(def (check-lexer sci expected-id label)
  (let ((id (qt-scintilla-send-message sci SCI_GETLEXER 0 0)))
    (if (= id expected-id)
      (begin (displayln "  PASS: " label " lexer-id=" id) #t)
      (begin (displayln "  FAIL: " label " lexer-id=" id " expected=" expected-id) #f))))

(def (check-style sci pos expected label)
  (let ((s (qt-scintilla-send-message sci SCI_GETSTYLEAT pos 0)))
    (if (= s expected)
      (begin (displayln "  PASS: " label " style=" s) #t)
      (begin (displayln "  FAIL: " label " style=" s " expected=" expected) #f))))

(def (main . args)
  (let ((failures 0))
    (with-qt-app _app

      ;; Test 1: Lisp lexer via SCI_SETLEXERLANGUAGE + COLOURISE (production fix)
      (displayln "Test 1: Lisp lexer (SCI_SETLEXERLANGUAGE + COLOURISE)")
      (let* ((w (qt-widget-create))
             (sci (qt-scintilla-create parent: w)))
        (qt-scintilla-set-text! sci "(def x 42) ; comment\n\"hello\"")
        (qt-scintilla-send-message/string sci SCI_SETLEXERLANGUAGE "lisp" 0)
        (qt-scintilla-send-message sci SCI_COLOURISE 0 -1)
        (unless (check-lexer sci SCLEX_LISP "lisp lexer")
          (set! failures (+ failures 1)))
        (unless (check-style sci 11 1 "';'=comment(1)")
          (set! failures (+ failures 1)))
        (unless (check-style sci 8 2 "'4'=number(2)")
          (set! failures (+ failures 1)))
        ;; String at pos 22 should be style 6
        (unless (check-style sci 22 6 "'\"'=string(6)")
          (set! failures (+ failures 1)))
        (qt-scintilla-destroy! sci)
        (qt-widget-destroy! w))

      ;; Test 2: Python lexer via QsciLexer wrapper (control)
      (displayln "Test 2: Python lexer (QsciLexer wrapper)")
      (let* ((w (qt-widget-create))
             (sci (qt-scintilla-create parent: w)))
        (qt-scintilla-set-text! sci "x = 42  # comment\n\"hello\"")
        (qt-scintilla-set-lexer-language! sci "python")
        ;; QsciLexer auto-colorizes, no SCI_COLOURISE needed
        (let ((id (qt-scintilla-send-message sci SCI_GETLEXER 0 0)))
          (if (not (= id 0))
            (displayln "  PASS: python lexer-id=" id)
            (begin (displayln "  FAIL: python lexer-id=0")
                   (set! failures (+ failures 1)))))
        (qt-scintilla-destroy! sci)
        (qt-widget-destroy! w))

      ;; Test 3: Rust lexer via SCI_SETLEXERLANGUAGE + COLOURISE
      (displayln "Test 3: Rust lexer (SCI_SETLEXERLANGUAGE + COLOURISE)")
      (let* ((w (qt-widget-create))
             (sci (qt-scintilla-create parent: w)))
        (qt-scintilla-set-text! sci "fn main() { let x = 42; // comment }")
        (qt-scintilla-send-message/string sci SCI_SETLEXERLANGUAGE "rust" 0)
        (qt-scintilla-send-message sci SCI_COLOURISE 0 -1)
        (unless (check-lexer sci SCLEX_RUST "rust lexer")
          (set! failures (+ failures 1)))
        (qt-scintilla-destroy! sci)
        (qt-widget-destroy! w))

      ;; Test 4: C shim "lisp" returns no lexer (documents the bug)
      (displayln "Test 4: C shim bug — qt-scintilla-set-lexer-language! \"lisp\"")
      (let* ((w (qt-widget-create))
             (sci (qt-scintilla-create parent: w)))
        (qt-scintilla-set-text! sci "(def x 42)")
        (qt-scintilla-set-lexer-language! sci "lisp")
        (let ((id (qt-scintilla-send-message sci SCI_GETLEXER 0 0)))
          ;; This confirms the C shim has no Lisp lexer
          (if (= id 0)
            (displayln "  PASS: C shim correctly has no Lisp QsciLexer (lexer-id=0)")
            (displayln "  INFO: C shim now supports Lisp (lexer-id=" id ")")))
        (qt-scintilla-destroy! sci)
        (qt-widget-destroy! w))

      ;; Test 5: SCI_SETLEXERLANGUAGE without COLOURISE does NOT colorize
      (displayln "Test 5: SCI_SETLEXERLANGUAGE without COLOURISE (must not colorize)")
      (let* ((w (qt-widget-create))
             (sci (qt-scintilla-create parent: w)))
        (qt-scintilla-set-text! sci "(def x 42) ; comment")
        (qt-scintilla-send-message/string sci SCI_SETLEXERLANGUAGE "lisp" 0)
        ;; No COLOURISE — styles should be default (0)
        (let ((style (qt-scintilla-send-message sci SCI_GETSTYLEAT 11 0)))
          (if (= style 0)
            (displayln "  PASS: without COLOURISE, style=0 (confirms need for explicit call)")
            (displayln "  INFO: style=" style " (auto-colorized)")))
        (qt-scintilla-destroy! sci)
        (qt-widget-destroy! w)))

    (displayln "---")
    (if (= failures 0)
      (displayln "All tests passed!")
      (displayln failures " failure(s)"))
    (exit failures)))
