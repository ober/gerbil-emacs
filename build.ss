#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Build script for gerbil-emacs

(import :std/build-script)

;; gerbil-scintilla FFI paths (needed for exe linking)
(def sci-base (path-expand "mine/gerbil-scintilla" (getenv "HOME")))
(def vendor-dir (path-expand "vendor" sci-base))
(def sci-dir (path-expand "scintilla" vendor-dir))
(def sci-tb-dir (path-expand "termbox" sci-dir))
(def termbox-dir (path-expand "termbox_next" sci-tb-dir))
(def lexilla-dir (path-expand "lexilla" vendor-dir))

(def cc-opts
  (string-append
   "-I" (path-expand "include" sci-dir) " "
   "-I" (path-expand "src" sci-dir) " "
   "-I" sci-tb-dir " "
   "-I" (path-expand "src" termbox-dir) " "
   "-I" (path-expand "include" lexilla-dir)))

(def ld-opts
  (string-append
   (path-expand "bin/scintilla.a" sci-dir) " "
   (path-expand "bin/liblexilla.a" lexilla-dir) " "
   (path-expand "bin/termbox.a" termbox-dir) " "
   "-lstdc++ -lpthread"))

(defbuild-script
  `("keymap"
    "buffer"
    "window"
    "modeline"
    "echo"
    "editor"
    "app"
    "emacs-test"
    (exe: "main" bin: "gerbil-emacs"
          "-cc-options" ,cc-opts
          "-ld-options" ,ld-opts)))
