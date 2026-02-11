#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Build script for gerbil-emacs

(import :std/build-script
        :std/make)

;; gerbil-scintilla FFI paths (needed for TUI exe linking)
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

;; gerbil-qt FFI paths (needed for Qt exe linking)
(def qt-base (path-expand "mine/gerbil-qt" (getenv "HOME")))
(def qt-vendor-dir (path-expand "vendor" qt-base))

(def qt-cc-opts
  (string-append
   "-I" qt-vendor-dir " "
   (cppflags "Qt6Widgets" "")))

(def qt-ld-opts
  (string-append
   "-L" qt-vendor-dir " -lqt_shim "
   "-Wl,-rpath," qt-vendor-dir " "
   (ldflags "Qt6Widgets" "-lQt6Widgets") " "
   "-lstdc++"))

(defbuild-script
  `(;; Shared core (no backend dependencies)
    "core"
    "repl"
    ;; TUI backend
    "keymap"
    "buffer"
    "window"
    "modeline"
    "echo"
    "editor"
    "app"
    "emacs-test"
    (exe: "main" bin: "gerbil-emacs"
          "-cc-options" ,cc-opts
          "-ld-options" ,ld-opts)
    ;; Qt backend
    (gxc: "qt/keymap"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/buffer"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/window"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/modeline" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/echo"     "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/app"      "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (exe: "qt/main" bin: "gerbil-emacs-qt"
          "-cc-options" ,qt-cc-opts
          "-ld-options" ,qt-ld-opts)))
