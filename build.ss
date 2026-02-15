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

;; Ensure pkg-config can find Qt6 .pc files
(let ((existing (or (getenv "PKG_CONFIG_PATH" #f) "")))
  (unless (string-contains existing "/usr/lib/x86_64-linux-gnu/pkgconfig")
    (setenv "PKG_CONFIG_PATH"
      (if (string=? existing "")
        "/usr/lib/x86_64-linux-gnu/pkgconfig"
        (string-append existing ":/usr/lib/x86_64-linux-gnu/pkgconfig")))))

;; Qt modules also need scintilla headers (editor.ss transitively includes them)
(def qt-cc-opts
  (string-append
   "-I" qt-vendor-dir " "
   (cppflags "Qt6Widgets" "") " "
   cc-opts))

;; Homebrew OpenSSL path — must come before system -L/usr/lib/... from Qt pkg-config
;; to avoid linking against system OpenSSL 3.0 (missing newer symbols)
(def openssl-lib-dir "/home/linuxbrew/.linuxbrew/opt/openssl@3/lib")

;; Qt exe also links scintilla/termbox/lexilla because editor.ss pulls them in
(def qt-ld-opts
  (string-append
   "-L" openssl-lib-dir " "
   "-Wl,-rpath," openssl-lib-dir " "
   "-L" qt-vendor-dir " -lqt_shim "
   "-Wl,-rpath," qt-vendor-dir " "
   (ldflags "Qt6Widgets" "-lQt6Widgets") " "
   (path-expand "bin/scintilla.a" sci-dir) " "
   (path-expand "bin/liblexilla.a" lexilla-dir) " "
   (path-expand "bin/termbox.a" termbox-dir) " "
   "-lstdc++ -lpthread"))

(defbuild-script
  `(;; Shared core (no backend dependencies)
    "core"
    "persist"
    "repl"
    ;; Syntax highlighting (depends on gerbil-scintilla/lexer)
    "highlight"
    ;; Eshell (built-in Gerbil shell)
    "eshell"
    ;; Shell (external $SHELL subprocess)
    "shell"
    ;; Terminal (PTY-backed vterm-like terminal)
    "terminal"
    ;; TUI backend
    "keymap"
    "buffer"
    "window"
    "modeline"
    "echo"
    ;; editor-extra sub-modules (must compile before facade)
    "editor-extra-helpers"
    "editor-extra-org"
    "editor-extra-web"
    "editor-extra-vcs"
    "editor-extra-editing"
    "editor-extra-tools"
    "editor-extra-tools2"
    "editor-extra-media"
    "editor-extra-modes"
    "editor-extra-final"
    "editor-extra"
    ;; editor sub-modules (must compile before facade)
    "editor-core"
    "editor-ui"
    "editor-text"
    "editor-advanced"
    "editor-cmds-a"
    "editor-cmds-b"
    "editor-cmds-c"
    "editor"
    "app"
    "emacs-test"
    "persist-test"
    (exe: "main" bin: "gerbil-emacs"
          "-cc-options" ,cc-opts
          "-ld-options" ,ld-opts)
    ;; Qt backend
    (gxc: "qt/keymap"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/buffer"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/window"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/modeline" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/echo"     "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/highlight" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/image"    "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-core"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-edit"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-search" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-file"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-sexp"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-ide"    "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-vcs"    "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-shell"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-modes"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands-config" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/commands"        "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/menubar"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (gxc: "qt/app"      "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
    (exe: "qt/main" bin: "gerbil-emacs-qt"
          "-cc-options" ,qt-cc-opts
          "-ld-options" ,qt-ld-opts)))
