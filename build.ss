#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Build script for gemacs

(import :std/build-script
        :std/make
        :std/misc/process)

;; Static build support: env vars for Docker/CI overrides
(def static-build? (getenv "GEMACS_STATIC" #f))
(def build-tui-only? (getenv "GEMACS_BUILD_TUI_ONLY" #f))

;; Helper: call pkg-config --static --libs for full transitive deps (static linking)
(def (static-pkg-config-libs lib)
  (run-process ["pkg-config" "--static" "--libs" lib] coprocess: read-line))

;; Resolve package source directory (linked or GitHub-installed)
;; NOTE: GERBIL_PATH points to the project-local .gerbil during builds,
;; but packages are installed/linked under $HOME/.gerbil/pkg/.
(def user-gerbil-dir (path-expand ".gerbil" (getenv "HOME")))

(def (find-pkg-source name)
  "Find package source dir via Gerbil package system."
  (let ((linked (path-expand (string-append "pkg/" name) user-gerbil-dir))
        (github (path-expand (string-append "pkg/github.com/ober/" name) user-gerbil-dir)))
    (cond
      ((file-exists? linked) linked)
      ((file-exists? github) github)
      (else (error (string-append "Package not found: " name
                                  "\nRun: gerbil pkg install github.com/ober/" name))))))

;; gerbil-scintilla FFI paths (needed for TUI exe linking)
(def sci-base (or (getenv "GEMACS_SCI_BASE" #f)
                  (find-pkg-source "gerbil-scintilla")))
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
   (if static-build? "-static " "")
   (path-expand "bin/scintilla.a" sci-dir) " "
   (path-expand "bin/liblexilla.a" lexilla-dir) " "
   (path-expand "bin/termbox.a" termbox-dir) " "
   "-lstdc++ -lpthread -lpcre2-8"))

;; gerbil-qt FFI paths (needed for Qt exe linking)
(def qt-base (or (getenv "GEMACS_QT_BASE" #f)
                 (find-pkg-source "gerbil-qt")))
(def qt-vendor-dir (path-expand "vendor" qt-base))

;; Ensure pkg-config can find Qt6 .pc files
(let ((existing (or (getenv "PKG_CONFIG_PATH" #f) "")))
  (unless (string-contains existing "/usr/lib/x86_64-linux-gnu/pkgconfig")
    (setenv "PKG_CONFIG_PATH"
      (if (string=? existing "")
        "/usr/lib/x86_64-linux-gnu/pkgconfig"
        (string-append existing ":/usr/lib/x86_64-linux-gnu/pkgconfig")))))

;; Detect QScintilla availability (same logic as gerbil-qt/build.ss)
(def have-qscintilla?
  (or (with-catch (lambda (_) #f)
        (lambda ()
          (run-process ["pkg-config" "--exists" "QScintilla"] coprocess: void)
          #t))
      (let ((arch (with-catch (lambda (_) "x86_64-linux-gnu")
                    (lambda () (run-process ["gcc" "-dumpmachine"] coprocess: read-line)))))
        (or (file-exists? (string-append "/usr/include/" arch "/qt6/Qsci/qsciscintilla.h"))
            (file-exists? "/usr/include/qt6/Qsci/qsciscintilla.h")
            (file-exists? "/usr/include/Qsci/qsciscintilla.h")
            (file-exists? (string-append "/usr/include/" arch "/Qsci/qsciscintilla.h"))))))

(def qsci-cppflags
  (if have-qscintilla?
    (with-catch (lambda (_) "-DQT_SCINTILLA_AVAILABLE")
      (lambda () (string-append "-DQT_SCINTILLA_AVAILABLE "
                                (run-process ["pkg-config" "--cflags" "QScintilla"]
                                             coprocess: read-line))))
    ""))

(def qsci-ldflags
  (if have-qscintilla?
    (if static-build?
      (with-catch (lambda (_) "-lqscintilla2_qt6")
        (lambda () (static-pkg-config-libs "QScintilla")))
      (with-catch (lambda (_) "-lqscintilla2_qt6")
        (lambda () (run-process ["pkg-config" "--libs" "QScintilla"]
                                coprocess: read-line))))
    ""))

;; Qt modules also need scintilla headers (editor.ss transitively includes them)
(def qt-cc-opts
  (string-append
   "-I" qt-vendor-dir " "
   (cppflags "Qt6Widgets" "") " "
   qsci-cppflags " "
   cc-opts))

;; Homebrew OpenSSL path — must come before system -L/usr/lib/... from Qt pkg-config
;; to avoid linking against system OpenSSL 3.0 (missing newer symbols)
(def openssl-lib-dir "/home/linuxbrew/.linuxbrew/opt/openssl@3/lib")

;; Qt resource objects needed for static linking (styles, PDF, shaders, etc.)
(def qt-prefix (or (getenv "QT_STATIC_PREFIX" #f) "/opt/qt6-static"))

(def (qt-resource-objects)
  "Collect Qt resource .o files from the static Qt installation."
  (if (not static-build?) ""
    (let ((objs-dir (path-expand "lib/objects-Release" qt-prefix)))
      (if (not (file-exists? objs-dir)) ""
        (let ((objects
               (with-catch (lambda (_) [])
                 (lambda ()
                   (run-process ["find" objs-dir "-name" "*.o" "-type" "f"]
                                coprocess: read-all-as-lines)))))
          (string-join objects " "))))))

;; Qt link flags (shared between gxc: modules and exe: targets).
;; NOTE: -static must NOT appear in gxc: module ld-options — it causes
;; "relocation R_X86_64_32 against hidden symbol __TMC_END__" on musl
;; because gsc compiles modules to .o files that need PIC.
;; Only exe: targets get -static for the final link step.
(def qt-ld-opts-base
  (string-append
   (if static-build? "" (string-append "-L" openssl-lib-dir " "))
   (if static-build? "" (string-append "-Wl,-rpath," openssl-lib-dir " "))
   "-L" qt-vendor-dir " "
   (if static-build?
     ;; Static: link libqt_shim.a + qt_static_plugins.o separately.
     ;; The .o must not be in an archive — it contains Q_IMPORT_PLUGIN
     ;; static constructors that the linker would otherwise drop.
     (string-append (path-expand "libqt_shim.a" qt-vendor-dir) " "
                    (path-expand "qt_static_plugins.o" qt-vendor-dir) " ")
     ;; Shared: link libqt_shim.so with rpath
     (string-append "-lqt_shim -Wl,-rpath," qt-vendor-dir " "))
   ;; Qt resource objects (static only — styles, PDF support, shaders)
   (if static-build? (string-append (qt-resource-objects) " ") "")
   ;; XCB platform plugin (static only)
   (if static-build?
     (with-catch (lambda (_) "")
       (lambda () (string-append (static-pkg-config-libs "Qt6XcbPlugin") " ")))
     "")
   ;; Qt6 libraries: use --static for full transitive deps
   (if static-build?
     (with-catch (lambda (_) "-lQt6Widgets -lQt6Gui -lQt6Core")
       (lambda () (static-pkg-config-libs "Qt6Widgets")))
     (ldflags "Qt6Widgets" "-lQt6Widgets"))
   " "
   qsci-ldflags " "
   (path-expand "bin/scintilla.a" sci-dir) " "
   (path-expand "bin/liblexilla.a" lexilla-dir) " "
   (path-expand "bin/termbox.a" termbox-dir) " "
   "-lstdc++ "
   ;; musl's pthread_once is a weak symbol; --whole-archive ensures it resolves
   (if static-build?
     "-Wl,--whole-archive -lpthread -Wl,--no-whole-archive"
     "-lpthread")
   " -lpcre2-8"))

;; For gxc: module compilation (no -static)
(def qt-ld-opts qt-ld-opts-base)

;; For exe: final linking (add -static for static builds)
(def qt-exe-ld-opts
  (string-append (if static-build? "-static " "") qt-ld-opts-base))

(defbuild-script
  `(;; Macros (must be compiled first - no dependencies)
    "macros"
    ;; PCRE2 compatibility wrapper (no dependencies)
    "pregexp-compat"
    ;; Face system (no dependencies)
    "face"
    ;; Built-in themes (depends on face)
    "themes"
    ;; Shared core (no backend dependencies)
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
    ;; Org-mode modules (must compile before editor-extra-org)
    "org-parse"
    "org-highlight"
    "org-table"
    "org-clock"
    "org-list"
    "org-export"
    "org-babel"
    "org-agenda"
    "org-capture"
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
    "editor-extra-regs"
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
    ;; IPC server for emacsclient-like remote file opening
    "ipc"
    "app"
    "emacs-test"
    "persist-test"
    "functional-test"
    "org-parse-test"
    "org-element-test"
    "org-table-test"
    "org-list-test"
    "org-clock-test"
    "org-duration-test"
    "org-export-test"
    "org-babel-test"
    "org-agenda-test"
    "org-capture-test"
    "org-tempo-test"
    "org-footnote-test"
    "org-lint-test"
    "org-src-test"
    "org-fold-test"
    "org-num-test"
    "org-property-test"
    (exe: "main" bin: "gemacs"
          "-cc-options" ,cc-opts
          "-ld-options" ,ld-opts)
    ;; emacsclient-like client binary
    (exe: "emacsclient" bin: "gemacs-client")
    ;; Qt backend (skipped when GEMACS_BUILD_TUI_ONLY is set)
    ,@(if build-tui-only? '()
        `((gxc: "qt/keymap"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/sci-shim" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
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
          (gxc: "qt/lsp-client"     "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-lsp"   "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-shell"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-modes"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-config" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-parity"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands-aliases" "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/commands"        "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/menubar"  "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (gxc: "qt/app"      "-cc-options" ,qt-cc-opts "-ld-options" ,qt-ld-opts)
          (exe: "qt-highlight-test" bin: "qt-highlight-test"
                "-cc-options" ,qt-cc-opts
                "-ld-options" ,qt-exe-ld-opts)
          (exe: "qt-functional-test" bin: "qt-functional-test"
                "-cc-options" ,qt-cc-opts
                "-ld-options" ,qt-exe-ld-opts)
          (exe: "lsp-functional-test" bin: "lsp-functional-test"
                "-cc-options" ,qt-cc-opts
                "-ld-options" ,qt-exe-ld-opts)
          ;; (exe: "qt-split-comprehensive-test" bin: "qt-split-comprehensive-test"
          ;;       "-cc-options" ,qt-cc-opts
          ;;       "-ld-options" ,qt-exe-ld-opts)
          ;; (exe: "qt-split-debug-test" bin: "qt-split-debug-test"
          ;;       "-cc-options" ,qt-cc-opts
          ;;       "-ld-options" ,qt-exe-ld-opts)
          (exe: "qt-split-simple-test" bin: "qt-split-simple-test"
                "-cc-options" ,qt-cc-opts
                "-ld-options" ,qt-exe-ld-opts)
          (exe: "qt/main" bin: "gemacs-qt"
                "-cc-options" ,qt-cc-opts
                "-ld-options" ,qt-exe-ld-opts))))
  parallelize: (max 1 (quotient (##cpu-count) 2)))
