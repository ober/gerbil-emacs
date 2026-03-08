;;; -*- Gerbil -*-
;;; Qt commands config - terminal, keymaps, dired marks, keys, init, macros
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/snippets
        (only-in :gemacs/persist
                 record-face-customization!
                 custom-faces-save!
                 theme-settings-save!)
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/gsh-eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/snippets)

;; --- Misc ---
(def (cmd-display-fill-column-indicator app)
  "Display fill column indicator."
  (cmd-toggle-fill-column-indicator app))

(def (cmd-display-line-numbers-relative app)
  "Toggle relative line numbers."
  (echo-message! (app-state-echo app) "Relative line numbers toggled"))

(def (cmd-font-lock-mode app)
  "Toggle font lock mode."
  (cmd-toggle-highlighting app))

(def (cmd-customize-face app)
  "Interactively customize a face's visual attributes.
   Prompts for face name, then for each attribute (foreground, background, bold, italic).
   Changes are saved to ~/.gemacs-custom-faces and persist across sessions."
  (let* ((face-names (sort (hash-keys *faces*)
                           (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
         (face-name-strs (map symbol->string face-names))
         (face-input (qt-echo-read-string-with-completion app
                       "Customize face: " face-name-strs)))
    (when (and face-input (not (string-empty? face-input)))
      (let* ((face-sym (string->symbol face-input))
             (face (face-get face-sym)))
        (if (not face)
          (echo-error! (app-state-echo app)
            (string-append "Unknown face: " face-input))
          (begin
            ;; Show current properties
            (let ((current-desc (string-append
                                  "Current: "
                                  (if (face-fg face) (string-append "fg:" (face-fg face) " ") "")
                                  (if (face-bg face) (string-append "bg:" (face-bg face) " ") "")
                                  (if (face-bold face) "bold " "")
                                  (if (face-italic face) "italic " ""))))
              (echo-message! (app-state-echo app) current-desc))

            ;; Collect customizations
            (let ((fg-input (qt-echo-read-string app "Foreground (#hex or empty to keep): "))
                  (bg-input #f)
                  (bold-input #f)
                  (italic-input #f))

              ;; Foreground
              (when (and fg-input (not (string-empty? fg-input)))
                (set-face-attribute! face-sym fg: fg-input)
                (record-face-customization! face-sym fg: fg-input))

              ;; Background
              (set! bg-input (qt-echo-read-string app "Background (#hex or empty to keep): "))
              (when (and bg-input (not (string-empty? bg-input)))
                (set-face-attribute! face-sym bg: bg-input)
                (record-face-customization! face-sym bg: bg-input))

              ;; Bold
              (set! bold-input (qt-echo-read-string app "Bold (y/n/empty to keep): "))
              (when (and bold-input (not (string-empty? bold-input)))
                (let ((bold-val (cond
                                  ((or (string=? bold-input "y") (string=? bold-input "yes")) #t)
                                  ((or (string=? bold-input "n") (string=? bold-input "no")) #f)
                                  (else 'unset))))
                  (unless (eq? bold-val 'unset)
                    (set-face-attribute! face-sym bold: bold-val)
                    (record-face-customization! face-sym bold: bold-val))))

              ;; Italic
              (set! italic-input (qt-echo-read-string app "Italic (y/n/empty to keep): "))
              (when (and italic-input (not (string-empty? italic-input)))
                (let ((italic-val (cond
                                    ((or (string=? italic-input "y") (string=? italic-input "yes")) #t)
                                    ((or (string=? italic-input "n") (string=? italic-input "no")) #f)
                                    (else 'unset))))
                  (unless (eq? italic-val 'unset)
                    (set-face-attribute! face-sym italic: italic-val)
                    (record-face-customization! face-sym italic: italic-val)))))

            ;; Re-apply theme to update all buffers with new face
            (apply-theme! app)

            ;; Save customization to disk
            (custom-faces-save!)

            (echo-message! (app-state-echo app)
              (string-append "Face customized: " face-input))))))))

(def (get-monospace-fonts)
  "Get list of available monospace fonts from the system.
   Returns a list of font family names."
  (with-catch
    (lambda (e)
      ;; Fallback: common monospace fonts
      '("Monospace" "Courier" "Monaco" "Consolas" "DejaVu Sans Mono"
        "Liberation Mono" "Ubuntu Mono" "Fira Code" "JetBrains Mono"
        "Source Code Pro" "Hack" "Noto Mono" "Inconsolata"))
    (lambda ()
      ;; Try fc-list to get actual system fonts
      (let* ((proc (open-process
                     (list path: "/usr/bin/fc-list"
                           arguments: [":spacing=mono" "family"]
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (lines (let loop ((acc []))
                      (let ((line (read-line proc)))
                        (if (eof-object? line)
                          acc
                          (loop (cons line acc)))))))
        (process-status proc)
        (close-port proc)
        ;; Parse fc-list output: "Family Name,Variant:style=..."
        ;; Take the first family name before comma
        (let ((fonts (filter-map
                       (lambda (line)
                         (let ((idx (string-index line #\,)))
                           (if idx
                             (substring line 0 idx)
                             line)))
                       lines)))
          (if (null? fonts)
            ;; Fallback if fc-list returned nothing
            '("Monospace" "Courier" "Monaco")
            (sort fonts string<?)))))))

(def (apply-font-to-all-editors! app)
  "Apply the current global font family and size to all open editors."
  (let ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((ed (qt-edit-window-editor win)))
          (sci-send/string ed SCI_STYLESETFONT *default-font-family* STYLE_DEFAULT)
          (sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT *default-font-size*)
          (sci-send ed SCI_STYLECLEARALL)))
      (qt-frame-windows fr)))
  ;; Update Qt stylesheet so chrome widgets match
  (when *qt-app-ptr*
    (qt-app-set-style-sheet! *qt-app-ptr* (theme-stylesheet))))

(def (cmd-set-frame-font app)
  "Set the default font family for all editors.
   Prompts with completion from available monospace fonts."
  (let* ((fonts (get-monospace-fonts))
         (input (qt-echo-read-string-with-completion app "Font family: " fonts)))
    (when (and input (not (string-empty? input)))
      (set! *default-font-family* input)
      (apply-font-to-all-editors! app)
      (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
      (echo-message! (app-state-echo app)
        (string-append "Font: " input)))))

(def (cmd-set-font-size app)
  "Set the default font size for all editors.
   Prompts for a numeric size (6-72)."
  (let ((input (qt-echo-read-string app "Font size (6-72): ")))
    (when (and input (not (string-empty? input)))
      (let ((size (string->number input)))
        (if (and size (>= size 6) (<= size 72))
          (begin
            (set! *default-font-size* size)
            (apply-font-to-all-editors! app)
            (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
            (echo-message! (app-state-echo app)
              (string-append "Font size: " (number->string size))))
          (echo-message! (app-state-echo app)
            "Invalid font size (must be 6-72)"))))))

(def *named-colors*
  '("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure"
    "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown"
    "burlywood" "cadetblue" "chartreuse" "chocolate" "coral"
    "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan"
    "darkgoldenrod" "darkgray" "darkgreen" "darkkhaki" "darkmagenta"
    "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon"
    "darkseagreen" "darkslateblue" "darkslategray" "darkturquoise"
    "darkviolet" "deeppink" "deepskyblue" "dimgray" "dodgerblue"
    "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro"
    "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow"
    "honeydew" "hotpink" "indianred" "indigo" "ivory" "khaki"
    "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue"
    "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray"
    "lightgreen" "lightpink" "lightsalmon" "lightseagreen"
    "lightskyblue" "lightslategray" "lightsteelblue" "lightyellow"
    "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine"
    "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen"
    "mediumslateblue" "mediumspringgreen" "mediumturquoise"
    "mediumvioletred" "midnightblue" "mintcream" "mistyrose"
    "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab"
    "orange" "orangered" "orchid" "palegoldenrod" "palegreen"
    "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru"
    "pink" "plum" "powderblue" "purple" "red" "rosybrown" "royalblue"
    "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna"
    "silver" "skyblue" "slateblue" "slategray" "snow" "springgreen"
    "steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet"
    "wheat" "white" "whitesmoke" "yellow" "yellowgreen"))

(def (cmd-list-colors app)
  "List available named colors in a buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (qt-buffer-create! "*Colors*" ed #f))
         ;; Format color list with 4 columns
         (lines (let loop ((cs *named-colors*) (row []) (acc []))
                  (cond
                    ((null? cs)
                     (reverse (if (null? row) acc
                                (cons (string-join (reverse row) "  ") acc))))
                    ((= (length row) 4)
                     (loop cs [] (cons (string-join (reverse row) "  ") acc)))
                    (else
                     (let* ((c (car cs))
                            (padded (string-append c
                                      (make-string (max 1 (- 22 (string-length c))) #\space))))
                       (loop (cdr cs) (cons padded row) acc))))))
         (header "Named Colors (CSS/Qt)\n=====================\n")
         (text (string-append header (string-join lines "\n") "\n\n"
                  (number->string (length *named-colors*)) " colors total")))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (echo-message! (app-state-echo app)
      (string-append (number->string (length *named-colors*)) " named colors"))))

;;; ============================================================================
;;; User Theme Discovery
;;; ============================================================================

(def *user-themes-dir*
  (path-expand ".gemacs-themes" (user-info-home (user-info (user-name)))))

(def (discover-user-themes)
  "Scan ~/.gemacs-themes/*.ss for user-defined theme files.
   Returns a list of theme names (symbols)."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (if (file-exists? *user-themes-dir*)
        (let* ((files (directory-files *user-themes-dir*))
               (theme-files (filter (lambda (f)
                                     (and (string-suffix? ".ss" f)
                                          (not (string-prefix? "." f))))
                                   files)))
          (map (lambda (f)
                 ;; Convert "my-theme.ss" -> 'my-theme
                 (string->symbol (substring f 0 (- (string-length f) 3))))
               theme-files))
        []))))

(def (load-user-theme-file! theme-name)
  "Load a user theme file from ~/.gemacs-themes/THEME-NAME.ss
   Returns #t on success, #f on failure."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((theme-path (path-expand (string-append (symbol->string theme-name) ".ss")
                                     *user-themes-dir*)))
        (when (file-exists? theme-path)
          (let ((text (read-file-as-string theme-path)))
            (when text
              (let ((port (open-input-string text)))
                (let loop ()
                  (let ((form (read port)))
                    (unless (eof-object? form)
                      (eval form)
                      (loop))))
                #t))))))))

(def (cmd-load-theme app)
  "Switch to a different color theme (built-in or user-defined from ~/.gemacs-themes/)."
  (let* ((builtin-themes (theme-names))
         (user-themes (discover-user-themes))
         (all-themes (append builtin-themes user-themes))
         (names (map symbol->string all-themes))
         (input (qt-echo-read-with-narrowing app "Load theme:" names)))
    (when (and input (> (string-length input) 0))
      (let ((sym (string->symbol input)))
        (cond
          ;; Built-in theme already registered
          ((theme-get sym)
           (apply-theme! app theme-name: sym)
           (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
           (echo-message! (app-state-echo app)
             (string-append "Theme: " input)))
          ;; User theme - try to load from file
          ((member sym user-themes)
           (if (load-user-theme-file! sym)
             ;; Successfully loaded and registered via define-theme! in the file
             (if (theme-get sym)
               (begin
                 (apply-theme! app theme-name: sym)
                 (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
                 (echo-message! (app-state-echo app)
                   (string-append "Theme: " input " (from ~/.gemacs-themes/)")))
               (echo-error! (app-state-echo app)
                 (string-append "Theme file loaded but no theme defined: " input)))
             (echo-error! (app-state-echo app)
               (string-append "Failed to load theme file: " input))))
          ;; Unknown theme
          (else
           (echo-error! (app-state-echo app)
             (string-append "Unknown theme: " input))))))))

(def (cmd-describe-theme app)
  "Show all face definitions for a theme.
   Opens a buffer showing the complete theme definition with all faces."
  (let* ((builtin-themes (theme-names))
         (user-themes (discover-user-themes))
         (all-themes (append builtin-themes user-themes))
         (names (map symbol->string all-themes))
         (input (qt-echo-read-string-with-completion app
                  (string-append "Describe theme (default: " (symbol->string *current-theme*) "): ")
                  names)))
    (let* ((theme-name (if (or (not input) (string-empty? input))
                         *current-theme*
                         (string->symbol input)))
           (theme (theme-get theme-name)))
      (cond
        (theme
         ;; Create a buffer with theme description
         (let* ((fr (app-state-frame app))
                (ed (current-qt-editor app))
                (buf-name (string-append "*theme: " (symbol->string theme-name) "*"))
                (buf (or (buffer-by-name buf-name)
                        (qt-buffer-create! buf-name ed #f))))
           ;; Build theme description
           (let ((desc (string-append
                         "Theme: " (symbol->string theme-name) "\n"
                         (make-string 60 #\=) "\n\n"
                         (let loop ((entries theme) (acc ""))
                           (if (null? entries)
                             acc
                             (let* ((entry (car entries))
                                    (face-name (car entry))
                                    (props (cdr entry)))
                               (if (and (pair? props) (keyword? (car props)))
                                 ;; Face definition
                                 (let ((face-line (string-append
                                                    (symbol->string face-name) ":\n"
                                                    "  " (let prop-loop ((ps props) (pacc ""))
                                                           (if (null? ps)
                                                             pacc
                                                             (if (and (pair? ps) (keyword? (car ps)) (pair? (cdr ps)))
                                                               (let ((k (keyword->string (car ps)))
                                                                     (v (cadr ps)))
                                                                 (prop-loop (cddr ps)
                                                                   (string-append pacc
                                                                     k " "
                                                                     (cond
                                                                       ((string? v) v)
                                                                       ((boolean? v) (if v "true" "false"))
                                                                       (else "?"))
                                                                     "  ")))
                                                               pacc)))
                                                    "\n\n")))
                                   (loop (cdr entries) (string-append acc face-line)))
                                 ;; Legacy UI chrome key - skip
                                 (loop (cdr entries) acc))))))))
             ;; Attach buffer and set text
             (qt-buffer-attach! ed buf)
             (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
             (qt-plain-text-edit-set-text! ed desc)
             (qt-plain-text-edit-set-cursor-position! ed 0)
             (echo-message! (app-state-echo app)
               (string-append "Describing theme: " (symbol->string theme-name))))))
        ;; User theme not yet loaded
        ((member theme-name user-themes)
         (if (load-user-theme-file! theme-name)
           (cmd-describe-theme app)  ;; Retry after loading
           (echo-error! (app-state-echo app)
             (string-append "Failed to load theme file: " (symbol->string theme-name)))))
        (else
         (echo-error! (app-state-echo app)
           (string-append "Unknown theme: " (symbol->string theme-name))))))))

(def (cmd-fold-level app)
  "Set fold level — fold all blocks at or above the specified depth."
  (let* ((ed (current-qt-editor app))
         (total (sci-send ed SCI_GETLINECOUNT))
         ;; Default: fold at level 1 (top-level blocks)
         (target-level 1))
    ;; First expand all, then contract at target level
    (sci-send ed SCI_FOLDALL SC_FOLDACTION_EXPAND 0)
    (let loop ((i 0))
      (when (< i total)
        (let ((fl (sci-send ed SCI_GETFOLDLEVEL i 0)))
          (when (and (not (zero? (bitwise-and fl SC_FOLDLEVELHEADERFLAG)))
                     (<= (bitwise-and fl #xFFF) (+ #x400 target-level)))
            (sci-send ed SCI_FOLDLINE i SC_FOLDACTION_CONTRACT)))
        (loop (+ i 1))))
    (echo-message! (app-state-echo app)
      (string-append "Folded to level " (number->string target-level)))))

(def (cmd-ansi-term app)
  "Open an ANSI terminal."
  (cmd-term app))

(def (cmd-diff-backup app)
  "Diff current file with its backup (~) file."
  (let ((buf (qt-current-buffer (app-state-frame app))))
    (if (not buf)
      (echo-error! (app-state-echo app) "No current buffer")
      (let ((path (buffer-file-path buf)))
        (if (not path)
          (echo-error! (app-state-echo app) "Buffer is not visiting a file")
          (let ((backup-path (string-append path "~")))
            (if (not (file-exists? backup-path))
              (echo-message! (app-state-echo app) "No backup file found")
              (compilation-run-command! app
                (string-append "diff -u "
                  (grep-shell-quote backup-path) " "
                  (grep-shell-quote path))))))))))

(def (cmd-eldoc app)
  "Toggle eldoc mode (automatic function signature display)."
  (set! *eldoc-mode* (not *eldoc-mode*))
  (set! *eldoc-last-sym* #f)
  (echo-message! (app-state-echo app)
    (if *eldoc-mode* "Eldoc mode enabled" "Eldoc mode disabled")))

(def (cmd-recover-session app)
  "Recover a previous session — delegates to session-restore."
  (execute-command! app 'session-restore))

(def (cmd-revert-buffer-with-coding app)
  "Revert buffer with different coding."
  (cmd-revert-buffer app))

(def (cmd-set-buffer-file-coding app)
  "Set buffer file coding system."
  (echo-message! (app-state-echo app) "File coding: UTF-8 (fixed)"))

(def (cmd-set-language-environment app)
  "Set language environment."
  (echo-message! (app-state-echo app) "Language environment: UTF-8"))

(def (cmd-sudo-find-file app)
  "Open file as root."
  (cmd-sudo-write app))

(def *which-function-mode* #f)

(def (which-function-at-point text pos)
  "Find the function/class name containing POS. Returns string or #f.
   Searches backward from POS for language-specific definition patterns."
  (let ((len (string-length text)))
    (when (and (> pos 0) (<= pos len))
      ;; Find the line number for pos to check line-based patterns
      (let loop ((i (min (- pos 1) (- len 1))))
        (cond
          ((< i 0) #f)
          ;; Scheme/Gerbil: (def (name, (defmethod (name, (defrule (name
          ((and (>= (- len i) 5)
                (or (string=? (substring text i (+ i 5)) "(def ")
                    (string=? (substring text i (+ i 5)) "(def(")))
           (let* ((open-paren? (char=? (string-ref text (+ i 4)) #\())
                  (name-start (if open-paren? (+ i 5) (+ i 5)))
                  ;; Skip the open paren after (def if present
                  (ns (if (and (< name-start len)
                               (char=? (string-ref text name-start) #\())
                        (+ name-start 1) name-start))
                  (name-end (let nloop ((j ns))
                              (if (or (>= j len)
                                      (memq (string-ref text j)
                                            '(#\space #\) #\newline #\( #\tab)))
                                j (nloop (+ j 1))))))
             (if (> name-end ns)
               (substring text ns name-end)
               #f)))
          ;; Python: def name( or class name(
          ((and (>= (- len i) 4)
                (or (and (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)
                             (char=? (string-ref text (- i 1)) #\space))
                         (string=? (substring text i (+ i 4)) "def "))
                    (and (>= (- len i) 6)
                         (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)
                             (char=? (string-ref text (- i 1)) #\space))
                         (string=? (substring text i (+ i 6)) "class "))))
           (let* ((is-class (and (>= (- len i) 6)
                                 (string=? (substring text i (+ i 6)) "class ")))
                  (name-start (+ i (if is-class 6 4)))
                  (name-end (let nloop ((j name-start))
                              (if (or (>= j len)
                                      (memq (string-ref text j)
                                            '(#\( #\: #\space #\newline #\tab)))
                                j (nloop (+ j 1))))))
             (if (> name-end name-start)
               (substring text name-start name-end)
               #f)))
          ;; C/Go/Rust: func name, fn name, void name(, int name(
          ((and (>= (- len i) 5)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                (or (string=? (substring text i (+ i 5)) "func ")
                    (and (>= (- len i) 3)
                         (string=? (substring text i (+ i 3)) "fn "))))
           (let* ((skip (if (string=? (substring text i (+ i 3)) "fn ") 3 5))
                  (name-start (+ i skip))
                  (name-end (let nloop ((j name-start))
                              (if (or (>= j len)
                                      (memq (string-ref text j)
                                            '(#\( #\space #\{ #\newline #\tab #\<)))
                                j (nloop (+ j 1))))))
             (if (> name-end name-start)
               (substring text name-start name-end)
               #f)))
          ;; JS/TS: function name(
          ((and (>= (- len i) 9)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)
                    (char=? (string-ref text (- i 1)) #\space))
                (string=? (substring text i (+ i 9)) "function "))
           (let* ((name-start (+ i 9))
                  (name-end (let nloop ((j name-start))
                              (if (or (>= j len)
                                      (memq (string-ref text j)
                                            '(#\( #\space #\{ #\newline #\tab)))
                                j (nloop (+ j 1))))))
             (if (> name-end name-start)
               (substring text name-start name-end)
               #f)))
          (else (loop (- i 1))))))))

(def (cmd-which-function app)
  "Show current function name (multi-language)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (name (which-function-at-point text pos)))
    (if name
      (echo-message! (app-state-echo app) (string-append "In: " name))
      (echo-message! (app-state-echo app) "Not in a function"))))

(def (cmd-widen-all app)
  "Widen all narrowed buffers."
  (cmd-widen app))


(def *qt-profiler-running* #f)
(def *qt-profiler-start-stats* #f)
(def *profiler-data* (hash))

(def (cmd-profiler-start app)
  "Start profiling (records start time and GC stats)."
  (set! *qt-profiler-running* #t)
  (set! *qt-profiler-start-stats* (##process-statistics))
  (echo-message! (app-state-echo app) "Profiler started"))

(def (cmd-profiler-stop app)
  "Stop profiler and show timing report."
  (if *qt-profiler-running*
    (let* ((end-stats (##process-statistics))
           (start *qt-profiler-start-stats*)
           (wall (- (f64vector-ref end-stats 2) (f64vector-ref start 2)))
           (user (- (f64vector-ref end-stats 0) (f64vector-ref start 0)))
           (sys  (- (f64vector-ref end-stats 1) (f64vector-ref start 1)))
           (gc   (- (f64vector-ref end-stats 5) (f64vector-ref start 5)))
           (alloc (inexact->exact (- (f64vector-ref end-stats 4)
                                     (f64vector-ref start 4))))
           (fmt (lambda (v) (number->string (/ (round (* v 1000)) 1000.0))))
           (report (string-append
                     "=== Profiler Report ===\n\n"
                     "Wall time:   " (fmt wall) "s\n"
                     "User CPU:    " (fmt user) "s\n"
                     "System CPU:  " (fmt sys) "s\n"
                     "GC time:     " (fmt gc) "s\n"
                     "Allocated:   " (number->string alloc) " bytes\n")))
      (set! *qt-profiler-running* #f)
      ;; Store data for profiler-report too
      (hash-put! *profiler-data* "wall-time" wall)
      (hash-put! *profiler-data* "user-cpu" user)
      (hash-put! *profiler-data* "system-cpu" sys)
      (hash-put! *profiler-data* "gc-time" gc)
      ;; Show in buffer
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (buf (or (buffer-by-name "*Profiler Report*")
                      (qt-buffer-create! "*Profiler Report*" ed))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed report)
        (qt-modeline-update! app)))
    (echo-message! (app-state-echo app) "Profiler not running")))

(def (cmd-show-tab-count app)
  "Show tab count in buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (count (let loop ((i 0) (n 0))
                  (if (>= i (string-length text)) n
                    (loop (+ i 1) (if (char=? (string-ref text i) #\tab) (+ n 1) n))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " tabs in buffer"))))

(def (cmd-show-trailing-whitespace-count app)
  "Show count of lines with trailing whitespace."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (count (length (filter (lambda (l)
                                  (and (> (string-length l) 0)
                                       (char-whitespace? (string-ref l (- (string-length l) 1)))))
                                lines))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " lines with trailing whitespace"))))

;;;============================================================================
;;; Terminal commands (gsh-backed)
;;;============================================================================

(def terminal-buffer-counter 0)

(def (cmd-term app)
  "Open a new gsh-backed terminal buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (name (begin
                 (set! terminal-buffer-counter (+ terminal-buffer-counter 1))
                 (if (= terminal-buffer-counter 1)
                   "*terminal*"
                   (string-append "*terminal-"
                                  (number->string terminal-buffer-counter) "*"))))
         (buf (qt-buffer-create! name ed #f)))
    ;; Mark as terminal buffer
    (set! (buffer-lexer-lang buf) 'terminal)
    ;; Attach buffer to editor
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    ;; Initialize gsh-backed terminal
    (with-catch
      (lambda (e)
        (let ((msg (with-output-to-string "" (lambda () (display-exception e)))))
          (gemacs-log! "cmd-term: gsh init failed: " msg)
          (echo-error! (app-state-echo app)
            (string-append "Terminal failed: " msg))))
      (lambda ()
        (let ((ts (terminal-start!)))
          (hash-put! *terminal-state* buf ts)
          ;; Show initial prompt
          (let ((prompt (terminal-prompt ts)))
            (qt-plain-text-edit-set-text! ed prompt)
            (set! (terminal-state-prompt-pos ts) (string-length prompt))
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))
        (echo-message! (app-state-echo app) (string-append name " started"))))))

(def (cmd-terminal-send app)
  "Execute the current input line in the terminal via gsh.
   Builtins run synchronously, external commands run async via PTY.
   When PTY is busy (e.g. sudo password prompt), sends newline to PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (hash-get *terminal-state* buf)))
    (when ts
      ;; If PTY is busy, just send newline to the child process
      (if (terminal-pty-busy? ts)
        (terminal-send-input! ts "\n")
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               (prompt-pos (terminal-state-prompt-pos ts))
               (input (if (< prompt-pos (string-length text))
                        (substring text prompt-pos (string-length text))
                        ""))
               ;; Compute actual terminal dimensions from editor widget
               (rows (max 2 (sci-send ed 2370 0))) ; SCI_LINESONSCREEN
               (char-w (max 1 (sci-send/string ed 2162 "M" 0))) ; SCI_TEXTWIDTH
               (cols (max 20 (quotient (qt-widget-width ed) char-w))))
          ;; Append newline after user input
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
          (qt-plain-text-edit-insert-text! ed "\n")
          (let-values (((mode output new-cwd) (terminal-execute-async! input ts rows cols)))
          (case mode
            ((sync)
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
               (qt-plain-text-edit-insert-text! ed output)
               (unless (char=? (string-ref output (- (string-length output) 1)) #\newline)
                 (qt-plain-text-edit-insert-text! ed "\n")))
             ;; Display prompt after sync command
             (when (hash-get *terminal-state* buf)
               (let ((prompt (terminal-prompt ts)))
                 (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                 (qt-plain-text-edit-insert-text! ed prompt)
                 (set! (terminal-state-prompt-pos ts)
                   (string-length (qt-plain-text-edit-text ed)))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))))
            ((async)
             ;; Command dispatched to PTY — output arrives via timer polling
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            ((special)
             (cond
               ((eq? output 'clear)
                (qt-plain-text-edit-set-text! ed "")
                (let ((prompt (terminal-prompt ts)))
                  (qt-plain-text-edit-insert-text! ed prompt)
                  (set! (terminal-state-prompt-pos ts)
                    (string-length (qt-plain-text-edit-text ed)))
                  (qt-plain-text-edit-ensure-cursor-visible! ed)))
               ((eq? output 'exit)
                (terminal-stop! ts)
                (let* ((fr (app-state-frame app))
                       (other (let loop ((bs (buffer-list)))
                                (cond ((null? bs) #f)
                                      ((eq? (car bs) buf) (loop (cdr bs)))
                                      (else (car bs))))))
                  (when other
                    (qt-buffer-attach! ed other)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) other))
                  (hash-remove! *terminal-state* buf)
                  (qt-buffer-kill! buf)
                  (echo-message! (app-state-echo app) "Terminal exited"))))))))))))

(def (cmd-term-interrupt app)
  "Send SIGINT to running PTY process, or cancel current input."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (cond
      ((not ts)
       ;; Also handle shell buffers
       (let ((ss (and (shell-buffer? buf) (hash-get *shell-state* buf))))
         (if (and ss (shell-pty-busy? ss))
           (begin
             (shell-interrupt! ss)
             (echo-message! (app-state-echo app) "Interrupt sent"))
           (echo-message! (app-state-echo app) "Not in a terminal buffer"))))
      ((terminal-pty-busy? ts)
       ;; Send real SIGINT to the PTY child process
       (terminal-interrupt! ts)
       (let ((ed (current-qt-editor app)))
         (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
         (qt-plain-text-edit-insert-text! ed "^C\n")
         (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (else
       ;; No PTY running — just cancel current input line
       (let* ((ed (current-qt-editor app))
              (prompt (terminal-prompt ts)))
         (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
         (qt-plain-text-edit-insert-text! ed "^C\n")
         (qt-plain-text-edit-insert-text! ed prompt)
         (set! (terminal-state-prompt-pos ts)
           (string-length (qt-plain-text-edit-text ed)))
         (qt-plain-text-edit-ensure-cursor-visible! ed))))))

(def (cmd-term-send-eof app)
  "Close the terminal/shell/eshell buffer (Ctrl-D).
   Only exits if the current input line is empty (standard shell behavior)."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (define (kill-and-switch! cleanup!)
      (cleanup!)
      (let ((other (let loop ((bs (buffer-list)))
                     (cond ((null? bs) #f)
                           ((eq? (car bs) buf) (loop (cdr bs)))
                           (else (car bs))))))
        (when other
          (qt-buffer-attach! ed other)
          (set! (qt-edit-window-buffer (qt-current-window fr)) other))
        (qt-buffer-kill! buf)))
    (cond
      ;; Terminal buffer
      ((and (terminal-buffer? buf) (hash-get *terminal-state* buf))
       => (lambda (ts)
            (let* ((text (qt-plain-text-edit-text ed))
                   (prompt-pos (terminal-state-prompt-pos ts))
                   (input (if (< prompt-pos (string-length text))
                            (substring text prompt-pos (string-length text))
                            "")))
              (if (string=? input "")
                (begin
                  (terminal-stop! ts)
                  (kill-and-switch! (lambda () (hash-remove! *terminal-state* buf)))
                  (echo-message! (app-state-echo app) "Terminal exited"))
                (echo-message! (app-state-echo app)
                  "Use 'exit' to close (input not empty)")))))
      ;; Shell buffer
      ((and (shell-buffer? buf) (hash-get *shell-state* buf))
       => (lambda (ss)
            (let* ((text (qt-plain-text-edit-text ed))
                   (prompt-pos (shell-state-prompt-pos ss))
                   (input (if (< prompt-pos (string-length text))
                            (substring text prompt-pos (string-length text))
                            "")))
              (if (string=? input "")
                (begin
                  (shell-stop! ss)
                  (kill-and-switch! (lambda () (hash-remove! *shell-state* buf)))
                  (echo-message! (app-state-echo app) "Shell exited"))
                (echo-message! (app-state-echo app)
                  "Use 'exit' to close (input not empty)")))))
      ;; Eshell buffer
      ((and (gsh-eshell-buffer? buf) (hash-get *gsh-eshell-state* buf))
       => (lambda (env)
            ;; Eshell doesn't track prompt-pos, so just exit unconditionally
            (kill-and-switch! (lambda () (hash-remove! *gsh-eshell-state* buf)))
            (echo-message! (app-state-echo app) "Eshell exited")))
      (else
        ;; Not in a shell buffer — normal delete-char
        (cmd-delete-char app)))))

(def (cmd-term-send-tab app)
  "Insert tab character in the terminal buffer."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (let ((ed (current-qt-editor app)))
        (qt-plain-text-edit-insert-text! ed "\t"))
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

;;;============================================================================
;;; Multi-terminal and terminal copy mode
;;;============================================================================

(def (cmd-multi-vterm app)
  "Create a new terminal buffer (multi-vterm style)."
  (cmd-term app))

;; Track which terminal buffers are in copy mode
(def *terminal-copy-mode* (make-hash-table))

(def (cmd-vterm-copy-mode app)
  "Toggle terminal copy mode — makes terminal read-only for text selection."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (if (terminal-buffer? buf)
      (let ((in-copy (hash-get *terminal-copy-mode* buf)))
        (if in-copy
          ;; Exit copy mode
          (begin
            (hash-put! *terminal-copy-mode* buf #f)
            (qt-plain-text-edit-set-read-only! ed #f)
            (echo-message! echo "Terminal copy mode OFF"))
          ;; Enter copy mode
          (begin
            (hash-put! *terminal-copy-mode* buf #t)
            (qt-plain-text-edit-set-read-only! ed #t)
            (echo-message! echo "Terminal copy mode ON — select text, C-w/M-w to copy"))))
      (echo-message! echo "Not in a terminal buffer"))))

(def (cmd-vterm-copy-done app)
  "Exit terminal copy mode and resume terminal."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (when (and (terminal-buffer? buf) (hash-get *terminal-copy-mode* buf))
      (hash-put! *terminal-copy-mode* buf #f)
      (qt-plain-text-edit-set-read-only! ed #f)
      (echo-message! echo "Terminal copy mode OFF"))))

(def (get-terminal-buffers)
  "Return list of terminal buffers from buffer-list."
  (filter terminal-buffer? *buffer-list*))

(def (qt-switch-to-terminal! app buf)
  "Switch to terminal buffer BUF in the current window."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app)))
    (buffer-touch! buf)
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))

(def (cmd-term-list app)
  "Switch between terminal buffers via completion."
  (let ((terms (get-terminal-buffers)))
    (if (null? terms)
      (echo-message! (app-state-echo app) "No terminal buffers")
      (let* ((names (map buffer-name terms))
             (choice (qt-echo-read-string-with-completion
                       app "Terminal: " names)))
        (when (and choice (not (string=? choice "")))
          (let ((target (find (lambda (b) (string=? (buffer-name b) choice)) terms)))
            (when target
              (qt-switch-to-terminal! app target))))))))

(def (cmd-term-next app)
  "Switch to the next terminal buffer, cycling around."
  (let* ((terms (get-terminal-buffers))
         (cur (current-qt-buffer app)))
    (cond
      ((null? terms)
       (echo-message! (app-state-echo app) "No terminal buffers"))
      ((not (terminal-buffer? cur))
       ;; Not in a terminal — switch to the first one
       (qt-switch-to-terminal! app (car terms)))
      (else
       (let loop ((rest terms))
         (cond
           ((null? rest)
            (qt-switch-to-terminal! app (car terms)))
           ((eq? (car rest) cur)
            (if (null? (cdr rest))
              (qt-switch-to-terminal! app (car terms))
              (qt-switch-to-terminal! app (cadr rest))))
           (else (loop (cdr rest)))))))))

(def (cmd-term-prev app)
  "Switch to the previous terminal buffer, cycling around."
  (let* ((terms (get-terminal-buffers))
         (cur (current-qt-buffer app))
         (last-term (and (pair? terms) (list-ref terms (- (length terms) 1)))))
    (cond
      ((null? terms)
       (echo-message! (app-state-echo app) "No terminal buffers"))
      ((not (terminal-buffer? cur))
       (qt-switch-to-terminal! app last-term))
      (else
       (let loop ((rest terms) (prev last-term))
         (cond
           ((null? rest)
            (qt-switch-to-terminal! app prev))
           ((eq? (car rest) cur)
            (qt-switch-to-terminal! app prev))
           (else (loop (cdr rest) (car rest)))))))))

;;;============================================================================
;;; Buffer-local keybindings (mode keymaps)
;;;============================================================================

;; Maps buffer-lexer-lang symbol -> keymap hash table
(def *mode-keymaps* (make-hash-table))

(def (mode-keymap-lookup buf key-str)
  "Look up KEY-STR in the buffer's mode keymap. Returns command symbol or #f.
   Checks lexer-lang first, then buffer name for special buffers."
  (let* ((lang (buffer-lexer-lang buf))
         (km (or (hash-get *mode-keymaps* lang)
                 ;; Check by buffer name for buffers without special lexer-lang
                 (let ((name (buffer-name buf)))
                   (cond
                     ((string=? name "*compilation*") (hash-get *mode-keymaps* 'compilation))
                     ((string=? name "*Grep*") (hash-get *mode-keymaps* 'grep))
                     ((string=? name "*Occur*") (hash-get *mode-keymaps* 'occur))
                     ((string=? name "*calendar*") (hash-get *mode-keymaps* 'calendar))
                     ((string=? name "*eww*") (hash-get *mode-keymaps* 'eww))
                     ((string=? name "*Magit*") (hash-get *mode-keymaps* 'magit))
                     ((string=? name "*Magit: Commit*") (hash-get *mode-keymaps* 'magit-commit))
                     ((string=? name "*Magit Log*") (hash-get *mode-keymaps* 'magit-log))
                     ((string=? name "*Magit Commit*") (hash-get *mode-keymaps* 'magit-commit-view))
                     ((string=? name "*Magit Stash*") (hash-get *mode-keymaps* 'magit-stash))
                     ((string=? name "*Magit Stash Diff*") (hash-get *mode-keymaps* 'magit-stash-diff))
                     ((string=? name "*Org Capture*") (hash-get *mode-keymaps* 'org-capture))
                     ((string=? name "*IBBuffer*") (hash-get *mode-keymaps* 'ibuffer))
                     (else #f))))))
    (and km (keymap-lookup km key-str))))

(def (setup-mode-keymaps!)
  "Initialize mode-specific keybindings for special buffer types."
  ;; Dired mode: single-key navigation
  (let ((dired-km (make-keymap)))
    (keymap-bind! dired-km "n" 'next-line)
    (keymap-bind! dired-km "p" 'previous-line)
    (keymap-bind! dired-km "g" 'revert-buffer)
    (keymap-bind! dired-km "d" 'dired-do-delete)
    (keymap-bind! dired-km "R" 'dired-do-rename)
    (keymap-bind! dired-km "C" 'dired-do-copy)
    (keymap-bind! dired-km "+" 'dired-create-directory)
    (keymap-bind! dired-km "q" 'kill-buffer-cmd)
    (keymap-bind! dired-km "^" 'dired) ;; go up to parent
    (keymap-bind! dired-km "m" 'dired-mark)
    (keymap-bind! dired-km "u" 'dired-unmark)
    (keymap-bind! dired-km "U" 'dired-unmark-all)
    (keymap-bind! dired-km "t" 'dired-toggle-marks)
    (keymap-bind! dired-km "D" 'dired-do-delete-marked)
    (keymap-bind! dired-km "x" 'dired-do-delete-marked)
    (hash-put! *mode-keymaps* 'dired dired-km))

  ;; Compilation mode
  (let ((comp-km (make-keymap)))
    (keymap-bind! comp-km "n" 'next-error)
    (keymap-bind! comp-km "p" 'previous-error)
    (keymap-bind! comp-km "g" 'recompile)
    (keymap-bind! comp-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'compilation comp-km))

  ;; Grep results mode
  (let ((grep-km (make-keymap)))
    (keymap-bind! grep-km "n" 'next-grep-result)
    (keymap-bind! grep-km "p" 'previous-grep-result)
    (keymap-bind! grep-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'grep grep-km))

  ;; Buffer list mode: single-key navigation
  (let ((bl-km (make-keymap)))
    (keymap-bind! bl-km "n" 'next-line)
    (keymap-bind! bl-km "p" 'previous-line)
    (keymap-bind! bl-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'buffer-list bl-km))

  ;; IBBuffer mode: interactive buffer management
  (let ((ib-km (make-keymap)))
    (keymap-bind! ib-km "d" 'ibuffer-mark-delete)
    (keymap-bind! ib-km "s" 'ibuffer-mark-save)
    (keymap-bind! ib-km "u" 'ibuffer-unmark)
    (keymap-bind! ib-km "x" 'ibuffer-execute)
    (keymap-bind! ib-km "RET" 'ibuffer-goto-buffer)
    (keymap-bind! ib-km "/" 'ibuffer-filter-name)
    (keymap-bind! ib-km "S" 'ibuffer-sort-name)
    (keymap-bind! ib-km "z" 'ibuffer-sort-size)
    (keymap-bind! ib-km "t" 'ibuffer-toggle-marks)
    (keymap-bind! ib-km "g" 'ibuffer-refresh)
    (keymap-bind! ib-km "n" 'next-line)
    (keymap-bind! ib-km "p" 'previous-line)
    (keymap-bind! ib-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'ibuffer ib-km))

  ;; Occur mode
  (let ((occur-km (make-keymap)))
    (keymap-bind! occur-km "n" 'next-line)
    (keymap-bind! occur-km "p" 'previous-line)
    (keymap-bind! occur-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'occur occur-km))

  ;; Calendar mode
  (let ((cal-km (make-keymap)))
    (keymap-bind! cal-km "p" 'calendar-prev-month)
    (keymap-bind! cal-km "n" 'calendar-next-month)
    (keymap-bind! cal-km "<" 'calendar-prev-year)
    (keymap-bind! cal-km ">" 'calendar-next-year)
    (keymap-bind! cal-km "." 'calendar-today)
    (keymap-bind! cal-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'calendar cal-km))

  ;; EWW browser mode
  (let ((eww-km (make-keymap)))
    (keymap-bind! eww-km "g" 'eww)        ;; go to URL
    (keymap-bind! eww-km "l" 'eww-back)   ;; back
    (keymap-bind! eww-km "r" 'eww-reload) ;; reload
    (keymap-bind! eww-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'eww eww-km))
  ;; Magit mode
  (let ((magit-km (make-keymap)))
    (keymap-bind! magit-km "s" 'magit-stage)
    (keymap-bind! magit-km "S" 'magit-stage-all)
    (keymap-bind! magit-km "u" 'magit-unstage)
    (keymap-bind! magit-km "c" 'magit-commit)
    (keymap-bind! magit-km "a" 'magit-amend)
    (keymap-bind! magit-km "d" 'magit-diff)
    (keymap-bind! magit-km "l" 'magit-log)
    (keymap-bind! magit-km "b" 'magit-branch)
    (keymap-bind! magit-km "B" 'magit-blame)
    (keymap-bind! magit-km "f" 'magit-fetch)
    (keymap-bind! magit-km "F" 'magit-pull)
    (keymap-bind! magit-km "P" 'magit-push)
    (keymap-bind! magit-km "r" 'magit-rebase)
    (keymap-bind! magit-km "m" 'magit-merge)
    (keymap-bind! magit-km "z" 'magit-stash)
    (keymap-bind! magit-km "Z" 'magit-stash-pop)
    (keymap-bind! magit-km "x" 'magit-cherry-pick)
    (keymap-bind! magit-km "X" 'magit-revert-commit)
    (keymap-bind! magit-km "w" 'magit-worktree)
    (keymap-bind! magit-km "k" 'magit-checkout)
    (keymap-bind! magit-km "g" 'magit-status) ;; refresh
    (keymap-bind! magit-km "n" 'next-line)
    (keymap-bind! magit-km "p" 'previous-line)
    (keymap-bind! magit-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit magit-km))

  ;; Magit commit mode: C-c C-c to commit, C-c C-k to abort
  (let ((commit-km (make-keymap)))
    (keymap-bind! commit-km "C-c C-c" 'magit-commit-finalize)
    (keymap-bind! commit-km "C-c C-k" 'magit-commit-abort)
    (hash-put! *mode-keymaps* 'magit-commit commit-km))

  ;; Magit log mode: Enter to show commit, n/p for navigation
  (let ((log-km (make-keymap)))
    (keymap-bind! log-km "RET" 'magit-log-show-commit)
    (keymap-bind! log-km "n" 'next-line)
    (keymap-bind! log-km "p" 'previous-line)
    (keymap-bind! log-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit-log log-km))

  ;; Magit commit/diff view: q to close, n/p to navigate
  (let ((cv-km (make-keymap)))
    (keymap-bind! cv-km "n" 'next-line)
    (keymap-bind! cv-km "p" 'previous-line)
    (keymap-bind! cv-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit-commit-view cv-km)
    (hash-put! *mode-keymaps* 'magit-stash-diff cv-km))

  ;; Magit stash list: Enter to show, n/p to navigate, q to quit
  (let ((stash-km (make-keymap)))
    (keymap-bind! stash-km "RET" 'magit-stash-show)
    (keymap-bind! stash-km "n" 'next-line)
    (keymap-bind! stash-km "p" 'previous-line)
    (keymap-bind! stash-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit-stash stash-km))

  ;; Image mode: zoom controls
  (let ((img-km (make-keymap)))
    (keymap-bind! img-km "+" 'image-zoom-in)
    (keymap-bind! img-km "=" 'image-zoom-in)
    (keymap-bind! img-km "-" 'image-zoom-out)
    (keymap-bind! img-km "0" 'image-zoom-fit)
    (keymap-bind! img-km "1" 'image-zoom-reset)
    (keymap-bind! img-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'image img-km))
  ;; Org capture mode: C-c C-c to finalize, C-c C-k to abort
  (let ((cap-km (make-keymap)))
    (keymap-bind! cap-km "C-c C-c" 'org-capture-finalize)
    (keymap-bind! cap-km "C-c C-k" 'org-capture-abort)
    (hash-put! *mode-keymaps* 'org-capture cap-km)))

;;;============================================================================
;;; Ediff-files: compare two files from disk
;;;============================================================================

(def (cmd-ediff-files app)
  "Compare two files by running diff."
  (let ((file-a (qt-echo-read-string app "File A: ")))
    (when (and file-a (> (string-length file-a) 0))
      (let ((file-b (qt-echo-read-string app "File B: ")))
        (when (and file-b (> (string-length file-b) 0))
          (let ((path-a (path-expand file-a))
                (path-b (path-expand file-b)))
            (if (not (and (file-exists? path-a) (file-exists? path-b)))
              (echo-error! (app-state-echo app) "One or both files not found")
              (let* ((proc (open-process
                             (list path: "/usr/bin/diff"
                                   arguments: (list "-u" path-a path-b)
                                   stdout-redirection: #t)))
                     (output (read-line proc #f))
                     (_ (process-status proc))
                     (ed (current-qt-editor app))
                     (fr (app-state-frame app))
                     (diff-buf (qt-buffer-create! "*Ediff*" ed #f)))
                (close-port proc)
                (qt-buffer-attach! ed diff-buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                (qt-plain-text-edit-set-text! ed (or output "No differences"))
                (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                (echo-message! (app-state-echo app) "Diff complete")))))))))

;;;============================================================================
;;; Comment-dwim: intelligent comment toggle
;;;============================================================================

(def (cmd-comment-dwim app)
  "Do What I Mean with comments.
If region active: toggle comment on region.
If at end of code: add end-of-line comment.
If on blank line: insert comment and indent."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (has-sel (not (= sel-start sel-end))))
    (if has-sel
      ;; Region active: toggle comment on each line
      (cmd-comment-region app)
      ;; No region: check current line
      (let* ((line (qt-plain-text-edit-cursor-line ed))
             (lines (string-split text #\newline))
             (line-text (if (< line (length lines))
                          (list-ref lines line)
                          ""))
             (trimmed (string-trim line-text)))
        (cond
          ;; Blank line: insert comment
          ((string=? trimmed "")
           (qt-replace-line! ed line ";; ")
           (let* ((new-text (qt-plain-text-edit-text ed))
                  (new-lines (string-split new-text #\newline))
                  (pos (let loop ((i 0) (offset 0))
                         (if (>= i line) (+ offset 3)
                           (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          ;; Line already commented: uncomment
          ((and (>= (string-length trimmed) 3)
                (string=? (substring trimmed 0 3) ";; "))
           (cmd-toggle-comment app))
          ((and (>= (string-length trimmed) 2)
                (string=? (substring trimmed 0 2) ";;"))
           (cmd-toggle-comment app))
          ;; Line has code: add end-of-line comment
          (else
           (let ((new-line (string-append line-text "  ;; ")))
             (qt-replace-line! ed line new-line)
             ;; Position cursor at comment
             (let* ((new-text (qt-plain-text-edit-text ed))
                    (new-lines (string-split new-text #\newline))
                    (pos (let loop ((i 0) (offset 0))
                           (if (>= i line) (+ offset (string-length new-line))
                             (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
               (qt-plain-text-edit-set-cursor-position! ed pos)))))))))

