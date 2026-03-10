;;; -*- Gerbil -*-
;;; Persistence for gemacs
;;;
;;; Backend-agnostic persistence: recent files, minibuffer history,
;;; desktop (session) save/restore. No Scintilla or Qt imports.

(export
  ;; Recent files
  *recent-files*
  *recent-files-max*
  recent-files-add!
  recent-files-save!
  recent-files-load!
  recent-files-cleanup!

  ;; Savehist (minibuffer history persistence)
  savehist-save!
  savehist-load!

  ;; M-x command history (frequency-sorted)
  *mx-history*
  *mx-history-file*
  mx-history-add!
  mx-history-save!
  mx-history-load!
  mx-history-ordered-candidates

  ;; Desktop (session persistence)
  desktop-save!
  desktop-load
  (struct-out desktop-entry)

  ;; Buffer-local variables
  *buffer-locals*
  buffer-local-get
  buffer-local-set!
  buffer-local-delete!
  buffer-locals-for

  ;; Auto-mode-alist
  *auto-mode-alist*
  detect-major-mode

  ;; Which-key
  *which-key-mode*
  *which-key-delay*
  which-key-summary

  ;; Scroll margin
  *scroll-margin*

  ;; Persistent scratch
  *scratch-file*
  scratch-save!
  scratch-load!

  ;; Theme and font persistence
  *theme-settings-file*
  theme-settings-save!
  theme-settings-load!

  ;; Custom face persistence
  *custom-faces-file*
  *custom-faces*
  custom-faces-save!
  custom-faces-load!
  record-face-customization!

  ;; Init file
  *init-file-path*
  init-file-load!

  ;; Save-place (remember cursor position per file)
  *save-place-enabled*
  *save-place-alist*
  save-place-remember!
  save-place-restore
  save-place-save!
  save-place-load!

  ;; Clean-on-save hooks
  *delete-trailing-whitespace-on-save*
  *require-final-newline*

  ;; Centered cursor mode
  *centered-cursor-mode*

  ;; Auto-fill mode
  *auto-fill-mode*
  *fill-column*

  ;; Abbrev mode
  *abbrev-table*
  *abbrev-mode-enabled*

  ;; Enriched mode
  *enriched-mode*

  ;; Picture mode
  *picture-mode*

  ;; Electric-pair mode
  *electric-pair-mode*

  ;; Copilot (AI inline completion)
  *copilot-mode*
  *copilot-api-key*
  *copilot-model*
  *copilot-api-url*
  *copilot-suggestion*
  *copilot-suggestion-pos*

  ;; Persistence paths
  persist-path)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core
        :gemacs/customize)

;;;============================================================================
;;; Persistence file paths
;;;============================================================================

(def (persist-dir)
  (let ((home (user-info-home (user-info (user-name)))))
    home))

(def (persist-path name)
  (path-expand name (persist-dir)))

(def *recent-files-file* ".gemacs-recent-files")
(def *savehist-file*     ".gemacs-history")
(def *desktop-file*      ".gemacs-desktop")

;;;============================================================================
;;; Recent files
;;;============================================================================

(def *recent-files* [])
(def *recent-files-max* 50)

(def (recent-files-add! path)
  "Add a file path to the recent files list. Deduplicates and limits size."
  (when (and (string? path) (> (string-length path) 0))
    ;; Normalize: expand to absolute path
    (let ((abs-path (path-expand path)))
      ;; Remove existing entry (move to front)
      (set! *recent-files*
        (cons abs-path
          (let loop ((files *recent-files*) (acc []))
            (cond
              ((null? files) (reverse acc))
              ((string=? (car files) abs-path) (loop (cdr files) acc))
              (else (loop (cdr files) (cons (car files) acc)))))))
      ;; Trim to max size
      (when (> (length *recent-files*) *recent-files-max*)
        (set! *recent-files*
          (let loop ((files *recent-files*) (n 0) (acc []))
            (if (or (null? files) (>= n *recent-files-max*))
              (reverse acc)
              (loop (cdr files) (+ n 1) (cons (car files) acc)))))))))

(def (recent-files-save!)
  "Save recent files list to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *recent-files-file*)
        (lambda (port)
          (for-each (lambda (f)
                      (display f port)
                      (newline port))
                    *recent-files*))))))

(def (recent-files-load!)
  "Load recent files list from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((path (persist-path *recent-files-file*)))
        (when (file-exists? path)
          (set! *recent-files*
            (call-with-input-file path
              (lambda (port)
                (let loop ((acc []))
                  (let ((line (read-line port)))
                    (if (eof-object? line)
                      (reverse acc)
                      (if (> (string-length line) 0)
                        (loop (cons line acc))
                        (loop acc)))))))))))))

(def (recent-files-cleanup!)
  "Remove non-existent files from recent files list."
  (let* ((before (length *recent-files*))
         (cleaned (filter file-exists? *recent-files*))
         (removed (- before (length cleaned))))
    (set! *recent-files* cleaned)
    (recent-files-save!)
    removed))

;;;============================================================================
;;; Savehist (minibuffer history persistence)
;;;============================================================================

(def (savehist-save! history)
  "Save minibuffer history list to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *savehist-file*)
        (lambda (port)
          (for-each (lambda (entry)
                      (display entry port)
                      (newline port))
                    history))))))

(def (savehist-load!)
  "Load minibuffer history from disk. Returns list of strings."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let ((path (persist-path *savehist-file*)))
        (if (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (reverse acc)
                    (if (> (string-length line) 0)
                      (loop (cons line acc))
                      (loop acc)))))))
          [])))))

;;;============================================================================
;;; M-x command history (frequency + recency sorted, persisted)
;;;============================================================================

;; Hash table: command-name-string -> count (number of times used)
(def *mx-history* (make-hash-table))
(def *mx-history-file*
  (string-append (getenv "HOME" "/tmp") "/.gemacs-mx-history"))

(def (mx-history-add! name)
  "Record a command usage. Increments frequency count."
  (when (and (string? name) (> (string-length name) 0))
    (let ((count (hash-ref *mx-history* name 0)))
      (hash-put! *mx-history* name (+ count 1)))))

(def (mx-history-save!)
  "Save M-x command history (name\\tcount per line)."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (> (hash-length *mx-history*) 0)
        (call-with-output-file *mx-history-file*
          (lambda (port)
            (hash-for-each
              (lambda (name count)
                (display name port)
                (display "\t" port)
                (display count port)
                (newline port))
              *mx-history*)))))))

(def (mx-history-load!)
  "Load M-x command history from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *mx-history-file*)
        (call-with-input-file *mx-history-file*
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (unless (eof-object? line)
                  (let ((tab-pos (string-index line #\tab)))
                    (when tab-pos
                      (let* ((name (substring line 0 tab-pos))
                             (count-str (substring line (+ tab-pos 1) (string-length line)))
                             (count (with-catch (lambda (e) 1)
                                      (lambda () (string->number count-str)))))
                        (when (and (> (string-length name) 0) count)
                          (hash-put! *mx-history* name count)))))
                  (loop))))))))))

(def (mx-history-ordered-candidates all-names)
  "Return all-names sorted: frequently used first (by count desc), then alphabetically."
  (let* ((with-count (map (lambda (n) (cons n (hash-ref *mx-history* n 0))) all-names))
         (frequent (filter (lambda (p) (> (cdr p) 0)) with-count))
         (rest (filter (lambda (p) (= (cdr p) 0)) with-count))
         ;; Sort frequent by count descending
         (sorted-freq (sort frequent (lambda (a b) (> (cdr a) (cdr b)))))
         ;; Rest already alphabetical (all-names is sorted)
         )
    (append (map car sorted-freq) (map car rest))))

;;;============================================================================
;;; Desktop (session persistence)
;;;============================================================================

(defstruct desktop-entry
  (buffer-name   ; string
   file-path     ; string or #f
   cursor-pos    ; integer
   major-mode)   ; symbol or #f
  transparent: #t)

(def (desktop-save! entries)
  "Save desktop entries (buffer list with positions) to disk.
   entries: list of desktop-entry structs."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *desktop-file*)
        (lambda (port)
          (for-each
            (lambda (entry)
              ;; Format: file-path\tcursor-pos\tbuffer-name\tmajor-mode
              (let ((fp (or (desktop-entry-file-path entry) ""))
                    (pos (number->string (desktop-entry-cursor-pos entry)))
                    (name (desktop-entry-buffer-name entry))
                    (mode (let ((m (desktop-entry-major-mode entry)))
                            (if m (symbol->string m) ""))))
                (display fp port)
                (display "\t" port)
                (display pos port)
                (display "\t" port)
                (display name port)
                (display "\t" port)
                (display mode port)
                (newline port)))
            entries))))))

(def (desktop-load)
  "Load desktop entries from disk. Returns list of desktop-entry structs."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let ((path (persist-path *desktop-file*)))
        (if (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (reverse acc)
                    (let ((parts (string-split line #\tab)))
                      (if (>= (length parts) 3)
                        (let ((fp (car parts))
                              (pos (string->number (cadr parts)))
                              (name (caddr parts))
                              (mode (if (>= (length parts) 4)
                                      (let ((m (cadddr parts)))
                                        (if (> (string-length m) 0)
                                          (string->symbol m)
                                          #f))
                                      #f)))
                          (loop (cons (make-desktop-entry
                                        name
                                        (if (string=? fp "") #f fp)
                                        (or pos 0)
                                        mode)
                                      acc)))
                        (loop acc))))))))
          [])))))

;;;============================================================================
;;; Buffer-local variables
;;;============================================================================

;; Side table: buffer -> hash-table of local bindings
(def *buffer-locals* (make-hash-table))

(def (buffer-local-get buf key (default #f))
  "Get a buffer-local variable value."
  (let ((locals (hash-get *buffer-locals* buf)))
    (if locals
      (let ((val (hash-get locals key)))
        (if val val default))
      default)))

(def (buffer-local-set! buf key value)
  "Set a buffer-local variable."
  (let ((locals (hash-get *buffer-locals* buf)))
    (unless locals
      (set! locals (make-hash-table))
      (hash-put! *buffer-locals* buf locals))
    (hash-put! locals key value)))

(def (buffer-local-delete! buf)
  "Remove all buffer-local variables for a buffer."
  (hash-remove! *buffer-locals* buf))

(def (buffer-locals-for buf)
  "Get the hash table of buffer-local variables, or #f."
  (hash-get *buffer-locals* buf))

;;;============================================================================
;;; Auto-mode-alist
;;;============================================================================

;; Maps file extensions to major mode symbols
(def *auto-mode-alist*
  '(;; Lisps
    (".ss"    . gerbil-mode)
    (".scm"   . scheme-mode)
    (".sld"   . scheme-mode)
    (".el"    . emacs-lisp-mode)
    (".clj"   . clojure-mode)
    (".lisp"  . lisp-mode)
    (".cl"    . lisp-mode)
    ;; Org/Markdown
    (".org"   . org-mode)
    (".md"    . markdown-mode)
    (".markdown" . markdown-mode)
    ;; Web
    (".html"  . html-mode)
    (".htm"   . html-mode)
    (".css"   . css-mode)
    (".js"    . js-mode)
    (".jsx"   . js-mode)
    (".ts"    . typescript-mode)
    (".tsx"   . typescript-mode)
    (".json"  . json-mode)
    ;; Systems
    (".c"     . c-mode)
    (".h"     . c-mode)
    (".cpp"   . c++-mode)
    (".cc"    . c++-mode)
    (".cxx"   . c++-mode)
    (".hpp"   . c++-mode)
    (".java"  . java-mode)
    (".rs"    . rust-mode)
    (".go"    . go-mode)
    (".zig"   . zig-mode)
    ;; Scripting
    (".py"    . python-mode)
    (".rb"    . ruby-mode)
    (".lua"   . lua-mode)
    (".pl"    . perl-mode)
    (".pm"    . perl-mode)
    (".sh"    . shell-mode)
    (".bash"  . shell-mode)
    (".zsh"   . shell-mode)
    (".fish"  . fish-mode)
    ;; Config
    (".yml"   . yaml-mode)
    (".yaml"  . yaml-mode)
    (".toml"  . toml-mode)
    (".ini"   . conf-mode)
    (".cfg"   . conf-mode)
    (".conf"  . conf-mode)
    ;; Documents
    (".tex"   . latex-mode)
    (".bib"   . bibtex-mode)
    (".rst"   . rst-mode)
    ;; Data
    (".xml"   . xml-mode)
    (".sql"   . sql-mode)
    (".csv"   . csv-mode)
    ;; Make/Build
    ("Makefile" . makefile-mode)
    ("makefile" . makefile-mode)
    ("GNUmakefile" . makefile-mode)
    ("Dockerfile" . dockerfile-mode)
    (".mk"    . makefile-mode)
    (".cmake" . cmake-mode)
    ;; Misc
    (".diff"  . diff-mode)
    (".patch" . diff-mode)
    (".erl"   . erlang-mode)
    (".ex"    . elixir-mode)
    (".exs"   . elixir-mode)
    (".hs"    . haskell-mode)
    (".ml"    . ocaml-mode)
    (".mli"   . ocaml-mode)
    (".nix"   . nix-mode)
    (".swift" . swift-mode)
    (".kt"    . kotlin-mode)
    (".scala" . scala-mode)
    (".r"     . r-mode)
    (".R"     . r-mode)
    (".rkt"   . racket-mode)))

(def (detect-major-mode filename)
  "Detect major mode from filename using auto-mode-alist.
   Returns a symbol like 'python-mode or #f."
  (when (and (string? filename) (> (string-length filename) 0))
    (let ((basename (path-strip-directory filename)))
      ;; First check exact basename matches (Makefile, Dockerfile)
      (let loop ((alist *auto-mode-alist*))
        (cond
          ((null? alist) #f)
          ((string=? basename (caar alist))
           (cdar alist))
          ((string-suffix? (caar alist) filename)
           (cdar alist))
          (else (loop (cdr alist))))))))

;;;============================================================================
;;; Which-key (prefix key hints)
;;;============================================================================

;; When #t, show available keybindings after a prefix key + delay
(def *which-key-mode* #t)
(defvar! 'which-key-mode #t "Show available keybindings after prefix key delay"
         setter: (lambda (v) (set! *which-key-mode* v))
         type: 'boolean group: 'display)

;; Delay in seconds before showing which-key hints (applies to both TUI and Qt)
(def *which-key-delay* 0.5)
(defvar! 'which-key-delay 0.5 "Seconds to wait before showing prefix key hints"
         setter: (lambda (v) (set! *which-key-delay* v))
         type: 'number group: 'display)

(def (which-key-summary keymap (max-entries 20))
  "Generate a summary string of available keys in a keymap.
   Returns a string like 'C-s → Save buffer  C-f → Find file  b → Switch to buffer'."
  (let* ((entries (hash->list keymap))
         (sorted (sort entries (lambda (a b) (string<? (car a) (car b)))))
         (items
           (let loop ((es sorted) (acc []) (n 0))
             (cond
               ((null? es) (reverse acc))
               ((>= n max-entries) (reverse (cons "..." acc)))
               (else
                 (let* ((key (caar es))
                        (val (cdar es))
                        (desc (cond
                                ((hash-table? val) "+prefix")
                                ((symbol? val) (command-name->description val))
                                (else "?"))))
                   (loop (cdr es)
                         (cons (string-append key " → " desc) acc)
                         (+ n 1))))))))
    (string-join items "  ")))

;;;============================================================================
;;; Scroll margin
;;;============================================================================

;; Number of lines to keep visible above/below the cursor
(def *scroll-margin* 3)
(defvar! 'scroll-margin 3 "Lines of margin at top/bottom when scrolling"
         setter: (lambda (v) (set! *scroll-margin* v))
         type: 'integer type-args: '(0 . 20) group: 'display)

;;;============================================================================
;;; Persistent scratch buffer
;;;============================================================================

(def *scratch-file* ".gemacs-scratch")

(def (scratch-save! text)
  "Save scratch buffer content to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *scratch-file*)
        (lambda (port)
          (display text port))))))

(def (scratch-load!)
  "Load scratch buffer content from disk. Returns string or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((path (persist-path *scratch-file*)))
        (if (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (read-line port #f)))  ;; read entire file
          #f)))))

;;;============================================================================
;;; Theme and Font Persistence
;;;============================================================================

(def *theme-settings-file* ".gemacs-theme")

(def (theme-settings-save! theme-name font-family font-size)
  "Save current theme and font settings to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *theme-settings-file*)
        (lambda (port)
          (displayln (string-append "theme:" (symbol->string theme-name)) port)
          (displayln (string-append "font-family:" font-family) port)
          (displayln (string-append "font-size:" (number->string font-size)) port))))))

(def (theme-settings-load!)
  "Load theme and font settings from disk. Returns (values theme-name font-family font-size) or (values #f #f #f)."
  (with-catch
    (lambda (e) (values #f #f #f))
    (lambda ()
      (let ((path (persist-path *theme-settings-file*)))
        (if (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (let* ((theme-line (read-line port))
                     (font-family-line (read-line port))
                     (font-size-line (read-line port))
                     (parse-line (lambda (line prefix)
                                   (and (string? line)
                                        (string-prefix? prefix line)
                                        (substring line (string-length prefix) (string-length line))))))
                (values
                  (let ((theme-str (parse-line theme-line "theme:")))
                    (and theme-str (string->symbol theme-str)))
                  (parse-line font-family-line "font-family:")
                  (let ((size-str (parse-line font-size-line "font-size:")))
                    (and size-str (string->number size-str)))))))
          (values #f #f #f))))))

;;;============================================================================
;;; Custom Face Persistence
;;;============================================================================

(def *custom-faces-file* ".gemacs-custom-faces")
(def *custom-faces* (make-hash-table-eq))  ;; face-name -> customizations

(def (custom-faces-save!)
  "Save custom face overrides to disk.
   Format: face-name	fg:#hex	bg:#hex	bold:true/false	italic:true/false
   Only saves faces that have been customized (tracked in *custom-faces*)."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *custom-faces-file*)
        (lambda (port)
          (hash-for-each
            (lambda (face-name customizations)
              (let ((line (string-append (symbol->string face-name))))
                (when (hash-key? customizations 'fg)
                  (set! line (string-append line "\tfg:" (hash-get customizations 'fg))))
                (when (hash-key? customizations 'bg)
                  (set! line (string-append line "\tbg:" (hash-get customizations 'bg))))
                (when (hash-key? customizations 'bold)
                  (set! line (string-append line "\tbold:"
                                            (if (hash-get customizations 'bold) "true" "false"))))
                (when (hash-key? customizations 'italic)
                  (set! line (string-append line "\titalic:"
                                            (if (hash-get customizations 'italic) "true" "false"))))
                (displayln line port)))
            *custom-faces*))))))

(def (custom-faces-load!)
  "Load custom face overrides from disk and apply them.
   Custom faces overlay theme faces — theme provides defaults, user customizations override."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((path (persist-path *custom-faces-file*)))
        (when (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (let loop ()
                (let ((line (read-line port)))
                  (unless (eof-object? line)
                    (let* ((parts (string-split line #\tab))
                           (face-name (and (pair? parts) (string->symbol (car parts))))
                           (attrs (cdr parts)))
                      (when face-name
                        (let ((customizations (make-hash-table-eq)))
                          (for-each
                            (lambda (attr-str)
                              (let ((colon-idx (string-index attr-str #\:)))
                                (when colon-idx
                                  (let ((key (string->symbol (substring attr-str 0 colon-idx)))
                                        (val (substring attr-str (+ colon-idx 1) (string-length attr-str))))
                                    (cond
                                      ((eq? key 'fg) (hash-put! customizations 'fg val))
                                      ((eq? key 'bg) (hash-put! customizations 'bg val))
                                      ((eq? key 'bold) (hash-put! customizations 'bold (string=? val "true")))
                                      ((eq? key 'italic) (hash-put! customizations 'italic (string=? val "true"))))))))
                            attrs)
                          ;; Store customizations for saving later
                          (hash-put! *custom-faces* face-name customizations)
                          ;; Apply customizations to the face
                          (when (hash-key? customizations 'fg)
                            (set-face-attribute! face-name fg: (hash-get customizations 'fg)))
                          (when (hash-key? customizations 'bg)
                            (set-face-attribute! face-name bg: (hash-get customizations 'bg)))
                          (when (hash-key? customizations 'bold)
                            (set-face-attribute! face-name bold: (hash-get customizations 'bold)))
                          (when (hash-key? customizations 'italic)
                            (set-face-attribute! face-name italic: (hash-get customizations 'italic))))))
                    (loop)))))))))))

(def (record-face-customization! face-name . attrs)
  "Record that a face has been customized, so it can be saved to disk.
   attrs are keyword pairs: fg: bg: bold: italic:"
  (let ((customizations (or (hash-get *custom-faces* face-name)
                            (let ((h (make-hash-table-eq)))
                              (hash-put! *custom-faces* face-name h)
                              h))))
    (let loop ((attrs attrs))
      (when (pair? attrs)
        (let ((key (car attrs)))
          (when (and (pair? (cdr attrs)) (keyword? key))
            (let ((val (cadr attrs)))
              (case key
                ((fg:) (hash-put! customizations 'fg val))
                ((bg:) (hash-put! customizations 'bg val))
                ((bold:) (hash-put! customizations 'bold val))
                ((italic:) (hash-put! customizations 'italic val))))
            (loop (cddr attrs))))))))

;;;============================================================================
;;; Init file
;;;============================================================================

(def *init-file-path*
  (path-expand ".gemacs-init" (user-info-home (user-info (user-name)))))

(def (init-file-load!)
  "Load init file and apply settings.
   Format: one setting per line, KEY VALUE (space-separated).
   Lines starting with ; or # are comments.
   Supported settings: scroll-margin, save-place,
   delete-trailing-whitespace-on-save, require-final-newline,
   centered-cursor, bind KEY COMMAND, unbind KEY,
   chord AB COMMAND, key-translate FROM TO,
   chord-mode true/false, chord-timeout MILLIS."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *init-file-path*)
        (call-with-input-file *init-file-path*
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (unless (eof-object? line)
                  (let ((trimmed (string-trim-both line)))
                    (when (and (> (string-length trimmed) 0)
                               (not (char=? (string-ref trimmed 0) (integer->char 59)))   ;; semicolon
                               (not (char=? (string-ref trimmed 0) (integer->char 35))))  ;; hash
                      ;; Parse "key value" pairs
                      (let ((space-idx (string-index trimmed #\space)))
                        (when space-idx
                          (let ((key (substring trimmed 0 space-idx))
                                (val (string-trim-both (substring trimmed (+ space-idx 1)
                                                                  (string-length trimmed)))))
                            (cond
                              ((string=? key "scroll-margin")
                               (let ((n (string->number val)))
                                 (when (and n (>= n 0) (<= n 20))
                                   (custom-set! 'scroll-margin n))))
                              ((string=? key "save-place")
                               (custom-set! 'save-place
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "delete-trailing-whitespace-on-save")
                               (custom-set! 'delete-trailing-whitespace-on-save
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "require-final-newline")
                               (custom-set! 'require-final-newline
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "centered-cursor")
                               (custom-set! 'centered-cursor
                                 (or (string=? val "true") (string=? val "1"))))
                              ;; Custom keybinding: bind KEY COMMAND
                              ;; e.g. "bind C-c a align-regexp"
                              ((string=? key "bind")
                               (let ((sp2 (string-index val #\space)))
                                 (when sp2
                                   (let ((key-str (substring val 0 sp2))
                                         (cmd-str (string-trim-both
                                                    (substring val (+ sp2 1) (string-length val)))))
                                     (when (> (string-length cmd-str) 0)
                                       (keymap-bind! *global-keymap* key-str
                                         (string->symbol cmd-str)))))))
                              ;; Unbind: unbind KEY
                              ;; e.g. "unbind <f12>"
                              ((string=? key "unbind")
                               (when (> (string-length val) 0)
                                 (hash-remove! *global-keymap* val)))
                              ;; Key chord: chord AB command
                              ;; e.g. "chord EE eshell"
                              ((string=? key "chord")
                               (let ((sp2 (string-index val #\space)))
                                 (when sp2
                                   (let ((chord-str (substring val 0 sp2))
                                         (cmd-str (string-trim-both
                                                    (substring val (+ sp2 1) (string-length val)))))
                                     (when (and (= (string-length chord-str) 2)
                                                (> (string-length cmd-str) 0))
                                       (key-chord-define-global chord-str
                                         (string->symbol cmd-str)))))))
                              ;; Key translation: key-translate FROM TO
                              ;; e.g. "key-translate ( ["
                              ((string=? key "key-translate")
                               (let ((sp2 (string-index val #\space)))
                                 (when sp2
                                   (let ((from-str (substring val 0 sp2))
                                         (to-str (string-trim-both
                                                   (substring val (+ sp2 1) (string-length val)))))
                                     (when (and (= (string-length from-str) 1)
                                                (= (string-length to-str) 1))
                                       (key-translate! (string-ref from-str 0)
                                                       (string-ref to-str 0)))))))
                              ;; Chord mode toggle: chord-mode true/false
                              ((string=? key "chord-mode")
                               (custom-set! 'chord-mode
                                 (or (string=? val "true") (string=? val "1"))))
                              ;; Chord timeout: chord-timeout MILLIS
                              ((string=? key "chord-timeout")
                               (let ((n (string->number val)))
                                 (when (and n (> n 0) (<= n 2000))
                                   (custom-set! 'chord-timeout n))))
                              ;; LSP server command: lsp-server-command PATH
                              ;; e.g. "lsp-server-command /home/user/gerbil-lsp/.gerbil/bin/gerbil-lsp"
                              ((string=? key "lsp-server-command")
                               (when (> (string-length val) 0)
                                 (custom-set! 'lsp-server-command val)))
                              ))))))
                  (loop))))))))))

;;;============================================================================
;;; Save-place: remember cursor position per file
;;;============================================================================

(def *save-place-enabled* #t)
(defvar! 'save-place #t "Remember cursor position in previously visited files"
         setter: (lambda (v) (set! *save-place-enabled* v))
         type: 'boolean group: 'files)
(def *save-place-file* ".gemacs-places")
(def *save-place-alist* (make-hash-table)) ;; file-path -> position
(def *save-place-max* 500) ;; max entries to persist

(def (save-place-remember! file-path position)
  "Remember cursor position for a file."
  (when (and *save-place-enabled* (string? file-path) (> (string-length file-path) 0))
    (hash-put! *save-place-alist* file-path position)))

(def (save-place-restore file-path)
  "Get remembered cursor position for a file. Returns integer or #f."
  (when (and *save-place-enabled* (string? file-path))
    (hash-get *save-place-alist* file-path)))

(def (save-place-save!)
  "Save all remembered positions to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file (persist-path *save-place-file*)
        (lambda (port)
          (let ((entries (hash->list *save-place-alist*))
                (count 0))
            (for-each
              (lambda (pair)
                (when (< count *save-place-max*)
                  (display (car pair) port)
                  (display "\t" port)
                  (display (number->string (cdr pair)) port)
                  (newline port)
                  (set! count (+ count 1))))
              entries)))))))

(def (save-place-load!)
  "Load remembered positions from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((path (persist-path *save-place-file*)))
        (when (file-exists? path)
          (call-with-input-file path
            (lambda (port)
              (let loop ()
                (let ((line (read-line port)))
                  (unless (eof-object? line)
                    (let ((tab-idx (string-index line #\tab)))
                      (when tab-idx
                        (let* ((fpath (substring line 0 tab-idx))
                               (pos-str (substring line (+ tab-idx 1) (string-length line)))
                               (pos (string->number pos-str)))
                          (when (and pos (> (string-length fpath) 0))
                            (hash-put! *save-place-alist* fpath pos)))))
                    (loop)))))))))))

;;;============================================================================
;;; Clean-on-save settings
;;;============================================================================

(def *delete-trailing-whitespace-on-save* #t)
(defvar! 'delete-trailing-whitespace-on-save #t
         "Delete trailing whitespace when saving a file"
         setter: (lambda (v) (set! *delete-trailing-whitespace-on-save* v))
         type: 'boolean group: 'editing)
(def *require-final-newline* #t)
(defvar! 'require-final-newline #t
         "Ensure files end with a newline when saving"
         setter: (lambda (v) (set! *require-final-newline* v))
         type: 'boolean group: 'editing)

;;;============================================================================
;;; Centered cursor mode
;;;============================================================================

(def *centered-cursor-mode* #f)
(defvar! 'centered-cursor #f "Keep cursor centered vertically in the window"
         setter: (lambda (v) (set! *centered-cursor-mode* v))
         type: 'boolean group: 'display)

;;;============================================================================
;;; Auto-fill mode
;;;============================================================================

(def *auto-fill-mode* #f)
(defvar! 'auto-fill-mode #f "Automatically break lines at fill-column"
         setter: (lambda (v) (set! *auto-fill-mode* v))
         type: 'boolean group: 'editing)

(def *fill-column* 80)
(defvar! 'fill-column 80 "Column at which auto-fill wraps text"
         setter: (lambda (v) (set! *fill-column* v))
         type: 'integer type-args: '(20 . 200) group: 'editing)

;;;============================================================================
;;; Abbrev mode
;;;============================================================================

(def *abbrev-table* (make-hash-table))
(def *abbrev-mode-enabled* #t)
(defvar! 'abbrev-mode #t "Enable abbreviation expansion"
         setter: (lambda (v) (set! *abbrev-mode-enabled* v))
         type: 'boolean group: 'editing)

;;;============================================================================
;;; Enriched text mode
;;;============================================================================

(def *enriched-mode* #f)
(defvar! 'enriched-mode #f "Enable basic text formatting (bold/italic)"
         setter: (lambda (v) (set! *enriched-mode* v))
         type: 'boolean group: 'editing)

;;;============================================================================
;;; Picture mode
;;;============================================================================

(def *picture-mode* #f)
(defvar! 'picture-mode #f "Overwrite mode with directional cursor drawing"
         setter: (lambda (v) (set! *picture-mode* v))
         type: 'boolean group: 'editing)

;;;============================================================================
;;; Electric-pair mode
;;;============================================================================

(def *electric-pair-mode* #f)
(defvar! 'electric-pair-mode #f "Auto-insert matching delimiters (parens, brackets, quotes)"
         setter: (lambda (v) (set! *electric-pair-mode* v))
         type: 'boolean group: 'editing)

;;;============================================================================
;;; Copilot (AI inline completion) settings
;;;============================================================================

(def *copilot-mode* #f)
(defvar! 'copilot-mode #f "Enable AI inline code completion (requires OPENAI_API_KEY)"
         setter: (lambda (v) (set! *copilot-mode* v))
         type: 'boolean group: 'editing)

(def *copilot-api-key* (getenv "OPENAI_API_KEY" ""))
(def *copilot-model* "gpt-4o-mini")
(defvar! 'copilot-model "gpt-4o-mini" "OpenAI model for code completion"
         setter: (lambda (v) (set! *copilot-model* v))
         type: 'string group: 'editing)

(def *copilot-api-url* "https://api.openai.com/v1/chat/completions")

;; Current pending suggestion and position where it was generated
(def *copilot-suggestion* #f)   ;; string or #f
(def *copilot-suggestion-pos* 0) ;; cursor position when suggestion was requested
