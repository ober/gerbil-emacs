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
  which-key-summary

  ;; Scroll margin
  *scroll-margin*

  ;; Persistent scratch
  *scratch-file*
  scratch-save!
  scratch-load!

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

  ;; Persistence paths
  persist-path)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core)

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

(def (which-key-summary keymap (max-entries 20))
  "Generate a summary string of available keys in a keymap.
   Returns a string like 'C-s:save  C-f:find  b:switch  k:kill'."
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
                                ((symbol? val) (symbol->string val))
                                (else "?"))))
                   (loop (cdr es)
                         (cons (string-append key ":" desc) acc)
                         (+ n 1))))))))
    (string-join items "  ")))

;;;============================================================================
;;; Scroll margin
;;;============================================================================

;; Number of lines to keep visible above/below the cursor
(def *scroll-margin* 3)

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
                                   (set! *scroll-margin* n))))
                              ((string=? key "save-place")
                               (set! *save-place-enabled*
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "delete-trailing-whitespace-on-save")
                               (set! *delete-trailing-whitespace-on-save*
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "require-final-newline")
                               (set! *require-final-newline*
                                 (or (string=? val "true") (string=? val "1"))))
                              ((string=? key "centered-cursor")
                               (set! *centered-cursor-mode*
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
                               (set! *chord-mode*
                                 (or (string=? val "true") (string=? val "1"))))
                              ;; Chord timeout: chord-timeout MILLIS
                              ((string=? key "chord-timeout")
                               (let ((n (string->number val)))
                                 (when (and n (> n 0) (<= n 2000))
                                   (set! *chord-timeout* n))))
                              ;; LSP server command: lsp-server-command PATH
                              ;; e.g. "lsp-server-command /home/user/gerbil-lsp/.gerbil/bin/gerbil-lsp"
                              ((string=? key "lsp-server-command")
                               (when (> (string-length val) 0)
                                 (set! *lsp-server-command* val)))
                              ))))))
                  (loop))))))))))

;;;============================================================================
;;; Save-place: remember cursor position per file
;;;============================================================================

(def *save-place-enabled* #t)
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
(def *require-final-newline* #t)

;;;============================================================================
;;; Centered cursor mode
;;;============================================================================

(def *centered-cursor-mode* #f)
