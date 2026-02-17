;;; -*- Gerbil -*-
;;; Qt status bar modeline for gemacs
;;;
;;; Shows buffer name, line, column, modified indicator, mode,
;;; position percentage, and git branch in the main window's status bar.

(export qt-modeline-update!
        detect-eol-from-text
        *buffer-eol-cache*)

(import :std/sugar
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/qt/window)

;;;============================================================================
;;; Git branch detection (cached per directory)
;;;============================================================================

(def *git-branch-cache* (make-hash-table))
(def *git-branch-cache-time* (make-hash-table))
(def *git-cache-ttl* 5.0) ;; refresh every 5 seconds

(def (git-branch-for-file file-path)
  "Get current git branch for a file's directory, with caching."
  (if (not file-path) #f
    (let ((dir (path-directory file-path)))
      (let ((cached-time (hash-get *git-branch-cache-time* dir)))
        (if (and cached-time
                 (< (- (time->seconds (current-time)) cached-time) *git-cache-ttl*))
          (hash-get *git-branch-cache* dir)
          ;; Refresh cache
          (let ((branch (with-catch
                          (lambda (e) #f)
                          (lambda ()
                            (let* ((proc (open-process
                                          (list path: "/usr/bin/git"
                                                arguments: ["rev-parse" "--abbrev-ref" "HEAD"]
                                                directory: dir
                                                stdin-redirection: #f
                                                stdout-redirection: #t
                                                stderr-redirection: #t)))
                                   (result (read-line proc)))
                              (process-status proc)
                              (close-port proc)
                              (if (string? result) result #f))))))
            (hash-put! *git-branch-cache* dir branch)
            (hash-put! *git-branch-cache-time* dir (time->seconds (current-time)))
            branch))))))

;;;============================================================================
;;; Mode name from lexer-lang
;;;============================================================================

(def (mode-name-for-buffer buf)
  "Get a short mode name string for the buffer."
  (let ((lang (buffer-lexer-lang buf)))
    (case lang
      ((scheme gerbil) "Gerbil")
      ((lisp) "Lisp")
      ((python) "Python")
      ((c) "C")
      ((cpp) "C++")
      ((javascript) "JS")
      ((typescript) "TS")
      ((rust) "Rust")
      ((go) "Go")
      ((java) "Java")
      ((ruby) "Ruby")
      ((shell bash) "Shell")
      ((markdown) "Markdown")
      ((org) "Org")
      ((json) "JSON")
      ((yaml) "YAML")
      ((toml) "TOML")
      ((html xml) "HTML")
      ((css) "CSS")
      ((sql) "SQL")
      ((lua) "Lua")
      ((zig) "Zig")
      ((nix) "Nix")
      ((dired) "Dired")
      ((repl) "REPL")
      ((eshell) "Eshell")
      ((shell-mode) "Shell")
      ((terminal) "Term")
      (else "Text"))))

;;;============================================================================
;;; Line ending detection
;;;============================================================================

(def *buffer-eol-cache* (make-hash-table)) ;; buffer-name -> "LF"/"CRLF"/"CR"

(def (detect-eol-from-text text)
  "Detect line ending style from first newline in text."
  (let loop ((i 0))
    (if (>= i (string-length text))
      "LF" ;; default
      (let ((ch (string-ref text i)))
        (cond
          ((char=? ch #\return)
           (if (and (< (+ i 1) (string-length text))
                    (char=? (string-ref text (+ i 1)) #\newline))
             "CRLF"
             "CR"))
          ((char=? ch #\newline) "LF")
          (else (loop (+ i 1))))))))

(def (buffer-eol-indicator buf)
  "Get cached EOL indicator for a buffer."
  (or (hash-get *buffer-eol-cache* (buffer-name buf)) "LF"))

;;;============================================================================
;;; Modeline rendering
;;;============================================================================

(def (qt-modeline-update! app)
  "Update the status bar with current buffer info."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-edit-window-buffer win))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (col  (+ 1 (qt-plain-text-edit-cursor-column ed)))
         (total-lines (qt-plain-text-edit-line-count ed))
         (mod? (qt-text-document-modified? (buffer-doc-pointer buf)))
         (ro? (qt-plain-text-edit-read-only? ed))
         ;; Position percentage
         (pct (cond
                ((<= total-lines 1) "All")
                ((= line 1) "Top")
                ((= line total-lines) "Bot")
                (else (string-append
                        (number->string
                          (inexact->exact (round (* 100 (/ (- line 1)
                                                           (max 1 (- total-lines 1)))))))
                        "%"))))
         ;; Modified/read-only indicator
         (state-str (cond
                      ((and ro? mod?) "%*")
                      (ro? "%%")
                      (mod? "**")
                      (else "--")))
         ;; Mode name
         (mode (mode-name-for-buffer buf))
         ;; Line ending style
         (eol (buffer-eol-indicator buf))
         ;; Git branch
         (branch (git-branch-for-file (buffer-file-path buf)))
         ;; Build the modeline string
         (info (string-append
                 "-U:" state-str "-  "
                 (buffer-name buf) "    "
                 "L" (number->string line)
                 " C" (number->string col)
                 "  " pct
                 "  (" mode " " eol ")"
                 (if branch
                   (string-append "  " branch)
                   ""))))
    (qt-main-window-set-status-bar-text! (qt-frame-main-win fr) info)))
