;;; -*- Gerbil -*-
;;; Org-roam (real backlink scanning), artist-mode (ASCII drawing),
;;; calc-embedded (in-buffer evaluation), hl-todo highlighting,
;;; auto-revert, writeroom-mode, and rainbow-mode.
;;; New module to keep other editor-extra files under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        (only-in :gerbil-scintilla/ffi scintilla-send-message-string scintilla-editor-handle)
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers)

;;;============================================================================
;;; Org-roam — real Zettelkasten with backlink scanning
;;;============================================================================

;; Configurable notes directory
(def *org-roam-directory* (string-append (getenv "HOME") "/org-roam/"))
(def *org-roam-db* (make-hash-table))  ;; file -> (title . list-of-links)
(def *org-roam-backlinks* (make-hash-table))  ;; file -> list of files linking to it

(def (org-roam-ensure-dir!)
  "Create org-roam directory if it doesn't exist."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (when (not (file-exists? *org-roam-directory*))
        (create-directory *org-roam-directory*))
      #t)))

(def (org-roam-list-org-files)
  "List all .org files in the org-roam directory recursively."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let ((p (open-process
                 (list path: "find"
                       arguments: (list *org-roam-directory*
                                        "-name" "*.org" "-type" "f")
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (process-status p)
          (if (and out (> (string-length out) 0))
            (filter (lambda (s) (not (string=? s "")))
                    (string-split out #\newline))
            '()))))))

(def (org-roam-extract-title file)
  "Extract #+title: from an org file, or use filename as fallback."
  (with-exception-catcher
    (lambda (e) (path-strip-extension (path-strip-directory file)))
    (lambda ()
      (let ((p (open-process
                 (list path: "grep"
                       arguments: (list "-i" "-m1" "^#+title:" file)
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((line (read-line p)))
          (process-status p)
          (if (and (string? line) (> (string-length line) 8))
            (string-trim (substring line (+ 1 (string-index line #\:)) (string-length line)))
            (path-strip-extension (path-strip-directory file))))))))

(def (org-roam-extract-links file)
  "Extract [[...]] links from an org file. Returns list of link targets."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let ((p (open-process
                 (list path: "grep"
                       arguments: (list "-oP" "\\[\\[([^]]+)\\]" file)
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (process-status p)
          (if (and out (> (string-length out) 0))
            (map (lambda (s)
                   ;; Strip [[ and ] delimiters
                   (let ((s2 (string-trim s)))
                     (if (and (>= (string-length s2) 3)
                              (string-prefix? "[[" s2))
                       (substring s2 2 (- (string-length s2) 1))
                       s2)))
                 (filter (lambda (s) (not (string=? s "")))
                         (string-split out #\newline)))
            '()))))))

(def (org-roam-build-db!)
  "Scan all org files and build the backlink database."
  (let ((files (org-roam-list-org-files)))
    ;; Clear old data
    (set! *org-roam-db* (make-hash-table))
    (set! *org-roam-backlinks* (make-hash-table))
    ;; First pass: collect titles and links
    (for-each
      (lambda (file)
        (let ((title (org-roam-extract-title file))
              (links (org-roam-extract-links file)))
          (hash-put! *org-roam-db* file (cons title links))))
      files)
    ;; Second pass: build backlinks
    (hash-for-each
      (lambda (file entry)
        (let ((links (cdr entry)))
          (for-each
            (lambda (link)
              ;; Resolve link to a file path
              (let ((target (org-roam-resolve-link link)))
                (when target
                  (let ((existing (or (hash-get *org-roam-backlinks* target) '())))
                    (unless (member file existing)
                      (hash-put! *org-roam-backlinks* target
                                       (cons file existing)))))))
            links)))
      *org-roam-db*)
    (length files)))

(def (org-roam-resolve-link link)
  "Resolve a link target to a file path. Handles roam:, file:, and bare names."
  (let* ((target (cond
                   ((string-prefix? "roam:" link)
                    (substring link 5 (string-length link)))
                   ((string-prefix? "file:" link)
                    (substring link 5 (string-length link)))
                   ((string-prefix? "id:" link) #f)  ;; skip ID links for now
                   (else link))))
    (and target
         (let ((full (if (and (> (string-length target) 0)
                            (char=? (string-ref target 0) #\/))
                       target
                        (string-append *org-roam-directory* target))))
           ;; Try with and without .org extension
           (cond
             ((file-exists? full) full)
             ((file-exists? (string-append full ".org")) (string-append full ".org"))
             ;; Search by title match
             (else (org-roam-find-by-title target)))))))

(def (org-roam-find-by-title title)
  "Find a file whose #+title matches the given string."
  (let ((result #f))
    (hash-for-each
      (lambda (file entry)
        (when (and (not result)
                   (string-ci=? (car entry) title))
          (set! result file)))
      *org-roam-db*)
    result))

(def (org-roam-node-titles)
  "Return list of (title . file) pairs for all nodes."
  (let ((pairs '()))
    (hash-for-each
      (lambda (file entry)
        (set! pairs (cons (cons (car entry) file) pairs)))
      *org-roam-db*)
    (sort pairs (lambda (a b) (string<? (car a) (car b))))))

;; Helper to open a file by path
(def (org-roam-open-file! app file)
  "Open a file by path in the editor."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (name (path-strip-directory file))
         (buf (or (buffer-by-name name)
                  (buffer-create! name ed file))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (when (file-exists? file)
      (let ((text (read-file-as-string file)))
        (when text
          (editor-set-text ed text)
          (editor-set-save-point ed))))))

;; --- Commands ---

(def (cmd-org-roam-node-find app)
  "Find and open an org-roam node with completion."
  (org-roam-ensure-dir!)
  (org-roam-build-db!)
  (let ((nodes (org-roam-node-titles)))
    (if (null? nodes)
      (echo-message! (app-state-echo app) "No org-roam nodes found. Create one with org-roam-capture.")
      (let ((query (app-read-string app
                     (string-append "Find node (" (number->string (length nodes)) " nodes): "))))
        (when (and query (not (string-empty? query)))
          (let ((match (find (lambda (pair)
                               (string-contains-ci (car pair) query))
                             nodes)))
            (if match
              (begin
                (org-roam-open-file! app (cdr match))
                (echo-message! (app-state-echo app) (string-append "Opened: " (car match))))
              ;; No match — offer to create new node
              (let ((new-file (string-append *org-roam-directory*
                                             (string-downcase (string-map-chars
                                               (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\-))
                                               query))
                                             ".org")))
                (org-roam-create-node! app new-file query)))))))))

(def (string-map-chars f str)
  "Map function over string characters, returning new string."
  (list->string (map f (string->list str))))

(def (org-roam-create-node! app file title)
  "Create a new org-roam node with the given title."
  (org-roam-ensure-dir!)
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Failed to create node"))
    (lambda ()
      (with-output-to-file file
        (lambda ()
          (display (string-append "#+title: " title "\n"))
          (display (string-append "#+date: " (current-date-string) "\n"))
          (display "#+filetags:\n\n")))
      (org-roam-open-file! app file)
      (echo-message! (app-state-echo app) (string-append "Created node: " title)))))

(def (current-date-string)
  "Return current date as YYYY-MM-DD."
  (with-exception-catcher
    (lambda (e) "2026-03-28")
    (lambda ()
      (let ((p (open-process
                 (list path: "date"
                       arguments: (list "+%Y-%m-%d")
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p)))
          (process-status p)
          (if (string? out) (string-trim out) "2026-03-28"))))))

(def (cmd-org-roam-node-insert app)
  "Insert a link to an org-roam node at point."
  (org-roam-build-db!)
  (let ((nodes (org-roam-node-titles)))
    (if (null? nodes)
      (echo-message! (app-state-echo app) "No org-roam nodes found")
      (let ((query (app-read-string app "Insert link to node: ")))
        (when (and query (not (string-empty? query)))
          (let ((match (find (lambda (pair)
                               (string-contains-ci (car pair) query))
                             nodes)))
            (when match
              (let* ((fr (app-state-frame app))
                     (win (current-window fr))
                     (ed (edit-window-editor win))
                     (pos (editor-get-current-pos ed))
                     (link (string-append "[[roam:" (car match) "]]")))
                (editor-insert-text ed pos link)
                (echo-message! (app-state-echo app)
                  (string-append "Linked to: " (car match)))))))))))

(def (cmd-org-roam-buffer-toggle app)
  "Toggle backlinks buffer for current file."
  (org-roam-build-db!)
  (let* ((buf (current-buffer-from-app app))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! (app-state-echo app) "Buffer has no file")
      (let ((backlinks (or (hash-get *org-roam-backlinks* file) '())))
        (if (null? backlinks)
          ;; Also search by title
          (let* ((title (org-roam-extract-title file))
                 (title-backlinks (org-roam-find-backlinks-by-title title)))
            (if (null? title-backlinks)
              (echo-message! (app-state-echo app)
                (string-append "No backlinks for: "
                  (or (buffer-name buf) (path-strip-directory file))))
              (org-roam-show-backlinks app file title-backlinks)))
          (org-roam-show-backlinks app file backlinks))))))

(def (org-roam-find-backlinks-by-title title)
  "Find files that link to the given title (by searching link targets)."
  (let ((results '()))
    (hash-for-each
      (lambda (file entry)
        (let ((links (cdr entry)))
          (when (find (lambda (link) (string-ci=? link title)) links)
            (set! results (cons file results)))))
      *org-roam-db*)
    results))

(def (org-roam-show-backlinks app file backlinks)
  "Display backlinks in a buffer."
  (let* ((title (org-roam-extract-title file))
         (out (string-append
                "Backlinks for: " title "\n"
                (make-string 40 #\=) "\n\n"
                (apply string-append
                  (map (lambda (bl)
                         (let ((bl-title (org-roam-extract-title bl)))
                           (string-append "  - " bl-title "\n"
                                          "    " bl "\n\n")))
                       backlinks)))))
    (open-output-buffer app "*Org-roam Backlinks*" out)))

(def (cmd-org-roam-capture app)
  "Create a new org-roam note (capture)."
  (org-roam-ensure-dir!)
  (let ((title (app-read-string app "New note title: ")))
    (when (and title (not (string-empty? title)))
      (let ((file (string-append *org-roam-directory*
                                 (string-map-chars
                                   (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\-))
                                   (string-downcase title))
                                 ".org")))
        (if (file-exists? file)
          (begin
            (org-roam-open-file! app file)
            (echo-message! (app-state-echo app) "Opened existing note"))
          (org-roam-create-node! app file title))))))

(def (cmd-org-roam-graph app)
  "Show a text representation of the org-roam link graph."
  (org-roam-build-db!)
  (let* ((nodes (org-roam-node-titles))
         (lines (map (lambda (pair)
                       (let* ((file (cdr pair))
                              (title (car pair))
                              (entry (or (hash-get *org-roam-db* file) '("" . ())))
                              (links (cdr entry))
                              (backlinks (or (hash-get *org-roam-backlinks* file) '())))
                         (string-append
                           title "\n"
                           "  -> " (number->string (length links)) " outgoing links"
                           ", " (number->string (length backlinks)) " backlinks\n"
                           (if (null? links) ""
                             (string-append
                               (apply string-append
                                 (map (lambda (l) (string-append "     -> " l "\n")) links))))
                           "\n")))
                     nodes))
         (out (string-append "Org-roam Graph (" (number->string (length nodes)) " nodes)\n"
                (make-string 50 #\=) "\n\n"
                (apply string-append lines))))
    (open-output-buffer app "*Org-roam Graph*" out)))

(def (cmd-org-roam-set-directory app)
  "Set the org-roam directory."
  (let ((dir (app-read-string app (string-append "Org-roam directory [" *org-roam-directory* "]: "))))
    (when (and dir (not (string-empty? dir)))
      (set! *org-roam-directory*
            (if (string-suffix? "/" dir) dir (string-append dir "/")))
      (echo-message! (app-state-echo app) (string-append "Org-roam directory: " *org-roam-directory*)))))

(def (cmd-org-roam-db-sync app)
  "Rebuild the org-roam database."
  (org-roam-ensure-dir!)
  (let ((count (org-roam-build-db!)))
    (echo-message! (app-state-echo app)
      (string-append "Org-roam: indexed " (number->string count) " nodes"))))

(def (cmd-org-roam-find-file app)
  "Alias for org-roam-node-find."
  (cmd-org-roam-node-find app))

(def (cmd-org-roam-dailies-today app)
  "Open or create today's daily note."
  (org-roam-ensure-dir!)
  (let* ((date (current-date-string))
         (dailies-dir (string-append *org-roam-directory* "daily/"))
         (file (string-append dailies-dir date ".org")))
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (when (not (file-exists? dailies-dir))
          (create-directory dailies-dir))))
    (if (file-exists? file)
      (begin
        (org-roam-open-file! app file)
        (echo-message! (app-state-echo app) (string-append "Daily: " date)))
      (begin
        (with-output-to-file file
          (lambda ()
            (display (string-append "#+title: " date "\n"))
            (display (string-append "#+date: " date "\n"))
            (display "#+filetags: :daily:\n\n* Journal\n\n")))
        (org-roam-open-file! app file)
        (echo-message! (app-state-echo app) (string-append "Created daily: " date))))))

;;;============================================================================
;;; Artist mode — real ASCII drawing
;;;============================================================================

(def *artist-mode-active* #f)
(def *artist-draw-char* #\+)
(def *artist-tool* 'line)  ;; line, rectangle, text, erase, freehand

(def (cmd-artist-mode app)
  "Toggle artist mode for ASCII art drawing."
  (set! *artist-mode-active* (not *artist-mode-active*))
  (echo-message! (app-state-echo app)
    (if *artist-mode-active*
      (string-append "Artist mode ON — tool: " (symbol->string *artist-tool*)
                     " char: " (string *artist-draw-char*))
      "Artist mode OFF")))

(def (cmd-artist-select-tool app)
  "Select artist drawing tool."
  (let ((tool (app-read-string app "Tool (line/rect/text/erase/freehand): ")))
    (when (and tool (not (string-empty? tool)))
      (let ((sym (string->symbol tool)))
        (when (memq sym '(line rect rectangle text erase freehand))
          (set! *artist-tool* (if (eq? sym 'rect) 'rectangle sym))
          (echo-message! (app-state-echo app)
            (string-append "Artist tool: " (symbol->string *artist-tool*))))))))

(def (cmd-artist-set-char app)
  "Set the drawing character for artist mode."
  (let ((ch (app-read-string app (string-append "Draw char [" (string *artist-draw-char*) "]: "))))
    (when (and ch (= (string-length ch) 1))
      (set! *artist-draw-char* (string-ref ch 0))
      (echo-message! (app-state-echo app) (string-append "Draw char: " (string *artist-draw-char*))))))

(def (artist-ensure-line-length! ed line min-col)
  "Ensure a line has at least min-col characters by padding with spaces."
  (let* ((line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (line-len (- line-end line-start)))
    (when (< line-len min-col)
      (editor-insert-text ed line-end (make-string (- min-col line-len) #\space)))))

(def (artist-set-char-at! ed line col ch)
  "Set the character at line/col to ch, padding if needed."
  (artist-ensure-line-length! ed line (+ col 1))
  (let* ((line-start (editor-position-from-line ed line))
         (pos (+ line-start col)))
    ;; Replace one char
    (send-message ed SCI_SETTARGETSTART pos 0)
    (send-message ed SCI_SETTARGETEND (+ pos 1) 0)
    (scintilla-send-message-string (scintilla-editor-handle ed) SCI_REPLACETARGET -1 (string ch))))

(def (cmd-artist-draw-line app)
  "Draw a line from point to a target position. Uses +, -, | characters."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos1 (editor-get-current-pos ed))
           (line1 (editor-line-from-position ed pos1))
           (col1 (- pos1 (editor-position-from-line ed line1))))
      (let ((input (app-read-string app "Line to (line,col): ")))
        (when (and input (string-contains input ","))
          (let* ((parts (string-split input #\,))
                 (line2 (string->number (string-trim (car parts))))
                 (col2 (string->number (string-trim (cadr parts)))))
            (when (and line2 col2)
              (send-message ed SCI_BEGINUNDOACTION)
              (artist-draw-line! ed line1 col1 line2 col2)
              (send-message ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Line drawn"))))))))

(def (artist-draw-line! ed r1 c1 r2 c2)
  "Draw a line from (r1,c1) to (r2,c2) using ASCII characters."
  (let ((dr (- r2 r1))
        (dc (- c2 c1)))
    (cond
      ;; Horizontal line
      ((= dr 0)
       (let ((start (min c1 c2))
             (end (max c1 c2)))
         (let loop ((c start))
           (when (<= c end)
             (artist-set-char-at! ed r1 c #\-)
             (loop (+ c 1))))))
      ;; Vertical line
      ((= dc 0)
       (let ((start (min r1 r2))
             (end (max r1 r2)))
         (let loop ((r start))
           (when (<= r end)
             (artist-set-char-at! ed r c1 #\|)
             (loop (+ r 1))))))
      ;; Diagonal or general — use Bresenham
      (else
       (let* ((steps (max (abs dr) (abs dc))))
         (let loop ((i 0))
           (when (<= i steps)
             (let ((r (+ r1 (inexact->exact (round (* dr (/ i steps))))))
                   (c (+ c1 (inexact->exact (round (* dc (/ i steps)))))))
               (let ((ch (cond
                           ((= (abs dr) (abs dc)) (if (> (abs dr) 0) #\\ #\/))
                           ((> (abs dc) (abs dr)) #\-)
                           (else #\|))))
                 (artist-set-char-at! ed r c ch)))
             (loop (+ i 1)))))))))

(def (cmd-artist-draw-rectangle app)
  "Draw an ASCII rectangle. Prompts for width and height."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos (editor-get-current-pos ed))
           (line (editor-line-from-position ed pos))
           (col (- pos (editor-position-from-line ed line))))
      (let ((dims (app-read-string app "Rectangle (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 1) (> h 1))
              (send-message ed SCI_BEGINUNDOACTION)
              ;; Top edge
              (let loop ((c col)) (when (< c (+ col w))
                (artist-set-char-at! ed line c #\-) (loop (+ c 1))))
              ;; Bottom edge
              (let loop ((c col)) (when (< c (+ col w))
                (artist-set-char-at! ed (+ line h -1) c #\-) (loop (+ c 1))))
              ;; Left edge
              (let loop ((r line)) (when (< r (+ line h))
                (artist-set-char-at! ed r col #\|) (loop (+ r 1))))
              ;; Right edge
              (let loop ((r line)) (when (< r (+ line h))
                (artist-set-char-at! ed r (+ col w -1) #\|) (loop (+ r 1))))
              ;; Corners
              (artist-set-char-at! ed line col #\+)
              (artist-set-char-at! ed line (+ col w -1) #\+)
              (artist-set-char-at! ed (+ line h -1) col #\+)
              (artist-set-char-at! ed (+ line h -1) (+ col w -1) #\+)
              (send-message ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app)
                (string-append "Rectangle: " (number->string w) "x" (number->string h))))))))))

(def (cmd-artist-draw-text app)
  "Insert text at current position (horizontal) in artist mode."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos (editor-get-current-pos ed))
           (line (editor-line-from-position ed pos))
           (col (- pos (editor-position-from-line ed line))))
      (let ((text (app-read-string app "Text: ")))
        (when (and text (not (string-empty? text)))
          (send-message ed SCI_BEGINUNDOACTION)
          (let loop ((i 0))
            (when (< i (string-length text))
              (artist-set-char-at! ed line (+ col i) (string-ref text i))
              (loop (+ i 1))))
          (send-message ed SCI_ENDUNDOACTION)
          (echo-message! (app-state-echo app) "Text drawn"))))))

(def (cmd-artist-erase-rect app)
  "Erase a rectangular region to spaces."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos (editor-get-current-pos ed))
           (line (editor-line-from-position ed pos))
           (col (- pos (editor-position-from-line ed line))))
      (let ((dims (app-read-string app "Erase rect (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 0) (> h 0))
              (send-message ed SCI_BEGINUNDOACTION)
              (let rloop ((r 0))
                (when (< r h)
                  (let cloop ((c 0))
                    (when (< c w)
                      (artist-set-char-at! ed (+ line r) (+ col c) #\space)
                      (cloop (+ c 1))))
                  (rloop (+ r 1))))
              (send-message ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Erased"))))))))

(def (cmd-artist-draw-arrow app)
  "Draw an ASCII arrow. Prompts for direction (up/down/left/right) and length."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos (editor-get-current-pos ed))
           (line (editor-line-from-position ed pos))
           (col (- pos (editor-position-from-line ed line))))
      (let ((input (app-read-string app "Arrow (direction length), e.g. right 10: ")))
        (when (and input (string-contains input " "))
          (let* ((parts (string-split input #\space))
                 (dir (string-trim (car parts)))
                 (len (string->number (string-trim (cadr parts)))))
            (when (and len (> len 0))
              (send-message ed SCI_BEGINUNDOACTION)
              (cond
                ((string=? dir "right")
                 (let loop ((c 0))
                   (when (< c (- len 1))
                     (artist-set-char-at! ed line (+ col c) #\-)
                     (loop (+ c 1))))
                 (artist-set-char-at! ed line (+ col len -1) #\>))
                ((string=? dir "left")
                 (artist-set-char-at! ed line col #\<)
                 (let loop ((c 1))
                   (when (< c len)
                     (artist-set-char-at! ed line (+ col c) #\-)
                     (loop (+ c 1)))))
                ((string=? dir "down")
                 (let loop ((r 0))
                   (when (< r (- len 1))
                     (artist-set-char-at! ed (+ line r) col #\|)
                     (loop (+ r 1))))
                 (artist-set-char-at! ed (+ line len -1) col #\v))
                ((string=? dir "up")
                 (artist-set-char-at! ed line col #\^)
                 (let loop ((r 1))
                   (when (< r len)
                     (artist-set-char-at! ed (+ line r) col #\|)
                     (loop (+ r 1))))))
              (send-message ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Arrow drawn"))))))))

(def (cmd-artist-draw-ellipse app)
  "Draw an ASCII ellipse. Prompts for width and height."
  (when *artist-mode-active*
    (let* ((ed (current-editor app))
           (pos (editor-get-current-pos ed))
           (line (editor-line-from-position ed pos))
           (col (- pos (editor-position-from-line ed line))))
      (let ((dims (app-read-string app "Ellipse (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 3) (> h 2))
              (send-message ed SCI_BEGINUNDOACTION)
              (let* ((rx (/ (- w 1) 2.0))
                     (ry (/ (- h 1) 2.0))
                     (cx (+ col (inexact->exact (round rx))))
                     (cy (+ line (inexact->exact (round ry)))))
                ;; Draw ellipse boundary points
                (let loop ((angle 0.0))
                  (when (< angle 6.30)
                    (let* ((x (inexact->exact (round (+ cx (* rx (cos angle))))))
                           (y (inexact->exact (round (+ cy (* ry (sin angle)))))))
                      (artist-set-char-at! ed y x
                        (cond
                          ((< (abs (sin angle)) 0.3) #\-)
                          ((< (abs (cos angle)) 0.3) #\|)
                          ((or (and (> (sin angle) 0) (> (cos angle) 0))
                               (and (< (sin angle) 0) (< (cos angle) 0))) #\\)
                          (else #\/))))
                    (loop (+ angle 0.1)))))
              (send-message ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app)
                (string-append "Ellipse: " (number->string w) "x" (number->string h))))))))))

;;;============================================================================
;;; Calc embedded — evaluate expressions in-buffer
;;;============================================================================

(def (cmd-calc-embedded app)
  "Evaluate math expression at point or in region, replacing with result."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection — try to find expression on current line
      (let* ((line (editor-line-from-position ed sel-start))
             (line-start (editor-position-from-line ed line))
             (line-end (editor-get-line-end-position ed line))
             (text (editor-get-line ed line)))
        ;; Look for pattern: expression = or just expression
        (let ((expr (string-trim text)))
          (when (not (string-empty? expr))
            (let ((result (calc-eval-expr expr)))
              (if result
                ;; Append " = result" at end of line if not already there
                (if (string-contains expr "=")
                  ;; Replace after = sign
                  (let* ((eq-pos (string-index expr #\=))
                         (prefix (substring expr 0 (+ eq-pos 1)))
                         (new-text (string-append prefix " " result)))
                    (send-message ed SCI_SETTARGETSTART line-start 0)
                    (send-message ed SCI_SETTARGETEND line-end 0)
                    (scintilla-send-message-string (scintilla-editor-handle ed) SCI_REPLACETARGET -1 new-text)
                    (echo-message! (app-state-echo app) (string-append "= " result)))
                  ;; Append = result
                  (begin
                    (editor-insert-text ed line-end (string-append " = " result))
                    (echo-message! (app-state-echo app) (string-append "= " result))))
                (echo-error! (app-state-echo app) "Could not evaluate expression"))))))
      ;; Has selection — evaluate it
      (let* ((len (- sel-end sel-start))
             (text (editor-get-selection-text ed)))
        (let ((result (calc-eval-expr (string-trim text))))
          (if result
            (begin
              (send-message ed SCI_SETTARGETSTART sel-start 0)
              (send-message ed SCI_SETTARGETEND sel-end 0)
              (scintilla-send-message-string (scintilla-editor-handle ed) SCI_REPLACETARGET -1 result)
              (echo-message! (app-state-echo app) (string-append "= " result)))
            (echo-error! (app-state-echo app) "Could not evaluate expression")))))))

(def (calc-eval-expr expr)
  "Evaluate a math expression string. Returns result as string or #f."
  ;; Strip trailing = and anything after it
  (let* ((clean (let ((eq-pos (string-index-opt expr #\=)))
                  (if eq-pos (string-trim (substring expr 0 eq-pos)) expr)))
         (clean (string-trim clean)))
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        ;; Use bc for evaluation (handles float, trig, etc.)
        (let ((p (open-process
                   (list path: "bc"
                         arguments: (list "-l")
                         stdin-redirection: #t stdout-redirection: #t
                         stderr-redirection: #t))))
          (display (string-append clean "\n") p)
          (force-output p)
          (close-output-port p)
          (let ((result (read-line p)))
            (process-status p)
            (if (and (string? result) (not (string=? result "")))
              (string-trim result)
              #f)))))))

(def (string-index-opt str ch)
  "Return index of ch in str, or #f if not found."
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

(def (cmd-calc-eval-line app)
  "Evaluate expression on current line and show result in echo area."
  (let* ((ed (current-editor app))
         (line (editor-line-from-position ed (editor-get-current-pos ed)))
         (text (string-trim (editor-get-line ed line))))
    (if (string-empty? text)
      (echo-message! (app-state-echo app) "Empty line")
      (let ((result (calc-eval-expr text)))
        (if result
          (echo-message! (app-state-echo app) (string-append text " = " result))
          (echo-error! (app-state-echo app) "Could not evaluate"))))))

(def (cmd-calc-embedded-eval-region app)
  "Evaluate each line in region as math, appending = result to each."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((line1 (editor-line-from-position ed sel-start))
             (line2 (editor-line-from-position ed sel-end))
             (count 0))
        (send-message ed SCI_BEGINUNDOACTION)
        (let loop ((line line2))  ;; Process bottom-up to avoid position shifts
          (when (>= line line1)
            (let* ((line-start (editor-position-from-line ed line))
                   (line-end (editor-get-line-end-position ed line))
                   (text (string-trim (editor-get-line ed line))))
              (when (and (not (string-empty? text))
                         (not (string-contains text "=")))
                (let ((result (calc-eval-expr text)))
                  (when result
                    (editor-insert-text ed line-end (string-append " = " result))
                    (set! count (+ count 1))))))
            (loop (- line 1))))
        (send-message ed SCI_ENDUNDOACTION)
        (echo-message! (app-state-echo app)
          (string-append "Evaluated " (number->string count) " expressions"))))))

(def (cmd-calc-grab-region app)
  "Grab numbers from region and push onto calc stack."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-selection-text ed))
             (nums (filter string->number
                           (map string-trim
                                (string-split text #\newline)))))
        (echo-message! (app-state-echo app)
          (string-append "Found " (number->string (length nums)) " numbers: "
                         (string-join (map number->string (map string->number nums)) ", ")))))))

(def (cmd-calc-sum-region app)
  "Sum all numbers in the selected region."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (editor-get-selection-text ed))
             ;; Extract all numbers from text
             (p (open-process
                  (list path: "grep"
                        arguments: (list "-oP" "-?[0-9]+\\.?[0-9]*")
                        stdin-redirection: #t stdout-redirection: #t
                        stderr-redirection: #t))))
        (display text p)
        (force-output p)
        (close-output-port p)
        (let ((out (read-line p #f)))
          (process-status p)
          (if (and out (> (string-length out) 0))
            (let* ((nums (filter-map string->number
                                     (string-split out #\newline)))
                   (total (apply + nums)))
              (echo-message! (app-state-echo app)
                (string-append "Sum of " (number->string (length nums))
                               " numbers = " (number->string total))))
            (echo-message! (app-state-echo app) "No numbers found in region")))))))

;;;============================================================================
;;; HL-todo — real keyword highlighting with Scintilla indicators
;;;============================================================================

(def *hl-todo-indicator* 2)  ;; indicator 0 = highlight-symbol, 1 = flyspell
(def *hl-todo-active* #f)
(def *hl-todo-keywords*
  '(("TODO"  . #x00CCFF)   ;; orange (BGR)
    ("FIXME" . #x0000FF)   ;; red
    ("HACK"  . #x00AAFF)   ;; orange-red
    ("BUG"   . #x0000CC)   ;; dark red
    ("XXX"   . #xFF00FF)   ;; magenta
    ("NOTE"  . #xFFCC00)))  ;; cyan-blue

(def (hl-todo-clear-indicators! ed)
  "Clear all hl-todo indicator highlights."
  (let ((len (send-message ed SCI_GETLENGTH 0 0)))
    (when (> len 0)
      (send-message ed SCI_SETINDICATORCURRENT *hl-todo-indicator* 0)
      (send-message ed SCI_INDICATORCLEARRANGE 0 len))))

(def (hl-todo-highlight-buffer! ed)
  "Scan buffer and highlight all TODO keywords with indicators."
  (let ((text (editor-get-text ed))
        (text-len (send-message ed SCI_GETLENGTH 0 0)))
    (when (> text-len 0)
      (hl-todo-clear-indicators! ed)
      ;; Use INDIC_ROUNDBOX (7) for visible highlighting
      (send-message ed SCI_INDICSETSTYLE *hl-todo-indicator* 7)
      ;; No SCI_INDICSETALPHA in this build; ROUNDBOX default alpha is fine
      (send-message ed SCI_SETINDICATORCURRENT *hl-todo-indicator* 0)
      (for-each
        (lambda (kw-pair)
          (let ((kw (car kw-pair))
                (color (cdr kw-pair))
                (kw-len (string-length (car kw-pair))))
            (send-message ed SCI_INDICSETFORE *hl-todo-indicator* color)
            (send-message ed SCI_SETINDICATORCURRENT *hl-todo-indicator* 0)
            ;; Find all occurrences
            (let loop ((start 0))
              (let ((found (string-contains text kw start)))
                (when found
                  (send-message ed SCI_INDICATORFILLRANGE found kw-len)
                  (loop (+ found kw-len)))))))
        *hl-todo-keywords*))))

(def (cmd-hl-todo-highlight app)
  "Toggle hl-todo highlighting — real indicator-based keyword marking."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (if *hl-todo-active*
      (begin
        (hl-todo-clear-indicators! ed)
        (set! *hl-todo-active* #f)
        (echo-message! (app-state-echo app) "HL-todo highlighting: off"))
      (begin
        (hl-todo-highlight-buffer! ed)
        (set! *hl-todo-active* #t)
        (echo-message! (app-state-echo app) "HL-todo highlighting: on")))))

(def (cmd-hl-todo-refresh app)
  "Refresh hl-todo highlighting in current buffer."
  (when *hl-todo-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win)))
      (hl-todo-highlight-buffer! ed)
      (echo-message! (app-state-echo app) "HL-todo: refreshed"))))

(def (cmd-hl-todo-occur app)
  "List all TODO/FIXME/HACK keywords in buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (keywords (map car *hl-todo-keywords*))
         (results '()))
    ;; Collect all keyword occurrences with line numbers
    (for-each
      (lambda (kw)
        (let loop ((start 0))
          (let ((found (string-contains text kw start)))
            (when found
              (let* ((line (editor-line-from-position ed found))
                     (line-text (string-trim (editor-get-line ed line))))
                (set! results (cons (cons line line-text) results)))
              (loop (+ found (string-length kw)))))))
      keywords)
    ;; Sort by line number and display
    (let ((sorted (sort (lambda (a b) (< (car a) (car b))) results)))
      (if (null? sorted)
        (echo-message! (app-state-echo app) "No TODO keywords found")
        (let ((buf (buffer-create! "*HL-todo Occur*" ed #f)))
          (buffer-attach! ed buf)
          (editor-set-text ed
            (string-join
              (map (lambda (r)
                     (string-append (number->string (+ (car r) 1)) ": " (cdr r)))
                   sorted)
              "\n"))
          (send-message ed SCI_SETREADONLY 1 0)
          (echo-message! (app-state-echo app)
            (string-append (number->string (length sorted)) " TODO keywords found")))))))

;;;============================================================================
;;; Auto-revert — real file change detection
;;;============================================================================

(def *auto-revert-active* #f)
(def *auto-revert-mtimes* (make-hash-table))  ;; file-path → mtime

(def (file-mtime path)
  "Get file modification time as seconds."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let ((info (file-info path)))
        (time->seconds (file-info-last-modification-time info))))))

(def (auto-revert-record-mtime! buf)
  "Record the current mtime for a buffer's file."
  (let ((path (buffer-file-path buf)))
    (when path
      (let ((mt (file-mtime path)))
        (when mt
          (hash-put! *auto-revert-mtimes* path mt))))))

(def (auto-revert-check-buffer! app)
  "Check if current buffer's file has changed on disk and reload if needed."
  (when *auto-revert-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (buf (edit-window-buffer win))
           (path (and buf (buffer-file-path buf))))
      (when (and path (file-exists? path))
        (let* ((current-mt (file-mtime path))
               (recorded-mt (hash-get *auto-revert-mtimes* path)))
          (when (and current-mt recorded-mt (> current-mt recorded-mt))
            ;; File changed on disk — reload
            (let ((ed (edit-window-editor win)))
              (with-exception-catcher
                (lambda (e) #f)
                (lambda ()
                  (let ((content (with-input-from-file path
                                   (lambda () (read-line (current-input-port) #f)))))
                    (when content
                      (let ((pos (editor-get-current-pos ed)))
                        (editor-set-text ed content)
                        (editor-goto-pos ed (min pos (string-length content)))
                        (editor-set-save-point ed)
                        (hash-put! *auto-revert-mtimes* path current-mt)
                        (echo-message! (app-state-echo app)
                          (string-append "Reverted: "
                            (path-strip-directory path)))))))))))))))

(def (cmd-auto-revert-mode app)
  "Toggle auto-revert mode — reloads buffers when files change on disk."
  (set! *auto-revert-active* (not *auto-revert-active*))
  (if *auto-revert-active*
    (begin
      ;; Record mtimes for all open file-backed buffers
      (for-each
        (lambda (win)
          (let ((buf (edit-window-buffer win)))
            (when buf (auto-revert-record-mtime! buf))))
        (frame-windows (app-state-frame app)))
      (echo-message! (app-state-echo app) "Auto-revert: on — files will reload on change"))
    (echo-message! (app-state-echo app) "Auto-revert: off")))

(def (cmd-revert-buffer app)
  "Revert buffer — reload file from disk."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (if (not (file-exists? path))
        (echo-error! (app-state-echo app) (string-append "File not found: " path))
        (let ((ed (edit-window-editor win)))
          (with-exception-catcher
            (lambda (e)
              (echo-error! (app-state-echo app) "Error reading file"))
            (lambda ()
              (let ((content (with-input-from-file path
                               (lambda () (read-line (current-input-port) #f)))))
                (when content
                  (let ((pos (editor-get-current-pos ed)))
                    (editor-set-text ed content)
                    (editor-goto-pos ed (min pos (string-length content)))
                    (editor-set-save-point ed)
                    (auto-revert-record-mtime! buf)
                    (echo-message! (app-state-echo app)
                      (string-append "Reverted: "
                        (path-strip-directory path)))))))))))))

;;;============================================================================
;;; Writeroom / Olivetti — distraction-free writing
;;;============================================================================

(def *writeroom-active* #f)
(def *writeroom-saved-margin-left* 0)
(def *writeroom-saved-margin-right* 0)
(def *writeroom-saved-line-numbers* #t)

(def (cmd-writeroom-mode-real app)
  "Toggle writeroom-mode — distraction-free writing with wide margins."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (if *writeroom-active*
      ;; Restore previous state
      (begin
        ;; SCI_SETMARGINLEFT=2155, SCI_SETMARGINRIGHT=2157 (not in our constants)
        (send-message ed 2155 0 *writeroom-saved-margin-left*)
        (send-message ed 2157 0 *writeroom-saved-margin-right*)
        (when *writeroom-saved-line-numbers*
          (send-message ed SCI_SETMARGINWIDTHN 0 40))
        (set! *writeroom-active* #f)
        (echo-message! (app-state-echo app) "Writeroom: off"))
      ;; Enter writeroom
      (begin
        ;; SCI_GETMARGINLEFT=2156, SCI_GETMARGINRIGHT=2158
        (set! *writeroom-saved-margin-left* (send-message ed 2156 0 0))
        (set! *writeroom-saved-margin-right* (send-message ed 2158 0 0))
        (set! *writeroom-saved-line-numbers*
              (> (send-message ed SCI_GETMARGINWIDTHN 0 0) 0))
        ;; Set wide margins for centered text
        (send-message ed 2155 0 80)
        (send-message ed 2157 0 80)
        ;; Hide line numbers
        (send-message ed SCI_SETMARGINWIDTHN 0 0)
        (set! *writeroom-active* #t)
        (echo-message! (app-state-echo app) "Writeroom: on — distraction-free mode")))))

(def (cmd-olivetti-mode-real app)
  "Toggle olivetti-mode — centered text with configurable width."
  (cmd-writeroom-mode-real app))

;;;============================================================================
;;; Rainbow-mode — highlight color literals (#RRGGBB) with actual colors
;;;============================================================================

(def *rainbow-indicator* 3)
(def *rainbow-active* #f)

(def (hex-char->int ch)
  "Convert hex character to integer."
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9)) (- (char->integer ch) (char->integer #\0)))
    ((and (char>=? ch #\a) (char<=? ch #\f)) (+ 10 (- (char->integer ch) (char->integer #\a))))
    ((and (char>=? ch #\A) (char<=? ch #\F)) (+ 10 (- (char->integer ch) (char->integer #\A))))
    (else #f)))

(def (parse-hex-color str start)
  "Parse #RRGGBB at position start. Returns BGR integer or #f."
  (and (< (+ start 6) (string-length str))
       (char=? (string-ref str start) #\#)
       (let loop ((i 1) (digits '()))
         (if (= i 7)
           (let ((ds (reverse digits)))
             ;; Convert to BGR for Scintilla
             (let ((r (+ (* (list-ref ds 0) 16) (list-ref ds 1)))
                   (g (+ (* (list-ref ds 2) 16) (list-ref ds 3)))
                   (b (+ (* (list-ref ds 4) 16) (list-ref ds 5))))
               (+ (* b 65536) (* g 256) r)))
           (let ((d (hex-char->int (string-ref str (+ start i)))))
             (and d (loop (+ i 1) (cons d digits))))))))

(def (rainbow-clear-indicators! ed)
  "Clear all rainbow indicators."
  (let ((len (send-message ed SCI_GETLENGTH 0 0)))
    (when (> len 0)
      (send-message ed SCI_SETINDICATORCURRENT *rainbow-indicator* 0)
      (send-message ed SCI_INDICATORCLEARRANGE 0 len))))

(def (rainbow-highlight-buffer! ed)
  "Scan buffer for #RRGGBB colors and highlight with their actual color."
  (let ((text (editor-get-text ed))
        (len (string-length (editor-get-text ed))))
    (when (> len 0)
      (rainbow-clear-indicators! ed)
      ;; Use INDIC_STRAIGHTBOX (8) for color preview
      (send-message ed SCI_INDICSETSTYLE *rainbow-indicator* 8)
      ;; No SCI_INDICSETALPHA in this build; STRAIGHTBOX default alpha is fine
      (send-message ed SCI_SETINDICATORCURRENT *rainbow-indicator* 0)
      (let loop ((pos 0))
        (when (< pos (- len 6))
          (if (char=? (string-ref text pos) #\#)
            (let ((color (parse-hex-color text pos)))
              (if color
                (begin
                  (send-message ed SCI_INDICSETFORE *rainbow-indicator* color)
                  (send-message ed SCI_SETINDICATORCURRENT *rainbow-indicator* 0)
                  (send-message ed SCI_INDICATORFILLRANGE pos 7)
                  (loop (+ pos 7)))
                (loop (+ pos 1))))
            (loop (+ pos 1))))))))

(def (cmd-rainbow-mode-real app)
  "Toggle rainbow-mode — highlight #RRGGBB color codes with their actual color."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (if *rainbow-active*
      (begin
        (rainbow-clear-indicators! ed)
        (set! *rainbow-active* #f)
        (echo-message! (app-state-echo app) "Rainbow: off"))
      (begin
        (rainbow-highlight-buffer! ed)
        (set! *rainbow-active* #t)
        (echo-message! (app-state-echo app) "Rainbow: on — color codes highlighted")))))

(def (cmd-rainbow-refresh app)
  "Refresh rainbow-mode highlighting."
  (when *rainbow-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win)))
      (rainbow-highlight-buffer! ed))))

;; Editorconfig already fully implemented in editor-extra-final.ss

;;;============================================================================
;;; Save-place — remember cursor positions across sessions
;;;============================================================================

(def *save-place-file* (string-append (getenv "HOME") "/.gemacs-places"))
(def *save-place-active* #f)
(def *save-place-db* (make-hash-table))  ;; file-path → position

(def (save-place-load-db!)
  "Load saved places from disk."
  (when (file-exists? *save-place-file*)
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (with-input-from-file *save-place-file*
          (lambda ()
            (let loop ()
              (let ((obj (read (current-input-port))))
                (unless (eof-object? obj)
                  (when (and (pair? obj) (string? (car obj)) (number? (cdr obj)))
                    (hash-put! *save-place-db* (car obj) (cdr obj)))
                  (loop))))))))))

(def (save-place-save-db!)
  "Save places to disk."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (with-output-to-file *save-place-file*
        (lambda ()
          (hash-for-each
            (lambda (k v) (write (cons k v)) (newline))
            *save-place-db*))))))

(def (save-place-record! app)
  "Record current buffer's cursor position."
  (when *save-place-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (buf (edit-window-buffer win))
           (path (and buf (buffer-file-path buf))))
      (when path
        (let ((pos (editor-get-current-pos (edit-window-editor win))))
          (hash-put! *save-place-db* path pos))))))

(def (save-place-restore! app)
  "Restore cursor position for current buffer."
  (when *save-place-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (buf (edit-window-buffer win))
           (path (and buf (buffer-file-path buf))))
      (when path
        (let ((pos (hash-get *save-place-db* path)))
          (when pos
            (let ((ed (edit-window-editor win)))
              (editor-goto-pos ed pos)
              (editor-scroll-caret ed))))))))

(def (cmd-save-place-mode-real app)
  "Toggle save-place-mode — remembers cursor positions across sessions."
  (set! *save-place-active* (not *save-place-active*))
  (if *save-place-active*
    (begin
      (save-place-load-db!)
      (save-place-restore! app)
      (echo-message! (app-state-echo app)
        (string-append "Save-place: on ("
                       (number->string (hash-length *save-place-db*))
                       " places remembered)")))
    (begin
      (save-place-save-db!)
      (echo-message! (app-state-echo app) "Save-place: off (positions saved)"))))

;;;============================================================================
;;; Envrc / direnv — real .envrc loading
;;;============================================================================

(def *envrc-active* #f)
(def *envrc-original-env* (make-hash-table))

(def (cmd-envrc-mode-real app)
  "Toggle envrc-mode — loads .envrc environment variables."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (dir (if path (path-directory path) (current-directory)))
         (envrc (string-append dir "/.envrc"))
         (echo (app-state-echo app)))
    (if (not *envrc-active*)
      ;; Load .envrc
      (if (not (file-exists? envrc))
        (echo-message! echo (string-append "No .envrc in " dir))
        (with-exception-catcher
          (lambda (e) (echo-error! echo "Failed to load .envrc"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "bash"
                                 arguments: (list "-c"
                                              (string-append "source " envrc " && env"))
                                 stdin-redirection: #f
                                 stdout-redirection: #t
                                 stderr-redirection: #f
                                 directory: dir)))
                   (output (read-line proc #f))
                   (count 0))
              (process-status proc)
              (when output
                (for-each
                  (lambda (line)
                    (let ((eq-pos (string-contains line "=")))
                      (when eq-pos
                        (let ((key (substring line 0 eq-pos))
                              (val (substring line (+ eq-pos 1) (string-length line))))
                          ;; Save original value if not already saved
                          (unless (hash-get *envrc-original-env* key)
                            (hash-put! *envrc-original-env* key
                                       (or (getenv key) "")))
                          (setenv key val)
                          (set! count (+ count 1))))))
                  (string-split output #\newline)))
              (set! *envrc-active* #t)
              (echo-message! echo
                (string-append "Envrc: loaded " (number->string count) " vars from " envrc))))))
      ;; Unload: restore original env
      (begin
        (hash-for-each
          (lambda (k v) (setenv k v))
          *envrc-original-env*)
        (hash-clear! *envrc-original-env*)
        (set! *envrc-active* #f)
        (echo-message! echo "Envrc: unloaded (environment restored)")))))

;;;============================================================================
;;; Focus-mode — dim lines far from cursor
;;;============================================================================

(def *focus-indicator* 4)
(def *focus-active* #f)
(def *focus-range* 5)  ;; lines above/below cursor to keep bright
(def *focus-dim-color* #x808080)  ;; gray overlay (BGR)

(def (focus-apply-dimming! ed)
  "Dim all lines except those near the cursor."
  (let* ((len (send-message ed SCI_GETLENGTH 0 0))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (total-lines (send-message ed SCI_GETLINECOUNT 0 0))
         (focus-start (max 0 (- cur-line *focus-range*)))
         (focus-end (min (- total-lines 1) (+ cur-line *focus-range*))))
    ;; Clear all indicators first
    (send-message ed SCI_SETINDICATORCURRENT *focus-indicator* 0)
    (when (> len 0)
      (send-message ed SCI_INDICATORCLEARRANGE 0 len))
    ;; Setup indicator: INDIC_FULLBOX (16) with dim color
    (send-message ed SCI_INDICSETSTYLE *focus-indicator* 6) ;; INDIC_BOX
    (send-message ed SCI_INDICSETFORE *focus-indicator* *focus-dim-color*)
    (send-message ed SCI_SETINDICATORCURRENT *focus-indicator* 0)
    ;; Dim lines before focus zone
    (when (> focus-start 0)
      (let ((start 0)
            (end (editor-position-from-line ed focus-start)))
        (when (> end start)
          (send-message ed SCI_INDICATORFILLRANGE start (- end start)))))
    ;; Dim lines after focus zone
    (when (< focus-end (- total-lines 1))
      (let ((start (editor-position-from-line ed (+ focus-end 1))))
        (when (< start len)
          (send-message ed SCI_INDICATORFILLRANGE start (- len start)))))))

(def (focus-clear-dimming! ed)
  "Remove all focus dimming indicators."
  (let ((len (send-message ed SCI_GETLENGTH 0 0)))
    (when (> len 0)
      (send-message ed SCI_SETINDICATORCURRENT *focus-indicator* 0)
      (send-message ed SCI_INDICATORCLEARRANGE 0 len))))

(def (cmd-focus-mode-real app)
  "Toggle focus-mode — dim lines far from cursor for distraction-free reading."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (if *focus-active*
      (begin
        (focus-clear-dimming! ed)
        (set! *focus-active* #f)
        (echo-message! (app-state-echo app) "Focus: off"))
      (begin
        (focus-apply-dimming! ed)
        (set! *focus-active* #t)
        (echo-message! (app-state-echo app)
          (string-append "Focus: on (±" (number->string *focus-range*) " lines)"))))))

(def (cmd-focus-refresh app)
  "Refresh focus-mode dimming (call after cursor movement)."
  (when *focus-active*
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win)))
      (focus-apply-dimming! ed))))

(def (cmd-focus-set-range app)
  "Set focus-mode range (lines above/below cursor to keep bright)."
  (let ((input (app-read-string app "Focus range (lines): ")))
    (when (and input (not (string-empty? input)))
      (let ((n (string->number input)))
        (when (and n (> n 0))
          (set! *focus-range* n)
          (when *focus-active*
            (cmd-focus-refresh app))
          (echo-message! (app-state-echo app)
            (string-append "Focus range: ±" (number->string n) " lines")))))))

;;;============================================================================
;;; Golden-ratio — auto-resize windows to golden ratio
;;;============================================================================

(def *golden-ratio-active* #f)
(def *golden-ratio* 1.618)

(def (golden-ratio-apply! app)
  "Resize current window to golden ratio proportion."
  (let* ((fr (app-state-frame app))
         (windows (frame-windows fr))
         (n (length windows)))
    (when (> n 1)
      (let* ((idx (frame-current-idx fr))
             ;; Reset all biases first
             (_ (for-each (lambda (w) (set! (edit-window-size-bias w) 0)) windows))
             ;; Calculate how much extra space the focused window gets
             ;; Golden ratio: focused = 61.8%, others share 38.2%
             ;; With n windows of base size 1, total = n
             ;; Focused gets golden_ratio/(golden_ratio + (n-1)) of total
             ;; The bias is relative extra rows/cols
             (bias (inexact->exact (round (* (/ *golden-ratio* (+ *golden-ratio* (- n 1))) n)))))
        (set! (edit-window-size-bias (list-ref windows idx)) bias)
        (frame-layout! fr)))))

(def (cmd-golden-ratio-mode-real app)
  "Toggle golden-ratio-mode — active window gets golden ratio of space."
  (set! *golden-ratio-active* (not *golden-ratio-active*))
  (if *golden-ratio-active*
    (begin
      (golden-ratio-apply! app)
      (echo-message! (app-state-echo app) "Golden ratio: on"))
    (begin
      ;; Reset all window biases
      (let ((fr (app-state-frame app)))
        (for-each (lambda (w) (set! (edit-window-size-bias w) 0))
                  (frame-windows fr))
        (frame-layout! fr))
      (echo-message! (app-state-echo app) "Golden ratio: off (windows balanced)"))))
