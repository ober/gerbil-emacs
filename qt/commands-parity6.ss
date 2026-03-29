;;; -*- Gerbil -*-
;;; Qt parity commands (part 6) — org-roam (real), artist-mode, calc-embedded.
;;; Chain position: after commands-parity5.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/sort
        :gemacs/core
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/commands-core
        :gemacs/qt/commands-parity5)

;;;============================================================================
;;; Org-roam (Qt) — real backlink scanning
;;;============================================================================

;; Re-use the TUI state via shared global variables from editor-extra-notes.ss
;; Since the Qt process is separate, we maintain Qt-local state here.

(def *qt-org-roam-directory* (string-append (getenv "HOME") "/org-roam/"))
(def *qt-org-roam-db* (make-hash-table))
(def *qt-org-roam-backlinks* (make-hash-table))

(def (qt-org-roam-ensure-dir!)
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (when (not (file-exists? *qt-org-roam-directory*))
        (create-directory *qt-org-roam-directory*))
      #t)))

(def (qt-org-roam-list-files)
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let ((p (open-process
                 (list path: "find"
                       arguments: (list *qt-org-roam-directory*
                                        "-name" "*.org" "-type" "f")
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (if (and out (> (string-length out) 0))
            (filter (lambda (s) (not (string=? s "")))
                    (string-split out #\newline))
            '()))))))

(def (qt-org-roam-extract-title file)
  (with-exception-catcher
    (lambda (e) (path-strip-extension (path-strip-directory file)))
    (lambda ()
      (let ((p (open-process
                 (list path: "grep"
                       arguments: (list "-i" "-m1" "^#+title:" file)
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((line (read-line p)))
          (if (and (string? line) (> (string-length line) 8))
            (string-trim (substring line (+ 1 (string-index line #\:)) (string-length line)))
            (path-strip-extension (path-strip-directory file))))))))

(def (qt-org-roam-extract-links file)
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let ((p (open-process
                 (list path: "grep"
                       arguments: (list "-oP" "\\[\\[([^]]+)\\]" file)
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (if (and out (> (string-length out) 0))
            (map (lambda (s)
                   (let ((s2 (string-trim s)))
                     (if (and (>= (string-length s2) 3) (string-prefix? "[[" s2))
                       (substring s2 2 (- (string-length s2) 1))
                       s2)))
                 (filter (lambda (s) (not (string=? s "")))
                         (string-split out #\newline)))
            '()))))))

(def (qt-org-roam-build-db!)
  (let ((files (qt-org-roam-list-files)))
    (set! *qt-org-roam-db* (make-hash-table))
    (set! *qt-org-roam-backlinks* (make-hash-table))
    (for-each
      (lambda (file)
        (let ((title (qt-org-roam-extract-title file))
              (links (qt-org-roam-extract-links file)))
          (hash-put! *qt-org-roam-db* file (cons title links))))
      files)
    (hash-for-each
      (lambda (file entry)
        (for-each
          (lambda (link)
            (let ((target (qt-org-roam-resolve-link link)))
              (when target
                (let ((existing (or (hash-get *qt-org-roam-backlinks* target) '())))
                  (unless (member file existing)
                    (hash-put! *qt-org-roam-backlinks* target (cons file existing)))))))
          (cdr entry)))
      *qt-org-roam-db*)
    (length files)))

(def (qt-org-roam-resolve-link link)
  (let ((target (cond
                  ((string-prefix? "roam:" link) (substring link 5 (string-length link)))
                  ((string-prefix? "file:" link) (substring link 5 (string-length link)))
                  ((string-prefix? "id:" link) #f)
                  (else link))))
    (and target
         (let ((full (if (and (> (string-length target) 0)
                              (char=? (string-ref target 0) #\/))
                       target
                       (string-append *qt-org-roam-directory* target))))
           (cond
             ((file-exists? full) full)
             ((file-exists? (string-append full ".org")) (string-append full ".org"))
             (else (qt-org-roam-find-by-title target)))))))

(def (qt-org-roam-find-by-title title)
  (let ((result #f))
    (hash-for-each
      (lambda (file entry)
        (when (and (not result) (string-ci=? (car entry) title))
          (set! result file)))
      *qt-org-roam-db*)
    result))

(def (qt-org-roam-node-titles)
  (let ((pairs '()))
    (hash-for-each
      (lambda (file entry)
        (set! pairs (cons (cons (car entry) file) pairs)))
      *qt-org-roam-db*)
    (sort pairs (lambda (a b) (string<? (car a) (car b))))))

(def (qt-org-roam-open-file! app file)
  "Open an org file in the Qt editor."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (name (path-strip-directory file))
         (buf (or (buffer-by-name name) (qt-buffer-create! name ed file))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (when (file-exists? file)
      (let ((text (read-file-as-string file)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (sci-send ed SCI_SETSAVEPOINT))))))

(def (qt-current-date-string)
  (with-exception-catcher
    (lambda (e) "2026-03-28")
    (lambda ()
      (let ((p (open-process
                 (list path: "date" arguments: (list "+%Y-%m-%d")
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p)))
          (if (string? out) (string-trim out) "2026-03-28"))))))

;; Commands

(def (cmd-org-roam-node-find app)
  "Find and open an org-roam node with completion (Qt)."
  (qt-org-roam-ensure-dir!)
  (qt-org-roam-build-db!)
  (let ((nodes (qt-org-roam-node-titles)))
    (if (null? nodes)
      (echo-message! (app-state-echo app) "No org-roam nodes found.")
      (let ((query (qt-echo-read-string app
                     (string-append "Find node (" (number->string (length nodes)) "): "))))
        (when (and query (not (string-empty? query)))
          (let ((match (find (lambda (pair) (string-contains-ci (car pair) query)) nodes)))
            (if match
              (begin
                (qt-org-roam-open-file! app (cdr match))
                (echo-message! (app-state-echo app) (string-append "Opened: " (car match))))
              ;; Create new
              (let ((new-file (string-append *qt-org-roam-directory*
                                (string-downcase (list->string
                                  (map (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\-))
                                       (string->list query))))
                                ".org")))
                (qt-org-roam-create-node! app new-file query)))))))))

(def (qt-org-roam-create-node! app file title)
  (qt-org-roam-ensure-dir!)
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Failed to create node"))
    (lambda ()
      (with-output-to-file file
        (lambda ()
          (display (string-append "#+title: " title "\n"))
          (display (string-append "#+date: " (qt-current-date-string) "\n"))
          (display "#+filetags:\n\n")))
      (qt-org-roam-open-file! app file)
      (echo-message! (app-state-echo app) (string-append "Created node: " title)))))

(def (cmd-org-roam-node-insert app)
  "Insert a link to an org-roam node at point (Qt)."
  (qt-org-roam-build-db!)
  (let ((nodes (qt-org-roam-node-titles)))
    (if (null? nodes)
      (echo-message! (app-state-echo app) "No org-roam nodes found")
      (let ((query (qt-echo-read-string app "Insert link to node: ")))
        (when (and query (not (string-empty? query)))
          (let ((match (find (lambda (pair) (string-contains-ci (car pair) query)) nodes)))
            (when match
              (let* ((ed (current-qt-editor app))
                     (pos (sci-send ed SCI_GETCURRENTPOS))
                     (link (string-append "[[roam:" (car match) "]]")))
                (sci-send/string ed SCI_INSERTTEXT pos link)
                (echo-message! (app-state-echo app) (string-append "Linked to: " (car match)))))))))))

(def (cmd-org-roam-buffer-toggle app)
  "Toggle backlinks buffer for current file (Qt)."
  (qt-org-roam-build-db!)
  (let* ((buf (current-qt-buffer app))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! (app-state-echo app) "Buffer has no file")
      (let ((backlinks (or (hash-get *qt-org-roam-backlinks* file) '())))
        (if (null? backlinks)
          (let* ((title (qt-org-roam-extract-title file))
                 (title-bls (let ((results '()))
                              (hash-for-each
                                (lambda (f entry)
                                  (when (find (lambda (l) (string-ci=? l title)) (cdr entry))
                                    (set! results (cons f results))))
                                *qt-org-roam-db*)
                              results)))
            (if (null? title-bls)
              (echo-message! (app-state-echo app) (string-append "No backlinks for: " (or (buffer-name buf) "?")))
              (qt-open-output-buffer app "*Org-roam Backlinks*"
                (qt-format-backlinks file title-bls))))
          (qt-open-output-buffer app "*Org-roam Backlinks*"
            (qt-format-backlinks file backlinks)))))))

(def (qt-format-backlinks file backlinks)
  (let ((title (qt-org-roam-extract-title file)))
    (string-append
      "Backlinks for: " title "\n"
      (make-string 40 #\=) "\n\n"
      (apply string-append
        (map (lambda (bl)
               (string-append "  - " (qt-org-roam-extract-title bl) "\n    " bl "\n\n"))
             backlinks)))))

(def (cmd-org-roam-capture app)
  "Create a new org-roam note (Qt)."
  (qt-org-roam-ensure-dir!)
  (let ((title (qt-echo-read-string app "New note title: ")))
    (when (and title (not (string-empty? title)))
      (let ((file (string-append *qt-org-roam-directory*
                    (string-downcase (list->string
                      (map (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)) c #\-))
                           (string->list title))))
                    ".org")))
        (if (file-exists? file)
          (begin (qt-org-roam-open-file! app file)
                 (echo-message! (app-state-echo app) "Opened existing note"))
          (qt-org-roam-create-node! app file title))))))

(def (cmd-org-roam-graph app)
  "Show text representation of org-roam link graph (Qt)."
  (qt-org-roam-build-db!)
  (let* ((nodes (qt-org-roam-node-titles))
         (lines (map (lambda (pair)
                       (let* ((file (cdr pair))
                              (title (car pair))
                              (entry (or (hash-get *qt-org-roam-db* file) '("" . ())))
                              (links (cdr entry))
                              (backlinks (or (hash-get *qt-org-roam-backlinks* file) '())))
                         (string-append
                           title "\n"
                           "  -> " (number->string (length links)) " outgoing, "
                           (number->string (length backlinks)) " backlinks\n"
                           (if (null? links) ""
                             (apply string-append
                               (map (lambda (l) (string-append "     -> " l "\n")) links)))
                           "\n")))
                     nodes)))
    (qt-open-output-buffer app "*Org-roam Graph*"
      (string-append "Org-roam Graph (" (number->string (length nodes)) " nodes)\n"
        (make-string 50 #\=) "\n\n"
        (apply string-append lines)))))

(def (cmd-org-roam-set-directory app)
  "Set the org-roam directory (Qt)."
  (let ((dir (qt-echo-read-string app
               (string-append "Org-roam directory [" *qt-org-roam-directory* "]: "))))
    (when (and dir (not (string-empty? dir)))
      (set! *qt-org-roam-directory* (if (string-suffix? "/" dir) dir (string-append dir "/")))
      (echo-message! (app-state-echo app)
        (string-append "Org-roam directory: " *qt-org-roam-directory*)))))

(def (cmd-org-roam-db-sync app)
  "Rebuild org-roam database (Qt)."
  (qt-org-roam-ensure-dir!)
  (let ((count (qt-org-roam-build-db!)))
    (echo-message! (app-state-echo app)
      (string-append "Org-roam: indexed " (number->string count) " nodes"))))

(def (cmd-org-roam-find-file app)
  (cmd-org-roam-node-find app))

(def (cmd-org-roam-dailies-today app)
  "Open or create today's daily note (Qt)."
  (qt-org-roam-ensure-dir!)
  (let* ((date (qt-current-date-string))
         (dailies-dir (string-append *qt-org-roam-directory* "daily/"))
         (file (string-append dailies-dir date ".org")))
    (with-exception-catcher (lambda (e) #f)
      (lambda () (when (not (file-exists? dailies-dir)) (create-directory dailies-dir))))
    (if (file-exists? file)
      (begin (qt-org-roam-open-file! app file)
             (echo-message! (app-state-echo app) (string-append "Daily: " date)))
      (begin
        (with-output-to-file file
          (lambda ()
            (display (string-append "#+title: " date "\n#+date: " date "\n#+filetags: :daily:\n\n* Journal\n\n"))))
        (qt-org-roam-open-file! app file)
        (echo-message! (app-state-echo app) (string-append "Created daily: " date))))))

;;;============================================================================
;;; Artist mode (Qt)
;;;============================================================================

(def *qt-artist-mode-active* #f)
(def *qt-artist-draw-char* #\+)
(def *qt-artist-tool* 'line)

(def (cmd-artist-mode app)
  "Toggle artist mode (Qt)."
  (set! *qt-artist-mode-active* (not *qt-artist-mode-active*))
  (echo-message! (app-state-echo app)
    (if *qt-artist-mode-active*
      (string-append "Artist mode ON — tool: " (symbol->string *qt-artist-tool*)
                     " char: " (string *qt-artist-draw-char*))
      "Artist mode OFF")))

(def (cmd-artist-select-tool app)
  "Select artist drawing tool (Qt)."
  (let ((tool (qt-echo-read-string app "Tool (line/rect/text/erase/freehand): ")))
    (when (and tool (not (string-empty? tool)))
      (let ((sym (string->symbol tool)))
        (when (memq sym '(line rect rectangle text erase freehand))
          (set! *qt-artist-tool* (if (eq? sym 'rect) 'rectangle sym))
          (echo-message! (app-state-echo app)
            (string-append "Artist tool: " (symbol->string *qt-artist-tool*))))))))

(def (cmd-artist-set-char app)
  "Set drawing character (Qt)."
  (let ((ch (qt-echo-read-string app (string-append "Draw char [" (string *qt-artist-draw-char*) "]: "))))
    (when (and ch (= (string-length ch) 1))
      (set! *qt-artist-draw-char* (string-ref ch 0))
      (echo-message! (app-state-echo app) (string-append "Draw char: " (string *qt-artist-draw-char*))))))

(def (qt-artist-ensure-line-length! ed line min-col)
  (let* ((line-start (sci-send ed SCI_POSITIONFROMLINE line))
         (line-end (sci-send ed SCI_GETLINEENDPOSITION line))
         (line-len (- line-end line-start)))
    (when (< line-len min-col)
      (sci-send/string ed SCI_INSERTTEXT line-end (make-string (- min-col line-len) #\space)))))

(def (qt-artist-set-char-at! ed line col ch)
  (qt-artist-ensure-line-length! ed line (+ col 1))
  (let* ((line-start (sci-send ed SCI_POSITIONFROMLINE line))
         (pos (+ line-start col)))
    (sci-send ed SCI_SETTARGETSTART pos 0)
    (sci-send ed SCI_SETTARGETEND (+ pos 1) 0)
    (sci-send/string ed SCI_REPLACETARGET -1 (string ch))))

(def (cmd-artist-draw-line app)
  "Draw ASCII line (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos1 (sci-send ed SCI_GETCURRENTPOS))
           (line1 (sci-send ed SCI_LINEFROMPOSITION pos1))
           (col1 (- pos1 (sci-send ed SCI_POSITIONFROMLINE line1))))
      (let ((input (qt-echo-read-string app "Line to (line,col): ")))
        (when (and input (string-contains input ","))
          (let* ((parts (string-split input #\,))
                 (line2 (string->number (string-trim (car parts))))
                 (col2 (string->number (string-trim (cadr parts)))))
            (when (and line2 col2)
              (sci-send ed SCI_BEGINUNDOACTION)
              (qt-artist-draw-line! ed line1 col1 line2 col2)
              (sci-send ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Line drawn"))))))))

(def (qt-artist-draw-line! ed r1 c1 r2 c2)
  (let ((dr (- r2 r1)) (dc (- c2 c1)))
    (cond
      ((= dr 0)
       (let ((s (min c1 c2)) (e (max c1 c2)))
         (let loop ((c s)) (when (<= c e) (qt-artist-set-char-at! ed r1 c #\-) (loop (+ c 1))))))
      ((= dc 0)
       (let ((s (min r1 r2)) (e (max r1 r2)))
         (let loop ((r s)) (when (<= r e) (qt-artist-set-char-at! ed r c1 #\|) (loop (+ r 1))))))
      (else
       (let ((steps (max (abs dr) (abs dc))))
         (let loop ((i 0))
           (when (<= i steps)
             (let ((r (+ r1 (inexact->exact (round (* dr (/ i steps))))))
                   (c (+ c1 (inexact->exact (round (* dc (/ i steps)))))))
               (qt-artist-set-char-at! ed r c
                 (cond ((= (abs dr) (abs dc)) #\\) ((> (abs dc) (abs dr)) #\-) (else #\|))))
             (loop (+ i 1)))))))))

(def (cmd-artist-draw-rectangle app)
  "Draw ASCII rectangle (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos (sci-send ed SCI_GETCURRENTPOS))
           (line (sci-send ed SCI_LINEFROMPOSITION pos))
           (col (- pos (sci-send ed SCI_POSITIONFROMLINE line))))
      (let ((dims (qt-echo-read-string app "Rectangle (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 1) (> h 1))
              (sci-send ed SCI_BEGINUNDOACTION)
              (let loop ((c col)) (when (< c (+ col w))
                (qt-artist-set-char-at! ed line c #\-) (loop (+ c 1))))
              (let loop ((c col)) (when (< c (+ col w))
                (qt-artist-set-char-at! ed (+ line h -1) c #\-) (loop (+ c 1))))
              (let loop ((r line)) (when (< r (+ line h))
                (qt-artist-set-char-at! ed r col #\|) (loop (+ r 1))))
              (let loop ((r line)) (when (< r (+ line h))
                (qt-artist-set-char-at! ed r (+ col w -1) #\|) (loop (+ r 1))))
              (qt-artist-set-char-at! ed line col #\+)
              (qt-artist-set-char-at! ed line (+ col w -1) #\+)
              (qt-artist-set-char-at! ed (+ line h -1) col #\+)
              (qt-artist-set-char-at! ed (+ line h -1) (+ col w -1) #\+)
              (sci-send ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app)
                (string-append "Rectangle: " (number->string w) "x" (number->string h))))))))))

(def (cmd-artist-draw-text app)
  "Insert text at point in artist mode (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos (sci-send ed SCI_GETCURRENTPOS))
           (line (sci-send ed SCI_LINEFROMPOSITION pos))
           (col (- pos (sci-send ed SCI_POSITIONFROMLINE line))))
      (let ((text (qt-echo-read-string app "Text: ")))
        (when (and text (not (string-empty? text)))
          (sci-send ed SCI_BEGINUNDOACTION)
          (let loop ((i 0))
            (when (< i (string-length text))
              (qt-artist-set-char-at! ed line (+ col i) (string-ref text i))
              (loop (+ i 1))))
          (sci-send ed SCI_ENDUNDOACTION)
          (echo-message! (app-state-echo app) "Text drawn"))))))

(def (cmd-artist-erase-rect app)
  "Erase rectangular region (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos (sci-send ed SCI_GETCURRENTPOS))
           (line (sci-send ed SCI_LINEFROMPOSITION pos))
           (col (- pos (sci-send ed SCI_POSITIONFROMLINE line))))
      (let ((dims (qt-echo-read-string app "Erase rect (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 0) (> h 0))
              (sci-send ed SCI_BEGINUNDOACTION)
              (let rloop ((r 0))
                (when (< r h)
                  (let cloop ((c 0))
                    (when (< c w) (qt-artist-set-char-at! ed (+ line r) (+ col c) #\space) (cloop (+ c 1))))
                  (rloop (+ r 1))))
              (sci-send ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Erased"))))))))

(def (cmd-artist-draw-arrow app)
  "Draw ASCII arrow (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos (sci-send ed SCI_GETCURRENTPOS))
           (line (sci-send ed SCI_LINEFROMPOSITION pos))
           (col (- pos (sci-send ed SCI_POSITIONFROMLINE line))))
      (let ((input (qt-echo-read-string app "Arrow (direction length): ")))
        (when (and input (string-contains input " "))
          (let* ((parts (string-split input #\space))
                 (dir (string-trim (car parts)))
                 (len (string->number (string-trim (cadr parts)))))
            (when (and len (> len 0))
              (sci-send ed SCI_BEGINUNDOACTION)
              (cond
                ((string=? dir "right")
                 (let loop ((c 0)) (when (< c (- len 1))
                   (qt-artist-set-char-at! ed line (+ col c) #\-) (loop (+ c 1))))
                 (qt-artist-set-char-at! ed line (+ col len -1) #\>))
                ((string=? dir "left")
                 (qt-artist-set-char-at! ed line col #\<)
                 (let loop ((c 1)) (when (< c len)
                   (qt-artist-set-char-at! ed line (+ col c) #\-) (loop (+ c 1)))))
                ((string=? dir "down")
                 (let loop ((r 0)) (when (< r (- len 1))
                   (qt-artist-set-char-at! ed (+ line r) col #\|) (loop (+ r 1))))
                 (qt-artist-set-char-at! ed (+ line len -1) col #\v))
                ((string=? dir "up")
                 (qt-artist-set-char-at! ed line col #\^)
                 (let loop ((r 1)) (when (< r len)
                   (qt-artist-set-char-at! ed (+ line r) col #\|) (loop (+ r 1))))))
              (sci-send ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app) "Arrow drawn"))))))))

(def (cmd-artist-draw-ellipse app)
  "Draw ASCII ellipse (Qt)."
  (when *qt-artist-mode-active*
    (let* ((ed (current-qt-editor app))
           (pos (sci-send ed SCI_GETCURRENTPOS))
           (line (sci-send ed SCI_LINEFROMPOSITION pos))
           (col (- pos (sci-send ed SCI_POSITIONFROMLINE line))))
      (let ((dims (qt-echo-read-string app "Ellipse (width,height): ")))
        (when (and dims (string-contains dims ","))
          (let* ((parts (string-split dims #\,))
                 (w (string->number (string-trim (car parts))))
                 (h (string->number (string-trim (cadr parts)))))
            (when (and w h (> w 3) (> h 2))
              (sci-send ed SCI_BEGINUNDOACTION)
              (let* ((rx (/ (- w 1) 2.0)) (ry (/ (- h 1) 2.0))
                     (cx (+ col (inexact->exact (round rx))))
                     (cy (+ line (inexact->exact (round ry)))))
                (let loop ((angle 0.0))
                  (when (< angle 6.30)
                    (let ((x (inexact->exact (round (+ cx (* rx (cos angle))))))
                          (y (inexact->exact (round (+ cy (* ry (sin angle)))))))
                      (qt-artist-set-char-at! ed y x
                        (cond ((< (abs (sin angle)) 0.3) #\-)
                              ((< (abs (cos angle)) 0.3) #\|)
                              ((or (and (> (sin angle) 0) (> (cos angle) 0))
                                   (and (< (sin angle) 0) (< (cos angle) 0))) #\\)
                              (else #\/))))
                    (loop (+ angle 0.1)))))
              (sci-send ed SCI_ENDUNDOACTION)
              (echo-message! (app-state-echo app)
                (string-append "Ellipse: " (number->string w) "x" (number->string h))))))))))

;;;============================================================================
;;; Calc embedded (Qt)
;;;============================================================================

(def (qt-calc-eval-expr expr)
  "Evaluate math expression via bc."
  (let* ((clean (let loop ((i 0))
                  (cond ((>= i (string-length expr)) expr)
                        ((char=? (string-ref expr i) #\=) (string-trim (substring expr 0 i)))
                        (else (loop (+ i 1))))))
         (clean (string-trim clean)))
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (let ((p (open-process
                   (list path: "bc" arguments: (list "-l")
                         stdin-redirection: #t stdout-redirection: #t
                         stderr-redirection: #t))))
          (display (string-append clean "\n") p)
          (force-output p)
          (close-output-port p)
          (let ((result (read-line p)))
            (if (and (string? result) (not (string=? result "")))
              (string-trim result)
              #f)))))))

(def (cmd-calc-embedded app)
  "Evaluate math at point or in region (Qt)."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= sel-start sel-end)
      ;; No selection — eval current line
      (let* ((line (sci-send ed SCI_LINEFROMPOSITION sel-start))
             (line-start (sci-send ed SCI_POSITIONFROMLINE line))
             (line-end (sci-send ed SCI_GETLINEENDPOSITION line))
             (text (qt-plain-text-edit-text-range ed line-start line-end)))
        (let ((expr (string-trim text)))
          (when (not (string-empty? expr))
            (let ((result (qt-calc-eval-expr expr)))
              (if result
                (if (string-contains expr "=")
                  (let* ((eq-pos (string-index expr #\=))
                         (prefix (substring expr 0 (+ eq-pos 1)))
                         (new-text (string-append prefix " " result)))
                    (sci-send ed SCI_SETTARGETSTART line-start 0)
                    (sci-send ed SCI_SETTARGETEND line-end 0)
                    (sci-send/string ed SCI_REPLACETARGET -1 new-text)
                    (echo-message! (app-state-echo app) (string-append "= " result)))
                  (begin
                    (sci-send/string ed SCI_INSERTTEXT line-end (string-append " = " result))
                    (echo-message! (app-state-echo app) (string-append "= " result))))
                (echo-error! (app-state-echo app) "Could not evaluate"))))))
      ;; Has selection
      (let ((text (qt-plain-text-edit-text-range ed sel-start sel-end)))
        (let ((result (qt-calc-eval-expr (string-trim text))))
          (if result
            (begin
              (sci-send ed SCI_SETTARGETSTART sel-start 0)
              (sci-send ed SCI_SETTARGETEND sel-end 0)
              (sci-send/string ed SCI_REPLACETARGET -1 result)
              (echo-message! (app-state-echo app) (string-append "= " result)))
            (echo-error! (app-state-echo app) "Could not evaluate")))))))

(def (cmd-calc-eval-line app)
  "Evaluate expression on current line (Qt)."
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (line-start (sci-send ed SCI_POSITIONFROMLINE line))
         (line-end (sci-send ed SCI_GETLINEENDPOSITION line))
         (text (string-trim (qt-plain-text-edit-text-range ed line-start line-end))))
    (if (string-empty? text)
      (echo-message! (app-state-echo app) "Empty line")
      (let ((result (qt-calc-eval-expr text)))
        (if result
          (echo-message! (app-state-echo app) (string-append text " = " result))
          (echo-error! (app-state-echo app) "Could not evaluate"))))))

(def (cmd-calc-embedded-eval-region app)
  "Evaluate each line in region as math (Qt)."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((line1 (sci-send ed SCI_LINEFROMPOSITION sel-start))
             (line2 (sci-send ed SCI_LINEFROMPOSITION sel-end))
             (count 0))
        (sci-send ed SCI_BEGINUNDOACTION)
        (let loop ((line line2))
          (when (>= line line1)
            (let* ((ls (sci-send ed SCI_POSITIONFROMLINE line))
                   (line-end (sci-send ed SCI_GETLINEENDPOSITION line))
                   (text (string-trim (qt-plain-text-edit-text-range ed ls line-end))))
              (when (and (not (string-empty? text)) (not (string-contains text "=")))
                (let ((result (qt-calc-eval-expr text)))
                  (when result
                    (sci-send/string ed SCI_INSERTTEXT line-end (string-append " = " result))
                    (set! count (+ count 1))))))
            (loop (- line 1))))
        (sci-send ed SCI_ENDUNDOACTION)
        (echo-message! (app-state-echo app)
          (string-append "Evaluated " (number->string count) " expressions"))))))

(def (cmd-calc-grab-region app)
  "Extract numbers from region (Qt)."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text-range ed sel-start sel-end))
             (nums (filter-map string->number
                               (map string-trim (string-split text #\newline)))))
        (echo-message! (app-state-echo app)
          (string-append "Found " (number->string (length nums)) " numbers"))))))

(def (cmd-calc-sum-region app)
  "Sum all numbers in selected region (Qt)."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text-range ed sel-start sel-end))
             (p (open-process
                  (list path: "grep"
                        arguments: (list "-oP" "-?[0-9]+\\.?[0-9]*")
                        stdin-redirection: #t stdout-redirection: #t
                        stderr-redirection: #t))))
        (display text p)
        (force-output p)
        (close-output-port p)
        (let ((out (read-line p #f)))
          (if (and out (> (string-length out) 0))
            (let* ((nums (filter-map string->number (string-split out #\newline)))
                   (total (apply + nums)))
              (echo-message! (app-state-echo app)
                (string-append "Sum of " (number->string (length nums))
                               " numbers = " (number->string total))))
            (echo-message! (app-state-echo app) "No numbers found")))))))

;;;============================================================================
;;; HL-todo — real keyword highlighting (Qt)
;;;============================================================================

(def *qt-hl-todo-indicator* 2)
(def *qt-hl-todo-active* #f)
(def *qt-hl-todo-keywords*
  '(("TODO"  . #x00CCFF)
    ("FIXME" . #x0000FF)
    ("HACK"  . #x00AAFF)
    ("BUG"   . #x0000CC)
    ("XXX"   . #xFF00FF)
    ("NOTE"  . #xFFCC00)))

(def (qt-hl-todo-clear! ed)
  (let ((len (sci-send ed SCI_GETLENGTH)))
    (when (> len 0)
      (sci-send ed SCI_SETINDICATORCURRENT *qt-hl-todo-indicator*)
      (sci-send ed SCI_INDICATORCLEARRANGE 0 len))))

(def (qt-hl-todo-highlight! ed)
  (let ((text (qt-plain-text-edit-text ed))
        (len (sci-send ed SCI_GETLENGTH)))
    (when (> len 0)
      (qt-hl-todo-clear! ed)
      (sci-send ed SCI_INDICSETSTYLE *qt-hl-todo-indicator* 7)  ;; ROUNDBOX
      (sci-send ed SCI_SETINDICATORCURRENT *qt-hl-todo-indicator*)
      (for-each
        (lambda (kw-pair)
          (let ((kw (car kw-pair))
                (color (cdr kw-pair))
                (kw-len (string-length (car kw-pair))))
            (sci-send ed SCI_INDICSETFORE *qt-hl-todo-indicator* color)
            (sci-send ed SCI_SETINDICATORCURRENT *qt-hl-todo-indicator*)
            (let loop ((start 0))
              (let ((found (string-contains text kw start)))
                (when found
                  (sci-send ed SCI_INDICATORFILLRANGE found kw-len)
                  (loop (+ found kw-len)))))))
        *qt-hl-todo-keywords*))))

(def (cmd-hl-todo-highlight app)
  "Toggle hl-todo highlighting (Qt)."
  (let ((ed (current-qt-editor app)))
    (if *qt-hl-todo-active*
      (begin (qt-hl-todo-clear! ed)
             (set! *qt-hl-todo-active* #f)
             (echo-message! (app-state-echo app) "HL-todo highlighting: off"))
      (begin (qt-hl-todo-highlight! ed)
             (set! *qt-hl-todo-active* #t)
             (echo-message! (app-state-echo app) "HL-todo highlighting: on")))))

(def (cmd-hl-todo-refresh app)
  "Refresh hl-todo highlighting (Qt)."
  (when *qt-hl-todo-active*
    (qt-hl-todo-highlight! (current-qt-editor app))
    (echo-message! (app-state-echo app) "HL-todo: refreshed")))

(def (cmd-hl-todo-occur app)
  "List all TODO keywords in buffer (Qt)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (keywords (map car *qt-hl-todo-keywords*))
         (results '()))
    (for-each
      (lambda (kw)
        (let loop ((start 0))
          (let ((found (string-contains text kw start)))
            (when found
              (let* ((line (sci-send ed SCI_LINEFROMPOSITION found))
                     (ls (sci-send ed SCI_POSITIONFROMLINE line))
                     (le (sci-send ed SCI_GETLINEENDPOSITION line))
                     (line-text (string-trim (qt-plain-text-edit-text-range ed ls le))))
                (set! results (cons (cons line line-text) results)))
              (loop (+ found (string-length kw)))))))
      keywords)
    (let ((sorted (sort (lambda (a b) (< (car a) (car b))) results)))
      (if (null? sorted)
        (echo-message! (app-state-echo app) "No TODO keywords found")
        (qt-open-output-buffer app "*HL-todo Occur*"
          (string-join
            (map (lambda (r)
                   (string-append (number->string (+ (car r) 1)) ": " (cdr r)))
                 sorted)
            "\n"))))))

;; Auto-revert and revert-buffer already exist in commands-core.ss and commands-shell.ss

;;;============================================================================
;;; Writeroom / Olivetti — distraction-free writing (Qt)
;;;============================================================================

(def *qt-writeroom-active* #f)
(def *qt-writeroom-saved-margin-width* 0)

(def (cmd-writeroom-mode-real app)
  "Toggle writeroom-mode — distraction-free writing (Qt)."
  (let ((ed (current-qt-editor app)))
    (if *qt-writeroom-active*
      (begin
        ;; SCI_SETMARGINLEFT=2155, SCI_SETMARGINRIGHT=2157
        (sci-send ed 2155 0 0)
        (sci-send ed 2157 0 0)
        (when (> *qt-writeroom-saved-margin-width* 0)
          (sci-send ed SCI_SETMARGINWIDTHN 0 *qt-writeroom-saved-margin-width*))
        (set! *qt-writeroom-active* #f)
        (echo-message! (app-state-echo app) "Writeroom: off"))
      (begin
        (set! *qt-writeroom-saved-margin-width*
              (sci-send ed SCI_GETMARGINWIDTHN 0))
        (sci-send ed 2155 0 80)
        (sci-send ed 2157 0 80)
        (sci-send ed SCI_SETMARGINWIDTHN 0 0)
        (set! *qt-writeroom-active* #t)
        (echo-message! (app-state-echo app) "Writeroom: on — distraction-free mode")))))

(def (cmd-olivetti-mode-real app)
  "Toggle olivetti-mode (Qt)."
  (cmd-writeroom-mode-real app))

;;;============================================================================
;;; Rainbow-mode — highlight #RRGGBB color literals (Qt)
;;;============================================================================

(def *qt-rainbow-indicator* 3)
(def *qt-rainbow-color-active* #f)

(def (qt-hex-char->int ch)
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9)) (- (char->integer ch) (char->integer #\0)))
    ((and (char>=? ch #\a) (char<=? ch #\f)) (+ 10 (- (char->integer ch) (char->integer #\a))))
    ((and (char>=? ch #\A) (char<=? ch #\F)) (+ 10 (- (char->integer ch) (char->integer #\A))))
    (else #f)))

(def (qt-parse-hex-color str start)
  (and (< (+ start 6) (string-length str))
       (char=? (string-ref str start) #\#)
       (let loop ((i 1) (digits '()))
         (if (= i 7)
           (let ((ds (reverse digits)))
             (let ((r (+ (* (list-ref ds 0) 16) (list-ref ds 1)))
                   (g (+ (* (list-ref ds 2) 16) (list-ref ds 3)))
                   (b (+ (* (list-ref ds 4) 16) (list-ref ds 5))))
               (+ (* b 65536) (* g 256) r)))
           (let ((d (qt-hex-char->int (string-ref str (+ start i)))))
             (and d (loop (+ i 1) (cons d digits))))))))

(def (qt-rainbow-clear! ed)
  (let ((len (sci-send ed SCI_GETLENGTH)))
    (when (> len 0)
      (sci-send ed SCI_SETINDICATORCURRENT *qt-rainbow-indicator*)
      (sci-send ed SCI_INDICATORCLEARRANGE 0 len))))

(def (qt-rainbow-highlight! ed)
  (let ((text (qt-plain-text-edit-text ed))
        (len (string-length (qt-plain-text-edit-text ed))))
    (when (> len 0)
      (qt-rainbow-clear! ed)
      (sci-send ed SCI_INDICSETSTYLE *qt-rainbow-indicator* 8)  ;; STRAIGHTBOX
      (sci-send ed SCI_SETINDICATORCURRENT *qt-rainbow-indicator*)
      (let loop ((pos 0))
        (when (< pos (- len 6))
          (if (char=? (string-ref text pos) #\#)
            (let ((color (qt-parse-hex-color text pos)))
              (if color
                (begin
                  (sci-send ed SCI_INDICSETFORE *qt-rainbow-indicator* color)
                  (sci-send ed SCI_SETINDICATORCURRENT *qt-rainbow-indicator*)
                  (sci-send ed SCI_INDICATORFILLRANGE pos 7)
                  (loop (+ pos 7)))
                (loop (+ pos 1))))
            (loop (+ pos 1))))))))

(def (cmd-rainbow-mode-real app)
  "Toggle rainbow-mode — highlight #RRGGBB colors (Qt)."
  (let ((ed (current-qt-editor app)))
    (if *qt-rainbow-color-active*
      (begin (qt-rainbow-clear! ed)
             (set! *qt-rainbow-color-active* #f)
             (echo-message! (app-state-echo app) "Rainbow: off"))
      (begin (qt-rainbow-highlight! ed)
             (set! *qt-rainbow-color-active* #t)
             (echo-message! (app-state-echo app) "Rainbow: on — color codes highlighted")))))

(def (cmd-rainbow-refresh app)
  "Refresh rainbow highlighting (Qt)."
  (when *qt-rainbow-color-active*
    (qt-rainbow-highlight! (current-qt-editor app))))

;;;============================================================================
;;; Save-place — remember cursor positions (Qt)
;;;============================================================================

(def *qt-save-place-file* (string-append (getenv "HOME") "/.gemacs-places"))
(def *qt-save-place-active* #f)
(def *qt-save-place-db* (make-hash-table))

(def (qt-save-place-load!)
  (when (file-exists? *qt-save-place-file*)
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (with-input-from-file *qt-save-place-file*
          (lambda ()
            (let loop ()
              (let ((obj (read (current-input-port))))
                (unless (eof-object? obj)
                  (when (and (pair? obj) (string? (car obj)) (number? (cdr obj)))
                    (hash-put! *qt-save-place-db* (car obj) (cdr obj)))
                  (loop))))))))))

(def (qt-save-place-save!)
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (with-output-to-file *qt-save-place-file*
        (lambda ()
          (hash-for-each
            (lambda (k v) (write (cons k v)) (newline))
            *qt-save-place-db*))))))

(def (cmd-save-place-mode-real app)
  "Toggle save-place-mode (Qt)."
  (set! *qt-save-place-active* (not *qt-save-place-active*))
  (if *qt-save-place-active*
    (begin
      (qt-save-place-load!)
      ;; Restore position for current buffer
      (let* ((buf (current-qt-buffer app))
             (path (and buf (buffer-file-path buf))))
        (when path
          (let ((pos (hash-get *qt-save-place-db* path)))
            (when pos
              (let ((ed (current-qt-editor app)))
                (sci-send ed SCI_GOTOPOS pos))))))
      (echo-message! (app-state-echo app)
        (string-append "Save-place: on ("
                       (number->string (hash-length *qt-save-place-db*))
                       " places remembered)")))
    (begin
      (qt-save-place-save!)
      (echo-message! (app-state-echo app) "Save-place: off (positions saved)"))))

;;;============================================================================
;;; Envrc / direnv — real .envrc loading (Qt)
;;;============================================================================

(def *qt-envrc-active* #f)
(def *qt-envrc-original-env* (make-hash-table))

(def (cmd-envrc-mode-real app)
  "Toggle envrc-mode — loads .envrc environment (Qt)."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (dir (if path (path-directory path) (current-directory)))
         (envrc (string-append dir "/.envrc"))
         (echo (app-state-echo app)))
    (if (not *qt-envrc-active*)
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
                          (unless (hash-get *qt-envrc-original-env* key)
                            (hash-put! *qt-envrc-original-env* key (or (getenv key) "")))
                          (setenv key val)
                          (set! count (+ count 1))))))
                  (string-split output #\newline)))
              (set! *qt-envrc-active* #t)
              (echo-message! echo
                (string-append "Envrc: loaded " (number->string count) " vars from " envrc))))))
      (begin
        (hash-for-each (lambda (k v) (setenv k v)) *qt-envrc-original-env*)
        (hash-clear! *qt-envrc-original-env*)
        (set! *qt-envrc-active* #f)
        (echo-message! echo "Envrc: unloaded (environment restored)")))))

;;;============================================================================
;;; Focus-mode — dim lines far from cursor (Qt)
;;;============================================================================

(def *qt-focus-indicator* 4)
(def *qt-focus-active* #f)
(def *qt-focus-range* 5)

(def (qt-focus-apply! ed)
  (let* ((len (sci-send ed SCI_GETLENGTH))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (cur-line (sci-send ed SCI_LINEFROMPOSITION pos))
         (total-lines (sci-send ed SCI_GETLINECOUNT))
         (focus-start (max 0 (- cur-line *qt-focus-range*)))
         (focus-end (min (- total-lines 1) (+ cur-line *qt-focus-range*))))
    (sci-send ed SCI_SETINDICATORCURRENT *qt-focus-indicator*)
    (when (> len 0)
      (sci-send ed SCI_INDICATORCLEARRANGE 0 len))
    (sci-send ed SCI_INDICSETSTYLE *qt-focus-indicator* 6)
    (sci-send ed SCI_INDICSETFORE *qt-focus-indicator* #x808080)
    (sci-send ed SCI_SETINDICATORCURRENT *qt-focus-indicator*)
    (when (> focus-start 0)
      (let ((end (sci-send ed SCI_POSITIONFROMLINE focus-start)))
        (when (> end 0)
          (sci-send ed SCI_INDICATORFILLRANGE 0 end))))
    (when (< focus-end (- total-lines 1))
      (let ((start (sci-send ed SCI_POSITIONFROMLINE (+ focus-end 1))))
        (when (< start len)
          (sci-send ed SCI_INDICATORFILLRANGE start (- len start)))))))

(def (qt-focus-clear! ed)
  (let ((len (sci-send ed SCI_GETLENGTH)))
    (when (> len 0)
      (sci-send ed SCI_SETINDICATORCURRENT *qt-focus-indicator*)
      (sci-send ed SCI_INDICATORCLEARRANGE 0 len))))

(def (cmd-focus-mode-real app)
  "Toggle focus-mode (Qt)."
  (let ((ed (current-qt-editor app)))
    (if *qt-focus-active*
      (begin (qt-focus-clear! ed)
             (set! *qt-focus-active* #f)
             (echo-message! (app-state-echo app) "Focus: off"))
      (begin (qt-focus-apply! ed)
             (set! *qt-focus-active* #t)
             (echo-message! (app-state-echo app)
               (string-append "Focus: on (±" (number->string *qt-focus-range*) " lines)"))))))

(def (cmd-focus-refresh app)
  "Refresh focus dimming (Qt)."
  (when *qt-focus-active*
    (qt-focus-apply! (current-qt-editor app))))

(def (cmd-focus-set-range app)
  "Set focus range (Qt)."
  (let ((input (qt-echo-read-string app "Focus range (lines): ")))
    (when (and input (not (string-empty? input)))
      (let ((n (string->number input)))
        (when (and n (> n 0))
          (set! *qt-focus-range* n)
          (when *qt-focus-active*
            (cmd-focus-refresh app))
          (echo-message! (app-state-echo app)
            (string-append "Focus range: ±" (number->string n) " lines")))))))

;;;============================================================================
;;; Golden-ratio — auto-resize windows (Qt)
;;;============================================================================

(def *qt-golden-ratio-active* #f)

(def (qt-golden-ratio-apply! app)
  "Apply golden ratio to the splitter containing the current window."
  (let* ((fr (app-state-frame app))
         (cur (qt-current-window fr))
         (parent (split-tree-find-parent (qt-frame-root fr) cur)))
    (when parent
      (let* ((splitter (split-node-splitter parent))
             (children (split-node-children parent))
             (n (length children))
             (cur-idx (let loop ((i 0) (cs children))
                        (cond ((null? cs) 0)
                              ((eq? (car cs) cur) i)
                              (else (loop (+ i 1) (cdr cs))))))
             (total 1000)
             (golden (inexact->exact (round (* total (/ 1.618 (+ 1.618 (- n 1)))))))
             (rest (if (> (- n 1) 0)
                     (inexact->exact (round (/ (- total golden) (- n 1))))
                     0))
             (sizes (let loop ((i 0) (acc '()))
                      (if (= i n) (reverse acc)
                          (loop (+ i 1) (cons (if (= i cur-idx) golden rest) acc))))))
        (qt-splitter-set-sizes! splitter sizes)))))

(def (cmd-golden-ratio-mode-real app)
  "Toggle golden-ratio-mode (Qt) — active window gets golden ratio of space."
  (set! *qt-golden-ratio-active* (not *qt-golden-ratio-active*))
  (if *qt-golden-ratio-active*
    (begin
      (with-exception-catcher
        (lambda (e) #f)
        (lambda () (qt-golden-ratio-apply! app)))
      (echo-message! (app-state-echo app) "Golden ratio: on"))
    (begin
      ;; Balance windows
      (let* ((fr (app-state-frame app))
             (cur (qt-current-window fr))
             (parent (split-tree-find-parent (qt-frame-root fr) cur)))
        (when parent
          (let* ((splitter (split-node-splitter parent))
                 (n (length (split-node-children parent))))
            (qt-splitter-set-sizes! splitter (make-list n 500)))))
      (echo-message! (app-state-echo app) "Golden ratio: off (windows balanced)"))))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity6-commands!)
  "Register parity6 commands."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
      ;; Org-roam (overrides old stubs)
      (cons 'org-roam-node-find cmd-org-roam-node-find)
      (cons 'org-roam-node-insert cmd-org-roam-node-insert)
      (cons 'org-roam-buffer-toggle cmd-org-roam-buffer-toggle)
      (cons 'org-roam-capture cmd-org-roam-capture)
      (cons 'org-roam-graph cmd-org-roam-graph)
      (cons 'org-roam-set-directory cmd-org-roam-set-directory)
      (cons 'org-roam-db-sync cmd-org-roam-db-sync)
      (cons 'org-roam-find-file cmd-org-roam-find-file)
      (cons 'org-roam-dailies-today cmd-org-roam-dailies-today)
      ;; Artist mode (overrides old toggle)
      (cons 'artist-mode cmd-artist-mode)
      (cons 'artist-select-tool cmd-artist-select-tool)
      (cons 'artist-set-char cmd-artist-set-char)
      (cons 'artist-draw-line cmd-artist-draw-line)
      (cons 'artist-draw-rectangle cmd-artist-draw-rectangle)
      (cons 'artist-draw-text cmd-artist-draw-text)
      (cons 'artist-erase-rect cmd-artist-erase-rect)
      (cons 'artist-draw-arrow cmd-artist-draw-arrow)
      (cons 'artist-draw-ellipse cmd-artist-draw-ellipse)
      ;; Calc embedded
      (cons 'calc-embedded cmd-calc-embedded)
      (cons 'calc-eval-line cmd-calc-eval-line)
      (cons 'calc-embedded-eval-region cmd-calc-embedded-eval-region)
      (cons 'calc-grab-region cmd-calc-grab-region)
      (cons 'calc-sum-region cmd-calc-sum-region)
      ;; HL-todo
      (cons 'hl-todo-highlight cmd-hl-todo-highlight)
      (cons 'hl-todo-refresh cmd-hl-todo-refresh)
      (cons 'hl-todo-occur cmd-hl-todo-occur)
      ;; Auto-revert: already in commands-core.ss / commands-shell.ss
      ;; Writeroom / Olivetti
      (cons 'writeroom-mode-real cmd-writeroom-mode-real)
      (cons 'olivetti-mode-real cmd-olivetti-mode-real)
      ;; Rainbow
      (cons 'rainbow-mode-real cmd-rainbow-mode-real)
      (cons 'rainbow-refresh cmd-rainbow-refresh)
      ;; Save-place
      (cons 'save-place-mode-real cmd-save-place-mode-real)
      ;; Envrc
      (cons 'envrc-mode-real cmd-envrc-mode-real)
      ;; Focus mode
      (cons 'focus-mode-real cmd-focus-mode-real)
      (cons 'focus-refresh cmd-focus-refresh)
      (cons 'focus-set-range cmd-focus-set-range)
      ;; Golden ratio
      (cons 'golden-ratio-mode-real cmd-golden-ratio-mode-real))))
