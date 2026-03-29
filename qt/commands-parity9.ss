;;; -*- Gerbil -*-
;;; Qt parity commands (part 9) — deft, dictionary, speed-type,
;;; pomodoro, doctor (Eliza), figlet, dice roller, morse code,
;;; gomoku, chronometer.
;;; Chain position: after commands-parity8.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/sort
        :std/misc/ports
        :gemacs/core
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/commands-core
        :gemacs/echo)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (qt-p9-echo app msg)
  (echo-message! (app-state-echo app) msg))

(def (qt-p9-echo-err app msg)
  (echo-error! (app-state-echo app) msg))

(def (qt-p9-open-buffer app name text)
  "Create or reuse a named buffer and set its text."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (or (buffer-by-name name) (qt-buffer-create! name ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; 1. Deft — Quick note search/create (Qt)
;;;============================================================================

(def *qt-deft-directory* #f)

(def (qt-deft-dir)
  (or *qt-deft-directory*
      (string-append (getenv "HOME" "/tmp") "/notes")))

(def (qt-deft-scan dir)
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let* ((proc (open-process
                     (list path: "find" arguments: (list dir "-maxdepth" "2"
                           "-name" "*.org" "-o" "-name" "*.md" "-o" "-name" "*.txt")
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #f)))
             (out (read-line proc #f)))
        (process-status proc)
        (if (or (not out) (string=? out ""))
          '()
          (filter (lambda (s) (not (string=? s "")))
            (string-split out #\newline)))))))

(def (cmd-qt-deft app)
  "Open Deft note browser."
  (let* ((dir (qt-deft-dir))
         (files (qt-deft-scan dir))
         (text (with-output-to-string
                 (lambda ()
                   (display "Deft: Notes in ") (display dir) (display "\n")
                   (display (make-string 60 #\-)) (display "\n\n")
                   (if (null? files)
                     (display "(no notes found)\n")
                     (for-each
                       (lambda (f)
                         (display (path-strip-directory f))
                         (display "\n  ") (display f) (display "\n"))
                       (sort string<? files)))))))
    (qt-p9-open-buffer app "*Deft*" text)
    (qt-p9-echo app (string-append "Deft: " (number->string (length files)) " notes"))))

(def (cmd-qt-deft-new app)
  "Create a new note."
  (let ((title (qt-echo-read-string app "Note title: ")))
    (if (or (not title) (string=? (string-trim-both title) ""))
      (qt-p9-echo-err app "No title given")
      (let* ((dir (qt-deft-dir))
             (safe-title (string-map
                           (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)
                                               (eqv? c #\-) (eqv? c #\_))
                                         c #\-))
                           title))
             (fname (string-append dir "/" safe-title ".md")))
        (with-exception-catcher void (lambda () (create-directory* dir)))
        (qt-p9-open-buffer app (path-strip-directory fname)
          (string-append "# " title "\n\n"))
        (qt-p9-echo app (string-append "New note: " fname))))))

(def (cmd-qt-deft-search app)
  "Search notes by content."
  (let ((query (qt-echo-read-string app "Search notes: ")))
    (if (or (not query) (string=? (string-trim-both query) ""))
      (qt-p9-echo-err app "No search query")
      (with-exception-catcher
        (lambda (e) (qt-p9-echo-err app "Search failed"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "grep" arguments: (list "-rl" "-i" query (qt-deft-dir))
                               stdin-redirection: #f stdout-redirection: #t
                               stderr-redirection: #f)))
                 (out (read-line proc #f)))
            (process-status proc)
            (let ((results (if (or (not out) (string=? out ""))
                             '()
                             (filter (lambda (s) (not (string=? s "")))
                               (string-split out #\newline)))))
              (qt-p9-open-buffer app "*Deft Search*"
                (string-append "Search: \"" query "\"\n"
                  (make-string 40 #\-) "\n\n"
                  (if (null? results) "(no matches)\n"
                    (string-join results "\n"))))
              (qt-p9-echo app (string-append (number->string (length results)) " matches")))))))))

;;;============================================================================
;;; 2. Dictionary (Qt)
;;;============================================================================

(def (cmd-qt-dictionary app)
  "Look up word definition."
  (let ((word (qt-echo-read-string app "Word: ")))
    (if (or (not word) (string=? (string-trim-both word) ""))
      (qt-p9-echo-err app "No word given")
      (with-exception-catcher
        (lambda (e) (qt-p9-echo-err app "dict command not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "dict" arguments: (list word)
                               stdin-redirection: #f stdout-redirection: #t
                               stderr-redirection: #t)))
                 (out (read-line proc #f)))
            (process-status proc)
            (qt-p9-open-buffer app "*Dictionary*"
              (string-append "Definition of \"" word "\"\n"
                (make-string 40 #\-) "\n\n"
                (or out "(no definition found)")))
            (qt-p9-echo app (string-append "Dictionary: " word))))))))

(def (cmd-qt-dictionary-at-point app)
  "Look up word at cursor."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (word-start (sci-send ed SCI_WORDSTARTPOSITION pos 1))
         (word-end (sci-send ed SCI_WORDENDPOSITION pos 1))
         (text (qt-plain-text-edit-text ed))
         (word (if (and (>= word-start 0) (<= word-end (string-length text))
                        (> word-end word-start))
                 (substring text word-start word-end) "")))
    (if (string=? word "")
      (qt-p9-echo-err app "No word at point")
      (with-exception-catcher
        (lambda (e) (qt-p9-echo-err app "dict not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "dict" arguments: (list word)
                               stdin-redirection: #f stdout-redirection: #t
                               stderr-redirection: #t)))
                 (out (read-line proc #f)))
            (process-status proc)
            (qt-p9-open-buffer app "*Dictionary*"
              (string-append "Definition of \"" word "\"\n"
                (make-string 40 #\-) "\n\n"
                (or out "(no definition found)")))
            (qt-p9-echo app (string-append "Dictionary: " word))))))))

;;;============================================================================
;;; 3. Speed-type (Qt)
;;;============================================================================

(def *qt-speed-samples*
  '("The quick brown fox jumps over the lazy dog near the river bank."
    "To be or not to be, that is the question whether it is nobler to suffer."
    "All that glitters is not gold; often have you heard that told."
    "It was the best of times, it was the worst of times, it was the age of wisdom."
    "The only thing we have to fear is fear itself."))

(def *qt-speed-start* #f)
(def *qt-speed-target* #f)

(def (cmd-qt-speed-type app)
  "Start typing speed test."
  (let* ((sample (list-ref *qt-speed-samples*
                   (modulo (time->seconds (current-time))
                           (length *qt-speed-samples*))))
         (text (string-append
                 "=== SPEED TYPE TEST ===\n\n"
                 "Type the following text:\n\n"
                 ">>> " sample " <<<\n\n"
                 "Your input:\n" (make-string 40 #\-) "\n")))
    (set! *qt-speed-target* sample)
    (set! *qt-speed-start* (time->seconds (current-time)))
    (qt-p9-open-buffer app "*Speed Type*" text)
    ;; Move cursor to end so user can type
    (let ((ed (current-qt-editor app)))
      (qt-plain-text-edit-set-cursor-position! ed (string-length text)))
    (qt-p9-echo app "Start typing! M-x speed-type-results when done.")))

(def (cmd-qt-speed-type-results app)
  "Show typing speed results."
  (if (not *qt-speed-target*)
    (qt-p9-echo-err app "No speed test in progress")
    (let* ((ed (current-qt-editor app))
           (text (qt-plain-text-edit-text ed))
           (marker "----")
           (marker-pos (string-contains text marker))
           (typed (if marker-pos
                    (string-trim-both (substring text (+ marker-pos (string-length marker))
                                       (string-length text)))
                    ""))
           (elapsed (- (time->seconds (current-time)) *qt-speed-start*))
           (target *qt-speed-target*)
           (target-len (string-length target))
           (typed-len (string-length typed))
           (min-len (min target-len typed-len))
           (correct (let loop ((i 0) (acc 0))
                      (if (>= i min-len) acc
                        (loop (+ i 1)
                          (if (eqv? (string-ref target i) (string-ref typed i))
                            (+ acc 1) acc)))))
           (accuracy (if (> target-len 0)
                       (inexact->exact (round (* 100.0 (/ correct target-len)))) 0))
           (wpm (let ((minutes (/ elapsed 60.0)))
                  (if (> minutes 0)
                    (inexact->exact (round (/ (/ typed-len 5.0) minutes))) 0))))
      (set! *qt-speed-target* #f)
      (qt-plain-text-edit-set-text! ed
        (string-append text "\n\n=== RESULTS ===\n"
          "WPM: " (number->string wpm) "\n"
          "Accuracy: " (number->string accuracy) "%\n"
          "Time: " (number->string (inexact->exact (round elapsed))) "s\n"))
      (qt-p9-echo app (string-append "WPM: " (number->string wpm)
                         " | Accuracy: " (number->string accuracy) "%")))))

;;;============================================================================
;;; 4. Pomodoro (Qt)
;;;============================================================================

(def *qt-pomo-start* #f)
(def *qt-pomo-mode* #f)
(def *qt-pomo-count* 0)
(def *qt-pomo-work-mins* 25)
(def *qt-pomo-break-mins* 5)

(def (cmd-qt-pomodoro-start app)
  "Start Pomodoro work timer."
  (set! *qt-pomo-start* (time->seconds (current-time)))
  (set! *qt-pomo-mode* 'work)
  (qt-p9-echo app (string-append "Pomodoro #" (number->string (+ *qt-pomo-count* 1))
                     " started (" (number->string *qt-pomo-work-mins*) " min)")))

(def (cmd-qt-pomodoro-break app)
  "Start Pomodoro break."
  (set! *qt-pomo-start* (time->seconds (current-time)))
  (set! *qt-pomo-mode* 'break)
  (set! *qt-pomo-count* (+ *qt-pomo-count* 1))
  (qt-p9-echo app (string-append "Break (" (number->string *qt-pomo-break-mins*) " min)")))

(def (cmd-qt-pomodoro-status app)
  "Show Pomodoro status."
  (if (not *qt-pomo-start*)
    (qt-p9-echo app "No Pomodoro running")
    (let* ((elapsed (- (time->seconds (current-time)) *qt-pomo-start*))
           (total (if (eq? *qt-pomo-mode* 'work)
                    (* *qt-pomo-work-mins* 60) (* *qt-pomo-break-mins* 60)))
           (remaining (max 0 (- total elapsed)))
           (mins (quotient (inexact->exact (floor remaining)) 60))
           (secs (modulo (inexact->exact (floor remaining)) 60))
           (mode-str (if (eq? *qt-pomo-mode* 'work) "WORK" "BREAK")))
      (if (<= remaining 0)
        (qt-p9-echo app (string-append mode-str " complete!"))
        (qt-p9-echo app (string-append mode-str " " (number->string mins) ":"
                           (if (< secs 10) "0" "") (number->string secs)))))))

(def (cmd-qt-pomodoro-reset app)
  "Reset Pomodoro."
  (set! *qt-pomo-start* #f)
  (set! *qt-pomo-mode* #f)
  (set! *qt-pomo-count* 0)
  (qt-p9-echo app "Pomodoro reset"))

;;;============================================================================
;;; 5. Doctor / Eliza (Qt)
;;;============================================================================

(def *qt-eliza-rules* (make-hash-table))
(begin
  (hash-put! *qt-eliza-rules* "hello" '("How do you do. Tell me your problem." "Hi. What troubles you?"))
  (hash-put! *qt-eliza-rules* "i need" '("Why do you need %s?" "Would it help to get %s?"))
  (hash-put! *qt-eliza-rules* "i am" '("How long have you been %s?" "Do you enjoy being %s?"))
  (hash-put! *qt-eliza-rules* "i feel" '("Tell me more about feeling %s." "Do you often feel %s?"))
  (hash-put! *qt-eliza-rules* "i want" '("What would it mean if you got %s?" "Why do you want %s?"))
  (hash-put! *qt-eliza-rules* "i can't" '("What makes you think you can't %s?" "Have you tried?"))
  (hash-put! *qt-eliza-rules* "why" '("Why do you ask?" "What do you think?" "Does that interest you?"))
  (hash-put! *qt-eliza-rules* "because" '("Is that the real reason?" "Other reasons?"))
  (hash-put! *qt-eliza-rules* "sorry" '("Don't apologize." "Apologies not necessary."))
  (hash-put! *qt-eliza-rules* "mother" '("Tell me about your mother." "How was your relationship?"))
  (hash-put! *qt-eliza-rules* "father" '("Tell me about your father." "How did he make you feel?"))
  (hash-put! *qt-eliza-rules* "yes" '("You seem sure." "I see." "Elaborate?"))
  (hash-put! *qt-eliza-rules* "no" '("Why not?" "You're being negative."))
  (hash-put! *qt-eliza-rules* "dream" '("What does that dream suggest?" "Do you dream often?"))
  (hash-put! *qt-eliza-rules* "sad" '("I'm sorry you're sad." "What makes you sad?"))
  (hash-put! *qt-eliza-rules* "happy" '("What makes you happy?" "Really happy?"))
  (hash-put! *qt-eliza-rules* "afraid" '("What are you afraid of?" "Does fear affect you?"))
  (hash-put! *qt-eliza-rules* "think" '("Why do you think that?" "Really think so?"))
  (hash-put! *qt-eliza-rules* "always" '("Can you give an example?" "Literally always?"))
  (hash-put! *qt-eliza-rules* "never" '("Never? Not once?" "Why say never?")))

(def *qt-eliza-defaults*
  '("Please go on." "Tell me more." "How does that make you feel?"
    "Elaborate?" "Interesting." "I see." "What does that suggest?"))
(def *qt-eliza-idx* 0)

(def (qt-eliza-respond input)
  (let ((lower (string-downcase (string-trim-both input))))
    (let loop ((keys (hash-keys *qt-eliza-rules*)))
      (if (null? keys)
        (let ((r (list-ref *qt-eliza-defaults* (modulo *qt-eliza-idx* (length *qt-eliza-defaults*)))))
          (set! *qt-eliza-idx* (+ *qt-eliza-idx* 1)) r)
        (let ((pos (string-contains lower (car keys))))
          (if pos
            (let* ((resps (hash-ref *qt-eliza-rules* (car keys)))
                   (r (list-ref resps (modulo *qt-eliza-idx* (length resps))))
                   (rest (string-trim-both
                           (substring lower (+ pos (string-length (car keys)))
                             (string-length lower)))))
              (set! *qt-eliza-idx* (+ *qt-eliza-idx* 1))
              (if (string-contains r "%s")
                (let ((sub (if (string=? rest "") "that" rest)))
                  (string-append
                    (substring r 0 (string-contains r "%s")) sub
                    (substring r (+ (string-contains r "%s") 2) (string-length r))))
                r))
            (loop (cdr keys))))))))

(def (cmd-qt-doctor app)
  "Start Eliza psychotherapist."
  (set! *qt-eliza-idx* 0)
  (qt-p9-open-buffer app "*Doctor*"
    (string-append
      "I am the psychotherapist. Please describe your problems.\n"
      "Type your message, then M-x doctor-submit.\n\n> "))
  ;; Move cursor to end
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-set-cursor-position! ed
      (string-length (qt-plain-text-edit-text ed))))
  (qt-p9-echo app "Doctor is in."))

(def (cmd-qt-doctor-submit app)
  "Submit input to Eliza."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (last-prompt (let loop ((pos (- (string-length text) 1)))
                        (if (< pos 1) 0
                          (if (and (eqv? (string-ref text (- pos 1)) #\>)
                                   (eqv? (string-ref text pos) #\space))
                            (+ pos 1)
                            (loop (- pos 1))))))
         (input (string-trim-both (substring text last-prompt (string-length text))))
         (response (qt-eliza-respond input))
         (new-text (string-append text "\n\n[Doctor]: " response "\n\n> ")))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (string-length new-text))))

;;;============================================================================
;;; 6. Figlet (Qt)
;;;============================================================================

(def (qt-figlet-fallback text)
  (let* ((upper (string-upcase text))
         (border (make-string (+ (string-length upper) 4) #\#)))
    (string-append border "\n# " upper " #\n" border "\n")))

(def (cmd-qt-figlet app)
  "Insert ASCII art banner."
  (let ((text (qt-echo-read-string app "Figlet text: ")))
    (if (or (not text) (string=? (string-trim-both text) ""))
      (qt-p9-echo-err app "No text")
      (let ((ed (current-qt-editor app)))
        (with-exception-catcher
          (lambda (e)
            (sci-send/string ed SCI_REPLACESEL (qt-figlet-fallback text))
            (qt-p9-echo app "Figlet: built-in renderer"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "figlet" arguments: (list text)
                                 stdin-redirection: #f stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (out (read-line proc #f)))
              (process-status proc)
              (if (or (not out) (string=? out ""))
                (begin
                  (sci-send/string ed SCI_REPLACESEL (qt-figlet-fallback text))
                  (qt-p9-echo app "Figlet: built-in renderer"))
                (begin
                  (sci-send/string ed SCI_REPLACESEL out)
                  (qt-p9-echo app "Figlet: inserted"))))))))))

(def (cmd-qt-figlet-comment app)
  "Insert figlet-style comment banner."
  (let ((text (qt-echo-read-string app "Banner text: ")))
    (if (or (not text) (string=? (string-trim-both text) ""))
      (qt-p9-echo-err app "No text")
      (let* ((ed (current-qt-editor app))
             (upper (string-upcase text))
             (border (make-string (+ (string-length upper) 8) #\=)))
        (sci-send/string ed SCI_REPLACESEL
          (string-append ";;;" border "\n;;;    " upper "    \n;;;" border "\n"))
        (qt-p9-echo app "Banner inserted")))))

;;;============================================================================
;;; 7. Dice roller (Qt)
;;;============================================================================

(def (qt-parse-dice expr)
  (let* ((s (string-downcase (string-trim-both expr)))
         (d-pos (string-contains s "d")))
    (if (not d-pos)
      (let ((n (string->number s)))
        (cons (or n 0) (or (and n (number->string n)) "invalid")))
      (let* ((num-str (substring s 0 d-pos))
             (num-dice (if (string=? num-str "") 1 (or (string->number num-str) 1)))
             (rest (substring s (+ d-pos 1) (string-length s)))
             (plus-pos (string-contains rest "+"))
             (minus-pos (string-contains rest "-"))
             (mod-pos (or plus-pos minus-pos))
             (sides (or (string->number (if mod-pos (substring rest 0 mod-pos) rest)) 6))
             (modifier (if mod-pos
                        (or (string->number (substring rest mod-pos (string-length rest))) 0) 0))
             (rolls (let loop ((i 0) (acc '()))
                      (if (>= i num-dice) (reverse acc)
                        (loop (+ i 1) (cons (+ 1 (random-integer sides)) acc)))))
             (total (+ (apply + rolls) modifier)))
        (cons total
          (string-append (number->string num-dice) "d" (number->string sides)
            " = [" (string-join (map number->string rolls) ", ") "]"
            (if (not (= modifier 0))
              (string-append (if (> modifier 0) "+" "") (number->string modifier)) "")
            " = " (number->string total)))))))

(def (cmd-qt-dice-roll app)
  "Roll dice."
  (let ((expr (qt-echo-read-string app "Dice (e.g. 2d6+3): ")))
    (if (or (not expr) (string=? (string-trim-both expr) ""))
      (qt-p9-echo-err app "No dice expression")
      (qt-p9-echo app (string-append "Roll: " (cdr (qt-parse-dice expr)))))))

(def (cmd-qt-dice-roll-insert app)
  "Roll dice and insert result."
  (let ((expr (qt-echo-read-string app "Dice (e.g. 2d6+3): ")))
    (if (or (not expr) (string=? (string-trim-both expr) ""))
      (qt-p9-echo-err app "No dice expression")
      (let* ((result (qt-parse-dice expr))
             (ed (current-qt-editor app)))
        (sci-send/string ed SCI_REPLACESEL (number->string (car result)))
        (qt-p9-echo app (cdr result))))))

;;;============================================================================
;;; 8. Morse code (Qt)
;;;============================================================================

(def *qt-morse* (make-hash-table))
(def *qt-morse-rev* (make-hash-table))
(begin
  (for-each (lambda (p) (hash-put! *qt-morse* (car p) (cdr p))
                        (hash-put! *qt-morse-rev* (cdr p) (car p)))
    '((#\A . ".-") (#\B . "-...") (#\C . "-.-.") (#\D . "-..") (#\E . ".")
      (#\F . "..-.") (#\G . "--.") (#\H . "....") (#\I . "..") (#\J . ".---")
      (#\K . "-.-") (#\L . ".-..") (#\M . "--") (#\N . "-.") (#\O . "---")
      (#\P . ".--.") (#\Q . "--.-") (#\R . ".-.") (#\S . "...") (#\T . "-")
      (#\U . "..-") (#\V . "...-") (#\W . ".--") (#\X . "-..-") (#\Y . "-.--")
      (#\Z . "--..") (#\0 . "-----") (#\1 . ".----") (#\2 . "..---")
      (#\3 . "...--") (#\4 . "....-") (#\5 . ".....") (#\6 . "-....")
      (#\7 . "--...") (#\8 . "---..") (#\9 . "----."))))

(def (qt-text->morse text)
  (string-join
    (map (lambda (c)
           (if (eqv? c #\space) "/"
             (hash-ref *qt-morse* (char-upcase c) "?")))
         (string->list text))
    " "))

(def (qt-morse->text morse)
  (string-join
    (map (lambda (word)
           (list->string
             (map (lambda (code)
                    (if (string=? code "") #\space
                      (hash-ref *qt-morse-rev* code #\?)))
                  (string-split word #\space))))
         (string-split morse #\/))
    " "))

(def (cmd-qt-morse-encode app)
  "Encode selection or prompted text to Morse."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (not (= sel-start sel-end))
      (let* ((text (qt-plain-text-edit-text ed))
             (sel (if (and (>= sel-start 0) (<= sel-end (string-length text)))
                    (substring text sel-start sel-end) ""))
             (morse (qt-text->morse sel)))
        (sci-send/string ed SCI_REPLACESEL morse)
        (qt-p9-echo app "Encoded to Morse"))
      (let ((text (qt-echo-read-string app "Text to encode: ")))
        (if (or (not text) (string=? text ""))
          (qt-p9-echo-err app "No text")
          (qt-p9-echo app (string-append "Morse: " (qt-text->morse text))))))))

(def (cmd-qt-morse-decode app)
  "Decode Morse from selection or prompt."
  (let* ((ed (current-qt-editor app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART))
         (sel-end (sci-send ed SCI_GETSELECTIONEND)))
    (if (not (= sel-start sel-end))
      (let* ((text (qt-plain-text-edit-text ed))
             (sel (if (and (>= sel-start 0) (<= sel-end (string-length text)))
                    (substring text sel-start sel-end) ""))
             (decoded (qt-morse->text sel)))
        (sci-send/string ed SCI_REPLACESEL decoded)
        (qt-p9-echo app "Decoded from Morse"))
      (let ((morse (qt-echo-read-string app "Morse to decode: ")))
        (if (or (not morse) (string=? morse ""))
          (qt-p9-echo-err app "No morse")
          (qt-p9-echo app (string-append "Text: " (qt-morse->text morse))))))))

;;;============================================================================
;;; 9. Gomoku (Qt)
;;;============================================================================

(def *qt-gom-size* 15)
(def *qt-gom-board* #f)
(def *qt-gom-cx* 7) (def *qt-gom-cy* 7)
(def *qt-gom-turn* 'X)

(def (qt-gom-init!)
  (set! *qt-gom-board* (make-vector (* *qt-gom-size* *qt-gom-size*) #\.))
  (set! *qt-gom-cx* 7) (set! *qt-gom-cy* 7) (set! *qt-gom-turn* 'X))

(def (qt-gom-get x y)
  (and (>= x 0) (< x *qt-gom-size*) (>= y 0) (< y *qt-gom-size*)
       (vector-ref *qt-gom-board* (+ x (* y *qt-gom-size*)))))

(def (qt-gom-set! x y v)
  (when (and (>= x 0) (< x *qt-gom-size*) (>= y 0) (< y *qt-gom-size*))
    (vector-set! *qt-gom-board* (+ x (* y *qt-gom-size*)) v)))

(def (qt-gom-check-win ch)
  (let ((dirs '((1 0) (0 1) (1 1) (1 -1))))
    (let yloop ((y 0))
      (if (>= y *qt-gom-size*) #f
        (let xloop ((x 0))
          (if (>= x *qt-gom-size*) (yloop (+ y 1))
            (if (not (eqv? (qt-gom-get x y) ch)) (xloop (+ x 1))
              (let dloop ((ds dirs))
                (if (null? ds) (xloop (+ x 1))
                  (let ((count (let cloop ((i 1))
                                 (if (>= i 5) 5
                                   (if (eqv? (qt-gom-get (+ x (* i (caar ds)))
                                               (+ y (* i (cadar ds)))) ch)
                                     (cloop (+ i 1)) i)))))
                    (if (>= count 5) #t (dloop (cdr ds)))))))))))))

(def (qt-gom-score x y ch)
  (let ((dirs '((1 0) (0 1) (1 1) (1 -1))) (total 0))
    (for-each
      (lambda (d)
        (let* ((dx (car d)) (dy (cadr d))
               (fwd (let loop ((i 1))
                      (if (or (>= i 5) (not (eqv? (qt-gom-get (+ x (* i dx)) (+ y (* i dy))) ch)))
                        (- i 1) (loop (+ i 1)))))
               (bwd (let loop ((i 1))
                      (if (or (>= i 5) (not (eqv? (qt-gom-get (- x (* i dx)) (- y (* i dy))) ch)))
                        (- i 1) (loop (+ i 1)))))
               (line (+ fwd bwd)))
          (set! total (+ total (cond ((>= line 4) 10000) ((>= line 3) 100)
                                     ((>= line 2) 10) ((>= line 1) 1) (else 0))))))
      dirs)
    total))

(def (qt-gom-ai)
  (let ((bx 0) (by 0) (bs -1))
    (let yloop ((y 0))
      (when (< y *qt-gom-size*)
        (let xloop ((x 0))
          (when (< x *qt-gom-size*)
            (when (eqv? (qt-gom-get x y) #\.)
              (let ((s (+ (* (qt-gom-score x y #\O) 2) (qt-gom-score x y #\X))))
                (when (> s bs) (set! bs s) (set! bx x) (set! by y))))
            (xloop (+ x 1))))
        (yloop (+ y 1))))
    (cons bx by)))

(def (qt-gom-render)
  (with-output-to-string
    (lambda ()
      (display "  GOMOKU (Five in a Row) - You: X | Computer: O\n\n   ")
      (let loop ((i 0))
        (when (< i *qt-gom-size*) (display (modulo i 10)) (display " ") (loop (+ i 1))))
      (display "\n")
      (let yloop ((y 0))
        (when (< y *qt-gom-size*)
          (display (if (< y 10) " " "")) (display y) (display " ")
          (let xloop ((x 0))
            (when (< x *qt-gom-size*)
              (display (if (and (= x *qt-gom-cx*) (= y *qt-gom-cy*))
                         (let ((ch (qt-gom-get x y))) (if (eqv? ch #\.) #\+ ch))
                         (qt-gom-get x y)))
              (display " ") (xloop (+ x 1))))
          (display "\n") (yloop (+ y 1))))
      (display "\n") (display (if (eq? *qt-gom-turn* 'X) "Your turn" "Computer...")) (display "\n"))))

(def (qt-gom-refresh! app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-set-text! ed (qt-gom-render))))

(def (cmd-qt-gomoku app)
  "Start Gomoku."
  (qt-gom-init!)
  (qt-p9-open-buffer app "*Gomoku*" (qt-gom-render))
  (qt-p9-echo app "Gomoku: gomoku-up/down/left/right, gomoku-place"))

(def (cmd-qt-gomoku-up app) (when (> *qt-gom-cy* 0) (set! *qt-gom-cy* (- *qt-gom-cy* 1))) (qt-gom-refresh! app))
(def (cmd-qt-gomoku-down app) (when (< *qt-gom-cy* (- *qt-gom-size* 1)) (set! *qt-gom-cy* (+ *qt-gom-cy* 1))) (qt-gom-refresh! app))
(def (cmd-qt-gomoku-left app) (when (> *qt-gom-cx* 0) (set! *qt-gom-cx* (- *qt-gom-cx* 1))) (qt-gom-refresh! app))
(def (cmd-qt-gomoku-right app) (when (< *qt-gom-cx* (- *qt-gom-size* 1)) (set! *qt-gom-cx* (+ *qt-gom-cx* 1))) (qt-gom-refresh! app))

(def (cmd-qt-gomoku-place app)
  "Place stone."
  (when (and *qt-gom-board* (eq? *qt-gom-turn* 'X))
    (if (not (eqv? (qt-gom-get *qt-gom-cx* *qt-gom-cy*) #\.))
      (qt-p9-echo-err app "Occupied!")
      (begin
        (qt-gom-set! *qt-gom-cx* *qt-gom-cy* #\X)
        (cond
          ((qt-gom-check-win #\X)
           (qt-gom-refresh! app) (qt-p9-echo app "You win!"))
          (else
           (set! *qt-gom-turn* 'O)
           (let ((m (qt-gom-ai)))
             (qt-gom-set! (car m) (cdr m) #\O)
             (set! *qt-gom-turn* 'X)
             (qt-gom-refresh! app)
             (if (qt-gom-check-win #\O)
               (qt-p9-echo app "Computer wins!")
               (qt-p9-echo app (string-append "Computer: ("
                 (number->string (car m)) "," (number->string (cdr m)) ")"))))))))))

;;;============================================================================
;;; 10. Chronometer (Qt)
;;;============================================================================

(def *qt-chrono-start* #f)
(def *qt-chrono-running* #f)
(def *qt-chrono-laps* '())

(def (cmd-qt-chronometer-start app)
  "Start chronometer."
  (set! *qt-chrono-start* (time->seconds (current-time)))
  (set! *qt-chrono-running* #t) (set! *qt-chrono-laps* '())
  (qt-p9-echo app "Chronometer started"))

(def (cmd-qt-chronometer-stop app)
  "Stop chronometer."
  (if (not *qt-chrono-running*)
    (qt-p9-echo-err app "Not running")
    (let* ((elapsed (- (time->seconds (current-time)) *qt-chrono-start*))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60)))
      (set! *qt-chrono-running* #f)
      (qt-p9-echo app (string-append "Stopped: " (number->string mins) ":"
                         (if (< secs 10) "0" "") (number->string secs))))))

(def (cmd-qt-chronometer-lap app)
  "Record lap."
  (if (not *qt-chrono-running*)
    (qt-p9-echo-err app "Not running")
    (let* ((elapsed (- (time->seconds (current-time)) *qt-chrono-start*))
           (n (+ 1 (length *qt-chrono-laps*)))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60)))
      (set! *qt-chrono-laps* (append *qt-chrono-laps* (list elapsed)))
      (qt-p9-echo app (string-append "Lap #" (number->string n) ": "
                         (number->string mins) ":"
                         (if (< secs 10) "0" "") (number->string secs))))))

(def (cmd-qt-chronometer-status app)
  "Show chronometer."
  (if (not *qt-chrono-start*)
    (qt-p9-echo app "No chronometer")
    (let* ((elapsed (if *qt-chrono-running*
                      (- (time->seconds (current-time)) *qt-chrono-start*) 0))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60))
           (text (with-output-to-string
                   (lambda ()
                     (display "=== CHRONOMETER ===\n\n")
                     (display (if *qt-chrono-running* "RUNNING" "STOPPED"))
                     (display "\nElapsed: ") (display (number->string mins))
                     (display ":") (display (if (< secs 10) "0" ""))
                     (display (number->string secs)) (display "\n\n")
                     (when (pair? *qt-chrono-laps*)
                       (display "Laps:\n")
                       (let loop ((ls *qt-chrono-laps*) (i 1))
                         (when (pair? ls)
                           (let* ((t (car ls))
                                  (m (quotient (inexact->exact (floor t)) 60))
                                  (s (modulo (inexact->exact (floor t)) 60)))
                             (display "  #") (display (number->string i))
                             (display ": ") (display (number->string m))
                             (display ":") (display (if (< s 10) "0" ""))
                             (display (number->string s)) (display "\n")
                             (loop (cdr ls) (+ i 1))))))))))
      (qt-p9-open-buffer app "*Chronometer*" text))))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity9-commands!)
  (for-each (lambda (p) (register-command! (car p) (cdr p)))
    (list
      (cons 'deft cmd-qt-deft)
      (cons 'deft-new cmd-qt-deft-new)
      (cons 'deft-search cmd-qt-deft-search)
      (cons 'dictionary cmd-qt-dictionary)
      (cons 'dictionary-at-point cmd-qt-dictionary-at-point)
      (cons 'speed-type cmd-qt-speed-type)
      (cons 'speed-type-results cmd-qt-speed-type-results)
      (cons 'pomodoro-start cmd-qt-pomodoro-start)
      (cons 'pomodoro-break cmd-qt-pomodoro-break)
      (cons 'pomodoro-status cmd-qt-pomodoro-status)
      (cons 'pomodoro-reset cmd-qt-pomodoro-reset)
      (cons 'doctor cmd-qt-doctor)
      (cons 'doctor-submit cmd-qt-doctor-submit)
      (cons 'figlet cmd-qt-figlet)
      (cons 'figlet-comment cmd-qt-figlet-comment)
      (cons 'dice-roll cmd-qt-dice-roll)
      (cons 'dice-roll-insert cmd-qt-dice-roll-insert)
      (cons 'morse-encode cmd-qt-morse-encode)
      (cons 'morse-decode cmd-qt-morse-decode)
      (cons 'gomoku cmd-qt-gomoku)
      (cons 'gomoku-up cmd-qt-gomoku-up)
      (cons 'gomoku-down cmd-qt-gomoku-down)
      (cons 'gomoku-left cmd-qt-gomoku-left)
      (cons 'gomoku-right cmd-qt-gomoku-right)
      (cons 'gomoku-place cmd-qt-gomoku-place)
      (cons 'chronometer-start cmd-qt-chronometer-start)
      (cons 'chronometer-stop cmd-qt-chronometer-stop)
      (cons 'chronometer-lap cmd-qt-chronometer-lap)
      (cons 'chronometer-status cmd-qt-chronometer-status))))
