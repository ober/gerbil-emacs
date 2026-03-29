;;; -*- Gerbil -*-
;;; Deft (note search), dictionary, speed-type, pomodoro timer,
;;; doctor (Eliza), figlet, dice roller, morse code, gomoku, chronometer.
;;; New module to keep other editor-extra files under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/ports
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
;;; Helpers
;;;============================================================================

(def (notes4-get-text-range ed start len)
  "Get text from editor at position START for LEN bytes."
  (if (<= len 0) ""
    (begin
      (send-message ed SCI_SETTARGETSTART start 0)
      (send-message ed SCI_SETTARGETEND (+ start len) 0)
      (let ((buf (make-string (+ len 1) #\nul)))
        (scintilla-send-message-string
          (scintilla-editor-handle ed) SCI_GETTARGETTEXT 0 buf)
        (let ((nul-pos (string-index buf #\nul)))
          (if nul-pos (substring buf 0 nul-pos) buf))))))

;;;============================================================================
;;; 1. Deft — Quick note search and create
;;;============================================================================

(def *deft-directory* #f) ;; defaults to ~/notes/

(def (deft-dir)
  (or *deft-directory*
      (let ((home (getenv "HOME" "/tmp")))
        (string-append home "/notes"))))

(def (deft-scan-files dir)
  "Scan directory for note files (.org .md .txt). Returns list of (path . title)."
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
          (let ((files (string-split out #\newline)))
            (filter-map
              (lambda (f)
                (if (string=? f "") #f
                  (with-exception-catcher
                    (lambda (e) (cons f (path-strip-directory f)))
                    (lambda ()
                      (let* ((p (open-input-file f))
                             (first-line (read-line p)))
                        (close-input-port p)
                        (cons f (if (eof-object? first-line)
                                  (path-strip-directory f)
                                  (string-append (path-strip-directory f)
                                    " - " (if (> (string-length first-line) 60)
                                            (substring first-line 0 60)
                                            first-line)))))))))
              files)))))))

(def (cmd-deft app)
  "Open Deft — quick note search and create interface."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (dir (deft-dir))
         (files (deft-scan-files dir))
         (buf (or (buffer-by-name "*Deft*") (buffer-create! "*Deft*" ed)))
         (text (with-output-to-string
                 (lambda ()
                   (display "Deft: Notes in ")
                   (display dir)
                   (display "\n")
                   (display (make-string 60 #\-))
                   (display "\n\n")
                   (if (null? files)
                     (display "(no notes found — create one with M-x deft-new)\n")
                     (for-each
                       (lambda (f)
                         (display (cdr f))
                         (display "\n  ")
                         (display (car f))
                         (display "\n"))
                       (sort (lambda (a b) (string<? (cdr a) (cdr b))) files)))))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Deft: " (number->string (length files)) " notes"))))

(def (cmd-deft-new app)
  "Create a new note in the Deft directory."
  (let* ((dir (deft-dir))
         (title (app-read-string app "Note title: ")))
    (if (or (not title) (string=? (string-trim-both title) ""))
      (echo-error! (app-state-echo app) "No title given")
      (let* ((safe-title (string-map
                           (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c)
                                               (eqv? c #\-) (eqv? c #\_))
                                         c #\-))
                           title))
             (fname (string-append dir "/" safe-title ".md"))
             (fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        ;; Ensure directory exists
        (with-exception-catcher void
          (lambda () (create-directory* dir)))
        (let ((buf (buffer-create! (path-strip-directory fname) ed)))
          (set! (buffer-file-path buf) fname)
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed (string-append "# " title "\n\n"))
          (editor-goto-pos ed (+ 4 (string-length title)))
          (echo-message! (app-state-echo app) (string-append "New note: " fname)))))))

(def (cmd-deft-search app)
  "Search notes by content in Deft directory."
  (let* ((query (app-read-string app "Search notes: ")))
    (if (or (not query) (string=? (string-trim-both query) ""))
      (echo-error! (app-state-echo app) "No search query")
      (let* ((dir (deft-dir))
             (fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        (with-exception-catcher
          (lambda (e) (echo-error! (app-state-echo app) "Search failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "grep" arguments: (list "-rl" "-i" query dir)
                                 stdin-redirection: #f stdout-redirection: #t
                                 stderr-redirection: #f)))
                   (out (read-line proc #f)))
              (process-status proc)
              (let* ((results (if (or (not out) (string=? out ""))
                               '()
                               (filter (lambda (s) (not (string=? s "")))
                                 (string-split out #\newline))))
                     (buf (or (buffer-by-name "*Deft Search*")
                              (buffer-create! "*Deft Search*" ed)))
                     (text (with-output-to-string
                             (lambda ()
                               (display "Deft search: \"")
                               (display query)
                               (display "\"\n")
                               (display (make-string 60 #\-))
                               (display "\n\n")
                               (if (null? results)
                                 (display "(no matches)\n")
                                 (for-each
                                   (lambda (f) (display f) (display "\n"))
                                   results))))))
                (buffer-attach! ed buf)
                (set! (edit-window-buffer win) buf)
                (editor-set-text ed text)
                (editor-goto-pos ed 0)
                (echo-message! (app-state-echo app)
                  (string-append (number->string (length results)) " matches"))))))))))

;;;============================================================================
;;; 2. Dictionary — Word definition lookup via `dict` command
;;;============================================================================

(def (cmd-dictionary app)
  "Look up word definition using dict command."
  (let* ((word (app-read-string app "Word: ")))
    (if (or (not word) (string=? (string-trim-both word) ""))
      (echo-error! (app-state-echo app) "No word given")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        (with-exception-catcher
          (lambda (e)
            ;; Fallback: try curl to dict.org
            (with-exception-catcher
              (lambda (e2)
                (echo-error! (app-state-echo app)
                  "dict command not found. Install dictd or use: apt install dict"))
              (lambda ()
                (let* ((proc (open-process
                               (list path: "curl" arguments: (list "-s"
                                     (string-append "dict://dict.org/d:" word))
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #f)))
                       (out (read-line proc #f)))
                  (process-status proc)
                  (let ((buf (or (buffer-by-name "*Dictionary*")
                                 (buffer-create! "*Dictionary*" ed))))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer win) buf)
                    (editor-set-text ed (or out "(no definition found)"))
                    (editor-goto-pos ed 0))))))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "dict" arguments: (list word)
                                 stdin-redirection: #f stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (out (read-line proc #f)))
              (process-status proc)
              (let ((buf (or (buffer-by-name "*Dictionary*")
                             (buffer-create! "*Dictionary*" ed))))
                (buffer-attach! ed buf)
                (set! (edit-window-buffer win) buf)
                (editor-set-text ed
                  (string-append "Definition of \"" word "\"\n"
                    (make-string 40 #\-) "\n\n"
                    (or out "(no definition found)")))
                (editor-goto-pos ed 0)
                (echo-message! (app-state-echo app)
                  (string-append "Dictionary: " word))))))))))

(def (cmd-dictionary-at-point app)
  "Look up the word at cursor in the dictionary."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (word-start (send-message ed SCI_WORDSTARTPOSITION pos 1))
         (word-end (send-message ed SCI_WORDENDPOSITION pos 1))
         (len (- word-end word-start)))
    (if (<= len 0)
      (echo-error! (app-state-echo app) "No word at point")
      (let ((word (notes4-get-text-range ed word-start len)))
        ;; Reuse the dictionary command with the word
        (let* ((proc-result
                (with-exception-catcher
                  (lambda (e) #f)
                  (lambda ()
                    (let* ((proc (open-process
                                   (list path: "dict" arguments: (list word)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t)))
                           (out (read-line proc #f)))
                      (process-status proc)
                      out)))))
          (if (not proc-result)
            (echo-error! (app-state-echo app) "dict command not available")
            (let ((buf (or (buffer-by-name "*Dictionary*")
                           (buffer-create! "*Dictionary*" ed))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer win) buf)
              (editor-set-text ed
                (string-append "Definition of \"" word "\"\n"
                  (make-string 40 #\-) "\n\n" proc-result))
              (editor-goto-pos ed 0)
              (echo-message! (app-state-echo app)
                (string-append "Dictionary: " word)))))))))

;;;============================================================================
;;; 3. Speed-type — Typing speed test
;;;============================================================================

(def *speed-type-samples*
  '("The quick brown fox jumps over the lazy dog near the river bank."
    "To be or not to be, that is the question whether it is nobler to suffer."
    "All that glitters is not gold; often have you heard that told."
    "In the beginning was the Word, and the Word was with God."
    "It was the best of times, it was the worst of times, it was the age of wisdom."
    "The only thing we have to fear is fear itself."
    "I think therefore I am, and I am therefore I think about thinking."
    "We hold these truths to be self-evident that all men are created equal."
    "Ask not what your country can do for you; ask what you can do for your country."
    "That which does not kill us makes us stronger, so they say."))

(def *speed-type-start-time* #f)
(def *speed-type-target* #f)

(def (cmd-speed-type app)
  "Start a typing speed test — type the displayed text as fast as you can."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (sample (list-ref *speed-type-samples*
                   (modulo (time->seconds (current-time))
                           (length *speed-type-samples*))))
         (buf (or (buffer-by-name "*Speed Type*")
                  (buffer-create! "*Speed Type*" ed))))
    (set! *speed-type-target* sample)
    (set! *speed-type-start-time* (time->seconds (current-time)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "=== SPEED TYPE TEST ===\n\n"
        "Type the following text exactly:\n\n"
        ">>> " sample " <<<\n\n"
        "Your input (type below this line):\n"
        (make-string 40 #\-) "\n"))
    (editor-goto-pos ed (string-length (editor-get-text ed)))
    (echo-message! (app-state-echo app) "Speed Type: start typing! Use M-x speed-type-results when done.")))

(def (cmd-speed-type-results app)
  "Calculate and display typing speed results."
  (if (not *speed-type-target*)
    (echo-error! (app-state-echo app) "No speed-type test in progress")
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (text (editor-get-text ed))
           (marker "----")
           (marker-pos (string-contains text marker))
           (typed (if marker-pos
                    (let ((start (+ marker-pos (string-length marker))))
                      (string-trim-both (substring text start (string-length text))))
                    ""))
           (elapsed (- (time->seconds (current-time)) *speed-type-start-time*))
           (target *speed-type-target*)
           ;; Calculate accuracy
           (target-len (string-length target))
           (typed-len (string-length typed))
           (min-len (min target-len typed-len))
           (correct (let loop ((i 0) (acc 0))
                      (if (>= i min-len) acc
                        (loop (+ i 1)
                          (if (eqv? (string-ref target i) (string-ref typed i))
                            (+ acc 1) acc)))))
           (accuracy (if (> target-len 0)
                       (inexact->exact (round (* 100.0 (/ correct target-len))))
                       0))
           ;; WPM = (chars / 5) / minutes
           (minutes (/ elapsed 60.0))
           (wpm (if (> minutes 0)
                  (inexact->exact (round (/ (/ typed-len 5.0) minutes)))
                  0)))
      (set! *speed-type-target* #f)
      (let ((result-text
              (string-append text "\n\n"
                "=== RESULTS ===\n"
                "Time: " (number->string (inexact->exact (round elapsed))) " seconds\n"
                "Words per minute: " (number->string wpm) "\n"
                "Accuracy: " (number->string accuracy) "%\n"
                "Characters typed: " (number->string typed-len) "/" (number->string target-len) "\n"
                "Correct characters: " (number->string correct) "\n")))
        (editor-set-text ed result-text)
        (editor-goto-pos ed (string-length result-text))
        (echo-message! (app-state-echo app)
          (string-append "WPM: " (number->string wpm)
            " | Accuracy: " (number->string accuracy) "%"))))))

;;;============================================================================
;;; 4. Pomodoro timer — Work/break productivity timer
;;;============================================================================

(def *pomodoro-work-minutes* 25)
(def *pomodoro-break-minutes* 5)
(def *pomodoro-start-time* #f)
(def *pomodoro-mode* #f) ;; 'work or 'break
(def *pomodoro-count* 0)

(def (cmd-pomodoro-start app)
  "Start a Pomodoro work timer (25 minutes)."
  (set! *pomodoro-start-time* (time->seconds (current-time)))
  (set! *pomodoro-mode* 'work)
  (echo-message! (app-state-echo app)
    (string-append "Pomodoro #" (number->string (+ *pomodoro-count* 1))
      " started (" (number->string *pomodoro-work-minutes*) " min work session)")))

(def (cmd-pomodoro-break app)
  "Start a Pomodoro break timer (5 minutes)."
  (set! *pomodoro-start-time* (time->seconds (current-time)))
  (set! *pomodoro-mode* 'break)
  (set! *pomodoro-count* (+ *pomodoro-count* 1))
  (echo-message! (app-state-echo app)
    (string-append "Break started (" (number->string *pomodoro-break-minutes*) " min)")))

(def (cmd-pomodoro-status app)
  "Show current Pomodoro timer status."
  (if (not *pomodoro-start-time*)
    (echo-message! (app-state-echo app) "No Pomodoro timer running. Use M-x pomodoro-start")
    (let* ((elapsed (- (time->seconds (current-time)) *pomodoro-start-time*))
           (total-secs (if (eq? *pomodoro-mode* 'work)
                         (* *pomodoro-work-minutes* 60)
                         (* *pomodoro-break-minutes* 60)))
           (remaining (max 0 (- total-secs elapsed)))
           (mins (quotient (inexact->exact (floor remaining)) 60))
           (secs (modulo (inexact->exact (floor remaining)) 60))
           (mode-str (if (eq? *pomodoro-mode* 'work) "WORK" "BREAK")))
      (if (<= remaining 0)
        (echo-message! (app-state-echo app)
          (string-append mode-str " session complete! "
            (if (eq? *pomodoro-mode* 'work)
              "Take a break (M-x pomodoro-break)"
              "Start working (M-x pomodoro-start)")))
        (echo-message! (app-state-echo app)
          (string-append mode-str " | "
            (number->string mins) ":"
            (if (< secs 10) "0" "") (number->string secs)
            " remaining | Pomodoro #" (number->string (+ *pomodoro-count* 1))))))))

(def (cmd-pomodoro-reset app)
  "Reset Pomodoro timer and counter."
  (set! *pomodoro-start-time* #f)
  (set! *pomodoro-mode* #f)
  (set! *pomodoro-count* 0)
  (echo-message! (app-state-echo app) "Pomodoro timer reset"))

;;;============================================================================
;;; 5. Doctor (Eliza) — Real pattern-matching psychotherapist
;;;============================================================================

(def *eliza-rules* (make-hash-table))
(begin
  (hash-put! *eliza-rules* "hello"
    '("How do you do. Please tell me your problem."
      "Hi there. What seems to be troubling you?"))
  (hash-put! *eliza-rules* "hi"
    '("Hello. How are you feeling today?"
      "Hi. Please tell me what brings you here."))
  (hash-put! *eliza-rules* "i need"
    '("Why do you need %s?"
      "Would it really help you to get %s?"
      "Are you sure you need %s?"))
  (hash-put! *eliza-rules* "i am"
    '("How long have you been %s?"
      "Do you enjoy being %s?"
      "Do you believe it is normal to be %s?"
      "How does being %s make you feel?"))
  (hash-put! *eliza-rules* "i feel"
    '("Tell me more about feeling %s."
      "Do you often feel %s?"
      "When did you first feel %s?"
      "What makes you feel %s?"))
  (hash-put! *eliza-rules* "i want"
    '("What would it mean to you if you got %s?"
      "Why do you want %s?"
      "Suppose you got %s. Then what?"))
  (hash-put! *eliza-rules* "i can't"
    '("What makes you think you can't %s?"
      "Have you tried?"
      "Perhaps you could %s if you tried."))
  (hash-put! *eliza-rules* "i don't"
    '("Why don't you %s?"
      "Do you wish you could %s?"
      "Does that trouble you?"))
  (hash-put! *eliza-rules* "why"
    '("Why do you ask?"
      "Does that question interest you?"
      "What answer would please you most?"
      "What do you think?"))
  (hash-put! *eliza-rules* "because"
    '("Is that the real reason?"
      "What other reasons come to mind?"
      "Does that reason explain anything else?"))
  (hash-put! *eliza-rules* "sorry"
    '("Please don't apologize."
      "Apologies are not necessary."
      "What feelings does apologizing give you?"))
  (hash-put! *eliza-rules* "mother"
    '("Tell me more about your mother."
      "What was your relationship with your mother like?"
      "How does that relate to your feelings today?"
      "Good family relations are important."))
  (hash-put! *eliza-rules* "father"
    '("Tell me more about your father."
      "How did your father make you feel?"
      "Does your relationship with your father relate to your feelings?"))
  (hash-put! *eliza-rules* "friend"
    '("Tell me more about your friends."
      "Are your friends important to you?"
      "Do your friends ever make you feel anxious?"))
  (hash-put! *eliza-rules* "yes"
    '("You seem quite sure." "I see." "I understand."
      "Can you elaborate on that?"))
  (hash-put! *eliza-rules* "no"
    '("Why not?" "Are you saying no just to be negative?"
      "You are being a bit negative."))
  (hash-put! *eliza-rules* "computer"
    '("Do computers worry you?"
      "What about machines worries you?"
      "What do you think about artificial intelligence?"
      "Why do you mention computers?"))
  (hash-put! *eliza-rules* "dream"
    '("What does that dream suggest to you?"
      "Do you dream often?"
      "What persons appear in your dreams?"
      "Do you believe dreams have meaning?"))
  (hash-put! *eliza-rules* "sad"
    '("I am sorry to hear that you are sad."
      "Do you think coming here will help you not to be sad?"
      "What makes you feel sad?"))
  (hash-put! *eliza-rules* "happy"
    '("What makes you happy?"
      "Are you really happy, or is this a facade?"
      "Do you believe happiness is achievable?"))
  (hash-put! *eliza-rules* "afraid"
    '("What are you most afraid of?"
      "Does fear often affect your decisions?"
      "What would you do if you were not afraid?"))
  (hash-put! *eliza-rules* "love"
    '("Tell me about your experience with love."
      "Is love important to you?"
      "Do you feel loved?"))
  (hash-put! *eliza-rules* "hate"
    '("Hate is a strong word. Why do you hate %s?"
      "What about %s do you dislike?"
      "Is it really hate, or something else?"))
  (hash-put! *eliza-rules* "think"
    '("Why do you think that?"
      "What makes you think so?"
      "Do you really think so?"
      "But you are not sure?"))
  (hash-put! *eliza-rules* "always"
    '("Can you think of a specific example?"
      "When you say always, do you mean literally always?"
      "What incident are you thinking of?"))
  (hash-put! *eliza-rules* "never"
    '("Never? Not even once?"
      "Why do you say never?"
      "Surely there must have been some occasion?")))

(def *eliza-default-responses*
  '("Please go on."
    "Tell me more."
    "How does that make you feel?"
    "Can you elaborate on that?"
    "That is interesting. Please continue."
    "I see. And what does that tell you?"
    "Very interesting."
    "I'm not sure I understand. Could you explain?"
    "What does that suggest to you?"
    "Do you feel strongly about discussing that?"))

(def *eliza-response-index* 0)

(def (eliza-respond input)
  "Generate an Eliza response to user input."
  (let* ((lower (string-downcase (string-trim-both input)))
         (words (string-split lower #\space)))
    ;; Try to find a matching rule
    (let loop ((keys (hash-keys *eliza-rules*)))
      (if (null? keys)
        ;; No match — use default
        (let ((resp (list-ref *eliza-default-responses*
                      (modulo *eliza-response-index*
                        (length *eliza-default-responses*)))))
          (set! *eliza-response-index* (+ *eliza-response-index* 1))
          resp)
        (let* ((key (car keys))
               (pos (string-contains lower key)))
          (if pos
            ;; Found a match
            (let* ((responses (hash-ref *eliza-rules* key))
                   (resp (list-ref responses
                           (modulo *eliza-response-index* (length responses))))
                   (rest-text (string-trim-both
                                (substring lower (+ pos (string-length key))
                                  (string-length lower)))))
              (set! *eliza-response-index* (+ *eliza-response-index* 1))
              (if (string-contains resp "%s")
                (if (string=? rest-text "")
                  (string-append (substring resp 0 (string-contains resp "%s")) "that")
                  ;; Swap pronouns
                  (let ((swapped (eliza-swap-pronouns rest-text)))
                    (string-append
                      (substring resp 0 (string-contains resp "%s"))
                      swapped
                      (substring resp (+ (string-contains resp "%s") 2)
                        (string-length resp)))))
                resp))
            (loop (cdr keys))))))))

(def (eliza-swap-pronouns text)
  "Swap first/second person pronouns in text."
  (let* ((s text)
         (s (string-replace-all s " i " " you "))
         (s (string-replace-all s " my " " your "))
         (s (string-replace-all s " me " " you "))
         (s (string-replace-all s " am " " are "))
         (s (string-replace-all s "i'm" "you are"))
         (s (string-replace-all s " myself " " yourself ")))
    s))

(def (string-replace-all str old new)
  "Replace all occurrences of OLD with NEW in STR."
  (let loop ((s str))
    (let ((pos (string-contains s old)))
      (if (not pos) s
        (loop (string-append
                (substring s 0 pos)
                new
                (substring s (+ pos (string-length old)) (string-length s))))))))

(def (cmd-doctor-real app)
  "Start the Eliza psychotherapist with real pattern matching."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Doctor*") (buffer-create! "*Doctor*" ed))))
    (set! *eliza-response-index* 0)
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "I am the psychotherapist. Please describe your problems.\n"
        "Each time you are finished talking, type your message and press\n"
        "M-x doctor-submit to get my response.\n\n"
        "> "))
    (editor-goto-pos ed (string-length (editor-get-text ed)))))

(def (cmd-doctor-submit app)
  "Submit your input to the Eliza psychotherapist and get a response."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         ;; Find the last "> " prompt
         (last-prompt (let loop ((pos (- (string-length text) 1)))
                        (if (< pos 1) 0
                          (if (and (eqv? (string-ref text (- pos 1)) #\>)
                                   (eqv? (string-ref text pos) #\space))
                            (+ pos 1)
                            (loop (- pos 1))))))
         (user-input (string-trim-both (substring text last-prompt (string-length text))))
         (response (eliza-respond user-input))
         (new-text (string-append text "\n\n[Doctor]: " response "\n\n> ")))
    (editor-set-text ed new-text)
    (editor-goto-pos ed (string-length new-text))))

;;;============================================================================
;;; 6. Figlet — ASCII art text banners
;;;============================================================================

(def (cmd-figlet app)
  "Generate ASCII art text banner using figlet."
  (let* ((text (app-read-string app "Figlet text: ")))
    (if (or (not text) (string=? (string-trim-both text) ""))
      (echo-error! (app-state-echo app) "No text given")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        (with-exception-catcher
          (lambda (e)
            ;; Fallback: simple ASCII art
            (let ((banner (figlet-fallback text)))
              (editor-replace-selection ed banner)
              (echo-message! (app-state-echo app) "Figlet: using built-in renderer")))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "figlet" arguments: (list text)
                                 stdin-redirection: #f stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (out (read-line proc #f)))
              (process-status proc)
              (if (or (not out) (string=? out ""))
                (begin
                  (editor-replace-selection ed (figlet-fallback text))
                  (echo-message! (app-state-echo app) "Figlet: using built-in renderer"))
                (begin
                  (editor-replace-selection ed out)
                  (echo-message! (app-state-echo app) "Figlet: inserted"))))))))))

(def (figlet-fallback text)
  "Simple built-in ASCII banner when figlet is not installed."
  (let* ((upper (string-upcase text))
         (len (string-length upper))
         (border (make-string (+ len 4) #\#)))
    (string-append
      border "\n"
      "# " upper " #\n"
      border "\n")))

(def (cmd-figlet-comment app)
  "Insert a figlet-style comment banner at point."
  (let* ((text (app-read-string app "Banner text: ")))
    (if (or (not text) (string=? (string-trim-both text) ""))
      (echo-error! (app-state-echo app) "No text given")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (upper (string-upcase text))
             (len (string-length upper))
             (border (make-string (+ len 8) #\=))
             (banner (string-append
                       ";;;" border "\n"
                       ";;;    " upper "    \n"
                       ";;;" border "\n")))
        (editor-replace-selection ed banner)
        (echo-message! (app-state-echo app) "Banner comment inserted")))))

;;;============================================================================
;;; 7. Dice roller — D&D dice expression evaluator
;;;============================================================================

(def (parse-dice-expr expr)
  "Parse a dice expression like '2d6+3' and return result.
   Returns (total . description)."
  (let* ((s (string-downcase (string-trim-both expr)))
         (d-pos (string-contains s "d")))
    (if (not d-pos)
      ;; Just a number
      (let ((n (string->number s)))
        (if n (cons n (number->string n))
          (cons 0 "invalid")))
      ;; NdS[+/-M] format
      (let* ((num-str (substring s 0 d-pos))
             (num-dice (if (string=? num-str "") 1 (or (string->number num-str) 1)))
             (rest (substring s (+ d-pos 1) (string-length s)))
             ;; Check for +/- modifier
             (plus-pos (string-contains rest "+"))
             (minus-pos (string-contains rest "-"))
             (mod-pos (or plus-pos minus-pos))
             (sides-str (if mod-pos (substring rest 0 mod-pos) rest))
             (sides (or (string->number sides-str) 6))
             (modifier (if mod-pos
                        (or (string->number (substring rest mod-pos (string-length rest))) 0)
                        0))
             ;; Roll the dice
             (rolls (let loop ((i 0) (acc '()))
                      (if (>= i num-dice) (reverse acc)
                        (loop (+ i 1)
                          (cons (+ 1 (random-integer sides)) acc)))))
             (sum (apply + rolls))
             (total (+ sum modifier))
             (desc (string-append
                     (number->string num-dice) "d" (number->string sides)
                     (if (> modifier 0)
                       (string-append "+" (number->string modifier))
                       (if (< modifier 0) (number->string modifier) ""))
                     " = [" (string-join (map number->string rolls) ", ") "]"
                     (if (not (= modifier 0))
                       (string-append
                         (if (> modifier 0) "+" "")
                         (number->string modifier))
                       "")
                     " = " (number->string total))))
        (cons total desc)))))

(def (cmd-dice-roll app)
  "Roll dice using D&D notation (e.g., 2d6+3, d20, 4d8-1)."
  (let* ((expr (app-read-string app "Dice (e.g. 2d6+3): ")))
    (if (or (not expr) (string=? (string-trim-both expr) ""))
      (echo-error! (app-state-echo app) "No dice expression")
      (let ((result (parse-dice-expr expr)))
        (echo-message! (app-state-echo app)
          (string-append "Roll: " (cdr result)))))))

(def (cmd-dice-roll-insert app)
  "Roll dice and insert result at point."
  (let* ((expr (app-read-string app "Dice (e.g. 2d6+3): ")))
    (if (or (not expr) (string=? (string-trim-both expr) ""))
      (echo-error! (app-state-echo app) "No dice expression")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (result (parse-dice-expr expr)))
        (editor-replace-selection ed (number->string (car result)))
        (echo-message! (app-state-echo app) (cdr result))))))

;;;============================================================================
;;; 8. Morse code — Encode/decode
;;;============================================================================

(def *morse-table* (make-hash-table))
(def *morse-reverse* (make-hash-table))
(begin
  (for-each (lambda (p) (hash-put! *morse-table* (car p) (cdr p))
                        (hash-put! *morse-reverse* (cdr p) (car p)))
    '((#\A . ".-") (#\B . "-...") (#\C . "-.-.") (#\D . "-..") (#\E . ".")
      (#\F . "..-.") (#\G . "--.") (#\H . "....") (#\I . "..") (#\J . ".---")
      (#\K . "-.-") (#\L . ".-..") (#\M . "--") (#\N . "-.") (#\O . "---")
      (#\P . ".--.") (#\Q . "--.-") (#\R . ".-.") (#\S . "...") (#\T . "-")
      (#\U . "..-") (#\V . "...-") (#\W . ".--") (#\X . "-..-") (#\Y . "-.--")
      (#\Z . "--..") (#\0 . "-----") (#\1 . ".----") (#\2 . "..---")
      (#\3 . "...--") (#\4 . "....-") (#\5 . ".....") (#\6 . "-....")
      (#\7 . "--...") (#\8 . "---..") (#\9 . "----."))))

(def (text->morse text)
  "Convert text to morse code."
  (string-join
    (map (lambda (c)
           (if (eqv? c #\space) "/"
             (hash-ref *morse-table* (char-upcase c) "?")))
         (string->list text))
    " "))

(def (morse->text morse)
  "Convert morse code back to text."
  (string-join
    (map (lambda (word)
           (list->string
             (map (lambda (code)
                    (if (string=? code "")
                      #\space
                      (hash-ref *morse-reverse* code #\?)))
                  (string-split word #\space))))
         (string-split morse #\/))
    " "))

(def (cmd-morse-encode app)
  "Encode selected text or prompted text to Morse code."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (sel-start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (send-message ed SCI_GETSELECTIONEND 0 0))
         (has-sel (not (= sel-start sel-end))))
    (if has-sel
      (let* ((text (notes4-get-text-range ed sel-start (- sel-end sel-start)))
             (morse (text->morse text)))
        (editor-replace-selection ed morse)
        (echo-message! (app-state-echo app) "Encoded to Morse"))
      (let ((text (app-read-string app "Text to encode: ")))
        (if (or (not text) (string=? text ""))
          (echo-error! (app-state-echo app) "No text given")
          (echo-message! (app-state-echo app)
            (string-append "Morse: " (text->morse text))))))))

(def (cmd-morse-decode app)
  "Decode Morse code from selection or prompt."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (sel-start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (send-message ed SCI_GETSELECTIONEND 0 0))
         (has-sel (not (= sel-start sel-end))))
    (if has-sel
      (let* ((morse (notes4-get-text-range ed sel-start (- sel-end sel-start)))
             (text (morse->text morse)))
        (editor-replace-selection ed text)
        (echo-message! (app-state-echo app) "Decoded from Morse"))
      (let ((morse (app-read-string app "Morse to decode: ")))
        (if (or (not morse) (string=? morse ""))
          (echo-error! (app-state-echo app) "No morse given")
          (echo-message! (app-state-echo app)
            (string-append "Text: " (morse->text morse))))))))

;;;============================================================================
;;; 9. Gomoku — Five-in-a-row board game
;;;============================================================================

(def *gomoku-board-size* 15)
(def *gomoku-board* #f)
(def *gomoku-cursor-x* 7)
(def *gomoku-cursor-y* 7)
(def *gomoku-turn* 'X) ;; X = human, O = computer

(def (gomoku-init!)
  (set! *gomoku-board* (make-vector (* *gomoku-board-size* *gomoku-board-size*) #\.))
  (set! *gomoku-cursor-x* 7)
  (set! *gomoku-cursor-y* 7)
  (set! *gomoku-turn* 'X))

(def (gomoku-get x y)
  (if (and (>= x 0) (< x *gomoku-board-size*)
           (>= y 0) (< y *gomoku-board-size*))
    (vector-ref *gomoku-board* (+ x (* y *gomoku-board-size*)))
    #f))

(def (gomoku-set! x y val)
  (when (and (>= x 0) (< x *gomoku-board-size*)
             (>= y 0) (< y *gomoku-board-size*))
    (vector-set! *gomoku-board* (+ x (* y *gomoku-board-size*)) val)))

(def (gomoku-check-win ch)
  "Check if character CH has 5 in a row."
  (let ((dirs '((1 0) (0 1) (1 1) (1 -1))))
    (let yloop ((y 0))
      (if (>= y *gomoku-board-size*) #f
        (let xloop ((x 0))
          (if (>= x *gomoku-board-size*)
            (yloop (+ y 1))
            (if (not (eqv? (gomoku-get x y) ch))
              (xloop (+ x 1))
              ;; Check each direction
              (let dloop ((ds dirs))
                (if (null? ds) (xloop (+ x 1))
                  (let* ((dx (caar ds)) (dy (cadar ds))
                         (count (let cloop ((i 1))
                                  (if (>= i 5) 5
                                    (if (eqv? (gomoku-get (+ x (* i dx)) (+ y (* i dy))) ch)
                                      (cloop (+ i 1))
                                      i)))))
                    (if (>= count 5) #t
                      (dloop (cdr ds)))))))))))))

(def (gomoku-ai-move)
  "Simple AI: try to extend own lines or block opponent. Returns (x . y)."
  (let* ((best-x 0) (best-y 0) (best-score -1))
    ;; Score each empty cell
    (let yloop ((y 0))
      (when (< y *gomoku-board-size*)
        (let xloop ((x 0))
          (when (< x *gomoku-board-size*)
            (when (eqv? (gomoku-get x y) #\.)
              (let* ((score-o (gomoku-score-cell x y #\O))
                     (score-x (gomoku-score-cell x y #\X))
                     (score (+ (* score-o 2) (* score-x 1)))) ;; Prefer offense
                (when (> score best-score)
                  (set! best-score score)
                  (set! best-x x)
                  (set! best-y y))))
            (xloop (+ x 1))))
        (yloop (+ y 1))))
    (cons best-x best-y)))

(def (gomoku-score-cell x y ch)
  "Score a cell for a given player character."
  (let ((dirs '((1 0) (0 1) (1 1) (1 -1)))
        (total 0))
    (for-each
      (lambda (d)
        (let* ((dx (car d)) (dy (cadr d))
               (count-fwd (let loop ((i 1))
                            (if (or (>= i 5) (not (eqv? (gomoku-get (+ x (* i dx)) (+ y (* i dy))) ch)))
                              (- i 1) (loop (+ i 1)))))
               (count-bwd (let loop ((i 1))
                            (if (or (>= i 5) (not (eqv? (gomoku-get (- x (* i dx)) (- y (* i dy))) ch)))
                              (- i 1) (loop (+ i 1)))))
               (line-len (+ count-fwd count-bwd)))
          (set! total (+ total (cond
                                 ((>= line-len 4) 10000)
                                 ((>= line-len 3) 100)
                                 ((>= line-len 2) 10)
                                 ((>= line-len 1) 1)
                                 (else 0))))))
      dirs)
    total))

(def (gomoku-render)
  "Render the board as a string."
  (with-output-to-string
    (lambda ()
      (display "  GOMOKU (Five in a Row)\n")
      (display "  You: X  |  Computer: O\n")
      (display "  Use gomoku-up/down/left/right to move, gomoku-place to play\n\n")
      ;; Column numbers
      (display "   ")
      (let loop ((i 0))
        (when (< i *gomoku-board-size*)
          (display (modulo i 10))
          (display " ")
          (loop (+ i 1))))
      (display "\n")
      ;; Board rows
      (let yloop ((y 0))
        (when (< y *gomoku-board-size*)
          (display (if (< y 10) " " ""))
          (display y)
          (display " ")
          (let xloop ((x 0))
            (when (< x *gomoku-board-size*)
              (if (and (= x *gomoku-cursor-x*) (= y *gomoku-cursor-y*))
                (let ((ch (gomoku-get x y)))
                  (if (eqv? ch #\.)
                    (display "+")
                    (display ch)))
                (display (gomoku-get x y)))
              (display " ")
              (xloop (+ x 1))))
          (display "\n")
          (yloop (+ y 1))))
      (display "\n")
      (display (if (eq? *gomoku-turn* 'X) "Your turn (X)" "Computer thinking..."))
      (display "\n"))))

(def (gomoku-refresh! app)
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-set-text ed (gomoku-render))
    (editor-goto-pos ed 0)))

(def (cmd-gomoku app)
  "Start a game of Gomoku (Five in a Row)."
  (gomoku-init!)
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Gomoku*") (buffer-create! "*Gomoku*" ed))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (gomoku-refresh! app)
    (echo-message! (app-state-echo app) "Gomoku: place stones with M-x gomoku-place")))

(def (cmd-gomoku-up app)
  "Move cursor up in Gomoku."
  (when (> *gomoku-cursor-y* 0)
    (set! *gomoku-cursor-y* (- *gomoku-cursor-y* 1)))
  (gomoku-refresh! app))

(def (cmd-gomoku-down app)
  "Move cursor down in Gomoku."
  (when (< *gomoku-cursor-y* (- *gomoku-board-size* 1))
    (set! *gomoku-cursor-y* (+ *gomoku-cursor-y* 1)))
  (gomoku-refresh! app))

(def (cmd-gomoku-left app)
  "Move cursor left in Gomoku."
  (when (> *gomoku-cursor-x* 0)
    (set! *gomoku-cursor-x* (- *gomoku-cursor-x* 1)))
  (gomoku-refresh! app))

(def (cmd-gomoku-right app)
  "Move cursor right in Gomoku."
  (when (< *gomoku-cursor-x* (- *gomoku-board-size* 1))
    (set! *gomoku-cursor-x* (+ *gomoku-cursor-x* 1)))
  (gomoku-refresh! app))

(def (cmd-gomoku-place app)
  "Place a stone at cursor position in Gomoku."
  (when (and *gomoku-board* (eq? *gomoku-turn* 'X))
    (if (not (eqv? (gomoku-get *gomoku-cursor-x* *gomoku-cursor-y*) #\.))
      (echo-error! (app-state-echo app) "Cell already occupied!")
      (begin
        (gomoku-set! *gomoku-cursor-x* *gomoku-cursor-y* #\X)
        (cond
          ((gomoku-check-win #\X)
           (gomoku-refresh! app)
           (echo-message! (app-state-echo app) "You win! Congratulations!"))
          (else
           ;; AI move
           (set! *gomoku-turn* 'O)
           (let ((move (gomoku-ai-move)))
             (gomoku-set! (car move) (cdr move) #\O)
             (set! *gomoku-turn* 'X)
             (gomoku-refresh! app)
             (if (gomoku-check-win #\O)
               (echo-message! (app-state-echo app) "Computer wins! Better luck next time.")
               (echo-message! (app-state-echo app)
                 (string-append "Computer played at ("
                   (number->string (car move)) "," (number->string (cdr move))
                   "). Your turn."))))))))))

;;;============================================================================
;;; 10. Chronometer — Stopwatch / elapsed time tracker
;;;============================================================================

(def *chrono-start-time* #f)
(def *chrono-laps* '())
(def *chrono-running* #f)

(def (cmd-chronometer-start app)
  "Start the chronometer / stopwatch."
  (set! *chrono-start-time* (time->seconds (current-time)))
  (set! *chrono-running* #t)
  (set! *chrono-laps* '())
  (echo-message! (app-state-echo app) "Chronometer started"))

(def (cmd-chronometer-stop app)
  "Stop the chronometer."
  (if (not *chrono-running*)
    (echo-error! (app-state-echo app) "Chronometer not running")
    (let* ((elapsed (- (time->seconds (current-time)) *chrono-start-time*))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60))
           (ms (inexact->exact (round (* 1000 (- elapsed (floor elapsed)))))))
      (set! *chrono-running* #f)
      (echo-message! (app-state-echo app)
        (string-append "Stopped: "
          (number->string mins) ":"
          (if (< secs 10) "0" "") (number->string secs) "."
          (cond ((< ms 10) "00") ((< ms 100) "0") (else ""))
          (number->string ms))))))

(def (cmd-chronometer-lap app)
  "Record a lap time."
  (if (not *chrono-running*)
    (echo-error! (app-state-echo app) "Chronometer not running")
    (let* ((elapsed (- (time->seconds (current-time)) *chrono-start-time*))
           (lap-num (+ 1 (length *chrono-laps*)))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60)))
      (set! *chrono-laps* (append *chrono-laps* (list elapsed)))
      (echo-message! (app-state-echo app)
        (string-append "Lap #" (number->string lap-num) ": "
          (number->string mins) ":"
          (if (< secs 10) "0" "") (number->string secs))))))

(def (cmd-chronometer-status app)
  "Show chronometer status and laps."
  (if (not *chrono-start-time*)
    (echo-message! (app-state-echo app) "Chronometer not started. Use M-x chronometer-start")
    (let* ((now (time->seconds (current-time)))
           (elapsed (if *chrono-running* (- now *chrono-start-time*) 0))
           (mins (quotient (inexact->exact (floor elapsed)) 60))
           (secs (modulo (inexact->exact (floor elapsed)) 60))
           (fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (or (buffer-by-name "*Chronometer*")
                    (buffer-create! "*Chronometer*" ed)))
           (text (with-output-to-string
                   (lambda ()
                     (display "=== CHRONOMETER ===\n\n")
                     (display (if *chrono-running* "RUNNING" "STOPPED"))
                     (display "\n")
                     (display "Elapsed: ")
                     (display (number->string mins))
                     (display ":")
                     (display (if (< secs 10) "0" ""))
                     (display (number->string secs))
                     (display "\n\n")
                     (when (pair? *chrono-laps*)
                       (display "Laps:\n")
                       (let loop ((laps *chrono-laps*) (i 1))
                         (when (pair? laps)
                           (let* ((t (car laps))
                                  (m (quotient (inexact->exact (floor t)) 60))
                                  (s (modulo (inexact->exact (floor t)) 60)))
                             (display "  #")
                             (display (number->string i))
                             (display ": ")
                             (display (number->string m))
                             (display ":")
                             (display (if (< s 10) "0" ""))
                             (display (number->string s))
                             (display "\n")
                             (loop (cdr laps) (+ i 1))))))))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed text)
      (editor-goto-pos ed 0))))
