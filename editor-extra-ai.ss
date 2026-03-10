;;; -*- Gerbil -*-
;;; AI inline code completion (Copilot-style) using OpenAI API.
;;; TUI implementation: shows suggestions in the echo area.

(export #t)

(import :std/sugar
        :std/srfi/13
        (only-in :std/misc/string string-split)
        (only-in :std/text/json
          json-object->string read-json string->json-object)
        (only-in :std/net/request
          http-post request-status request-text request-close)
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gemacs/core
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        (only-in :gemacs/persist
          *copilot-mode* *copilot-api-key* *copilot-model*
          *copilot-api-url* *copilot-suggestion* *copilot-suggestion-pos*)
        :gemacs/editor-extra-helpers
        (only-in :gemacs/editor-extra-editing2 occur-parse-source-name))

;;;============================================================================
;;; Copilot helper: call OpenAI chat completions API
;;;============================================================================

(def (copilot-get-context ed max-chars)
  "Get buffer text before cursor (up to max-chars) as completion context."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (start (max 0 (- pos max-chars))))
    (if (> pos 0)
      (substring text start pos)
      "")))

(def (copilot-get-suffix ed max-chars)
  "Get buffer text after cursor (up to max-chars) for suffix context."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (end (min len (+ pos max-chars))))
    (if (< pos len)
      (substring text pos end)
      "")))

(def (copilot-detect-language app)
  "Detect programming language from the current buffer's filename."
  (let* ((buf (current-buffer-from-app app))
         (file (and buf (buffer-file-path buf))))
    (if (and file (string? file))
      (let ((ext (path-extension file)))
        (cond
          ((member ext '(".ss" ".scm" ".sld")) "Scheme")
          ((member ext '(".py")) "Python")
          ((member ext '(".rs")) "Rust")
          ((member ext '(".go")) "Go")
          ((member ext '(".c" ".h")) "C")
          ((member ext '(".cpp" ".cc" ".hpp")) "C++")
          ((member ext '(".js" ".jsx")) "JavaScript")
          ((member ext '(".ts" ".tsx")) "TypeScript")
          ((member ext '(".rb")) "Ruby")
          ((member ext '(".lua")) "Lua")
          ((member ext '(".java")) "Java")
          ((member ext '(".sh" ".bash")) "Shell/Bash")
          ((member ext '(".html" ".htm")) "HTML")
          ((member ext '(".css")) "CSS")
          ((member ext '(".sql")) "SQL")
          ((member ext '(".md" ".markdown")) "Markdown")
          ((member ext '(".org")) "Org-mode")
          (else "code")))
      "code")))

(def (copilot-request-completion prefix suffix language)
  "Call OpenAI API for code completion. Returns suggestion string or #f."
  (when (string=? *copilot-api-key* "")
    (error "OPENAI_API_KEY not set"))
  (let* ((system-prompt
           (string-append
             "You are a code completion engine for " language ". "
             "Given the code context, provide ONLY the completion text that should "
             "be inserted at the cursor position. Do NOT repeat the existing code. "
             "Do NOT add explanations or markdown formatting. "
             "Provide a short, natural continuation (1-3 lines max). "
             "If no completion makes sense, respond with an empty string."))
         (user-msg
           (string-append
             "Complete the code at the cursor position [CURSOR]:\n\n"
             prefix "[CURSOR]" suffix))
         (body (json-object->string
                 (hash ("model" *copilot-model*)
                       ("messages" [(hash ("role" "system")
                                          ("content" system-prompt))
                                    (hash ("role" "user")
                                          ("content" user-msg))])
                       ("max_tokens" 150)
                       ("temperature" 0.2)
                       ("stop" ["\n\n\n"]))))
         (resp (http-post *copilot-api-url*
                 data: body
                 headers: [["Content-Type" . "application/json"]
                           ["Authorization" . (string-append "Bearer " *copilot-api-key*)]])))
    (if (= (request-status resp) 200)
      (let* ((json-str (request-text resp))
             (result (call-with-input-string json-str read-json))
             (choices (hash-ref result "choices" []))
             (first-choice (and (pair? choices) (car choices)))
             (message (and first-choice (hash-ref first-choice "message" #f)))
             (content (and message (hash-ref message "content" ""))))
        (request-close resp)
        (if (and content (string? content) (> (string-length (string-trim-both content)) 0))
          (string-trim-both content)
          #f))
      (begin
        (request-close resp)
        #f))))

;;;============================================================================
;;; TUI Copilot commands
;;;============================================================================

(def (cmd-copilot-mode app)
  "Toggle copilot mode — AI-assisted code completion."
  (set! *copilot-mode* (not *copilot-mode*))
  ;; Clear any pending suggestion when toggling off
  (unless *copilot-mode*
    (set! *copilot-suggestion* #f))
  (echo-message! (app-state-echo app)
    (if *copilot-mode*
      (if (string=? *copilot-api-key* "")
        "Copilot mode: on (WARNING: OPENAI_API_KEY not set!)"
        (string-append "Copilot mode: on (model: " *copilot-model* ")"))
      "Copilot mode: off")))

(def (cmd-copilot-complete app)
  "Request AI code completion at point. Shows suggestion in echo area."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (cond
      ((string=? *copilot-api-key* "")
       (echo-message! echo "Copilot: set OPENAI_API_KEY environment variable"))
      (else
       (echo-message! echo "Copilot: requesting completion...")
       (with-catch
         (lambda (e)
           (set! *copilot-suggestion* #f)
           (echo-message! echo
             (string-append "Copilot error: " (with-output-to-string (lambda () (display-exception e))))))
         (lambda ()
           (let* ((prefix (copilot-get-context ed 2000))
                  (suffix (copilot-get-suffix ed 500))
                  (language (copilot-detect-language app))
                  (suggestion (copilot-request-completion prefix suffix language)))
             (if suggestion
               (begin
                 (set! *copilot-suggestion* suggestion)
                 (set! *copilot-suggestion-pos* (editor-get-current-pos ed))
                 ;; Show in echo area (TUI cannot do inline ghost text)
                 (let* ((preview (if (> (string-length suggestion) 80)
                                   (string-append (substring suggestion 0 77) "...")
                                   suggestion))
                        ;; Replace newlines with visible markers for echo display
                        (display-text (string-join
                                        (string-split preview #\newline)
                                        "\\n")))
                   (echo-message! echo
                     (string-append "Copilot> " display-text "  [TAB=accept, C-g=dismiss]"))))
               (begin
                 (set! *copilot-suggestion* #f)
                 (echo-message! echo "Copilot: no suggestion"))))))))))

(def (cmd-copilot-accept app)
  "Accept the current copilot suggestion and insert it at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (if *copilot-suggestion*
      (let ((suggestion *copilot-suggestion*))
        (set! *copilot-suggestion* #f)
        ;; Insert at current position
        (let ((pos (editor-get-current-pos ed)))
          (editor-insert-text ed pos suggestion)
          (editor-goto-pos ed (+ pos (string-length suggestion))))
        (echo-message! echo "Copilot: suggestion accepted"))
      (echo-message! echo "Copilot: no pending suggestion"))))

(def (cmd-copilot-dismiss app)
  "Dismiss the current copilot suggestion."
  (let ((echo (app-state-echo app)))
    (if *copilot-suggestion*
      (begin
        (set! *copilot-suggestion* #f)
        (echo-message! echo "Copilot: suggestion dismissed"))
      (echo-message! echo "Copilot: no pending suggestion"))))

(def (cmd-copilot-accept-completion app)
  "Accept copilot suggestion (alias for copilot-accept)."
  (cmd-copilot-accept app))

(def (cmd-copilot-next-completion app)
  "Request next copilot suggestion (re-requests completion)."
  (cmd-copilot-complete app))

;;;============================================================================
;;; String inflection — cycle naming conventions (TUI)
;;;============================================================================

(def (tui-inflection-split-to-tokens word)
  "Split a word into lowercase tokens: handles snake_case, UPPER_CASE, CamelCase, kebab-case."
  (let loop ((chars (string->list word)) (cur "") (tokens []))
    (cond
      ((null? chars)
       (if (> (string-length cur) 0)
         (reverse (cons (string-downcase cur) tokens))
         (reverse tokens)))
      ((or (char=? (car chars) #\_) (char=? (car chars) #\-))
       (if (> (string-length cur) 0)
         (loop (cdr chars) "" (cons (string-downcase cur) tokens))
         (loop (cdr chars) "" tokens)))
      ((and (char-upper-case? (car chars))
            (> (string-length cur) 0)
            (char-lower-case? (string-ref cur (- (string-length cur) 1))))
       (loop (cdr chars) (string (char-downcase (car chars)))
             (cons (string-downcase cur) tokens)))
      ((and (char-upper-case? (car chars))
            (> (string-length cur) 0)
            (char-upper-case? (string-ref cur (- (string-length cur) 1)))
            (not (null? (cdr chars)))
            (char-lower-case? (cadr chars)))
       (loop (cdr chars) (string (char-downcase (car chars)))
             (cons (string-downcase cur) tokens)))
      (else
       (loop (cdr chars) (string-append cur (string (char-downcase (car chars)))) tokens)))))

(def (tui-tokens->snake ts)  (string-join ts "_"))
(def (tui-tokens->upper ts)  (string-upcase (string-join ts "_")))
(def (tui-tokens->kebab ts)  (string-join ts "-"))
(def (tui-tokens->camel ts)
  (if (null? ts) ""
    (apply string-append (car ts)
           (map (lambda (t)
                  (if (= (string-length t) 0) ""
                    (string-append (string (char-upcase (string-ref t 0)))
                                   (substring t 1 (string-length t)))))
                (cdr ts)))))
(def (tui-tokens->pascal ts)
  (apply string-append
         (map (lambda (t)
                (if (= (string-length t) 0) ""
                  (string-append (string (char-upcase (string-ref t 0)))
                                 (substring t 1 (string-length t)))))
              ts)))

(def (tui-inflection-detect-style word)
  (cond
    ((string-contains word "_")
     (if (string=? word (string-upcase word)) 'upper 'snake))
    ((string-contains word "-") 'kebab)
    ((and (> (string-length word) 0)
          (char-upper-case? (string-ref word 0))) 'pascal)
    (else 'camel)))

(def (tui-inflection-next-style current)
  (case current
    ((snake)  'camel)
    ((camel)  'pascal)
    ((pascal) 'upper)
    ((upper)  'kebab)
    ((kebab)  'snake)
    (else     'camel)))

(def (tui-inflection-apply tokens style)
  (case style
    ((snake)  (tui-tokens->snake tokens))
    ((upper)  (tui-tokens->upper tokens))
    ((kebab)  (tui-tokens->kebab tokens))
    ((camel)  (tui-tokens->camel tokens))
    ((pascal) (tui-tokens->pascal tokens))
    (else     (tui-tokens->snake tokens))))

(def (tui-inflection-replace! ed ws we new-word)
  "Replace text in editor from ws to we with new-word using Scintilla target API."
  (send-message ed SCI_SETTARGETSTART ws 0)
  (send-message ed SCI_SETTARGETEND we 0)
  (send-message/string ed SCI_REPLACETARGET new-word)
  (send-message ed SCI_GOTOPOS (+ ws (string-length new-word)) 0))

(def (tui-current-editor app)
  "Get current editor from TUI app state."
  (edit-window-editor (current-window (app-state-frame app))))

(def (cmd-string-inflection-cycle app)
  "Cycle word at point through naming conventions: snake→camel→PascalCase→UPPER→kebab."
  (let* ((ed   (tui-current-editor app))
         (echo (app-state-echo app))
         (pos  (editor-get-current-pos ed)))
    (let-values (((ws we) (word-bounds-at ed pos)))
      (if (not ws)
        (echo-error! echo "No word at point")
        (let* ((text  (editor-get-text ed))
               (word  (substring text ws we))
               (tokens (tui-inflection-split-to-tokens word))
               (style  (tui-inflection-detect-style word))
               (next   (tui-inflection-next-style style))
               (new-word (tui-inflection-apply tokens next)))
          (tui-inflection-replace! ed ws we new-word)
          (echo-message! echo
            (string-append word " → " new-word " (" (symbol->string next) ")")))))))

(def (cmd-string-inflection-snake-case app)
  "Convert word at point to snake_case."
  (let* ((ed (tui-current-editor app)) (echo (app-state-echo app))
         (pos (editor-get-current-pos ed)))
    (let-values (((ws we) (word-bounds-at ed pos)))
      (if (not ws) (echo-error! echo "No word at point")
        (let* ((text (editor-get-text ed))
               (word (substring text ws we))
               (new-word (tui-tokens->snake (tui-inflection-split-to-tokens word))))
          (tui-inflection-replace! ed ws we new-word)
          (echo-message! echo (string-append word " → " new-word)))))))

(def (cmd-string-inflection-camelcase app)
  "Convert word at point to camelCase."
  (let* ((ed (tui-current-editor app)) (echo (app-state-echo app))
         (pos (editor-get-current-pos ed)))
    (let-values (((ws we) (word-bounds-at ed pos)))
      (if (not ws) (echo-error! echo "No word at point")
        (let* ((text (editor-get-text ed))
               (word (substring text ws we))
               (new-word (tui-tokens->camel (tui-inflection-split-to-tokens word))))
          (tui-inflection-replace! ed ws we new-word)
          (echo-message! echo (string-append word " → " new-word)))))))

(def (cmd-string-inflection-upcase app)
  "Convert word at point to UPPER_CASE."
  (let* ((ed (tui-current-editor app)) (echo (app-state-echo app))
         (pos (editor-get-current-pos ed)))
    (let-values (((ws we) (word-bounds-at ed pos)))
      (if (not ws) (echo-error! echo "No word at point")
        (let* ((text (editor-get-text ed))
               (word (substring text ws we))
               (new-word (tui-tokens->upper (tui-inflection-split-to-tokens word))))
          (tui-inflection-replace! ed ws we new-word)
          (echo-message! echo (string-append word " → " new-word)))))))

;;;============================================================================
;;; Occur edit mode (TUI) — make *Occur* buffer editable
;;;============================================================================

(def *tui-occur-edit-originals* (make-hash-table)) ; line-num -> original-text

(def (cmd-occur-edit-mode app)
  "Enable editing in *Occur* buffer. C-c C-c commits changes back to source buffer."
  (let* ((buf  (current-buffer-from-app app))
         (echo (app-state-echo app))
         (ed   (current-editor app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (begin
        (let* ((text  (editor-get-text ed))
               (lines (string-split text #\newline)))
          (hash-clear! *tui-occur-edit-originals*)
          (for-each
            (lambda (line)
              (let ((colon (string-index line #\:)))
                (when (and colon (> colon 0)
                           (char-numeric? (string-ref line 0)))
                  (let ((lnum (string->number (substring line 0 colon))))
                    (when lnum
                      (hash-put! *tui-occur-edit-originals* lnum
                        (if (< (+ colon 1) (string-length line))
                          (substring line (+ colon 1) (string-length line))
                          "")))))))
            lines)
          (editor-set-read-only ed #f)
          (echo-message! echo
            "Occur edit ON — edit lines then C-c C-c to commit"))))))

(def (cmd-occur-commit-edits app)
  "Commit *Occur* edits back to source buffer."
  (let* ((buf    (current-buffer-from-app app))
         (echo   (app-state-echo app))
         (ed     (current-editor app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (let* ((full-text   (editor-get-text ed))
             (source-name (occur-parse-source-name full-text))
             (source      (and source-name (buffer-by-name source-name))))
        (if (not source)
          (echo-error! echo "Source buffer not found")
          (let* ((occur-lines (string-split full-text #\newline))
                 (changes
                  (let loop ((ls occur-lines) (acc []))
                    (if (null? ls) (reverse acc)
                      (let* ((line (car ls))
                             (colon (string-index line #\:)))
                        (if (and colon (> colon 0)
                                 (char-numeric? (string-ref line 0)))
                          (let ((lnum (string->number (substring line 0 colon))))
                            (if lnum
                              (let* ((new-text (if (< (+ colon 1) (string-length line))
                                               (substring line (+ colon 1) (string-length line))
                                               ""))
                                     (orig (hash-get *tui-occur-edit-originals* lnum)))
                                (if (and orig (not (string=? new-text orig)))
                                  (loop (cdr ls) (cons (cons lnum new-text) acc))
                                  (loop (cdr ls) acc)))
                              (loop (cdr ls) acc)))
                          (loop (cdr ls) acc)))))))
            (if (null? changes)
              (begin
                (editor-set-read-only ed #t)
                (echo-message! echo "No changes to commit"))
              (begin
                (buffer-attach! ed source)
                (set! (edit-window-buffer (current-window (app-state-frame app))) source)
                (let* ((src-text  (editor-get-text ed))
                       (src-lines (list->vector (string-split src-text #\newline)))
                       (n-lines   (vector-length src-lines)))
                  (for-each
                    (lambda (change)
                      (when (and (> (car change) 0) (<= (car change) n-lines))
                        (vector-set! src-lines (- (car change) 1) (cdr change))))
                    changes)
                  (let ((new-src (string-join (vector->list src-lines) "\n")))
                    (editor-set-read-only ed #f)
                    (editor-set-text ed new-src)))
                (echo-message! echo
                  (string-append (number->string (length changes))
                                 " change(s) applied to " source-name))))))))))

;;;============================================================================
;;; wdired mode (TUI) — edit filenames in dired buffer
;;;============================================================================

(def *tui-wdired-originals* (make-hash-table)) ; buffer-name -> vector of original lines

(def (tui-dired-line? str)
  (and (>= (string-length str) 10)
       (or (char=? (string-ref str 0) #\-)
           (char=? (string-ref str 0) #\d)
           (char=? (string-ref str 0) #\l))
       (or (char=? (string-ref str 1) #\r)
           (char=? (string-ref str 1) #\-))))

(def (tui-dired-line-filename str)
  "Extract filename from dired listing line (after 8 space-separated fields)."
  (let loop ((i 0) (fields 0) (in-ws #t))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) #\space) (loop (+ i 1) fields #t))
      (in-ws (if (= fields 8)
               (substring str i (string-length str))
               (loop (+ i 1) (+ fields 1) #f)))
      (else (loop (+ i 1) fields #f)))))

(def (cmd-wdired-mode app)
  "Enable wdired: make filenames in dired buffer editable. C-c C-c commits."
  (let* ((buf  (current-buffer-from-app app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (if (not (or (string=? name "*Dired*")
                 (string-prefix? "*dired:" name)))
      (echo-error! echo "Not in a dired buffer")
      (let* ((ed   (current-editor app))
             (text (editor-get-text ed)))
        (hash-put! *tui-wdired-originals* name
          (list->vector (string-split text #\newline)))
        (editor-set-read-only ed #f)
        (echo-message! echo
          "wdired: edit filenames, C-c C-c to commit, C-c C-k to abort")))))

(def (cmd-wdired-finish-edit app)
  "Commit wdired renames."
  (let* ((buf  (current-buffer-from-app app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (if (not (hash-get *tui-wdired-originals* name))
      (echo-error! echo "Not in wdired mode")
      (let* ((ed        (current-editor app))
             (text      (editor-get-text ed))
             (cur-lines (string-split text #\newline))
             (orig-vec  (hash-get *tui-wdired-originals* name))
             (renames   []))
        (let loop ((cur cur-lines) (i 0))
          (when (and (not (null? cur)) (< i (vector-length orig-vec)))
            (let* ((cl (car cur))
                   (ol (vector-ref orig-vec i)))
              (when (and (tui-dired-line? ol) (tui-dired-line? cl))
                (let ((old-name (tui-dired-line-filename ol))
                      (new-name (tui-dired-line-filename cl)))
                  (when (and old-name new-name (not (string=? old-name new-name)))
                    (set! renames (cons (cons old-name new-name) renames)))))
              (loop (cdr cur) (+ i 1)))))
        (if (null? renames)
          (begin
            (hash-remove! *tui-wdired-originals* name)
            (editor-set-read-only ed #t)
            (echo-message! echo "wdired: no renames"))
          (let* ((dir-line (if (> (length cur-lines) 0) (car cur-lines) ""))
                 (dir      (string-trim dir-line))
                 (ok-count 0) (err-count 0))
            (for-each
              (lambda (rename)
                (let* ((old-f (string-append dir "/" (car rename)))
                       (new-f (string-append dir "/" (cdr rename)))
                       (result
                        (with-exception-catcher
                          (lambda (e) #f)
                          (lambda ()
                            (let ((proc (open-process
                                          (list path: "mv"
                                                arguments: (list "--" old-f new-f)
                                                stdin-redirection: #f
                                                stdout-redirection: #f
                                                stderr-redirection: #f))))
                              (close-port proc)
                              #t)))))
                  (if result
                    (set! ok-count (+ ok-count 1))
                    (set! err-count (+ err-count 1)))))
              renames)
            (hash-remove! *tui-wdired-originals* name)
            (editor-set-read-only ed #t)
            (echo-message! echo
              (string-append "wdired: " (number->string ok-count)
                             " rename(s), " (number->string err-count) " error(s)"))))))))

(def (cmd-wdired-abort app)
  "Abort wdired: restore original dired buffer."
  (let* ((buf  (current-buffer-from-app app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (if (not (hash-get *tui-wdired-originals* name))
      (echo-error! echo "Not in wdired mode")
      (let* ((ed   (current-editor app))
             (orig (hash-get *tui-wdired-originals* name))
             (text (string-join (vector->list orig) "\n")))
        (editor-set-read-only ed #f)
        (editor-set-text ed text)
        (editor-set-read-only ed #t)
        (hash-remove! *tui-wdired-originals* name)
        (echo-message! echo "wdired: aborted")))))
