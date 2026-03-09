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
        :gemacs/editor-extra-helpers)

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
