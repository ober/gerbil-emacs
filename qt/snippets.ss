;;; -*- Gerbil -*-
;;; Qt snippet UI commands
;;; Uses shared snippet infrastructure from :gemacs/snippets

(export #t)

(import :std/sugar
        :std/srfi/13
        :gemacs/core
        :gemacs/snippets
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/commands-core)

;;; ============================================================================
;;; Qt snippet commands
;;; ============================================================================

(def (cmd-snippet-expand app)
  "Try to expand snippet at point. Returns #t if expanded, #f otherwise."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (prefix (get-word-prefix ed))
         (lang (or (buffer-lexer-lang buf) 'global)))
    (if (string=? prefix "")
      #f  ;; No trigger word
      (let ((template (snippet-lookup prefix lang)))
        (if (not template)
          #f  ;; No matching snippet
          (let* ((expanded (snippet-expand-template template))
                 (text (car expanded))
                 (fields (cdr expanded))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (trigger-start (- pos (string-length prefix)))
                 ;; Replace trigger with expanded text
                 (full-text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring full-text 0 trigger-start)
                             text
                             (substring full-text pos (string-length full-text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (if (null? fields)
              ;; No fields — place cursor at end of expansion
              (qt-plain-text-edit-set-cursor-position! ed
                (+ trigger-start (string-length text)))
              ;; Place cursor at first field
              (let ((first-field (car fields)))
                (set! *snippet-active* #t)
                (set! *snippet-field-positions*
                  (map (lambda (f) (+ trigger-start (cdr f))) fields))
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ trigger-start (cdr first-field)))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            #t))))))

(def (cmd-snippet-next-field app)
  "Jump to next snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find next field after current position
           (next (let loop ((fps *snippet-field-positions*))
                   (if (null? fps) #f
                     (if (> (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (if next
        (begin
          (qt-plain-text-edit-set-cursor-position! ed next)
          (qt-plain-text-edit-ensure-cursor-visible! ed))
        ;; No more fields — deactivate snippet
        (snippet-deactivate!)))))

(def (cmd-snippet-prev-field app)
  "Jump to previous snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find previous field before current position
           (prev (let loop ((fps (reverse *snippet-field-positions*)))
                   (if (null? fps) #f
                     (if (< (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (when prev
        (qt-plain-text-edit-set-cursor-position! ed prev)
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-define-snippet app)
  "Interactively define a snippet."
  (let* ((lang-str (qt-echo-read-string app "Language (or global): "))
         (trigger (qt-echo-read-string app "Trigger: "))
         (template (qt-echo-read-string app "Template ($1,$2 for fields, ${1:default}): ")))
    (when (and lang-str trigger template
               (> (string-length trigger) 0)
               (> (string-length template) 0))
      (let ((lang (string->symbol lang-str)))
        (snippet-define! lang trigger template)
        (echo-message! (app-state-echo app)
          (string-append "Snippet '" trigger "' defined for " lang-str))))))

(def (cmd-list-snippets app)
  "List all defined snippets."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (out (open-output-string)))
    (display "Snippets:\n\n" out)
    (hash-for-each
      (lambda (lang lang-table)
        (display (string-append "--- " (symbol->string lang) " ---\n") out)
        (hash-for-each
          (lambda (trigger template)
            (let ((first-line (let ((nl (string-index template #\newline)))
                                (if nl (substring template 0 nl) template))))
              (display (string-append "  " trigger " → " first-line "\n") out)))
          lang-table))
      *snippet-table*)
    (let* ((text (get-output-string out))
           (buf (or (buffer-by-name "*Snippets*")
                    (qt-buffer-create! "*Snippets*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

(def (cmd-snippet-insert app)
  "Browse and insert a snippet using narrowing."
  (let* ((buf (current-qt-buffer app))
         (lang (or (buffer-lexer-lang buf) 'global))
         (triggers (snippet-all-triggers lang))
         (candidates (map (lambda (pair)
                            (let* ((trigger (car pair))
                                   (template (cdr pair))
                                   (first-line (let ((nl (string-index template #\newline)))
                                                 (if nl (substring template 0 nl) template))))
                              (string-append trigger " → " first-line)))
                          triggers)))
    (if (null? candidates)
      (echo-message! (app-state-echo app) "No snippets available")
      (let ((choice (qt-echo-read-with-narrowing app "Insert snippet:" candidates)))
        (when (and choice (> (string-length choice) 0))
          ;; Extract trigger from "trigger → template" format
          (let* ((arrow-pos (string-contains choice " → "))
                 (trigger (if arrow-pos (substring choice 0 arrow-pos) choice))
                 (template (snippet-lookup trigger lang)))
            (when template
              (let* ((expanded (snippet-expand-template template))
                     (text (car expanded))
                     (fields (cdr expanded))
                     (ed (current-qt-editor app))
                     (pos (qt-plain-text-edit-cursor-position ed))
                     (full-text (qt-plain-text-edit-text ed))
                     (new-text (string-append
                                 (substring full-text 0 pos)
                                 text
                                 (substring full-text pos (string-length full-text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (if (null? fields)
                  (qt-plain-text-edit-set-cursor-position! ed (+ pos (string-length text)))
                  (let ((first-field (car fields)))
                    (set! *snippet-active* #t)
                    (set! *snippet-field-positions*
                      (map (lambda (f) (+ pos (cdr f))) fields))
                    (qt-plain-text-edit-set-cursor-position! ed
                      (+ pos (cdr first-field)))))
                (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))
