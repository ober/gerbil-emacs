;;; -*- Gerbil -*-
;;; Qt echo area / minibuffer for gerbil-emacs
;;;
;;; Uses a QLabel for displaying messages and QInputDialog for prompts.

(export qt-echo-draw!
        qt-echo-read-string
        qt-echo-read-string-with-completion)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/qt/window)

;;;============================================================================
;;; Draw the echo area (QLabel)
;;;============================================================================

(def (qt-echo-draw! echo label)
  "Update the echo QLabel with current message."
  (let ((msg (echo-state-message echo)))
    (if msg
      (begin
        (qt-label-set-text! label msg)
        ;; Red text for errors, normal for messages
        (if (echo-state-error? echo)
          (qt-widget-set-style-sheet!
            label "color: #ff4040; background: #181818; font-family: monospace;")
          (qt-widget-set-style-sheet!
            label "color: #d8d8d8; background: #181818; font-family: monospace;")))
      (qt-label-set-text! label ""))))

;;;============================================================================
;;; Read a string via Qt input dialog
;;;============================================================================

(def (qt-echo-read-string app prompt)
  "Show a Qt input dialog for minibuffer input.
   Returns the input string, or #f if cancelled."
  (let ((fr (app-state-frame app)))
    ;; qt-input-dialog-get-text returns #f on cancel, string on accept
    (qt-input-dialog-get-text "gerbil-emacs" prompt
      parent: (qt-frame-main-win fr))))

;;;============================================================================
;;; Read a string with completion via custom dialog + QCompleter
;;;============================================================================

(def (qt-echo-read-string-with-completion app prompt completions)
  "Show a dialog with QLineEdit + QCompleter for completing input.
   Returns the input string, or #f if cancelled."
  (let* ((fr (app-state-frame app))
         (dlg (qt-dialog-create parent: (qt-frame-main-win fr)))
         (layout (qt-vbox-layout-create dlg))
         (label (qt-label-create prompt))
         (line-edit (qt-line-edit-create))
         (completer (qt-completer-create completions)))
    (qt-dialog-set-title! dlg "gerbil-emacs")
    (qt-widget-set-minimum-width! dlg 400)
    ;; Configure completer
    (qt-completer-set-case-sensitivity! completer QT_CASE_INSENSITIVE)
    (qt-completer-set-filter-mode! completer QT_MATCH_CONTAINS)
    (qt-completer-set-max-visible-items! completer 15)
    (qt-completer-set-widget! completer line-edit)
    (qt-line-edit-set-completer! line-edit completer)
    ;; Build layout
    (qt-layout-add-widget! layout label)
    (qt-layout-add-widget! layout line-edit)
    ;; Enter accepts the dialog
    (qt-on-return-pressed! line-edit (lambda () (qt-dialog-accept! dlg)))
    ;; Run modal dialog
    (let ((result (qt-dialog-exec! dlg)))
      (let ((text (if (= result 1)
                    (let ((t (qt-line-edit-text line-edit)))
                      (if (string=? t "") #f t))
                    #f)))
        (qt-completer-destroy! completer)
        text))))
