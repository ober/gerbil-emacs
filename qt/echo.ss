;;; -*- Gerbil -*-
;;; Qt echo area / minibuffer for gerbil-emacs
;;;
;;; Uses a QLabel for displaying messages and QInputDialog for prompts.

(export qt-echo-draw!
        qt-echo-read-string)

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
