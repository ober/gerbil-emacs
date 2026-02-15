;;; -*- Gerbil -*-
;;; Qt echo area / minibuffer for gerbil-emacs
;;;
;;; Uses a QLabel for displaying messages and compact dark-styled dialogs
;;; for minibuffer prompts.

(export qt-echo-draw!
        qt-echo-read-string
        qt-echo-read-string-with-completion)

(import :std/sugar
        :gerbil-emacs/qt/sci-shim
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
            label "color: #ff4040; background: #282828; font-family: monospace; font-size: 10pt; padding: 2px 4px;")
          (qt-widget-set-style-sheet!
            label "color: #d8d8d8; background: #282828; font-family: monospace; font-size: 10pt; padding: 2px 4px;")))
      (qt-label-set-text! label ""))))

;;; Shared minibuffer dialog style
(def *minibuffer-style*
  "QDialog { background: #282828; }
   QLabel { color: #b0b0b0; background: transparent; font-family: monospace; font-size: 10pt; padding: 0; }
   QLineEdit { color: #d8d8d8; background: #282828; border: none; font-family: monospace; font-size: 10pt; padding: 2px; }
   QListView { color: #d8d8d8; background: #282828; border: 1px solid #484848; font-family: monospace; font-size: 10pt; }")

(def (make-minibuffer-dialog parent prompt)
  "Create a compact dark dialog for minibuffer input.
   Positioned at the bottom of the parent window to look like Emacs minibuffer.
   Returns (values dialog line-edit)."
  (let* ((dlg (qt-dialog-create parent: parent))
         (layout (qt-hbox-layout-create dlg))
         (label (qt-label-create prompt))
         (line-edit (qt-line-edit-create)))
    (qt-dialog-set-title! dlg "")
    (qt-widget-set-style-sheet! dlg *minibuffer-style*)
    ;; Tight layout: prompt label + input field side by side
    (qt-layout-set-margins! layout 4 2 4 2)
    (qt-layout-set-spacing! layout 4)
    (qt-layout-add-widget! layout label)
    (qt-layout-add-widget! layout line-edit)
    (qt-layout-set-stretch-factor! layout label 0)
    (qt-layout-set-stretch-factor! layout line-edit 1)
    ;; Full width of parent, minimal height, positioned at bottom
    (let* ((pw (qt-widget-width parent))
           (ph (qt-widget-height parent))
           (px (qt-widget-x parent))
           (py (qt-widget-y parent))
           (dlg-h 36)
           (dlg-w pw))
      (qt-widget-resize! dlg dlg-w dlg-h)
      (qt-widget-move! dlg px (+ py (- ph dlg-h))))
    ;; Enter accepts, Escape rejects
    (qt-on-return-pressed! line-edit (lambda () (qt-dialog-accept! dlg)))
    (values dlg line-edit)))

;;;============================================================================
;;; Read a string via minibuffer dialog
;;;============================================================================

(def (qt-echo-read-string app prompt)
  "Show a minibuffer-style dialog for input.
   Returns the input string, or #f if cancelled."
  (let ((fr (app-state-frame app)))
    (let-values (((dlg line-edit)
                  (make-minibuffer-dialog (qt-frame-main-win fr) prompt)))
      (let ((result (qt-dialog-exec! dlg)))
        (if (= result 1)
          (let ((t (qt-line-edit-text line-edit)))
            (if (string=? t "") #f t))
          #f)))))

;;;============================================================================
;;; Read a string with completion via minibuffer dialog + QCompleter
;;;============================================================================

(def (qt-echo-read-string-with-completion app prompt completions)
  "Show a minibuffer-style dialog with QCompleter for completing input.
   Returns the input string, or #f if cancelled."
  (let ((fr (app-state-frame app)))
    (let-values (((dlg line-edit)
                  (make-minibuffer-dialog (qt-frame-main-win fr) prompt)))
      ;; Add completer
      (let ((completer (qt-completer-create completions)))
        (qt-completer-set-case-sensitivity! completer QT_CASE_INSENSITIVE)
        (qt-completer-set-filter-mode! completer QT_MATCH_CONTAINS)
        (qt-completer-set-max-visible-items! completer 15)
        (qt-completer-set-widget! completer line-edit)
        (qt-line-edit-set-completer! line-edit completer)
        ;; Run modal
        (let ((result (qt-dialog-exec! dlg)))
          (let ((text (if (= result 1)
                        (let ((t (qt-line-edit-text line-edit)))
                          (if (string=? t "") #f t))
                        #f)))
            (qt-completer-destroy! completer)
            text))))))
