;;; -*- Gerbil -*-
;;; Qt status bar modeline for gerbil-emacs
;;;
;;; Shows buffer name, line, column, modified indicator in the
;;; main window's status bar.

(export qt-modeline-update!)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/qt/window)

;;;============================================================================
;;; Modeline rendering
;;;============================================================================

(def (qt-modeline-update! app)
  "Update the status bar with current buffer info."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-edit-window-buffer win))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (col  (+ 1 (qt-plain-text-edit-cursor-column ed)))
         (mod? (qt-text-document-modified? (buffer-doc-pointer buf)))
         (info (string-append
                "-UU-:" (if mod? "**" "--") "-  "
                (buffer-name buf) "    ("
                (number->string line) ","
                (number->string col) ")")))
    (qt-main-window-set-status-bar-text! (qt-frame-main-win fr) info)))
