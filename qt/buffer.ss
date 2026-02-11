;;; -*- Gerbil -*-
;;; Qt document management for gerbil-emacs
;;;
;;; Uses QTextDocument for multi-buffer support.
;;; Each buffer owns a QTextDocument (preserves undo history per buffer).

(export qt-buffer-create!
        qt-buffer-kill!
        qt-buffer-attach!)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core)

;;;============================================================================
;;; Qt buffer operations
;;;============================================================================

(def (qt-buffer-create! name editor (file-path #f))
  "Create buffer with a new QTextDocument (using QPlainTextDocumentLayout)."
  (let* ((doc (qt-plain-text-document-create))
         (buf (make-buffer name file-path doc #f #f #f #f)))
    (buffer-list-add! buf)
    buf))

(def (qt-buffer-kill! buf)
  "Destroy the buffer's QTextDocument and remove from buffer list."
  (qt-text-document-destroy! (buffer-doc-pointer buf))
  (buffer-list-remove! buf))

(def (qt-buffer-attach! editor buf)
  "Switch editor to display this buffer's document."
  (qt-plain-text-edit-set-document! editor (buffer-doc-pointer buf)))
