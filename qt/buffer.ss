;;; -*- Gerbil -*-
;;; Qt document management for gerbil-emacs
;;;
;;; Uses Scintilla document model for multi-buffer support.
;;; Each buffer owns a Scintilla document (preserves undo history per buffer).

(export qt-buffer-create!
        qt-buffer-kill!
        qt-buffer-attach!)

(import :std/sugar
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core)

;;;============================================================================
;;; Qt buffer operations
;;;============================================================================

(def (qt-buffer-create! name editor (file-path #f))
  "Create buffer with a new Scintilla document."
  (let* ((doc (sci-send editor SCI_CREATEDOCUMENT 0 0))
         (buf (make-buffer name file-path doc #f #f #f #f)))
    (doc-editor-register! doc editor)
    (doc-buffer-register! doc buf)
    (buffer-list-add! buf)
    buf))

(def (qt-buffer-kill! buf)
  "Release the Scintilla document and remove from buffer list."
  (let* ((doc (buffer-doc-pointer buf))
         (ed (hash-get *doc-editor-map* doc)))
    (when ed
      (sci-send ed SCI_RELEASEDOCUMENT 0 doc))
    (hash-remove! *doc-editor-map* doc)
    (hash-remove! *doc-buffer-map* doc)
    (buffer-list-remove! buf)))

(def (qt-buffer-attach! editor buf)
  "Switch editor to display this buffer's document."
  (let ((doc (buffer-doc-pointer buf)))
    (sci-send editor SCI_SETDOCPOINTER 0 doc)
    (doc-editor-register! doc editor)))
