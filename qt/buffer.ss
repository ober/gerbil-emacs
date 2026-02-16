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
  "Release the Scintilla document and remove from buffer list.
   For image buffers, also destroys the pixmap."
  (let* ((doc (buffer-doc-pointer buf))
         (ed (hash-get *doc-editor-map* doc)))
    (when ed
      (sci-send ed SCI_RELEASEDOCUMENT 0 doc))
    ;; Clean up image buffer state if applicable
    (let ((state (hash-get *image-buffer-state* buf)))
      (when state
        (qt-pixmap-destroy! (car state))
        (hash-remove! *image-buffer-state* buf)))
    (hash-remove! *doc-editor-map* doc)
    (hash-remove! *doc-buffer-map* doc)
    (buffer-list-remove! buf)))

(def (qt-buffer-attach! editor buf)
  "Switch editor to display this buffer's document.
   Re-applies the document's read-only state after swap because QScintilla
   may have a widget-level readOnly flag that persists across document switches.
   Calls *post-buffer-attach-hook* to handle image/text display toggling."
  (let ((doc (buffer-doc-pointer buf)))
    (sci-send editor SCI_SETDOCPOINTER 0 doc)
    (doc-editor-register! doc editor)
    ;; Force QScintilla widget to sync with the new document's read-only state.
    ;; Without this, viewing a read-only buffer (e.g. *Buffer List*) makes all
    ;; subsequent buffers uneditable.
    (let ((ro (sci-send editor SCI_GETREADONLY)))
      (sci-send editor SCI_SETREADONLY ro))
    ;; Toggle image/editor display via hook (set up in qt/app.ss)
    (*post-buffer-attach-hook* editor buf)))
