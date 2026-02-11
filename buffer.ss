;;; -*- Gerbil -*-
;;; TUI buffer management for gerbil-emacs
;;;
;;; Each buffer owns a Scintilla document pointer (via SCI_CREATEDOCUMENT/
;;; SCI_ADDREFDOCUMENT/SCI_RELEASEDOCUMENT). Switching buffers calls
;;; SCI_SETDOCPOINTER on the editor widget.
;;;
;;; Buffer struct, list management, and constants are in core.ss.

(export
  (struct-out buffer)
  *buffer-list*
  buffer-list
  buffer-list-add!
  buffer-list-remove!
  buffer-by-name
  buffer-create!
  buffer-create-from-editor!
  buffer-kill!
  buffer-attach!
  buffer-scratch-name)

(import :std/sugar
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-emacs/core)

;;;============================================================================
;;; Buffer creation (Scintilla-specific)
;;;============================================================================

(def (buffer-create! name editor (file-path #f))
  "Create a new buffer with a fresh Scintilla document."
  (let* ((doc (send-message editor SCI_CREATEDOCUMENT 0 0))
         (buf (make-buffer name file-path doc #f #f #f)))
    (buffer-list-add! buf)
    buf))

(def (buffer-create-from-editor! name editor)
  "Create a buffer wrapping the editor's current document.
   Adds a reference so the buffer owns the document independently."
  (let ((doc (send-message editor SCI_GETDOCPOINTER)))
    (send-message editor SCI_ADDREFDOCUMENT 0 doc)
    (let ((buf (make-buffer name #f doc #f #f #f)))
      (buffer-list-add! buf)
      buf)))

;;;============================================================================
;;; Buffer operations (Scintilla-specific)
;;;============================================================================

(def (buffer-kill! editor buf)
  "Release the buffer's document and remove from the buffer list."
  (send-message editor SCI_RELEASEDOCUMENT 0 (buffer-doc-pointer buf))
  (buffer-list-remove! buf))

(def (buffer-attach! editor buf)
  "Switch editor to display this buffer's document."
  (send-message editor SCI_SETDOCPOINTER 0 (buffer-doc-pointer buf)))
