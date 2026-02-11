;;; -*- Gerbil -*-
;;; Buffer management for gerbil-emacs
;;;
;;; Each buffer owns a Scintilla document pointer (via SCI_CREATEDOCUMENT/
;;; SCI_ADDREFDOCUMENT/SCI_RELEASEDOCUMENT). Switching buffers calls
;;; SCI_SETDOCPOINTER on the editor widget.

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
        :gerbil-scintilla/scintilla)

;;;============================================================================
;;; Buffer structure
;;;============================================================================

(defstruct buffer
  (name        ; string: display name (e.g. "*scratch*", "foo.txt")
   file-path   ; string or #f: file path if visiting a file
   doc-pointer ; long: Scintilla document pointer
   mark        ; integer or #f: mark position for region
   modified    ; boolean
   lexer-lang) ; symbol or #f: lexer language
  transparent: #t)

;;;============================================================================
;;; Global buffer list
;;;============================================================================

(def *buffer-list* [])

(def (buffer-list) *buffer-list*)

(def (buffer-list-add! buf)
  (set! *buffer-list* (cons buf *buffer-list*)))

(def (buffer-list-remove! buf)
  (set! *buffer-list*
    (let loop ((bufs *buffer-list*) (acc []))
      (cond
        ((null? bufs) (reverse acc))
        ((eq? (car bufs) buf) (loop (cdr bufs) acc))
        (else (loop (cdr bufs) (cons (car bufs) acc)))))))

(def (buffer-by-name name)
  "Find a buffer by name. Returns #f if not found."
  (let loop ((bufs *buffer-list*))
    (cond
      ((null? bufs) #f)
      ((string=? (buffer-name (car bufs)) name) (car bufs))
      (else (loop (cdr bufs))))))

;;;============================================================================
;;; Buffer creation
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
;;; Buffer operations
;;;============================================================================

(def (buffer-kill! editor buf)
  "Release the buffer's document and remove from the buffer list."
  (send-message editor SCI_RELEASEDOCUMENT 0 (buffer-doc-pointer buf))
  (buffer-list-remove! buf))

(def (buffer-attach! editor buf)
  "Switch editor to display this buffer's document."
  (send-message editor SCI_SETDOCPOINTER 0 (buffer-doc-pointer buf)))

;;;============================================================================
;;; Constants
;;;============================================================================

(def buffer-scratch-name "*scratch*")
