;;; -*- Gerbil -*-
;;; Face System — Visual Attributes for Text and UI Elements
;;;
;;; This module defines an Emacs-like "face" abstraction that maps semantic
;;; names to visual properties (foreground, background, bold, italic, underline).
;;; Themes define collections of faces, enabling theme-aware syntax highlighting.

(export #t)
(import :std/sugar)

;;; ============================================================================
;;; Face Data Structure
;;; ============================================================================

;; A face defines visual attributes for text or UI elements.
;; All fields are optional — #f means "inherit from parent" or "no override".
(defstruct face (fg bg bold italic underline))

;; Keyword constructor for faces with defaults
;; Note: defstruct auto-generates make-face, so we use new-face for the keyword version
(def (new-face
      fg: (fg #f)
      bg: (bg #f)
      bold: (bold #f)
      italic: (italic #f)
      underline: (underline #f))
  "Create a face with keyword arguments. All attributes optional."
  (make-face fg bg bold italic underline))

;;; ============================================================================
;;; Face Registry
;;; ============================================================================

;; Global face registry: symbol -> face struct
(def *faces* (make-hash-table-eq))

(def (define-face! name . props)
  "Register a face by name. Props are keyword args: fg: bg: bold: italic: underline:
   Example: (define-face! 'default fg: \"#d8d8d8\" bg: \"#181818\")"
  (let ((f (apply new-face props)))
    (hash-put! *faces* name f)))

(def (face-get name)
  "Look up a face by name. Returns the face struct or #f if not found."
  (hash-get *faces* name))

(def (face-ref name)
  "Look up a face by name. Raises an error if not found."
  (hash-ref *faces* name))

(def (set-face-attribute! name
                            fg: (fg 'unset)
                            bg: (bg 'unset)
                            bold: (bold 'unset)
                            italic: (italic 'unset)
                            underline: (underline 'unset))
  "Modify an existing face's properties, or create it if it doesn't exist.
   Example: (set-face-attribute! 'font-lock-keyword-face fg: \"#ff79c6\" bold: #t)"
  (let* ((existing (face-get name))
         (final-fg (if (eq? fg 'unset) (and existing (face-fg existing)) fg))
         (final-bg (if (eq? bg 'unset) (and existing (face-bg existing)) bg))
         (final-bold (if (eq? bold 'unset) (and existing (face-bold existing)) bold))
         (final-italic (if (eq? italic 'unset) (and existing (face-italic existing)) italic))
         (final-underline (if (eq? underline 'unset) (and existing (face-underline existing)) underline)))
    (hash-put! *faces* name (new-face fg: final-fg bg: final-bg bold: final-bold
                                       italic: final-italic underline: final-underline))))

(def (face-clear!)
  "Clear all registered faces. Useful for theme switching."
  (hash-clear! *faces*))

;;; ============================================================================
;;; Font State
;;; ============================================================================

;; Global default font settings for all editors
(def *default-font-family* "Monospace")
(def *default-font-size* 11)

(def (set-default-font! family size)
  "Set the default font family and size for all editors."
  (set! *default-font-family* family)
  (set! *default-font-size* size))

(def (get-default-font)
  "Returns (values family size) for the current default font."
  (values *default-font-family* *default-font-size*))

;;; ============================================================================
;;; Color Parsing Utilities
;;; ============================================================================

(def (parse-hex-color color-str)
  "Parse a hex color string \"#RRGGBB\" and return (values r g b) as integers 0-255.
   Returns (values #xd8 #xd8 #xd8) if parsing fails."
  (if (and (string? color-str)
           (>= (string-length color-str) 7)
           (char=? (string-ref color-str 0) #\#))
    (let* ((hex-str (substring color-str 1 7))
           (r-str (substring hex-str 0 2))
           (g-str (substring hex-str 2 4))
           (b-str (substring hex-str 4 6)))
      (with-catch
        (lambda (e)
          ;; Default gray on parse failure
          (values #xd8 #xd8 #xd8))
        (lambda ()
          (values
            (string->number r-str 16)
            (string->number g-str 16)
            (string->number b-str 16)))))
    ;; Default gray for malformed input
    (values #xd8 #xd8 #xd8)))

(def (rgb->hex r g b)
  "Convert RGB values (0-255) to hex color string \"#RRGGBB\"."
  (let ((r-hex (number->string r 16))
        (g-hex (number->string g 16))
        (b-hex (number->string b 16)))
    (string-append "#"
                   (if (< r 16) "0" "") r-hex
                   (if (< g 16) "0" "") g-hex
                   (if (< b 16) "0" "") b-hex)))

;;; ============================================================================
;;; Standard Face Definitions (Dark Theme Defaults)
;;; ============================================================================

(def (define-standard-faces!)
  "Define standard faces with dark theme defaults.
   These provide fallback values if no theme is loaded."

  ;; Base faces
  (define-face! 'default
    fg: "#d8d8d8" bg: "#181818")

  (define-face! 'region
    bg: "#404060")

  ;; Syntax highlighting faces
  (define-face! 'font-lock-keyword-face
    fg: "#cc99cc" bold: #t)

  (define-face! 'font-lock-builtin-face
    fg: "#66cccc")

  (define-face! 'font-lock-string-face
    fg: "#99cc99")

  (define-face! 'font-lock-comment-face
    fg: "#999999" italic: #t)

  (define-face! 'font-lock-number-face
    fg: "#f99157")

  (define-face! 'font-lock-operator-face
    fg: "#b8b8b8")

  (define-face! 'font-lock-type-face
    fg: "#ffcc66")

  (define-face! 'font-lock-preprocessor-face
    fg: "#f99157")

  (define-face! 'font-lock-heading-face
    fg: "#6699cc" bold: #t)

  ;; UI element faces
  (define-face! 'modeline
    fg: "#d8d8d8" bg: "#282828")

  (define-face! 'modeline-inactive
    fg: "#808080" bg: "#282828")

  (define-face! 'line-number
    fg: "#8c8c8c" bg: "#202020")

  (define-face! 'cursor-line
    bg: "#222228")

  ;; Search and matching
  (define-face! 'match
    fg: "#ffff00" bg: "#404060" bold: #t)

  (define-face! 'mismatch
    fg: "#ff4040" bg: "#602020" bold: #t)

  (define-face! 'isearch
    fg: "#000000" bg: "#ffcc00")

  (define-face! 'error
    fg: "#ff4040")

  ;; Org-mode faces — heading levels
  (define-face! 'org-heading-1
    fg: "#6699cc" bold: #t)

  (define-face! 'org-heading-2
    fg: "#f99157" bold: #t)

  (define-face! 'org-heading-3
    fg: "#99cc99" bold: #t)

  (define-face! 'org-heading-4
    fg: "#cc99cc" bold: #t)

  (define-face! 'org-heading-5
    fg: "#66cccc" bold: #t)

  (define-face! 'org-heading-6
    fg: "#ffcc66" bold: #t)

  (define-face! 'org-heading-7
    fg: "#f2777a" bold: #t)

  (define-face! 'org-heading-8
    fg: "#d27b53" bold: #t)

  ;; Org-mode faces — keywords and elements
  (define-face! 'org-todo
    fg: "#dc3232" bold: #t)

  (define-face! 'org-done
    fg: "#32b432" bold: #t)

  (define-face! 'org-link
    fg: "#5078dc" underline: #t)

  (define-face! 'org-code
    fg: "#3ca03c")

  (define-face! 'org-verbatim
    fg: "#32b4b4")

  (define-face! 'org-table
    fg: "#32b4b4")

  (define-face! 'org-comment
    fg: "#828282" italic: #t)

  (define-face! 'org-tag
    fg: "#9650b4")

  (define-face! 'org-date
    fg: "#b450b4")

  (define-face! 'org-property
    fg: "#646464")

  (define-face! 'org-block-delimiter
    fg: "#d28c32")

  (define-face! 'org-block-body
    fg: "#646464")

  ;; Tab bar faces
  (define-face! 'tab-active
    fg: "#ffffff" bg: "#404060")

  (define-face! 'tab-inactive
    fg: "#a0a0a0" bg: "#252525")

  ;; Window border faces
  (define-face! 'window-border-active
    fg: "#51afef")

  (define-face! 'window-border-inactive
    fg: "#3a3a3a"))

;; Initialize standard faces on module load
;; Note: Commented out automatic initialization to avoid module loading issues
;; Call (define-standard-faces!) explicitly from application startup
;; (define-standard-faces!)
