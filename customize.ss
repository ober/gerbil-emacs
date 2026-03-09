;;; -*- Gerbil -*-
;;; Customization system for gemacs
;;;
;;; Provides a centralized variable registry with metadata (type, docstring,
;;; group, default value) — similar to Emacs defcustom. Each registered
;;; variable has a setter callback so the underlying global is updated
;;; when the user changes a value through the customize interface.

(export
  ;; Registry
  defvar!
  custom-get
  custom-set!
  custom-reset!
  custom-describe
  custom-list-group
  custom-list-all
  custom-groups
  custom-registered?
  *custom-registry*

  ;; Hook documentation
  defhook!
  hook-doc
  hook-list-all)

(import :std/sugar
        :std/iter
        :std/sort)

;;;============================================================================
;;; Custom variable registry
;;;============================================================================

;; Each entry in the registry is a hash table with keys:
;;   name      - symbol
;;   default   - factory default value
;;   value     - current value
;;   docstring - human-readable description
;;   type      - one of: boolean, integer, string, symbol, choice, sexp
;;   type-args - for integer: (min . max), for choice: list of valid values
;;   group     - symbol (e.g. editing, display, files)
;;   setter    - procedure called with new value to update the backing global

(def *custom-registry* (make-hash-table-eq))

(def (validate-type type type-args value)
  "Validate VALUE against TYPE and TYPE-ARGS. Returns #t if valid."
  (case type
    ((boolean) (boolean? value))
    ((integer)
     (and (integer? value)
          (if (pair? type-args)
            (and (>= value (car type-args))
                 (<= value (cdr type-args)))
            #t)))
    ((string) (string? value))
    ((symbol) (symbol? value))
    ((choice)
     (and (list? type-args)
          (member value type-args)))
    ((sexp) #t)
    (else #t)))

(def (type->string type type-args)
  "Human-readable type description."
  (case type
    ((boolean) "boolean")
    ((integer)
     (if (pair? type-args)
       (string-append "integer (" (number->string (car type-args))
                      ".." (number->string (cdr type-args)) ")")
       "integer"))
    ((string) "string")
    ((symbol) "symbol")
    ((choice)
     (string-append "one of: "
       (string-join (map (lambda (v) (with-output-to-string (lambda () (write v))))
                         (or type-args []))
                    ", ")))
    ((sexp) "any value")
    (else "unknown")))

(def (defvar! name default docstring
              setter: (setter #f)
              type: (type 'sexp)
              type-args: (type-args #f)
              group: (group 'misc))
  "Register a customizable variable with metadata.
   NAME is a symbol, DEFAULT is the factory default, DOCSTRING describes it.
   SETTER is a procedure (lambda (new-value) ...) that updates the backing global.
   TYPE is one of: boolean, integer, string, symbol, choice, sexp.
   TYPE-ARGS: for integer → (min . max), for choice → list of valid values.
   GROUP is a symbol for UI grouping (e.g. 'editing, 'display, 'files)."
  (let ((entry (make-hash-table-eq)))
    (hash-put! entry 'name name)
    (hash-put! entry 'default default)
    (hash-put! entry 'value default)
    (hash-put! entry 'docstring docstring)
    (hash-put! entry 'type type)
    (hash-put! entry 'type-args type-args)
    (hash-put! entry 'group group)
    (hash-put! entry 'setter setter)
    (hash-put! *custom-registry* name entry)))

(def (custom-registered? name)
  "Check if a variable is registered."
  (and (hash-get *custom-registry* name) #t))

(def (custom-get name)
  "Get the current value of a registered variable."
  (let ((entry (hash-get *custom-registry* name)))
    (if entry
      (hash-get entry 'value)
      (error "Unknown customizable variable" name))))

(def (custom-set! name value)
  "Set a customizable variable, with type validation.
   Calls the setter to update the backing global."
  (let ((entry (hash-get *custom-registry* name)))
    (unless entry
      (error "Unknown customizable variable" name))
    (let ((type (hash-get entry 'type))
          (type-args (hash-get entry 'type-args)))
      (unless (validate-type type type-args value)
        (error "Type error for variable" name
               (string-append "expected " (type->string type type-args)
                              ", got: " (with-output-to-string (lambda () (write value))))))
      (hash-put! entry 'value value)
      (let ((setter (hash-get entry 'setter)))
        (when setter
          (setter value))))))

(def (custom-reset! name)
  "Reset a variable to its default value."
  (let ((entry (hash-get *custom-registry* name)))
    (unless entry
      (error "Unknown customizable variable" name))
    (let ((default (hash-get entry 'default)))
      (custom-set! name default))))

(def (custom-describe name)
  "Return a description string for a customizable variable."
  (let ((entry (hash-get *custom-registry* name)))
    (unless entry
      (error "Unknown customizable variable" name))
    (let ((docstring (hash-get entry 'docstring))
          (type (hash-get entry 'type))
          (type-args (hash-get entry 'type-args))
          (value (hash-get entry 'value))
          (default (hash-get entry 'default))
          (group (hash-get entry 'group)))
      (string-append
        (symbol->string name) "\n"
        "  " docstring "\n"
        "  Type:    " (type->string type type-args) "\n"
        "  Group:   " (symbol->string group) "\n"
        "  Value:   " (with-output-to-string (lambda () (write value))) "\n"
        "  Default: " (with-output-to-string (lambda () (write default)))))))

(def (custom-list-group group)
  "List all variable names in a group."
  (let ((result []))
    (hash-for-each
      (lambda (name entry)
        (when (eq? (hash-get entry 'group) group)
          (set! result (cons name result))))
      *custom-registry*)
    (sort result (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))

(def (custom-list-all)
  "List all registered variable names, sorted."
  (let ((result []))
    (hash-for-each
      (lambda (name entry)
        (set! result (cons name result)))
      *custom-registry*)
    (sort result (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))

(def (custom-groups)
  "List all groups that have registered variables."
  (let ((groups (make-hash-table-eq)))
    (hash-for-each
      (lambda (name entry)
        (hash-put! groups (hash-get entry 'group) #t))
      *custom-registry*)
    (sort (hash-keys groups)
          (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))

;;;============================================================================
;;; Hook documentation registry
;;;============================================================================

;; Separate from the variable registry — hooks are documented but not
;; "settable" like variables. This just provides metadata for describe-hook.

(def *hook-registry* (make-hash-table-eq))

(def (defhook! name docstring)
  "Register a hook with documentation.
   NAME is a symbol (e.g. 'before-save-hook)."
  (hash-put! *hook-registry* name docstring))

(def (hook-doc name)
  "Get the docstring for a registered hook."
  (or (hash-get *hook-registry* name)
      (string-append (symbol->string name) " (undocumented)")))

(def (hook-list-all)
  "List all registered hook names, sorted."
  (sort (hash-keys *hook-registry*)
        (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
