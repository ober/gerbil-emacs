;;; -*- Gerbil -*-
;;; Qt key event adapter for gemacs
;;;
;;; Converts Qt key events to the same "C-x", "M-f", "<up>" string format
;;; used by the shared keymap in core.ss.

(export qt-key-event->string
        qt-key-state-feed!)

(import :std/sugar
        :gemacs/qt/sci-shim
        :gemacs/core)

;;;============================================================================
;;; Qt key event -> Emacs key string conversion
;;;============================================================================

(def (qt-key-event->string code mods text)
  "Convert Qt key code + modifiers to Emacs key string.
   Returns #f for bare modifier keys (Shift, Ctrl, Alt, etc.) that should be ignored."
  (if (and (>= code #x01000020) (<= code #x01000026))
    #f  ;; Bare modifier keys — ignore
    (let ((ctrl? (not (zero? (bitwise-and mods QT_MOD_CTRL))))
          (alt?  (not (zero? (bitwise-and mods QT_MOD_ALT))))
          (shift? (not (zero? (bitwise-and mods QT_MOD_SHIFT)))))
      (cond
        ;; Function keys
      ((= code QT_KEY_F1)  "<f1>")
      ((= code QT_KEY_F2)  "<f2>")
      ((= code QT_KEY_F3)  "<f3>")
      ((= code QT_KEY_F4)  "<f4>")
      ((= code QT_KEY_F5)  "<f5>")
      ((= code QT_KEY_F6)  "<f6>")
      ((= code QT_KEY_F7)  "<f7>")
      ((= code QT_KEY_F8)  "<f8>")
      ((= code QT_KEY_F9)  "<f9>")
      ((= code QT_KEY_F10) "<f10>")
      ((= code QT_KEY_F11) "<f11>")
      ((= code QT_KEY_F12) "<f12>")
      ;; Navigation keys
      ((= code QT_KEY_UP)        (if alt? "M-<up>" "<up>"))
      ((= code QT_KEY_DOWN)      (if alt? "M-<down>" "<down>"))
      ((= code QT_KEY_LEFT)      (if alt? "M-<left>" "<left>"))
      ((= code QT_KEY_RIGHT)     (if alt? "M-<right>" "<right>"))
      ((= code QT_KEY_HOME)      "<home>")
      ((= code QT_KEY_END)       "<end>")
      ((= code QT_KEY_PAGE_UP)   "<prior>")
      ((= code QT_KEY_PAGE_DOWN) "<next>")
      ((= code QT_KEY_DELETE)    "<delete>")
      ((= code QT_KEY_INSERT)    "<insert>")
      ;; ESC
      ((= code QT_KEY_ESCAPE)    "ESC")
      ;; Return/Enter
      ((or (= code QT_KEY_RETURN) (= code QT_KEY_ENTER))
       (if ctrl? "C-m" "C-m"))
      ;; Tab
      ((= code QT_KEY_TAB)
       (if ctrl? "C-i" "TAB"))
      ;; Backspace
      ((= code QT_KEY_BACKSPACE)
       (if alt? "M-DEL" "DEL"))
      ;; Space
      ((= code QT_KEY_SPACE)
       (cond
         ((and ctrl? alt?) "C-M-SPC")
         (ctrl? "C-@")    ;; C-SPC = C-@ in Emacs
         (alt? "M-SPC")
         (else "SPC")))
      ;; Ctrl+letter (A-Z)
      ((and ctrl? (>= code QT_KEY_A) (<= code QT_KEY_Z))
       (let ((ch (string (integer->char (+ (- code QT_KEY_A) 97)))))
         (if alt?
           (string-append "C-M-" ch)
           (string-append "C-" ch))))
      ;; Ctrl+special keys
      ((and ctrl? (= code QT_KEY_SPACE)) "C-@")
      ;; Alt + printable character from text
      ((and alt? (= (string-length text) 1))
       (string-append "M-" text))
      ;; Shift + < or > with Alt (M-< and M->)
      ;; These come as text "<" or ">" with alt modifier
      ;; Regular printable character from text
      ((and (= (string-length text) 1) (not ctrl?) (not alt?))
       text)
      ;; Unknown key
      (else
       (string-append "<key-" (number->string code) ">"))))))

;;;============================================================================
;;; Qt key state machine — feeds Qt key events into keymap
;;;============================================================================

(def (qt-key-state-feed! state code mods text)
  "Feed a Qt key event into the keymap state machine.
   Returns (values action data new-state) — same protocol as TUI version."
  (let ((key-str (qt-key-event->string code mods text)))
    ;; Bare modifier keys return #f — ignore them
    (if (not key-str)
      (values 'ignore #f state)
      (let ((binding (keymap-lookup (key-state-keymap state) key-str)))
    (cond
      ;; Sub-keymap -> enter prefix mode
      ((hash-table? binding)
       (values 'prefix key-str
               (make-key-state binding
                               (append (key-state-prefix-keys state)
                                       (list key-str)))))
      ;; Command symbol -> execute
      ((symbol? binding)
       (values 'command binding (make-initial-key-state)))
      ;; No binding, top level, printable text -> self-insert
      ((and (null? (key-state-prefix-keys state))
            (= (string-length text) 1)
            (> (char->integer (string-ref text 0)) 31)
            (not (not (zero? (bitwise-and mods QT_MOD_CTRL))))
            (not (not (zero? (bitwise-and mods QT_MOD_ALT)))))
       (values 'self-insert text (make-initial-key-state)))
      ;; No binding -> undefined
      (else
       (values 'undefined key-str (make-initial-key-state))))))))
