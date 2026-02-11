;;; -*- Gerbil -*-
;;; TUI keybinding adapter for gerbil-emacs
;;;
;;; Converts termbox key events to Emacs key strings.
;;; Keymap data structures and bindings are in core.ss.

(export
  key-event->string
  make-keymap
  keymap-bind!
  keymap-lookup
  (struct-out key-state)
  make-initial-key-state
  key-state-feed!
  *global-keymap*
  *ctrl-x-map*
  setup-default-bindings!)

(import :std/sugar
        :gerbil-scintilla/tui
        :gerbil-emacs/core)

;;;============================================================================
;;; Key event -> string conversion (TUI-specific)
;;;
;;; termbox_next delivers:
;;;   Regular char 'a':  key=0, ch=97, mod=0
;;;   Ctrl+A:            key=0x01, ch=0, mod=0
;;;   Alt+a:             key=0, ch=97, mod=TB_MOD_ALT
;;;   Arrow up:          key=0xFFE1, ch=0, mod=0
;;;   C-@/C-SPC:         key=0x00, ch=0, mod=0
;;;============================================================================

(def (key-event->string ev)
  (let ((key (tui-event-key ev))
        (ch  (tui-event-ch ev))
        (mod (tui-event-mod ev)))
    (let ((alt? (not (zero? (bitwise-and mod TB_MOD_ALT)))))
      (cond
        ;; Function keys (high values 0xFFFF-N)
        ((= key TB_KEY_F1)  "<f1>")
        ((= key TB_KEY_F2)  "<f2>")
        ((= key TB_KEY_F3)  "<f3>")
        ((= key TB_KEY_F4)  "<f4>")
        ((= key TB_KEY_F5)  "<f5>")
        ((= key TB_KEY_F6)  "<f6>")
        ((= key TB_KEY_F7)  "<f7>")
        ((= key TB_KEY_F8)  "<f8>")
        ((= key TB_KEY_F9)  "<f9>")
        ((= key TB_KEY_F10) "<f10>")
        ((= key TB_KEY_F11) "<f11>")
        ((= key TB_KEY_F12) "<f12>")
        ;; Navigation keys
        ((= key TB_KEY_ARROW_UP)    (if alt? "M-<up>" "<up>"))
        ((= key TB_KEY_ARROW_DOWN)  (if alt? "M-<down>" "<down>"))
        ((= key TB_KEY_ARROW_LEFT)  (if alt? "M-<left>" "<left>"))
        ((= key TB_KEY_ARROW_RIGHT) (if alt? "M-<right>" "<right>"))
        ((= key TB_KEY_HOME)   "<home>")
        ((= key TB_KEY_END)    "<end>")
        ((= key TB_KEY_PGUP)   "<prior>")
        ((= key TB_KEY_PGDN)   "<next>")
        ((= key TB_KEY_DELETE) "<delete>")
        ((= key TB_KEY_INSERT) "<insert>")
        ;; Mouse keys
        ((= key TB_KEY_MOUSE_LEFT)       "<mouse-1>")
        ((= key TB_KEY_MOUSE_RIGHT)      "<mouse-3>")
        ((= key TB_KEY_MOUSE_MIDDLE)     "<mouse-2>")
        ((= key TB_KEY_MOUSE_RELEASE)    "<mouse-release>")
        ((= key TB_KEY_MOUSE_WHEEL_UP)   "<mouse-4>")
        ((= key TB_KEY_MOUSE_WHEEL_DOWN) "<mouse-5>")
        ;; C-@ / C-SPC (key=0x00, ch=0)
        ((and (= key 0) (= ch 0) (not alt?))
         "C-@")
        ;; Tab (0x09) -> "TAB" (before generic C- mapping)
        ((= key #x09) (if alt? "M-TAB" "TAB"))
        ;; Ctrl keys 0x01-0x1A -> C-a through C-z
        ((and (>= key #x01) (<= key #x1A))
         (string-append "C-" (string (integer->char (+ key #x60)))))
        ;; ESC (0x1B)
        ((= key #x1B) "ESC")
        ;; C-\ (0x1C)
        ((= key #x1C) "C-\\")
        ;; C-] (0x1D)
        ((= key #x1D) "C-]")
        ;; C-^ (0x1E)
        ((= key #x1E) "C-^")
        ;; C-_ / C-/ (0x1F)
        ((= key #x1F) "C-_")
        ;; Space (0x20)
        ((= key #x20) (if alt? "M-SPC" "SPC"))
        ;; DEL / Backspace (0x7F)
        ((= key #x7F) "DEL")
        ;; Alt + printable character
        ((and alt? (> ch 31))
         (string-append "M-" (string (integer->char ch))))
        ;; Regular printable character
        ((> ch 31)
         (string (integer->char ch)))
        ;; Unknown
        (else
         (string-append "<key-" (number->string key) ">"))))))

;;;============================================================================
;;; TUI key state machine — feeds termbox events into keymap
;;;============================================================================

;;; Feed a key event into the state machine.
;;; Returns: (values action data new-state)
;;;   action: 'command | 'prefix | 'self-insert | 'undefined
(def (key-state-feed! state ev)
  (let* ((key-str (key-event->string ev))
         (binding (keymap-lookup (key-state-keymap state) key-str)))
    (cond
      ;; Sub-keymap -> enter prefix mode
      ((hash-table? binding)
       (values 'prefix #f
               (make-key-state binding
                               (append (key-state-prefix-keys state)
                                       (list key-str)))))
      ;; Command symbol -> execute
      ((symbol? binding)
       (values 'command binding (make-initial-key-state)))
      ;; No binding, top level, printable char -> self-insert
      ;; Space comes as key=0x20, ch=0 so check for it explicitly
      ((and (null? (key-state-prefix-keys state))
            (or (> (tui-event-ch ev) 31)
                (= (tui-event-key ev) #x20))
            (zero? (bitwise-and (tui-event-mod ev) TB_MOD_ALT)))
       (values 'self-insert
               (if (> (tui-event-ch ev) 31) (tui-event-ch ev) 32)
               (make-initial-key-state)))
      ;; No binding -> undefined
      (else
       (values 'undefined key-str (make-initial-key-state))))))
