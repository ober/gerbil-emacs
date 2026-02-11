;;; -*- Gerbil -*-
;;; Keybinding system for gerbil-emacs
;;;
;;; Keymaps are hash tables mapping key-string -> symbol or sub-keymap.
;;; Multi-key sequences (C-x C-s) use nested keymaps.
;;; Key state machine tracks prefix accumulation.

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
        :gerbil-scintilla/tui)

;;;============================================================================
;;; Keymap data structure
;;;============================================================================

(def (make-keymap)
  (make-hash-table))

(def (keymap-bind! km key-str value)
  (hash-put! km key-str value))

(def (keymap-lookup km key-str)
  (hash-get km key-str))

;;;============================================================================
;;; Key event -> string conversion
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
;;; Key state machine for multi-key sequences
;;;============================================================================

(defstruct key-state
  (keymap        ; current keymap to look up in
   prefix-keys)  ; list of accumulated key strings (for echo display)
  transparent: #t)

;;; Global keymaps
(def *global-keymap* (make-keymap))
(def *ctrl-x-map*   (make-keymap))

(def (make-initial-key-state)
  (make-key-state *global-keymap* []))

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
      ((and (null? (key-state-prefix-keys state))
            (> (tui-event-ch ev) 31)
            (zero? (bitwise-and (tui-event-mod ev) TB_MOD_ALT)))
       (values 'self-insert (tui-event-ch ev) (make-initial-key-state)))
      ;; No binding -> undefined
      (else
       (values 'undefined key-str (make-initial-key-state))))))

;;;============================================================================
;;; Default Emacs-like keybindings
;;;============================================================================

(def (setup-default-bindings!)
  ;; C-x prefix
  (keymap-bind! *global-keymap* "C-x" *ctrl-x-map*)

  ;; Navigation
  (keymap-bind! *global-keymap* "C-f" 'forward-char)
  (keymap-bind! *global-keymap* "C-b" 'backward-char)
  (keymap-bind! *global-keymap* "C-n" 'next-line)
  (keymap-bind! *global-keymap* "C-p" 'previous-line)
  (keymap-bind! *global-keymap* "C-a" 'beginning-of-line)
  (keymap-bind! *global-keymap* "C-e" 'end-of-line)
  (keymap-bind! *global-keymap* "C-v" 'scroll-down)
  (keymap-bind! *global-keymap* "C-l" 'recenter)

  ;; Arrow keys and navigation
  (keymap-bind! *global-keymap* "<up>"     'previous-line)
  (keymap-bind! *global-keymap* "<down>"   'next-line)
  (keymap-bind! *global-keymap* "<left>"   'backward-char)
  (keymap-bind! *global-keymap* "<right>"  'forward-char)
  (keymap-bind! *global-keymap* "<home>"   'beginning-of-line)
  (keymap-bind! *global-keymap* "<end>"    'end-of-line)
  (keymap-bind! *global-keymap* "<prior>"  'scroll-up)
  (keymap-bind! *global-keymap* "<next>"   'scroll-down)
  (keymap-bind! *global-keymap* "<delete>" 'delete-char)

  ;; Alt/Meta navigation
  (keymap-bind! *global-keymap* "M-f" 'forward-word)
  (keymap-bind! *global-keymap* "M-b" 'backward-word)
  (keymap-bind! *global-keymap* "M-v" 'scroll-up)
  (keymap-bind! *global-keymap* "M-<" 'beginning-of-buffer)
  (keymap-bind! *global-keymap* "M->" 'end-of-buffer)

  ;; Editing
  (keymap-bind! *global-keymap* "C-d" 'delete-char)
  (keymap-bind! *global-keymap* "DEL" 'backward-delete-char)
  (keymap-bind! *global-keymap* "C-h" 'backward-delete-char)
  (keymap-bind! *global-keymap* "C-k" 'kill-line)
  (keymap-bind! *global-keymap* "C-y" 'yank)
  (keymap-bind! *global-keymap* "C-w" 'kill-region)
  (keymap-bind! *global-keymap* "M-w" 'copy-region)
  (keymap-bind! *global-keymap* "C-_" 'undo)
  (keymap-bind! *global-keymap* "C-m" 'newline)
  (keymap-bind! *global-keymap* "C-j" 'newline)
  (keymap-bind! *global-keymap* "C-o" 'open-line)

  ;; Mark
  (keymap-bind! *global-keymap* "C-@" 'set-mark)

  ;; Search
  (keymap-bind! *global-keymap* "C-s" 'search-forward)
  (keymap-bind! *global-keymap* "C-r" 'search-backward)

  ;; Misc
  (keymap-bind! *global-keymap* "C-g" 'keyboard-quit)

  ;; C-x commands
  (keymap-bind! *ctrl-x-map* "C-s" 'save-buffer)
  (keymap-bind! *ctrl-x-map* "C-f" 'find-file)
  (keymap-bind! *ctrl-x-map* "C-c" 'quit)
  (keymap-bind! *ctrl-x-map* "b"   'switch-buffer)
  (keymap-bind! *ctrl-x-map* "k"   'kill-buffer-cmd)
  (keymap-bind! *ctrl-x-map* "2"   'split-window)
  (keymap-bind! *ctrl-x-map* "o"   'other-window)
  (keymap-bind! *ctrl-x-map* "0"   'delete-window)
  (keymap-bind! *ctrl-x-map* "1"   'delete-other-windows))
