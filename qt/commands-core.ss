;;; -*- Gerbil -*-
;;; Qt commands core - helpers, navigation, editing, window management
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/text/json
        :std/net/uri
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/persist
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        (only-in :gemacs/editor-core paredit-delimiter? auto-pair-char))

;;; ========================================================================
;;; Winner mode — undo/redo window configuration changes
;;; ========================================================================

(def *winner-history* [])   ; list of (tree-snapshot . cur-buf-name)
(def *winner-future* [])    ; redo stack
(def *winner-max-history* 50)

(def (winner-snapshot-tree node)
  "Serialize split tree as an s-expr with buffer names for snapshot."
  (cond
    ((split-leaf? node)
     `(leaf ,(buffer-name (qt-edit-window-buffer (split-leaf-edit-window node)))))
    ((split-node? node)
     `(node ,(split-node-orientation node)
            ,@(map winner-snapshot-tree (split-node-children node))))))

(def (winner-snapshot-count node)
  "Count leaf nodes (windows) in a snapshot s-expr."
  (cond
    ((and (pair? node) (eq? (car node) 'leaf)) 1)
    ((and (pair? node) (eq? (car node) 'node))
     (apply + (map winner-snapshot-count (cddr node))))
    (else 0)))

(def (winner-snapshot-leaf-names snapshot)
  "Return ordered list of buffer names from snapshot leaves."
  (cond
    ((and (pair? snapshot) (eq? (car snapshot) 'leaf)) (list (cadr snapshot)))
    ((and (pair? snapshot) (eq? (car snapshot) 'node))
     (apply append (map winner-snapshot-leaf-names (cddr snapshot))))
    (else [])))

(def (winner-current-config fr)
  "Capture current window configuration as (tree-snapshot . cur-buf-name)."
  (let ((cur-buf (buffer-name (qt-edit-window-buffer (qt-current-window fr)))))
    (cons (winner-snapshot-tree (qt-frame-root fr))
          cur-buf)))

(def (winner-save! fr)
  "Save current window configuration to history."
  (let ((config (winner-current-config fr)))
    (set! *winner-history* (cons config *winner-history*))
    (when (> (length *winner-history*) *winner-max-history*)
      (set! *winner-history* (take *winner-history* *winner-max-history*)))
    (set! *winner-future* [])))

(def (winner-restore-config! app config)
  "Restore a saved window configuration.

   Strategy for UNDO (fewer windows): delete last windows. Since splits always
   append to the end of the flat list, deleting from the end naturally restores
   the correct tree structure — the delete logic handles tree cleanup itself.

   Strategy for REDO (more windows): split to create extra windows."
  (let* ((fr            (app-state-frame app))
         (snapshot      (car config))
         (cur-buf-name  (cdr config))
         (desired-count (winner-snapshot-count snapshot))
         (current-count (length (qt-frame-windows fr))))
    ;; Phase 1: Adjust window count
    (cond
      ;; Need to delete windows (undo path)
      ((> current-count desired-count)
       (let loop ((n (- current-count desired-count)))
         (when (> n 0)
           ;; Point current-idx at last window so qt-frame-delete-window! deletes it
           (set! (qt-frame-current-idx fr) (- (length (qt-frame-windows fr)) 1))
           (qt-frame-delete-window! fr)
           (loop (- n 1)))))
      ;; Need to create windows (redo path)
      ((< current-count desired-count)
       (let loop ((n (- desired-count current-count)))
         (when (> n 0)
           ;; Split to add a window (use vertical as default)
           (qt-frame-split! fr)
           (loop (- n 1))))))
    ;; Phase 2: Assign buffers from snapshot leaf list (in order)
    (let* ((leaf-names (winner-snapshot-leaf-names snapshot))
           (wins       (qt-frame-windows fr)))
      (let loop ((ws wins) (ns leaf-names))
        (when (and (pair? ws) (pair? ns))
          (let* ((w          (car ws))
                 (target-name (car ns))
                 (target-buf  (buffer-by-name target-name)))
            (when (and target-buf
                       (not (string=? (buffer-name (qt-edit-window-buffer w))
                                      target-name)))
              (qt-buffer-attach! (qt-edit-window-editor w) target-buf)
              (set! (qt-edit-window-buffer w) target-buf)))
          (loop (cdr ws) (cdr ns)))))
    ;; Phase 3: Restore active window (find by buffer name)
    (let* ((wins (qt-frame-windows fr))
           (idx  (let find-idx ((ws wins) (i 0))
                   (cond
                     ((null? ws) 0)
                     ((string=? (buffer-name (qt-edit-window-buffer (car ws)))
                                cur-buf-name)
                      i)
                     (else (find-idx (cdr ws) (+ i 1)))))))
      (when (< idx (length wins))
        (set! (qt-frame-current-idx fr) idx)))))

(def (cmd-winner-undo app)
  "Undo the last window configuration change."
  (if (null? *winner-history*)
    (echo-error! (app-state-echo app) "No further window configuration to undo")
    (let* ((fr (app-state-frame app))
           (current (winner-current-config fr))
           (prev (car *winner-history*)))
      (set! *winner-future* (cons current *winner-future*))
      (set! *winner-history* (cdr *winner-history*))
      (winner-restore-config! app prev)
      (echo-message! (app-state-echo app) "Window configuration restored"))))

(def (cmd-winner-redo app)
  "Redo a window configuration change."
  (if (null? *winner-future*)
    (echo-error! (app-state-echo app) "No further window configuration to redo")
    (let* ((fr (app-state-frame app))
           (current (winner-current-config fr))
           (next (car *winner-future*)))
      (set! *winner-history* (cons current *winner-history*))
      (set! *winner-future* (cdr *winner-future*))
      (winner-restore-config! app next)
      (echo-message! (app-state-echo app) "Window configuration redone"))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-qt-editor app)
  (qt-edit-window-editor (qt-current-window (app-state-frame app))))

(def (current-qt-buffer app)
  (qt-edit-window-buffer (qt-current-window (app-state-frame app))))

;; Qt application pointer for clipboard access (set by qt/app.ss at startup)
(def *qt-app-ptr* #f)

;; Tab bar visibility (used by qt/app.ss for the tab bar widget)
(def *tab-bar-visible* #t)

;; Push text to kill ring AND system clipboard
(def (qt-kill-ring-push! app text)
  "Push text onto the kill ring and sync to system clipboard."
  (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
  (when *qt-app-ptr*
    (qt-clipboard-set-text! *qt-app-ptr* text)))

;; Get text from system clipboard (fallback to kill ring top)
(def (qt-clipboard-or-kill-ring app)
  "Get clipboard text, or top of kill ring if clipboard is empty."
  (let ((clip (and *qt-app-ptr*
                   (let ((t (qt-clipboard-text *qt-app-ptr*)))
                     (and (string? t) (> (string-length t) 0) t)))))
    (or clip
        (let ((kr (app-state-kill-ring app)))
          (and (pair? kr) (car kr))))))

;;;============================================================================
;;; Theme system
;;;============================================================================

;; Current theme name (themes themselves live in :gemacs/themes)
(def *current-theme* 'dark)

(def (theme-color key)
  "Get a color value from the current theme (legacy UI chrome keys).
   Reads from the theme's face-alist for backward compatibility."
  (let ((theme (theme-get *current-theme*)))
    (and theme (let ((pair (assoc key theme)))
                 (and pair (cdr pair))))))

(def (load-theme! theme-name)
  "Load a theme by applying its face definitions to the global *faces* registry."
  (let ((theme (theme-get theme-name)))
    (unless theme
      (error "Unknown theme" theme-name))
    ;; Clear existing faces
    (face-clear!)
    ;; Apply each face from the theme
    (for-each
      (lambda (entry)
        (let ((face-name (car entry))
              (props (cdr entry)))
          ;; Only process entries that look like face definitions (have keyword args)
          ;; Skip legacy UI chrome keys like 'bg, 'fg, 'selection
          (when (and (pair? props)
                     (keyword? (car props)))
            (apply define-face! face-name props))))
      theme)
    ;; Update current theme
    (set! *current-theme* theme-name)))

;;; ============================================================================
;;; Init File Convenience API
;;; ============================================================================

(def (load-theme theme-name)
  "Load a theme and apply its face definitions (convenience wrapper for init files).
   Example: (load-theme 'dracula)"
  (load-theme! theme-name))

(def (define-theme! theme-name face-alist)
  "Define a custom theme (convenience wrapper for init files).
   Example: (define-theme! 'my-theme
              '((default . (fg: \"#e0e0e0\" bg: \"#1a1a2e\"))
                (font-lock-keyword-face . (fg: \"#e94560\" bold: #t))))"
  (register-theme! theme-name face-alist))

(def (theme-stylesheet)
  "Generate a Qt stylesheet from the current theme."
  (let ((bg (or (theme-color 'bg) "#181818"))
        (fg (or (theme-color 'fg) "#d8d8d8"))
        (sel (or (theme-color 'selection) "#404060"))
        (ml-bg (or (theme-color 'modeline-bg) "#282828"))
        (ml-fg (or (theme-color 'modeline-fg) "#d8d8d8"))
        (echo-bg (or (theme-color 'echo-bg) "#282828"))
        (echo-fg (or (theme-color 'echo-fg) "#d8d8d8"))
        (split (or (theme-color 'split) "#383838"))
        (font-css (string-append " font-family: " *default-font-family*
                                 "; font-size: " (number->string *default-font-size*) "pt;")))
    (string-append
      "QPlainTextEdit { background-color: " bg "; color: " fg ";"
      font-css
      " selection-background-color: " sel "; }"
      " QLabel { color: " echo-fg "; background: " echo-bg ";"
      font-css " }"
      " QMainWindow { background: " bg "; }"
      " QStatusBar { color: " ml-fg "; background: " ml-bg ";"
      font-css " }"
      " QLineEdit { background: " bg "; color: " fg "; border: none;"
      font-css " }"
      " QSplitter::handle { background: " split "; }")))

(def (apply-theme! app theme-name: (theme-name #f))
  "Apply a theme to the Qt application. If theme-name provided, load that theme first."
  (when theme-name
    (load-theme! theme-name))
  (when *qt-app-ptr*
    (qt-app-set-style-sheet! *qt-app-ptr* (theme-stylesheet))
    (let ((fr (app-state-frame app)))
      ;; Apply Scintilla base colors (bg, fg, caret, selection, line numbers)
      ;; to ALL visible editors via the face system
      (for-each
        (lambda (win)
          (let ((ed (qt-edit-window-editor win)))
            (qt-apply-editor-theme! ed)))
        (qt-frame-windows fr))
      ;; Update line number area widget colors (separate from Scintilla margin)
      (let ((g-bg (theme-color 'gutter-bg))
            (g-fg (theme-color 'gutter-fg)))
        (when (and g-bg g-fg)
          (let ((parse-color (lambda (hex)
                  (let ((r (string->number (substring hex 1 3) 16))
                        (g (string->number (substring hex 3 5) 16))
                        (b (string->number (substring hex 5 7) 16)))
                    (values r g b)))))
            (for-each
              (lambda (win)
                (let ((lna (qt-edit-window-line-number-area win)))
                  (when lna
                    (let-values (((r g b) (parse-color g-bg)))
                      (qt-line-number-area-set-bg-color! lna r g b))
                    (let-values (((r g b) (parse-color g-fg)))
                      (qt-line-number-area-set-fg-color! lna r g b)))))
              (qt-frame-windows fr))))))
    ;; Re-apply syntax highlighting to all open buffers
    (for-each
      (lambda (buf)
        (qt-setup-highlighting! app buf))
      (buffer-list))))

;; Auto-save path: #filename# (Emacs convention)
(def (make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

;; Buffer recency tracking (MRU order for buffer switching)
(def *buffer-recent* [])  ; list of buffer names, most recent first

(def (buffer-touch! buf)
  "Record buffer as most recently used."
  (let ((name (buffer-name buf)))
    (set! *buffer-recent*
      (cons name (filter (lambda (n) (not (string=? n name))) *buffer-recent*)))))

(def (buffer-names-mru)
  "Return buffer names sorted by most recently used, excluding current."
  (let* ((all-names (map buffer-name (buffer-list)))
         ;; Start with MRU order, then append any not yet tracked
         (mru (filter (lambda (n) (member n all-names)) *buffer-recent*))
         (rest (filter (lambda (n) (not (member n mru))) all-names)))
    (append mru rest)))

;; File modification tracking for auto-revert
(def *auto-revert-mode* #t)   ; enabled by default
(def *file-mtimes* (make-hash-table)) ; file-path -> mtime (seconds)
(def *auto-revert-tail-buffers* (make-hash-table)) ; buffer-name -> #t for tail-follow mode

(def (file-mtime path)
  "Get file modification time as seconds, or #f if file doesn't exist."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (time->seconds (file-info-last-modification-time (file-info path))))))

(def (file-mtime-record! path)
  "Record current modification time for a file."
  (when path
    (let ((mt (file-mtime path)))
      (when mt
        (hash-put! *file-mtimes* path mt)))))

(def (file-mtime-changed? path)
  "Check if file has been modified externally since we last recorded it.
Returns #t if changed, #f if not or if no record exists."
  (and path
       (let ((recorded (hash-get *file-mtimes* path))
             (current (file-mtime path)))
         (and recorded current
              (> current recorded)))))

;;;============================================================================
;;; Directory-local variables (.gemacs-config)
;;;============================================================================

(def *dir-locals-cache* (make-hash-table))  ; dir -> (mtime . alist)

(def (find-dir-locals-file dir)
  "Search DIR and parent directories for .gemacs-config file."
  (let loop ((d dir))
    (let ((config-path (path-expand ".gemacs-config" d)))
      (cond
        ((file-exists? config-path) config-path)
        ((string=? d "/") #f)
        (else (loop (path-directory (string-append d "/"))))))))

(def (read-dir-locals file)
  "Read directory-local settings from FILE. Returns alist or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-input-file file
        (lambda (port) (read port))))))


;;;============================================================================
;;; Navigation commands
;;;============================================================================

(def (update-mark-region! app ed)
  "If mark is active, extend visual selection between mark and current point.
   This implements Emacs transient-mark-mode behavior for Qt."
  (let* ((buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (when mark
      ;; SCI_SETSEL anchor caret: anchor=mark (fixed), caret=point (moves)
      (qt-plain-text-edit-set-selection! ed mark (qt-plain-text-edit-cursor-position ed)))))

(def (collapse-selection-to-caret! ed)
  "Collapse any existing selection to the caret position before movement.
   Required so that move-cursor! advances the caret rather than collapsing
   an existing selection to its endpoint."
  (let ((pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-selection! ed pos pos)))

(def (cmd-forward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (collapse-selection-to-caret! ed)
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_NEXT_CHAR QT_CURSOR_PREVIOUS_CHAR))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

(def (cmd-backward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (collapse-selection-to-caret! ed)
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_PREVIOUS_CHAR QT_CURSOR_NEXT_CHAR))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

(def (cmd-next-line app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (collapse-selection-to-caret! ed)
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_DOWN QT_CURSOR_UP))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

(def (cmd-previous-line app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (collapse-selection-to-caret! ed)
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_UP QT_CURSOR_DOWN))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

(def (cmd-beginning-of-line app)
  "Smart beginning of line: toggle between first non-whitespace and column 0."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (qt-plain-text-edit-cursor-column ed))
         (line (qt-plain-text-edit-cursor-line ed))
         ;; Find start of current line
         (line-start (let loop ((p pos))
                       (if (or (<= p 0)
                               (and (> p 0)
                                    (char=? (string-ref text (- p 1)) #\newline)))
                         p
                         (loop (- p 1)))))
         ;; Find first non-whitespace on line
         (indent-pos (let loop ((p line-start))
                       (if (or (>= p (string-length text))
                               (char=? (string-ref text p) #\newline))
                         line-start  ; all whitespace
                         (if (or (char=? (string-ref text p) #\space)
                                 (char=? (string-ref text p) #\tab))
                           (loop (+ p 1))
                           p)))))
    ;; Toggle: if at indentation, go to column 0; otherwise go to indentation
    (if (= pos indent-pos)
      (qt-plain-text-edit-set-cursor-position! ed line-start)
      (qt-plain-text-edit-set-cursor-position! ed indent-pos))
    (update-mark-region! app ed)))

(def (cmd-end-of-line app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END_OF_BLOCK)
    (update-mark-region! app ed)))

(def (cmd-forward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_NEXT_WORD QT_CURSOR_PREVIOUS_WORD))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

(def (cmd-backward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_PREVIOUS_WORD QT_CURSOR_NEXT_WORD))
        (loop (+ i 1))))
    (update-mark-region! app ed)))

;;; Subword movement (camelCase / snake_case boundaries)
(def (subword-boundary? text i direction)
  "Check if position i is a subword boundary in the given direction (1=forward, -1=backward)."
  (let ((len (string-length text)))
    (and (> i 0) (< i len)
         (let ((prev (string-ref text (- i 1)))
               (cur (string-ref text i)))
           (or ;; underscore/hyphen boundary
               (and (= direction 1) (or (char=? cur #\_) (char=? cur #\-)))
               (and (= direction -1) (or (char=? prev #\_) (char=? prev #\-)))
               ;; lowercase -> uppercase (camelCase boundary)
               (and (char-lower-case? prev) (char-upper-case? cur))
               ;; letter -> non-letter or non-letter -> letter
               (and (char-alphabetic? prev) (not (or (char-alphabetic? cur) (char-numeric? cur))))
               (and (not (or (char-alphabetic? prev) (char-numeric? prev))) (char-alphabetic? cur)))))))

(def (cmd-forward-subword app)
  "Move forward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (cond
        ((>= i len) (qt-plain-text-edit-set-cursor-position! ed len))
        ((subword-boundary? text i 1) (qt-plain-text-edit-set-cursor-position! ed i))
        (else (loop (+ i 1)))))))

(def (cmd-backward-subword app)
  "Move backward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((<= i 0) (qt-plain-text-edit-set-cursor-position! ed 0))
        ((subword-boundary? text i -1) (qt-plain-text-edit-set-cursor-position! ed i))
        (else (loop (- i 1)))))))

(def (cmd-kill-subword app)
  "Kill forward to the next subword boundary."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (let ((end (cond
                   ((>= i len) len)
                   ((subword-boundary? text i 1) i)
                   (else #f))))
        (if end
          (let ((killed (substring text pos end))
                (new-text (string-append
                            (substring text 0 pos)
                            (substring text end len))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-kill-ring-push! app killed))
          (loop (+ i 1)))))))

(def (cmd-beginning-of-buffer app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START)
    (update-mark-region! app ed)))

(def (cmd-end-of-buffer app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
    (update-mark-region! app ed)))

(def (cmd-scroll-down app)
  ;; Move down 20 lines to simulate page down
  (let ((ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i 20)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (loop (+ i 1))))
    (update-mark-region! app ed)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-scroll-up app)
  ;; Move up 20 lines to simulate page up
  (let ((ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i 20)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_UP)
        (loop (+ i 1))))
    (update-mark-region! app ed)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-recenter app)
  (qt-plain-text-edit-center-cursor! (current-qt-editor app)))

;;;============================================================================
;;; Editing commands
;;;============================================================================

(def (qt-paredit-strict-allow-delete? ed pos direction)
  "Check if deleting char at pos is allowed in strict mode.
   direction: 'forward or 'backward."
  (let* ((text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (or (< pos 0) (>= pos len))
      #t
      (let ((ch (char->integer (string-ref text pos))))
        (if (not (paredit-delimiter? ch))
          #t
          ;; Delimiter — only allow if empty pair
          (cond
            ((or (= ch 40) (= ch 91) (= ch 123))
             (and (< (+ pos 1) len)
                  (eqv? (char->integer (string-ref text (+ pos 1)))
                         (auto-pair-char ch))))
            ((= ch 41)
             (and (> pos 0)
                  (= (char->integer (string-ref text (- pos 1))) 40)))
            ((= ch 93)
             (and (> pos 0)
                  (= (char->integer (string-ref text (- pos 1))) 91)))
            ((= ch 125)
             (and (> pos 0)
                  (= (char->integer (string-ref text (- pos 1))) 123)))
            (else #t)))))))

(def (cmd-delete-char app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if (and *paredit-strict-mode*
             (not (qt-paredit-strict-allow-delete? ed pos 'forward)))
      (echo-message! (app-state-echo app) "Paredit: cannot delete delimiter")
      (begin
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_NEXT_CHAR
                                         mode: QT_KEEP_ANCHOR)
        (qt-plain-text-edit-remove-selected-text! ed)))))

(def (cmd-backward-delete-char app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ;; Terminal buffers: send DEL to PTY (PTY handles echo)
      ((terminal-buffer? buf)
       (let ((ts (hash-get *terminal-state* buf)))
         (when ts (terminal-send-raw! ts "\x7f;"))))
      ;; In REPL buffers, don't delete past the prompt.
      ((repl-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (> pos (repl-state-prompt-pos rs)))
           (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                            mode: QT_KEEP_ANCHOR)
           (qt-plain-text-edit-remove-selected-text! ed))))
      ;; Shell: don't delete past the prompt
      ((shell-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (ss (hash-get *shell-state* buf)))
         (when (and ss (> pos (shell-state-prompt-pos ss)))
           (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                            mode: QT_KEEP_ANCHOR)
           (qt-plain-text-edit-remove-selected-text! ed))))
      (else
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed)))
         (if (and *paredit-strict-mode* (> pos 0)
                  (not (qt-paredit-strict-allow-delete? ed (- pos 1) 'backward)))
           (echo-message! (app-state-echo app) "Paredit: cannot delete delimiter")
           (begin
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                              mode: QT_KEEP_ANCHOR)
             (qt-plain-text-edit-remove-selected-text! ed))))))))

(def (cmd-backward-delete-char-untabify app)
  "Delete backward, converting tabs to spaces if in leading whitespace."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (> pos 0)
      (let* ((text (qt-plain-text-edit-text ed))
             (line (qt-plain-text-edit-cursor-line ed))
             ;; Find line start
             (line-start (let loop ((i 0) (ln 0))
                           (if (>= ln line) i
                             (let ((nl (string-index text #\newline i)))
                               (if nl (loop (+ nl 1) (+ ln 1)) i)))))
             (ch-before (if (> pos 0)
                          (string-ref text (- pos 1))
                          #\nul)))
        ;; If char before is tab and we're in leading whitespace
        (if (and (char=? ch-before #\tab)
                 (let loop ((p line-start))
                   (or (>= p pos)
                       (let ((c (string-ref text p)))
                         (and (or (char=? c #\space) (char=? c #\tab))
                              (loop (+ p 1)))))))
          ;; Delete the tab
          (begin
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                              mode: QT_KEEP_ANCHOR)
            (qt-plain-text-edit-remove-selected-text! ed))
          ;; Normal backspace
          (begin
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                              mode: QT_KEEP_ANCHOR)
            (qt-plain-text-edit-remove-selected-text! ed)))))))

(def (cmd-buffer-list-select app)
  "Switch to the buffer named on the current line in *Buffer List*."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      "")))
    ;; Line format: "  CM NNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMPATH"
    ;; Name field is 24 chars starting at column 5
    (let* ((name (if (>= (string-length line-text) 29)
                   (string-trim-both (substring line-text 5 29))
                   "")))
      (if (and (> (string-length name) 0)
               (not (string=? name "Buffer"))
               (not (string=? name "------")))
        (let ((buf (buffer-by-name name)))
          (if buf
            (let ((fr (app-state-frame app)))
              ;; Clear read-only before switching — QScintilla read-only
              ;; is document-level but clear explicitly to be safe
              (qt-plain-text-edit-set-read-only! ed #f)
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              ;; Restore default caret line background
              (sci-send ed SCI_SETCARETLINEBACK (rgb->sci #x22 #x22 #x28))
              (echo-message! (app-state-echo app) (buffer-name buf)))
            (echo-error! (app-state-echo app) (string-append "No buffer: " name))))
        (echo-message! (app-state-echo app) "No buffer on this line")))))

(def (current-line-indent ed)
  "Get leading whitespace of the current line."
  (let* ((text (qt-plain-text-edit-text ed))
         (text-len (string-length text))
         (pos (min (qt-plain-text-edit-cursor-position ed) text-len))
         ;; Find start of current line
         (line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (and (< i text-len)
                                             (char=? (string-ref text i) #\newline)))
                         (+ i 1) (loop (- i 1))))))
    ;; Extract leading whitespace
    (let loop ((i line-start) (acc []))
      (if (and (< i text-len)
               (let ((ch (string-ref text i)))
                 (or (char=? ch #\space) (char=? ch #\tab))))
        (loop (+ i 1) (cons (string-ref text i) acc))
        (list->string (reverse acc))))))


(def (cmd-open-line app)
  (let ((ed (current-qt-editor app)))
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (qt-plain-text-edit-insert-text! ed "\n")
      (qt-plain-text-edit-set-cursor-position! ed pos))))

(def (cmd-undo app)
  (let ((ed (current-qt-editor app)))
    (if (qt-plain-text-edit-can-undo? ed)
      (qt-plain-text-edit-undo! ed)
      (echo-message! (app-state-echo app) "No further undo information"))))

(def (cmd-redo app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-redo! ed)))

;;;============================================================================
;;; Kill / Yank
;;;============================================================================

(def (cmd-kill-line app)
  "Kill from point to end of line, or kill newline if at end."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-line-from-position ed pos))
         (line-end (qt-plain-text-edit-line-end-position ed line)))
    (if (= pos line-end)
      ;; At end of line: kill the newline
      (let ((killed (qt-plain-text-edit-text-range ed pos (+ pos 1))))
        (qt-plain-text-edit-set-selection! ed pos (+ pos 1))
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed)))
      ;; Kill to end of line
      (let ((killed (qt-plain-text-edit-text-range ed pos line-end)))
        (qt-plain-text-edit-set-selection! ed pos line-end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed))))))

(def (cmd-yank app)
  (let* ((ed (current-qt-editor app))
         (pos-before (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-paste! ed)
    (let ((pos-after (qt-plain-text-edit-cursor-position ed)))
      (set! (app-state-last-yank-pos app) pos-before)
      (set! (app-state-last-yank-len app) (- pos-after pos-before))
      (set! (app-state-kill-ring-idx app) 0))))

;;;============================================================================
;;; Mark and region
;;;============================================================================

(def (cmd-set-mark app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app)))
    ;; Push previous mark to mark ring
    (when (buffer-mark buf)
      (set! (app-state-mark-ring app)
        (cons (cons (buffer-name buf) (buffer-mark buf))
              (app-state-mark-ring app))))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-kill-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (killed (qt-plain-text-edit-text-range ed start end)))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed))
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-copy-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text-range ed start end)))
        ;; Push to kill ring + clipboard
        (when (and (string? text) (> (string-length text) 0))
          (qt-kill-ring-push! app text))
        ;; Deselect
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region copied"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (path-char-delimiter? ch)
  "Check if character is a path delimiter (space, tab, newline, quotes, parens)."
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch (integer->char 34))  ; double quote
      (char=? ch (integer->char 39))  ; single quote
      (char=? ch #\()
      (char=? ch #\))))

(def (file-path-at-point ed)
  "Extract a file-path-like string at the cursor position.
Returns (path . line) or #f. Handles file:line format."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (and (< pos len)
         ;; Expand backward to find start of path
         (let* ((start (let scan ((i pos))
                         (if (and (> i 0)
                                  (not (path-char-delimiter? (string-ref text (- i 1)))))
                           (scan (- i 1)) i)))
                ;; Expand forward to find end of path
                (end (let scan ((i pos))
                       (if (and (< i len)
                                (not (path-char-delimiter? (string-ref text i))))
                         (scan (+ i 1)) i)))
                (raw (substring text start end)))
           (and (> (string-length raw) 0)
                ;; Check for file:line format
                (let ((colon-pos (let scan ((i (- (string-length raw) 1)))
                                   (cond
                                     ((< i 0) #f)
                                     ((char=? (string-ref raw i) #\:) i)
                                     ((char-numeric? (string-ref raw i)) (scan (- i 1)))
                                     (else #f)))))
                  (if colon-pos
                    (let* ((path (substring raw 0 colon-pos))
                           (num-str (substring raw (+ colon-pos 1) (string-length raw)))
                           (line-num (string->number num-str)))
                      (if (and line-num (> (string-length path) 0))
                        (cons path line-num)
                        (cons raw #f)))
                    (cons raw #f))))))))


;;;============================================================================
;;; Buffer commands
;;;============================================================================

(def (cmd-switch-buffer app)
  (let* ((echo (app-state-echo app))
         (names (buffer-names-mru))
         (name (qt-echo-read-with-narrowing app "Switch to buffer:" names)))
    (when name
      (let ((buf (buffer-by-name name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (buffer-touch! buf)
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf))
          ;; Create new buffer if name doesn't match existing
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (new-buf (qt-buffer-create! name ed #f)))
            (buffer-touch! new-buf)
            (qt-buffer-attach! ed new-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
            (echo-message! echo (string-append "New buffer: " name))))))))


;;;============================================================================
;;; Window commands
;;;============================================================================

(def (cmd-split-window app)
  (winner-save! (app-state-frame app))
  (let ((new-ed (qt-frame-split! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

(def (cmd-split-window-right app)
  (winner-save! (app-state-frame app))
  (let ((new-ed (qt-frame-split-right! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

(def (cmd-other-window app)
  (qt-frame-other-window! (app-state-frame app)))

(def (cmd-delete-window app)
  (let ((fr (app-state-frame app)))
    (if (> (length (qt-frame-windows fr)) 1)
      (begin
        (winner-save! fr)
        (qt-frame-delete-window! fr))
      (echo-error! (app-state-echo app) "Can't delete sole window"))))

(def (cmd-delete-other-windows app)
  (winner-save! (app-state-frame app))
  (qt-frame-delete-other-windows! (app-state-frame app)))

;;; ace-window — quick window switching by number
(def (cmd-ace-window app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (if (<= n 1)
      (echo-message! (app-state-echo app) "Only one window")
      (if (= n 2)
        ;; With only 2 windows, just switch to the other one
        (qt-frame-other-window! fr)
        ;; Show numbered window list and prompt
        (let* ((labels
                (let loop ((ws wins) (i 0) (acc []))
                  (if (null? ws) (reverse acc)
                    (let* ((w (car ws))
                           (bname (buffer-name (qt-edit-window-buffer w)))
                           (marker (if (= i (qt-frame-current-idx fr)) "*" " "))
                           (label (string-append (number->string (+ i 1)) marker ": " bname)))
                      (loop (cdr ws) (+ i 1) (cons label acc))))))
               (prompt-str (string-append "Window [" (string-join labels " | ") "]: "))
               (input (qt-echo-read-string app prompt-str))
               (num (string->number (string-trim input))))
          (cond
            ((not num)
             (echo-error! (app-state-echo app) "Not a number"))
            ((or (< num 1) (> num n))
             (echo-error! (app-state-echo app)
                          (string-append "Window " (number->string num) " does not exist")))
            (else
             (set! (qt-frame-current-idx fr) (- num 1))
             (echo-message! (app-state-echo app)
                            (string-append "Switched to window "
                                           (number->string num))))))))))

;;; Swap window contents
(def (cmd-swap-window app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (if (<= n 1)
      (echo-error! (app-state-echo app) "Only one window")
      (let* ((cur-idx (qt-frame-current-idx fr))
             (next-idx (modulo (+ cur-idx 1) n))
             (cur-win (list-ref wins cur-idx))
             (next-win (list-ref wins next-idx))
             (cur-buf (qt-edit-window-buffer cur-win))
             (next-buf (qt-edit-window-buffer next-win)))
        ;; Swap buffers between the two windows
        (set! (qt-edit-window-buffer cur-win) next-buf)
        (set! (qt-edit-window-buffer next-win) cur-buf)
        (qt-buffer-attach! (qt-edit-window-editor cur-win) next-buf)
        (qt-buffer-attach! (qt-edit-window-editor next-win) cur-buf)
        (echo-message! (app-state-echo app) "Windows swapped")))))

;;;============================================================================
;;; Write file (save as)
;;;============================================================================

(def (cmd-write-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Write file: ")))
    (when (and filename (> (string-length filename) 0))
      (let* ((filename (expand-filename filename))
             (buf (current-qt-buffer app))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (set! (buffer-file-path buf) filename)
        (set! (buffer-name buf) (path-strip-directory filename))
        (write-string-to-file filename text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (echo-message! echo (string-append "Wrote " filename))))))

;;;============================================================================
;;; Revert buffer
;;;============================================================================

(def (cmd-revert-buffer app)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-qt-editor app))
             (text (read-file-as-string path)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (file-mtime-record! path)
          (echo-message! echo (string-append "Reverted " path))))
      (echo-error! echo "Buffer is not visiting a file"))))

;;;============================================================================
;;; Select all
;;;============================================================================

(def (cmd-select-all app)
  (qt-plain-text-edit-select-all! (current-qt-editor app))
  (echo-message! (app-state-echo app) "Mark set (whole buffer)"))

;;;============================================================================
;;; Goto line
;;;============================================================================

(def (cmd-goto-line app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto line: ")))
    (when (and input (> (string-length input) 0))
      (let ((line-num (string->number input)))
        (if (and line-num (> line-num 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 ;; Find position of the Nth newline
                 (target-line (- line-num 1))
                 (pos (let loop ((i 0) (line 0))
                        (cond
                          ((= line target-line) i)
                          ((>= i (string-length text)) i)
                          ((char=? (string-ref text i) #\newline)
                           (loop (+ i 1) (+ line 1)))
                          (else (loop (+ i 1) line))))))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! echo (string-append "Line " input)))
          (echo-error! echo "Invalid line number"))))))

;;;============================================================================
;;; M-x (execute extended command)
;;;============================================================================

(def *mx-command-history* [])
(def *mx-history-max* 50)

(def (mx-history-add! name)
  "Add a command name to M-x history (most recent first, no duplicates)."
  (set! *mx-command-history*
    (cons name
      (let loop ((h *mx-command-history*) (acc []))
        (cond
          ((null? h) (reverse acc))
          ((string=? (car h) name) (loop (cdr h) acc))
          (else (loop (cdr h) (cons (car h) acc)))))))
  (when (> (length *mx-command-history*) *mx-history-max*)
    (set! *mx-command-history*
      (let loop ((h *mx-command-history*) (n 0) (acc []))
        (if (or (null? h) (>= n *mx-history-max*))
          (reverse acc)
          (loop (cdr h) (+ n 1) (cons (car h) acc)))))))

(def (cmd-execute-extended-command app)
  (let* ((all-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         ;; Put recent commands first, then rest alphabetically
         (recent-set (let loop ((h *mx-command-history*) (s (make-hash-table)))
                       (if (null? h) s
                         (begin (hash-put! s (car h) #t)
                                (loop (cdr h) s)))))
         (non-recent (filter (lambda (n) (not (hash-get recent-set n))) all-names))
         (ordered (append *mx-command-history* non-recent))
         (input (qt-echo-read-with-narrowing app "M-x" ordered)))
    (when (and input (> (string-length input) 0))
      (mx-history-add! input)
      (execute-command! app (string->symbol input)))))

(def (cmd-helm-buffers-list app)
  "Fuzzy buffer switcher — like helm-buffers-list."
  (let* ((names (buffer-names-mru))
         (name (qt-echo-read-with-narrowing app "Buffer:" names)))
    (when (and name (> (string-length name) 0))
      (let ((buf (buffer-by-name name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (buffer-touch! buf)
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-modeline-update! app))
          ;; Create new buffer if not found
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (new-buf (qt-buffer-create! name ed #f)))
            (buffer-touch! new-buf)
            (qt-buffer-attach! ed new-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
            (echo-message! (app-state-echo app)
              (string-append "New buffer: " name))))))))

;;;============================================================================
;;; Help commands
;;;============================================================================

(def (cmd-list-bindings app)
  "Display all keybindings in a *Help* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (lines '()))
    (for-each
      (lambda (entry)
        (let ((key (car entry))
              (val (cdr entry)))
          (cond
            ((symbol? val)
             (set! lines (cons (string-append "  " key "\t" (symbol->string val))
                               lines)))
            ((hash-table? val)
             (for-each
               (lambda (sub-entry)
                 (let ((sub-key (car sub-entry))
                       (sub-val (cdr sub-entry)))
                   (when (symbol? sub-val)
                     (set! lines
                       (cons (string-append "  " key " " sub-key "\t"
                                            (symbol->string sub-val))
                             lines)))))
               (keymap-entries val))))))
      (keymap-entries *global-keymap*))
    (let* ((sorted (sort lines string<?))
           (text (string-append "Key Bindings:\n\n"
                                (string-join sorted "\n")
                                "\n")))
      (let ((buf (or (buffer-by-name "*Help*")
                     (qt-buffer-create! "*Help*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "*Help*")))))

;;;============================================================================
;;; Buffer list
;;;============================================================================

(def (cmd-list-buffers app)
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (cur-buf (current-qt-buffer app))
         (bufs (buffer-list))
         (header "  MR  Buffer                    Mode         File\n  --  ------                    ----         ----\n")
         (lines (map (lambda (buf)
                       (let* ((name (buffer-name buf))
                              (path (or (buffer-file-path buf) ""))
                              (mod (if (qt-text-document-modified?
                                        (buffer-doc-pointer buf)) "*" " "))
                              (cur (if (eq? buf cur-buf) "." " "))
                              (lang (or (buffer-lexer-lang buf) 'fundamental))
                              (mode-str (let ((s (if (symbol? lang)
                                                   (symbol->string lang)
                                                   (if (string? lang) lang "fundamental"))))
                                          (if (> (string-length s) 12)
                                            (substring s 0 12)
                                            s)))
                              ;; Pad name to 24 chars
                              (padded-name (if (>= (string-length name) 24)
                                             (substring name 0 24)
                                             (string-append name
                                               (make-string (- 24 (string-length name)) #\space))))
                              ;; Pad mode to 13 chars
                              (padded-mode (if (>= (string-length mode-str) 13) mode-str
                                             (string-append mode-str
                                               (make-string (- 13 (string-length mode-str)) #\space)))))
                         (string-append "  " cur mod " " padded-name padded-mode path)))
                     bufs))
         (text (string-append header (string-join lines "\n") "\n")))
    (let ((buf (or (buffer-by-name "*Buffer List*")
                   (qt-buffer-create! "*Buffer List*" ed #f))))
      (set! (buffer-lexer-lang buf) 'buffer-list)
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      ;; Clear read-only before setting text — Scintilla's SCI_SETTEXT
      ;; silently fails on read-only documents (from previous invocation)
      (qt-plain-text-edit-set-read-only! ed #f)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (qt-plain-text-edit-set-read-only! ed #t)
      ;; Brighter caret line for buffer-list row selection
      (sci-send ed SCI_SETCARETLINEBACK (rgb->sci #x2a #x2a #x4a))
      (echo-message! (app-state-echo app) "*Buffer List*"))))

;;;============================================================================
;;; What line
;;;============================================================================

(def (cmd-what-line app)
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (total (qt-plain-text-edit-line-count ed)))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     " of " (number->string total)))))

;;;============================================================================
;;; Duplicate line
;;;============================================================================

(def (cmd-duplicate-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      "")))
    ;; Insert duplicate after current line
    (let* ((new-lines (let loop ((ls lines) (i 0) (acc []))
                        (if (null? ls)
                          (reverse acc)
                          (if (= i line)
                            (loop (cdr ls) (+ i 1) (cons (car ls) (cons (car ls) acc)))
                            (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
           (new-text (string-join new-lines "\n")))
      (qt-plain-text-edit-set-text! ed new-text)
      ;; Position cursor on the duplicated line by computing position
      (let ((pos (let loop ((i 0) (ln 0))
                   (cond
                     ((= ln (+ line 1)) i)
                     ((>= i (string-length new-text)) i)
                     ((char=? (string-ref new-text i) #\newline)
                      (loop (+ i 1) (+ ln 1)))
                     (else (loop (+ i 1) ln))))))
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Beginning/end of defun
;;;============================================================================

(def (cmd-beginning-of-defun app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((< i 0)
         (qt-plain-text-edit-set-cursor-position! ed 0)
         (echo-message! (app-state-echo app) "Beginning of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (qt-plain-text-edit-set-cursor-position! ed i)
         (qt-plain-text-edit-ensure-cursor-visible! ed))
        (else (loop (- i 1)))))))

(def (cmd-end-of-defun app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let find-start ((i pos))
      (cond
        ((>= i len)
         (qt-plain-text-edit-set-cursor-position! ed len)
         (echo-message! (app-state-echo app) "End of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (let match ((j (+ i 1)) (depth 1))
           (cond
             ((>= j len)
              (qt-plain-text-edit-set-cursor-position! ed len))
             ((= depth 0)
              (qt-plain-text-edit-set-cursor-position! ed j)
              (qt-plain-text-edit-ensure-cursor-visible! ed))
             ((char=? (string-ref text j) #\() (match (+ j 1) (+ depth 1)))
             ((char=? (string-ref text j) #\)) (match (+ j 1) (- depth 1)))
             (else (match (+ j 1) depth)))))
        (else (find-start (+ i 1)))))))

;;;============================================================================
;;; Tab / indent
;;;============================================================================

;;;----------------------------------------------------------------------------
;;; Autocomplete support
;;;----------------------------------------------------------------------------

;; Per-editor completer
(def *editor-completers* (make-hash-table))

(def (word-char-for-complete? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-) (char=? ch #\!)
      (char=? ch #\?) (char=? ch #\*) (char=? ch #\>)))

(def (get-word-prefix ed)
  "Get the word prefix before the cursor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (pos (min pos len)))
    (let loop ((i (- pos 1)))
      (if (or (< i 0) (not (word-char-for-complete? (string-ref text i))))
        (if (< (+ i 1) pos)
          (substring text (+ i 1) pos)
          "")
        (loop (- i 1))))))

(def (collect-buffer-words text)
  "Collect unique words from buffer text."
  (let ((words (make-hash-table))
        (len (string-length text)))
    (let loop ((i 0))
      (if (>= i len) (hash-keys words)
        (if (word-char-for-complete? (string-ref text i))
          ;; Start of a word
          (let find-end ((j (+ i 1)))
            (if (or (>= j len) (not (word-char-for-complete? (string-ref text j))))
              (begin
                (when (> (- j i) 1) ;; skip single-char words
                  (hash-put! words (substring text i j) #t))
                (loop j))
              (find-end (+ j 1))))
          (loop (+ i 1)))))))

(def (get-or-create-completer! ed app)
  "Get or create a completer for an editor."
  (or (hash-get *editor-completers* ed)
      (let ((c (qt-completer-create [])))
        (qt-completer-set-case-sensitivity! c #f)
        (qt-completer-set-widget! c ed)
        ;; When completion accepted, insert the remaining text
        (qt-on-completer-activated! c
          (lambda (text)
            (let ((prefix (get-word-prefix ed)))
              (when (> (string-length text) (string-length prefix))
                (qt-plain-text-edit-insert-text! ed
                  (substring text (string-length prefix) (string-length text)))))))
        (hash-put! *editor-completers* ed c)
        c)))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (text-line-position text line-num)
  "Find the character position of the start of LINE-NUM (1-based) in TEXT."
  (if (<= line-num 1) 0
    (let loop ((i 0) (line 1))
      (cond
        ((>= i (string-length text)) i)
        ((char=? (string-ref text i) #\newline)
         (if (= (+ line 1) line-num)
           (+ i 1)
           (loop (+ i 1) (+ line 1))))
        (else (loop (+ i 1) line))))))

;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state))
  ;; Deactivate mark and clear visual selection (Emacs C-g behavior)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app)))
    (when (buffer-mark buf)
      (set! (buffer-mark buf) #f)
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (qt-plain-text-edit-set-selection! ed pos pos)))))

;;;============================================================================
;;; Scroll margin commands (Qt)
;;;============================================================================

(def (cmd-set-scroll-margin app)
  "Set the scroll margin (lines to keep visible above/below cursor)."
  (let* ((input (qt-echo-read-string app
                  (string-append "Scroll margin (current "
                                 (number->string *scroll-margin*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (when (and n (>= n 0) (<= n 20))
          (set! *scroll-margin* n)
          (echo-message! (app-state-echo app)
            (string-append "Scroll margin set to " (number->string n))))))))

(def (cmd-toggle-scroll-margin app)
  "Toggle scroll margin between 0 and 3."
  (if (> *scroll-margin* 0)
    (set! *scroll-margin* 0)
    (set! *scroll-margin* 3))
  (echo-message! (app-state-echo app)
    (if (> *scroll-margin* 0)
      (string-append "Scroll margin: " (number->string *scroll-margin*))
      "Scroll margin: off")))

;;;============================================================================
;;; Save-place mode (Qt)
;;;============================================================================

(def (cmd-toggle-save-place-mode app)
  "Toggle save-place mode — remembers cursor position in files."
  (set! *save-place-enabled* (not *save-place-enabled*))
  (echo-message! (app-state-echo app)
    (if *save-place-enabled* "Save-place mode ON" "Save-place mode OFF")))

;;;============================================================================
;;; Require-final-newline (Qt)
;;;============================================================================

(def (cmd-toggle-require-final-newline app)
  "Toggle requiring files to end with a newline on save."
  (set! *require-final-newline* (not *require-final-newline*))
  (echo-message! (app-state-echo app)
    (if *require-final-newline*
      "Require final newline: ON"
      "Require final newline: OFF")))

;;;============================================================================
;;; Centered cursor mode (Qt)
;;;============================================================================

(def (cmd-toggle-centered-cursor-mode app)
  "Toggle centered cursor mode — keeps cursor vertically centered."
  (set! *centered-cursor-mode* (not *centered-cursor-mode*))
  (echo-message! (app-state-echo app)
    (if *centered-cursor-mode*
      "Centered cursor mode ON"
      "Centered cursor mode OFF")))

;;;============================================================================
;;; Tab insertion (Qt)
;;;============================================================================

(def *qt-tab-width* 4)

(def (cmd-tab-to-tab-stop app)
  "Insert spaces (or tab) to the next tab stop."
  (let* ((ed (current-qt-editor app))
         (col (qt-plain-text-edit-cursor-column ed))
         (next-stop (* (+ 1 (quotient col *qt-tab-width*)) *qt-tab-width*))
         (spaces (- next-stop col))
         (str (make-string spaces #\space)))
    (qt-plain-text-edit-insert-text! ed str)))

(def (cmd-set-tab-width app)
  "Set the tab width for the current buffer."
  (let* ((input (qt-echo-read-string app
                  (string-append "Tab width (" (number->string *qt-tab-width*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (exact? n) (> n 0) (<= n 16))
          (begin
            (set! *qt-tab-width* n)
            (echo-message! (app-state-echo app)
              (string-append "Tab width: " (number->string n))))
          (echo-error! (app-state-echo app) "Invalid tab width (1-16)"))))))

;;;============================================================================
;;; JSON format / minify / pretty-print
;;;============================================================================

(def (qt-json-pretty-print obj indent)
  "Pretty-print a JSON value with indentation."
  (let ((out (open-output-string)))
    (let pp ((val obj) (level 0))
      (let ((prefix (make-string (* level indent) #\space)))
        (cond
          ((hash-table? val)
           (display "{\n" out)
           (let ((keys (sort (hash-keys val) string<?))
                 (first #t))
             (for-each
               (lambda (k)
                 (unless first (display ",\n" out))
                 (display (make-string (* (+ level 1) indent) #\space) out)
                 (write k out)
                 (display ": " out)
                 (pp (hash-ref val k) (+ level 1))
                 (set! first #f))
               keys))
           (display "\n" out)
           (display prefix out)
           (display "}" out))
          ((list? val)
           (if (null? val)
             (display "[]" out)
             (begin
               (display "[\n" out)
               (let ((first #t))
                 (for-each
                   (lambda (item)
                     (unless first (display ",\n" out))
                     (display (make-string (* (+ level 1) indent) #\space) out)
                     (pp item (+ level 1))
                     (set! first #f))
                   val))
               (display "\n" out)
               (display prefix out)
               (display "]" out))))
          ((string? val) (write val out))
          ((number? val) (display val out))
          ((boolean? val) (display (if val "true" "false") out))
          ((not val) (display "null" out))
          (else (write val out)))))
    (get-output-string out)))

(def (cmd-json-format-buffer app)
  "Pretty-print JSON in the current buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (formatted (qt-json-pretty-print obj 2))
               (pos (qt-plain-text-edit-cursor-position ed)))
          (qt-plain-text-edit-set-text! ed (string-append formatted "\n"))
          (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length formatted)))
          (echo-message! echo "JSON formatted"))))))

(def (cmd-json-minify-buffer app)
  "Minify JSON in the current buffer (remove whitespace)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (minified (call-with-output-string
                           (lambda (port) (write-json obj port))))
               (pos (qt-plain-text-edit-cursor-position ed)))
          (qt-plain-text-edit-set-text! ed minified)
          (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length minified)))
          (echo-message! echo
            (string-append "JSON minified (" (number->string (string-length minified)) " bytes)")))))))

(def (cmd-json-pretty-print-region app)
  "Pretty-print JSON in selected region using python3."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "Select JSON region first")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end)))
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Invalid JSON"))
          (lambda ()
            (let* ((obj (call-with-input-string region read-json))
                   (formatted (qt-json-pretty-print obj 2))
                   (new-text (string-append
                               (substring text 0 sel-start)
                               formatted
                               (substring text sel-end (string-length text)))))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-set-cursor-position! ed sel-start)
              (echo-message! (app-state-echo app) "JSON formatted"))))))))

;;;============================================================================
;;; URL encode / decode
;;;============================================================================

(def (cmd-url-encode-region app)
  "URL-encode the selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (encoded (uri-encode region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         encoded
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length encoded)))
        (echo-message! (app-state-echo app) "URL encoded")))))

(def (cmd-url-decode-region app)
  "URL-decode the selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (decoded (uri-decode region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         decoded
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length decoded)))
        (echo-message! (app-state-echo app) "URL decoded")))))

;;;============================================================================
;;; Reverse / shuffle lines
;;;============================================================================

(def (qt-reverse-lines-in-string text)
  "Reverse order of lines in a string."
  (let* ((lines (string-split text #\newline))
         (reversed (reverse lines)))
    (string-join reversed "\n")))

(def (cmd-reverse-lines app)
  "Reverse the order of lines in region or entire buffer."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection => reverse entire buffer
      (let* ((text (qt-plain-text-edit-text ed))
             (result (qt-reverse-lines-in-string text)))
        (qt-plain-text-edit-set-text! ed result)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "Reversed all lines"))
      ;; Selection => reverse selected region
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (result (qt-reverse-lines-in-string region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed sel-start)
        (echo-message! (app-state-echo app) "Reversed selected lines")))))

(def (qt-shuffle lst)
  "Fisher-Yates shuffle of a list."
  (let ((vec (list->vector lst)))
    (let loop ((i (- (vector-length vec) 1)))
      (when (> i 0)
        (let* ((j (random-integer (+ i 1)))
               (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)
          (loop (- i 1)))))
    (vector->list vec)))

(def (cmd-shuffle-lines app)
  "Randomly shuffle lines in region or entire buffer."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (shuffled (qt-shuffle lines))
             (result (string-join shuffled "\n")))
        (qt-plain-text-edit-set-text! ed result)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "Shuffled all lines"))
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (shuffled (qt-shuffle lines))
             (result (string-join shuffled "\n"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed sel-start)
        (echo-message! (app-state-echo app) "Shuffled selected lines")))))

;;;============================================================================
;;; XML format
;;;============================================================================

(def (cmd-xml-format app)
  "Format XML in the current buffer using xmllint."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! (app-state-echo app) "xmllint not available"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "xmllint"
                             arguments: '("--format" "-")
                             stdin-redirection: #t stdout-redirection: #t
                             stderr-redirection: #t)))
               (_ (begin (display text proc) (close-output-port proc)))
               (result (read-line proc #f)))
          (process-status proc)
          (if (and result (> (string-length result) 0))
            (begin
              (qt-plain-text-edit-set-text! ed result)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (echo-message! (app-state-echo app) "XML formatted"))
            (echo-error! (app-state-echo app) "XML format failed")))))))

;;;============================================================================
;;; Open URL at point
;;;============================================================================

(def (qt-find-url-at-point text pos)
  "Find URL boundaries at position in text."
  (let ((len (string-length text)))
    ;; Scan backward from pos to find start of URL-like string
    (let find-start ((i pos))
      (if (or (<= i 0)
              (memv (string-ref text (- i 1)) '(#\space #\tab #\newline #\( #\) #\[ #\] #\" #\')))
        ;; Check if this looks like a URL
        (let ((candidate (let find-end ((j (max i pos)))
                           (if (or (>= j len)
                                   (memv (string-ref text j) '(#\space #\tab #\newline #\) #\] #\" #\')))
                             (substring text i j)
                             (find-end (+ j 1))))))
          (if (or (string-prefix? "http://" candidate)
                  (string-prefix? "https://" candidate)
                  (string-prefix? "ftp://" candidate))
            (cons i (+ i (string-length candidate)))
            #f))
        (find-start (- i 1))))))

(def (cmd-open-url-at-point app)
  "Open URL at point in external browser."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let ((url-bounds (qt-find-url-at-point text pos)))
      (if (not url-bounds)
        (echo-error! (app-state-echo app) "No URL at point")
        (let ((url (substring text (car url-bounds) (cdr url-bounds))))
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Failed to open URL"))
            (lambda ()
              (open-process
                (list path: "xdg-open" arguments: (list url)
                      stdin-redirection: #f stdout-redirection: #f
                      stderr-redirection: #f))
              (echo-message! (app-state-echo app) (string-append "Opening: " url)))))))))

;;;============================================================================
;;; Compare windows
;;;============================================================================

(def (cmd-compare-windows app)
  "Compare text in two visible windows and jump to first difference."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (< (length wins) 2)
      (echo-error! (app-state-echo app) "Need at least 2 windows")
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2))
             (t1 (qt-plain-text-edit-text e1))
             (t2 (qt-plain-text-edit-text e2))
             (len (min (string-length t1) (string-length t2))))
        (let loop ((i 0))
          (cond
            ((>= i len)
             (if (= (string-length t1) (string-length t2))
               (echo-message! (app-state-echo app) "Windows are identical")
               (echo-message! (app-state-echo app)
                 (string-append "Differ at pos " (number->string i) " (one buffer is shorter)"))))
            ((not (char=? (string-ref t1 i) (string-ref t2 i)))
             (echo-message! (app-state-echo app)
               (string-append "First difference at position " (number->string i))))
            (else (loop (+ i 1)))))))))

;;;============================================================================
;;; Dedent region (remove one level of indentation)
;;;============================================================================

(def (cmd-dedent-region app)
  "Remove one level of indentation from selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No selection to dedent")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (line)
                              (let ((len (string-length line)))
                                (cond
                                  ((and (> len 0) (char=? (string-ref line 0) #\tab))
                                   (substring line 1 len))
                                  ((string-prefix? "    " line)
                                   (substring line 4 len))
                                  ((string-prefix? "  " line)
                                   (substring line 2 len))
                                  (else line))))
                            lines))
             (result (string-join dedented "\n"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-selection! ed sel-start (+ sel-start (string-length result)))
        (echo-message! (app-state-echo app) "Region dedented")))))

;;;============================================================================
;;; Count words in current line
;;;============================================================================

(def (cmd-count-words-line app)
  "Count words in current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) "")))
    (let loop ((i 0) (count 0) (in-word #f))
      (if (>= i (string-length line-text))
        (echo-message! (app-state-echo app)
          (string-append "Words in line: " (number->string (if in-word (+ count 1) count))))
        (let ((ch (string-ref line-text i)))
          (if (or (char=? ch #\space) (char=? ch #\tab))
            (loop (+ i 1) (if in-word (+ count 1) count) #f)
            (loop (+ i 1) count #t)))))))

;;;============================================================================
;;; Diff goto source (jump from diff buffer to source)
;;;============================================================================

(def (cmd-diff-goto-source app)
  "Jump from a diff hunk to the corresponding source location."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-plain-text-edit-cursor-line ed)))
    ;; Find the @@ hunk header and +++ file header above
    (let find-context ((l cur-line) (file #f) (hunk-line #f))
      (cond
        ((< l 0)
         (if (and file hunk-line)
           ;; Count lines from hunk start to cursor
           (let* ((offset (- cur-line (+ l 1)))
                  (target-line (+ hunk-line offset)))
             (if (file-exists? file)
               (begin
                 (cmd-find-file-by-path app file)
                 (echo-message! (app-state-echo app)
                   (string-append file ":" (number->string target-line))))
               (echo-error! (app-state-echo app) (string-append "File not found: " file))))
           (echo-error! (app-state-echo app) "No diff context found")))
        (else
         (let ((line-text (if (< l (length lines)) (list-ref lines l) "")))
           (cond
             ((and (not hunk-line) (string-prefix? "@@ " line-text))
              ;; Parse @@ -x,y +N,M @@ to get N
              (let* ((plus-pos (string-contains line-text "+"))
                     (comma-pos (and plus-pos (string-contains line-text "," (+ plus-pos 1))))
                     (space-pos (and plus-pos (string-contains line-text " " (+ plus-pos 1))))
                     (end-pos (or comma-pos space-pos (string-length line-text)))
                     (num-str (and plus-pos (substring line-text (+ plus-pos 1) end-pos)))
                     (num (and num-str (string->number num-str))))
                (find-context (- l 1) file (or num 1))))
             ((and (not file) (string-prefix? "+++ " line-text))
              (let* ((path (substring line-text 4 (string-length line-text)))
                     (clean (if (string-prefix? "b/" path) (substring path 2 (string-length path)) path)))
                (find-context (- l 1) clean hunk-line)))
             (else (find-context (- l 1) file hunk-line)))))))))

(def (cmd-find-file-by-path app path)
  "Open file by path (helper for diff-goto-source)."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (name (path-strip-directory path)))
    (let ((buf (or (buffer-by-name name) (qt-buffer-create! name ed))))
      (when (file-exists? path)
        (let ((content (read-file-as-string path)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed (or content ""))
          (set! (buffer-file-path buf) path)
          (qt-modeline-update! app))))))

;;;============================================================================
;;; Insert date ISO (alias for consistency with TUI)
;;;============================================================================

(def (cmd-insert-date-iso app)
  "Insert current date in ISO 8601 format (YYYY-MM-DD)."
  (let* ((ed (current-qt-editor app))
         (out (open-process (list path: "date" arguments: '("+%Y-%m-%d"))))
         (result (read-line out)))
    (close-port out)
    (qt-plain-text-edit-insert-text! ed (if (string? result) result "???"))))

;;;============================================================================
;;; Org-mode parity commands
;;;============================================================================

(def (cmd-org-schedule app)
  "Insert SCHEDULED timestamp on next line."
  (let* ((ed (current-qt-editor app))
         (date (qt-echo-read-string app "Schedule date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (new-text (string-append (substring text 0 line-end)
                                      "\n  SCHEDULED: <" date ">"
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Scheduled: " date))))))

(def (cmd-org-deadline app)
  "Insert DEADLINE timestamp on next line."
  (let* ((ed (current-qt-editor app))
         (date (qt-echo-read-string app "Deadline date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (new-text (string-append (substring text 0 line-end)
                                      "\n  DEADLINE: <" date ">"
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Deadline: " date))))))

(def (cmd-org-insert-src-block app)
  "Insert #+BEGIN_SRC ... #+END_SRC template at point."
  (let* ((ed (current-qt-editor app))
         (lang (qt-echo-read-string app "Language (default: empty): "))
         (lang-str (if (and lang (> (string-length lang) 0))
                     (string-append " " lang) ""))
         (template (string-append "#+BEGIN_SRC" lang-str "\n\n#+END_SRC\n")))
    (qt-plain-text-edit-insert-text! ed template)
    (echo-message! (app-state-echo app) "Source block inserted")))

(def (cmd-org-clock-in app)
  "Insert CLOCK-IN timestamp in :LOGBOOK: drawer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1)))))
         (now (with-exception-catcher (lambda (e) "")
                (lambda ()
                  (let ((p (open-process
                             (list path: "date"
                                   arguments: '("+[%Y-%m-%d %a %H:%M]")))))
                    (let ((out (read-line p)))
                      (close-port p) (or out "")))))))
    (when (> (string-length now) 0)
      (let* ((clock-text (string-append "\n  :LOGBOOK:\n  CLOCK: " now "\n  :END:"))
             (new-text (string-append (substring text 0 line-end)
                                      clock-text
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Clocked in: " now))))))

(def (cmd-org-clock-out app)
  "Close open CLOCK entry with end timestamp."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (echo (app-state-echo app))
         ;; Find last open CLOCK entry
         (clock-line
           (let loop ((i (- (length lines) 1)))
             (cond
               ((< i 0) #f)
               ((let ((l (list-ref lines i)))
                  (and (string-contains l "CLOCK: [")
                       (not (string-contains l "--"))))
                i)
               (else (loop (- i 1)))))))
    (if (not clock-line)
      (echo-message! echo "No open clock entry")
      (let ((now (with-exception-catcher (lambda (e) "")
                   (lambda ()
                     (let ((p (open-process
                                (list path: "date"
                                      arguments: '("+[%Y-%m-%d %a %H:%M]")))))
                       (let ((out (read-line p)))
                         (close-port p) (or out "")))))))
        (when (> (string-length now) 0)
          (let* ((old-line (list-ref lines clock-line))
                 (new-line (string-append old-line "--" now))
                 (new-lines (let loop ((ls lines) (i 0) (acc []))
                              (if (null? ls) (reverse acc)
                                (loop (cdr ls) (+ i 1)
                                      (cons (if (= i clock-line) new-line (car ls)) acc))))))
            (qt-plain-text-edit-set-text! ed (string-join new-lines "\n"))
            (echo-message! echo (string-append "Clocked out: " now))))))))
