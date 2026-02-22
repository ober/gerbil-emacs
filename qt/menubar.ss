;;; -*- Gerbil -*-
;;; Qt menu bar and toolbar for gemacs

(export qt-setup-menubar!)

(import :std/sugar
        :gemacs/qt/sci-shim
        :gemacs/core)

;;;============================================================================
;;; Menu bar setup
;;;============================================================================

(def (qt-setup-menubar! app win)
  "Set up the menu bar and toolbar for the main window."
  (let ((menu-bar (qt-main-window-menu-bar win)))

    ;; ---- File menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&File")))
      (add-menu-command! menu win app "&New"          "Ctrl+X,Ctrl+N" 'find-file)
      (add-menu-command! menu win app "&Open..."      "Ctrl+X,Ctrl+F" 'find-file)
      (add-menu-command! menu win app "&Save"         "Ctrl+X,Ctrl+S" 'save-buffer)
      (add-menu-command! menu win app "Save &As..."   "Ctrl+X,Ctrl+W" 'write-file)
      (add-menu-command! menu win app "&Revert"       ""              'revert-buffer)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "&Quit"         "Ctrl+X,Ctrl+C" 'quit))

    ;; ---- Edit menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&Edit")))
      (add-menu-command! menu win app "&Undo"         "Ctrl+/"        'undo)
      (add-menu-command! menu win app "&Redo"         "Alt+/"         'redo)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "Cu&t"          "Ctrl+W"        'kill-region)
      (add-menu-command! menu win app "&Copy"         "Alt+W"         'copy-region)
      (add-menu-command! menu win app "&Paste"        "Ctrl+Y"        'yank)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "&Find..."      "Ctrl+S"        'search-forward)
      (add-menu-command! menu win app "R&eplace..."   "Alt+%"         'query-replace)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "Select &All"   "Ctrl+X,H"      'select-all)
      (add-menu-command! menu win app "&Go to Line"   "Alt+G,G"       'goto-line))

    ;; ---- View menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&View")))
      (add-menu-command! menu win app "Toggle &Line Numbers"  "Ctrl+X,L"   'toggle-line-numbers)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "Split &Below"          "Ctrl+X,2"   'split-window)
      (add-menu-command! menu win app "Split &Right"          "Ctrl+X,3"   'split-window-right)
      (add-menu-command! menu win app "&Close Split"          "Ctrl+X,0"   'delete-window)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "Zoom &In"              "Ctrl+="     'zoom-in)
      (add-menu-command! menu win app "Zoom &Out"             "Ctrl+-"     'zoom-out))

    ;; ---- Buffer menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&Buffer")))
      (add-menu-command! menu win app "&Switch Buffer"  "Ctrl+X,B"     'switch-buffer)
      (add-menu-command! menu win app "&List Buffers"   "Ctrl+X,Ctrl+B" 'list-buffers)
      (add-menu-command! menu win app "&Kill Buffer"    "Ctrl+X,K"     'kill-buffer-cmd))

    ;; ---- Tools menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&Tools")))
      (add-menu-command! menu win app "&REPL"             "Ctrl+C,Z"   'repl)
      (add-menu-command! menu win app "&Eshell"           "Ctrl+C,E"   'eshell)
      (add-menu-command! menu win app "&Shell"            "Ctrl+C,$"   'shell)
      (qt-menu-add-separator! menu)
      (add-menu-command! menu win app "Eval E&xpression"  "Alt+:"      'eval-expression))

    ;; ---- Help menu ----
    (let ((menu (qt-menu-bar-add-menu menu-bar "&Help")))
      (add-menu-command! menu win app "List &Bindings"    "Ctrl+H,B"   'list-bindings))

    ;; ---- Toolbar ----
    (let ((toolbar (qt-toolbar-create "Main" parent: win)))
      (qt-main-window-add-toolbar! win toolbar)
      (qt-toolbar-set-movable! toolbar #f)
      (add-toolbar-command! toolbar win app "New"   'find-file)
      (add-toolbar-command! toolbar win app "Open"  'find-file)
      (add-toolbar-command! toolbar win app "Save"  'save-buffer)
      (qt-toolbar-add-separator! toolbar)
      (add-toolbar-command! toolbar win app "Undo"  'undo)
      (add-toolbar-command! toolbar win app "Redo"  'redo)
      (qt-toolbar-add-separator! toolbar)
      (add-toolbar-command! toolbar win app "Find"  'search-forward))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (add-menu-command! menu win app label shortcut command-name)
  "Add a menu action that executes a named command.
   Shortcut text is shown in the menu label but NOT registered as a Qt shortcut,
   because the custom keymap system handles all key bindings. Registering Qt
   shortcuts would cause Ctrl+X prefix keys to be intercepted before the
   custom handler sees them."
  (let* ((display-label (if (and (string? shortcut) (> (string-length shortcut) 0))
                          (string-append label "\t" shortcut)
                          label))
         (action (qt-action-create display-label parent: win)))
    ;; Do NOT call qt-action-set-shortcut! — it conflicts with the custom keymap.
    (qt-on-triggered! action
      (lambda () (execute-command! app command-name)))
    (qt-menu-add-action! menu action)
    action))

(def (add-toolbar-command! toolbar win app label command-name)
  "Add a toolbar action that executes a named command."
  (let ((action (qt-action-create label parent: win)))
    (qt-on-triggered! action
      (lambda () (execute-command! app command-name)))
    (qt-toolbar-add-action! toolbar action)
    action))
