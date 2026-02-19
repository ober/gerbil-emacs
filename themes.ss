;;; -*- Gerbil -*-
;;; Built-in Theme Definitions
;;;
;;; Each theme defines a complete set of faces for syntax highlighting, UI chrome,
;;; org-mode, and other visual elements. Themes are alists mapping face names
;;; (symbols) to face property lists (fg: bg: bold: italic: underline:).

(export #t)
(import :std/sugar)

;;; ============================================================================
;;; Theme: Dark (Default Dark Theme)
;;; ============================================================================

(def theme-dark
  '((default . (fg: "#d8d8d8" bg: "#181818"))
    (region . (bg: "#404060"))

    ;; Syntax highlighting
    (font-lock-keyword-face . (fg: "#cc99cc" bold: #t))
    (font-lock-builtin-face . (fg: "#66cccc"))
    (font-lock-string-face . (fg: "#99cc99"))
    (font-lock-comment-face . (fg: "#999999" italic: #t))
    (font-lock-number-face . (fg: "#f99157"))
    (font-lock-operator-face . (fg: "#b8b8b8"))
    (font-lock-type-face . (fg: "#ffcc66"))
    (font-lock-preprocessor-face . (fg: "#f99157"))
    (font-lock-heading-face . (fg: "#6699cc" bold: #t))

    ;; UI elements
    (modeline . (fg: "#d8d8d8" bg: "#282828"))
    (modeline-inactive . (fg: "#808080" bg: "#282828"))
    (line-number . (fg: "#8c8c8c" bg: "#202020"))
    (cursor-line . (bg: "#222228"))

    ;; Search and matching
    (match . (fg: "#ffff00" bg: "#404060" bold: #t))
    (mismatch . (fg: "#ff4040" bg: "#602020" bold: #t))
    (isearch . (fg: "#000000" bg: "#ffcc00"))
    (error . (fg: "#ff4040"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#6699cc" bold: #t))
    (org-heading-2 . (fg: "#f99157" bold: #t))
    (org-heading-3 . (fg: "#99cc99" bold: #t))
    (org-heading-4 . (fg: "#cc99cc" bold: #t))
    (org-heading-5 . (fg: "#66cccc" bold: #t))
    (org-heading-6 . (fg: "#ffcc66" bold: #t))
    (org-heading-7 . (fg: "#f2777a" bold: #t))
    (org-heading-8 . (fg: "#d27b53" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#dc3232" bold: #t))
    (org-done . (fg: "#32b432" bold: #t))
    (org-link . (fg: "#5078dc" underline: #t))
    (org-code . (fg: "#3ca03c"))
    (org-verbatim . (fg: "#32b4b4"))
    (org-table . (fg: "#32b4b4"))
    (org-comment . (fg: "#828282" italic: #t))
    (org-tag . (fg: "#9650b4"))
    (org-date . (fg: "#b450b4"))
    (org-property . (fg: "#646464"))
    (org-block-delimiter . (fg: "#d28c32"))
    (org-block-body . (fg: "#646464"))

    ;; Tab bar
    (tab-active . (fg: "#ffffff" bg: "#404060"))
    (tab-inactive . (fg: "#a0a0a0" bg: "#252525"))

    ;; Window borders
    (window-border-active . (fg: "#51afef"))
    (window-border-inactive . (fg: "#3a3a3a"))

    ;; Legacy UI chrome keys (for backward compatibility)
    (bg . "#181818")
    (fg . "#d8d8d8")
    (selection . "#404060")
    (modeline-bg . "#282828")
    (modeline-fg . "#d8d8d8")
    (echo-bg . "#282828")
    (echo-fg . "#d8d8d8")
    (gutter-bg . "#202020")
    (gutter-fg . "#8c8c8c")
    (split . "#383838")
    (tab-bg . "#1e1e1e")
    (tab-border . "#383838")
    (tab-active-bg . "#404060")
    (tab-active-fg . "#ffffff")
    (tab-inactive-bg . "#252525")
    (tab-inactive-fg . "#a0a0a0")))

;;; ============================================================================
;;; Theme: Light (Default Light Theme)
;;; ============================================================================

(def theme-light
  '((default . (fg: "#383838" bg: "#fafafa"))
    (region . (bg: "#c0d0e8"))

    ;; Syntax highlighting
    (font-lock-keyword-face . (fg: "#7f00ff" bold: #t))
    (font-lock-builtin-face . (fg: "#008787"))
    (font-lock-string-face . (fg: "#008700"))
    (font-lock-comment-face . (fg: "#8a8a8a" italic: #t))
    (font-lock-number-face . (fg: "#d75f00"))
    (font-lock-operator-face . (fg: "#5f5f5f"))
    (font-lock-type-face . (fg: "#af8700"))
    (font-lock-preprocessor-face . (fg: "#d75f00"))
    (font-lock-heading-face . (fg: "#005f87" bold: #t))

    ;; UI elements
    (modeline . (fg: "#383838" bg: "#e8e8e8"))
    (modeline-inactive . (fg: "#909090" bg: "#e8e8e8"))
    (line-number . (fg: "#a0a0a0" bg: "#f0f0f0"))
    (cursor-line . (bg: "#f0f0f8"))

    ;; Search and matching
    (match . (fg: "#000000" bg: "#ffff00" bold: #t))
    (mismatch . (fg: "#ffffff" bg: "#d70000" bold: #t))
    (isearch . (fg: "#000000" bg: "#ffaf00"))
    (error . (fg: "#d70000"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#005f87" bold: #t))
    (org-heading-2 . (fg: "#d75f00" bold: #t))
    (org-heading-3 . (fg: "#008700" bold: #t))
    (org-heading-4 . (fg: "#7f00ff" bold: #t))
    (org-heading-5 . (fg: "#008787" bold: #t))
    (org-heading-6 . (fg: "#af8700" bold: #t))
    (org-heading-7 . (fg: "#d70087" bold: #t))
    (org-heading-8 . (fg: "#875f00" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#d70000" bold: #t))
    (org-done . (fg: "#008700" bold: #t))
    (org-link . (fg: "#0000ff" underline: #t))
    (org-code . (fg: "#008700"))
    (org-verbatim . (fg: "#008787"))
    (org-table . (fg: "#008787"))
    (org-comment . (fg: "#8a8a8a" italic: #t))
    (org-tag . (fg: "#7f00ff"))
    (org-date . (fg: "#af00d7"))
    (org-property . (fg: "#767676"))
    (org-block-delimiter . (fg: "#af5f00"))
    (org-block-body . (fg: "#767676"))

    ;; Tab bar
    (tab-active . (fg: "#000000" bg: "#c0d0e8"))
    (tab-inactive . (fg: "#808080" bg: "#f0f0f0"))

    ;; Window borders
    (window-border-active . (fg: "#005faf"))
    (window-border-inactive . (fg: "#bcbcbc"))

    ;; Legacy UI chrome keys
    (bg . "#fafafa")
    (fg . "#383838")
    (selection . "#c0d0e8")
    (modeline-bg . "#e8e8e8")
    (modeline-fg . "#383838")
    (echo-bg . "#e8e8e8")
    (echo-fg . "#383838")
    (gutter-bg . "#f0f0f0")
    (gutter-fg . "#a0a0a0")
    (split . "#d0d0d0")
    (tab-bg . "#f0f0f0")
    (tab-border . "#d0d0d0")
    (tab-active-bg . "#c0d0e8")
    (tab-active-fg . "#000000")
    (tab-inactive-bg . "#f0f0f0")
    (tab-inactive-fg . "#808080")))

;;; ============================================================================
;;; Theme: Solarized Dark
;;; ============================================================================

(def theme-solarized-dark
  '((default . (fg: "#839496" bg: "#002b36"))
    (region . (bg: "#073642"))

    ;; Syntax highlighting (Solarized palette)
    (font-lock-keyword-face . (fg: "#859900" bold: #t))  ; green
    (font-lock-builtin-face . (fg: "#268bd2"))           ; blue
    (font-lock-string-face . (fg: "#2aa198"))            ; cyan
    (font-lock-comment-face . (fg: "#586e75" italic: #t)) ; base01
    (font-lock-number-face . (fg: "#cb4b16"))            ; orange
    (font-lock-operator-face . (fg: "#93a1a1"))          ; base1
    (font-lock-type-face . (fg: "#b58900"))              ; yellow
    (font-lock-preprocessor-face . (fg: "#cb4b16"))      ; orange
    (font-lock-heading-face . (fg: "#268bd2" bold: #t))  ; blue

    ;; UI elements
    (modeline . (fg: "#93a1a1" bg: "#073642"))
    (modeline-inactive . (fg: "#586e75" bg: "#073642"))
    (line-number . (fg: "#586e75" bg: "#002b36"))
    (cursor-line . (bg: "#073642"))

    ;; Search and matching
    (match . (fg: "#b58900" bg: "#073642" bold: #t))
    (mismatch . (fg: "#dc322f" bg: "#073642" bold: #t))
    (isearch . (fg: "#002b36" bg: "#b58900"))
    (error . (fg: "#dc322f"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#268bd2" bold: #t))
    (org-heading-2 . (fg: "#cb4b16" bold: #t))
    (org-heading-3 . (fg: "#859900" bold: #t))
    (org-heading-4 . (fg: "#d33682" bold: #t))
    (org-heading-5 . (fg: "#2aa198" bold: #t))
    (org-heading-6 . (fg: "#b58900" bold: #t))
    (org-heading-7 . (fg: "#6c71c4" bold: #t))
    (org-heading-8 . (fg: "#cb4b16" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#dc322f" bold: #t))
    (org-done . (fg: "#859900" bold: #t))
    (org-link . (fg: "#268bd2" underline: #t))
    (org-code . (fg: "#2aa198"))
    (org-verbatim . (fg: "#2aa198"))
    (org-table . (fg: "#2aa198"))
    (org-comment . (fg: "#586e75" italic: #t))
    (org-tag . (fg: "#d33682"))
    (org-date . (fg: "#6c71c4"))
    (org-property . (fg: "#586e75"))
    (org-block-delimiter . (fg: "#cb4b16"))
    (org-block-body . (fg: "#586e75"))

    ;; Tab bar
    (tab-active . (fg: "#fdf6e3" bg: "#073642"))
    (tab-inactive . (fg: "#586e75" bg: "#002b36"))

    ;; Window borders
    (window-border-active . (fg: "#268bd2"))
    (window-border-inactive . (fg: "#073642"))

    ;; Legacy UI chrome keys
    (bg . "#002b36")
    (fg . "#839496")
    (selection . "#073642")
    (modeline-bg . "#073642")
    (modeline-fg . "#93a1a1")
    (echo-bg . "#073642")
    (echo-fg . "#93a1a1")
    (gutter-bg . "#002b36")
    (gutter-fg . "#586e75")
    (split . "#073642")
    (tab-bg . "#002b36")
    (tab-border . "#073642")
    (tab-active-bg . "#073642")
    (tab-active-fg . "#fdf6e3")
    (tab-inactive-bg . "#002b36")
    (tab-inactive-fg . "#586e75")))

;;; ============================================================================
;;; Theme: Solarized Light
;;; ============================================================================

(def theme-solarized-light
  '((default . (fg: "#657b83" bg: "#fdf6e3"))
    (region . (bg: "#eee8d5"))

    ;; Syntax highlighting (Solarized light palette)
    (font-lock-keyword-face . (fg: "#859900" bold: #t))
    (font-lock-builtin-face . (fg: "#268bd2"))
    (font-lock-string-face . (fg: "#2aa198"))
    (font-lock-comment-face . (fg: "#93a1a1" italic: #t))
    (font-lock-number-face . (fg: "#cb4b16"))
    (font-lock-operator-face . (fg: "#586e75"))
    (font-lock-type-face . (fg: "#b58900"))
    (font-lock-preprocessor-face . (fg: "#cb4b16"))
    (font-lock-heading-face . (fg: "#268bd2" bold: #t))

    ;; UI elements
    (modeline . (fg: "#586e75" bg: "#eee8d5"))
    (modeline-inactive . (fg: "#93a1a1" bg: "#eee8d5"))
    (line-number . (fg: "#93a1a1" bg: "#fdf6e3"))
    (cursor-line . (bg: "#eee8d5"))

    ;; Search and matching
    (match . (fg: "#b58900" bg: "#eee8d5" bold: #t))
    (mismatch . (fg: "#dc322f" bg: "#eee8d5" bold: #t))
    (isearch . (fg: "#fdf6e3" bg: "#b58900"))
    (error . (fg: "#dc322f"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#268bd2" bold: #t))
    (org-heading-2 . (fg: "#cb4b16" bold: #t))
    (org-heading-3 . (fg: "#859900" bold: #t))
    (org-heading-4 . (fg: "#d33682" bold: #t))
    (org-heading-5 . (fg: "#2aa198" bold: #t))
    (org-heading-6 . (fg: "#b58900" bold: #t))
    (org-heading-7 . (fg: "#6c71c4" bold: #t))
    (org-heading-8 . (fg: "#cb4b16" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#dc322f" bold: #t))
    (org-done . (fg: "#859900" bold: #t))
    (org-link . (fg: "#268bd2" underline: #t))
    (org-code . (fg: "#2aa198"))
    (org-verbatim . (fg: "#2aa198"))
    (org-table . (fg: "#2aa198"))
    (org-comment . (fg: "#93a1a1" italic: #t))
    (org-tag . (fg: "#d33682"))
    (org-date . (fg: "#6c71c4"))
    (org-property . (fg: "#93a1a1"))
    (org-block-delimiter . (fg: "#cb4b16"))
    (org-block-body . (fg: "#93a1a1"))

    ;; Tab bar
    (tab-active . (fg: "#002b36" bg: "#eee8d5"))
    (tab-inactive . (fg: "#93a1a1" bg: "#fdf6e3"))

    ;; Window borders
    (window-border-active . (fg: "#268bd2"))
    (window-border-inactive . (fg: "#eee8d5"))

    ;; Legacy UI chrome keys
    (bg . "#fdf6e3")
    (fg . "#657b83")
    (selection . "#eee8d5")
    (modeline-bg . "#eee8d5")
    (modeline-fg . "#586e75")
    (echo-bg . "#eee8d5")
    (echo-fg . "#586e75")
    (gutter-bg . "#fdf6e3")
    (gutter-fg . "#93a1a1")
    (split . "#eee8d5")
    (tab-bg . "#fdf6e3")
    (tab-border . "#eee8d5")
    (tab-active-bg . "#eee8d5")
    (tab-active-fg . "#002b36")
    (tab-inactive-bg . "#fdf6e3")
    (tab-inactive-fg . "#93a1a1")))

;;; ============================================================================
;;; Theme: Monokai
;;; ============================================================================

(def theme-monokai
  '((default . (fg: "#f8f8f2" bg: "#272822"))
    (region . (bg: "#49483e"))

    ;; Syntax highlighting (Monokai palette)
    (font-lock-keyword-face . (fg: "#f92672" bold: #t))  ; pink
    (font-lock-builtin-face . (fg: "#66d9ef"))           ; cyan
    (font-lock-string-face . (fg: "#e6db74"))            ; yellow
    (font-lock-comment-face . (fg: "#75715e" italic: #t)) ; gray
    (font-lock-number-face . (fg: "#ae81ff"))            ; purple
    (font-lock-operator-face . (fg: "#f92672"))          ; pink
    (font-lock-type-face . (fg: "#66d9ef"))              ; cyan
    (font-lock-preprocessor-face . (fg: "#a6e22e"))      ; green
    (font-lock-heading-face . (fg: "#66d9ef" bold: #t))  ; cyan

    ;; UI elements
    (modeline . (fg: "#f8f8f2" bg: "#3e3d32"))
    (modeline-inactive . (fg: "#75715e" bg: "#3e3d32"))
    (line-number . (fg: "#75715e" bg: "#272822"))
    (cursor-line . (bg: "#3e3d32"))

    ;; Search and matching
    (match . (fg: "#000000" bg: "#e6db74" bold: #t))
    (mismatch . (fg: "#f92672" bg: "#49483e" bold: #t))
    (isearch . (fg: "#000000" bg: "#e6db74"))
    (error . (fg: "#f92672"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#66d9ef" bold: #t))
    (org-heading-2 . (fg: "#a6e22e" bold: #t))
    (org-heading-3 . (fg: "#e6db74" bold: #t))
    (org-heading-4 . (fg: "#f92672" bold: #t))
    (org-heading-5 . (fg: "#ae81ff" bold: #t))
    (org-heading-6 . (fg: "#fd971f" bold: #t))
    (org-heading-7 . (fg: "#66d9ef" bold: #t))
    (org-heading-8 . (fg: "#a6e22e" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#f92672" bold: #t))
    (org-done . (fg: "#a6e22e" bold: #t))
    (org-link . (fg: "#66d9ef" underline: #t))
    (org-code . (fg: "#a6e22e"))
    (org-verbatim . (fg: "#66d9ef"))
    (org-table . (fg: "#66d9ef"))
    (org-comment . (fg: "#75715e" italic: #t))
    (org-tag . (fg: "#ae81ff"))
    (org-date . (fg: "#ae81ff"))
    (org-property . (fg: "#75715e"))
    (org-block-delimiter . (fg: "#fd971f"))
    (org-block-body . (fg: "#75715e"))

    ;; Tab bar
    (tab-active . (fg: "#f8f8f2" bg: "#49483e"))
    (tab-inactive . (fg: "#75715e" bg: "#272822"))

    ;; Window borders
    (window-border-active . (fg: "#66d9ef"))
    (window-border-inactive . (fg: "#3e3d32"))

    ;; Legacy UI chrome keys
    (bg . "#272822")
    (fg . "#f8f8f2")
    (selection . "#49483e")
    (modeline-bg . "#3e3d32")
    (modeline-fg . "#f8f8f2")
    (echo-bg . "#3e3d32")
    (echo-fg . "#f8f8f2")
    (gutter-bg . "#272822")
    (gutter-fg . "#75715e")
    (split . "#3e3d32")
    (tab-bg . "#272822")
    (tab-border . "#3e3d32")
    (tab-active-bg . "#49483e")
    (tab-active-fg . "#f8f8f2")
    (tab-inactive-bg . "#272822")
    (tab-inactive-fg . "#75715e")))

;;; ============================================================================
;;; Theme: Gruvbox Dark
;;; ============================================================================

(def theme-gruvbox-dark
  '((default . (fg: "#ebdbb2" bg: "#282828"))
    (region . (bg: "#504945"))

    ;; Syntax highlighting (Gruvbox dark palette)
    (font-lock-keyword-face . (fg: "#fb4934" bold: #t))  ; bright red
    (font-lock-builtin-face . (fg: "#83a598"))           ; bright blue
    (font-lock-string-face . (fg: "#b8bb26"))            ; bright green
    (font-lock-comment-face . (fg: "#928374" italic: #t)) ; gray
    (font-lock-number-face . (fg: "#d3869b"))            ; bright purple
    (font-lock-operator-face . (fg: "#fe8019"))          ; bright orange
    (font-lock-type-face . (fg: "#fabd2f"))              ; bright yellow
    (font-lock-preprocessor-face . (fg: "#8ec07c"))      ; bright aqua
    (font-lock-heading-face . (fg: "#83a598" bold: #t))  ; bright blue

    ;; UI elements
    (modeline . (fg: "#ebdbb2" bg: "#3c3836"))
    (modeline-inactive . (fg: "#928374" bg: "#3c3836"))
    (line-number . (fg: "#928374" bg: "#282828"))
    (cursor-line . (bg: "#3c3836"))

    ;; Search and matching
    (match . (fg: "#282828" bg: "#fabd2f" bold: #t))
    (mismatch . (fg: "#fb4934" bg: "#504945" bold: #t))
    (isearch . (fg: "#282828" bg: "#fe8019"))
    (error . (fg: "#fb4934"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#83a598" bold: #t))
    (org-heading-2 . (fg: "#b8bb26" bold: #t))
    (org-heading-3 . (fg: "#fabd2f" bold: #t))
    (org-heading-4 . (fg: "#fb4934" bold: #t))
    (org-heading-5 . (fg: "#d3869b" bold: #t))
    (org-heading-6 . (fg: "#fe8019" bold: #t))
    (org-heading-7 . (fg: "#8ec07c" bold: #t))
    (org-heading-8 . (fg: "#83a598" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#fb4934" bold: #t))
    (org-done . (fg: "#b8bb26" bold: #t))
    (org-link . (fg: "#83a598" underline: #t))
    (org-code . (fg: "#8ec07c"))
    (org-verbatim . (fg: "#8ec07c"))
    (org-table . (fg: "#8ec07c"))
    (org-comment . (fg: "#928374" italic: #t))
    (org-tag . (fg: "#d3869b"))
    (org-date . (fg: "#d3869b"))
    (org-property . (fg: "#928374"))
    (org-block-delimiter . (fg: "#fe8019"))
    (org-block-body . (fg: "#928374"))

    ;; Tab bar
    (tab-active . (fg: "#ebdbb2" bg: "#504945"))
    (tab-inactive . (fg: "#928374" bg: "#282828"))

    ;; Window borders
    (window-border-active . (fg: "#83a598"))
    (window-border-inactive . (fg: "#504945"))

    ;; Legacy UI chrome keys
    (bg . "#282828")
    (fg . "#ebdbb2")
    (selection . "#504945")
    (modeline-bg . "#3c3836")
    (modeline-fg . "#ebdbb2")
    (echo-bg . "#3c3836")
    (echo-fg . "#ebdbb2")
    (gutter-bg . "#282828")
    (gutter-fg . "#928374")
    (split . "#3c3836")
    (tab-bg . "#282828")
    (tab-border . "#3c3836")
    (tab-active-bg . "#504945")
    (tab-active-fg . "#ebdbb2")
    (tab-inactive-bg . "#282828")
    (tab-inactive-fg . "#928374")))

;;; ============================================================================
;;; Theme: Gruvbox Light
;;; ============================================================================

(def theme-gruvbox-light
  '((default . (fg: "#3c3836" bg: "#fbf1c7"))
    (region . (bg: "#ebdbb2"))

    ;; Syntax highlighting (Gruvbox light palette)
    (font-lock-keyword-face . (fg: "#9d0006" bold: #t))
    (font-lock-builtin-face . (fg: "#076678"))
    (font-lock-string-face . (fg: "#79740e"))
    (font-lock-comment-face . (fg: "#928374" italic: #t))
    (font-lock-number-face . (fg: "#8f3f71"))
    (font-lock-operator-face . (fg: "#af3a03"))
    (font-lock-type-face . (fg: "#b57614"))
    (font-lock-preprocessor-face . (fg: "#427b58"))
    (font-lock-heading-face . (fg: "#076678" bold: #t))

    ;; UI elements
    (modeline . (fg: "#3c3836" bg: "#ebdbb2"))
    (modeline-inactive . (fg: "#928374" bg: "#ebdbb2"))
    (line-number . (fg: "#928374" bg: "#fbf1c7"))
    (cursor-line . (bg: "#ebdbb2"))

    ;; Search and matching
    (match . (fg: "#fbf1c7" bg: "#b57614" bold: #t))
    (mismatch . (fg: "#9d0006" bg: "#ebdbb2" bold: #t))
    (isearch . (fg: "#fbf1c7" bg: "#af3a03"))
    (error . (fg: "#9d0006"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#076678" bold: #t))
    (org-heading-2 . (fg: "#79740e" bold: #t))
    (org-heading-3 . (fg: "#b57614" bold: #t))
    (org-heading-4 . (fg: "#9d0006" bold: #t))
    (org-heading-5 . (fg: "#8f3f71" bold: #t))
    (org-heading-6 . (fg: "#af3a03" bold: #t))
    (org-heading-7 . (fg: "#427b58" bold: #t))
    (org-heading-8 . (fg: "#076678" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#9d0006" bold: #t))
    (org-done . (fg: "#79740e" bold: #t))
    (org-link . (fg: "#076678" underline: #t))
    (org-code . (fg: "#427b58"))
    (org-verbatim . (fg: "#427b58"))
    (org-table . (fg: "#427b58"))
    (org-comment . (fg: "#928374" italic: #t))
    (org-tag . (fg: "#8f3f71"))
    (org-date . (fg: "#8f3f71"))
    (org-property . (fg: "#928374"))
    (org-block-delimiter . (fg: "#af3a03"))
    (org-block-body . (fg: "#928374"))

    ;; Tab bar
    (tab-active . (fg: "#3c3836" bg: "#ebdbb2"))
    (tab-inactive . (fg: "#928374" bg: "#fbf1c7"))

    ;; Window borders
    (window-border-active . (fg: "#076678"))
    (window-border-inactive . (fg: "#ebdbb2"))

    ;; Legacy UI chrome keys
    (bg . "#fbf1c7")
    (fg . "#3c3836")
    (selection . "#ebdbb2")
    (modeline-bg . "#ebdbb2")
    (modeline-fg . "#3c3836")
    (echo-bg . "#ebdbb2")
    (echo-fg . "#3c3836")
    (gutter-bg . "#fbf1c7")
    (gutter-fg . "#928374")
    (split . "#ebdbb2")
    (tab-bg . "#fbf1c7")
    (tab-border . "#ebdbb2")
    (tab-active-bg . "#ebdbb2")
    (tab-active-fg . "#3c3836")
    (tab-inactive-bg . "#fbf1c7")
    (tab-inactive-fg . "#928374")))

;;; ============================================================================
;;; Theme: Dracula
;;; ============================================================================

(def theme-dracula
  '((default . (fg: "#f8f8f2" bg: "#282a36"))
    (region . (bg: "#44475a"))

    ;; Syntax highlighting (Dracula palette)
    (font-lock-keyword-face . (fg: "#ff79c6" bold: #t))  ; pink
    (font-lock-builtin-face . (fg: "#8be9fd"))           ; cyan
    (font-lock-string-face . (fg: "#f1fa8c"))            ; yellow
    (font-lock-comment-face . (fg: "#6272a4" italic: #t)) ; comment
    (font-lock-number-face . (fg: "#bd93f9"))            ; purple
    (font-lock-operator-face . (fg: "#ff79c6"))          ; pink
    (font-lock-type-face . (fg: "#50fa7b"))              ; green
    (font-lock-preprocessor-face . (fg: "#ffb86c"))      ; orange
    (font-lock-heading-face . (fg: "#8be9fd" bold: #t))  ; cyan

    ;; UI elements
    (modeline . (fg: "#f8f8f2" bg: "#44475a"))
    (modeline-inactive . (fg: "#6272a4" bg: "#44475a"))
    (line-number . (fg: "#6272a4" bg: "#282a36"))
    (cursor-line . (bg: "#44475a"))

    ;; Search and matching
    (match . (fg: "#282a36" bg: "#f1fa8c" bold: #t))
    (mismatch . (fg: "#ff5555" bg: "#44475a" bold: #t))
    (isearch . (fg: "#282a36" bg: "#ffb86c"))
    (error . (fg: "#ff5555"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#8be9fd" bold: #t))
    (org-heading-2 . (fg: "#50fa7b" bold: #t))
    (org-heading-3 . (fg: "#f1fa8c" bold: #t))
    (org-heading-4 . (fg: "#ff79c6" bold: #t))
    (org-heading-5 . (fg: "#bd93f9" bold: #t))
    (org-heading-6 . (fg: "#ffb86c" bold: #t))
    (org-heading-7 . (fg: "#8be9fd" bold: #t))
    (org-heading-8 . (fg: "#50fa7b" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#ff5555" bold: #t))
    (org-done . (fg: "#50fa7b" bold: #t))
    (org-link . (fg: "#8be9fd" underline: #t))
    (org-code . (fg: "#50fa7b"))
    (org-verbatim . (fg: "#8be9fd"))
    (org-table . (fg: "#8be9fd"))
    (org-comment . (fg: "#6272a4" italic: #t))
    (org-tag . (fg: "#bd93f9"))
    (org-date . (fg: "#bd93f9"))
    (org-property . (fg: "#6272a4"))
    (org-block-delimiter . (fg: "#ffb86c"))
    (org-block-body . (fg: "#6272a4"))

    ;; Tab bar
    (tab-active . (fg: "#f8f8f2" bg: "#44475a"))
    (tab-inactive . (fg: "#6272a4" bg: "#282a36"))

    ;; Window borders
    (window-border-active . (fg: "#bd93f9"))
    (window-border-inactive . (fg: "#44475a"))

    ;; Legacy UI chrome keys
    (bg . "#282a36")
    (fg . "#f8f8f2")
    (selection . "#44475a")
    (modeline-bg . "#44475a")
    (modeline-fg . "#f8f8f2")
    (echo-bg . "#44475a")
    (echo-fg . "#f8f8f2")
    (gutter-bg . "#282a36")
    (gutter-fg . "#6272a4")
    (split . "#44475a")
    (tab-bg . "#282a36")
    (tab-border . "#44475a")
    (tab-active-bg . "#44475a")
    (tab-active-fg . "#f8f8f2")
    (tab-inactive-bg . "#282a36")
    (tab-inactive-fg . "#6272a4")))

;;; ============================================================================
;;; Theme: Nord
;;; ============================================================================

(def theme-nord
  '((default . (fg: "#d8dee9" bg: "#2e3440"))
    (region . (bg: "#434c5e"))

    ;; Syntax highlighting (Nord palette)
    (font-lock-keyword-face . (fg: "#81a1c1" bold: #t))
    (font-lock-builtin-face . (fg: "#88c0d0"))
    (font-lock-string-face . (fg: "#a3be8c"))
    (font-lock-comment-face . (fg: "#616e88" italic: #t))
    (font-lock-number-face . (fg: "#b48ead"))
    (font-lock-operator-face . (fg: "#81a1c1"))
    (font-lock-type-face . (fg: "#8fbcbb"))
    (font-lock-preprocessor-face . (fg: "#d08770"))
    (font-lock-heading-face . (fg: "#88c0d0" bold: #t))

    ;; UI elements
    (modeline . (fg: "#d8dee9" bg: "#3b4252"))
    (modeline-inactive . (fg: "#616e88" bg: "#3b4252"))
    (line-number . (fg: "#616e88" bg: "#2e3440"))
    (cursor-line . (bg: "#3b4252"))

    ;; Search and matching
    (match . (fg: "#2e3440" bg: "#ebcb8b" bold: #t))
    (mismatch . (fg: "#bf616a" bg: "#434c5e" bold: #t))
    (isearch . (fg: "#2e3440" bg: "#d08770"))
    (error . (fg: "#bf616a"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#88c0d0" bold: #t))
    (org-heading-2 . (fg: "#a3be8c" bold: #t))
    (org-heading-3 . (fg: "#ebcb8b" bold: #t))
    (org-heading-4 . (fg: "#81a1c1" bold: #t))
    (org-heading-5 . (fg: "#b48ead" bold: #t))
    (org-heading-6 . (fg: "#d08770" bold: #t))
    (org-heading-7 . (fg: "#8fbcbb" bold: #t))
    (org-heading-8 . (fg: "#88c0d0" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#bf616a" bold: #t))
    (org-done . (fg: "#a3be8c" bold: #t))
    (org-link . (fg: "#88c0d0" underline: #t))
    (org-code . (fg: "#8fbcbb"))
    (org-verbatim . (fg: "#8fbcbb"))
    (org-table . (fg: "#8fbcbb"))
    (org-comment . (fg: "#616e88" italic: #t))
    (org-tag . (fg: "#b48ead"))
    (org-date . (fg: "#b48ead"))
    (org-property . (fg: "#616e88"))
    (org-block-delimiter . (fg: "#d08770"))
    (org-block-body . (fg: "#616e88"))

    ;; Tab bar
    (tab-active . (fg: "#d8dee9" bg: "#434c5e"))
    (tab-inactive . (fg: "#616e88" bg: "#2e3440"))

    ;; Window borders
    (window-border-active . (fg: "#88c0d0"))
    (window-border-inactive . (fg: "#434c5e"))

    ;; Legacy UI chrome keys
    (bg . "#2e3440")
    (fg . "#d8dee9")
    (selection . "#434c5e")
    (modeline-bg . "#3b4252")
    (modeline-fg . "#d8dee9")
    (echo-bg . "#3b4252")
    (echo-fg . "#d8dee9")
    (gutter-bg . "#2e3440")
    (gutter-fg . "#616e88")
    (split . "#3b4252")
    (tab-bg . "#2e3440")
    (tab-border . "#3b4252")
    (tab-active-bg . "#434c5e")
    (tab-active-fg . "#d8dee9")
    (tab-inactive-bg . "#2e3440")
    (tab-inactive-fg . "#616e88")))

;;; ============================================================================
;;; Theme: Zenburn
;;; ============================================================================

(def theme-zenburn
  '((default . (fg: "#dcdccc" bg: "#3f3f3f"))
    (region . (bg: "#5f5f5f"))

    ;; Syntax highlighting (Zenburn palette)
    (font-lock-keyword-face . (fg: "#f0dfaf" bold: #t))
    (font-lock-builtin-face . (fg: "#8cd0d3"))
    (font-lock-string-face . (fg: "#cc9393"))
    (font-lock-comment-face . (fg: "#7f9f7f" italic: #t))
    (font-lock-number-face . (fg: "#dca3a3"))
    (font-lock-operator-face . (fg: "#f0dfaf"))
    (font-lock-type-face . (fg: "#dfdfbf"))
    (font-lock-preprocessor-face . (fg: "#ffcfaf"))
    (font-lock-heading-face . (fg: "#8cd0d3" bold: #t))

    ;; UI elements
    (modeline . (fg: "#dcdccc" bg: "#2b2b2b"))
    (modeline-inactive . (fg: "#7f7f7f" bg: "#2b2b2b"))
    (line-number . (fg: "#7f7f7f" bg: "#3f3f3f"))
    (cursor-line . (bg: "#4f4f4f"))

    ;; Search and matching
    (match . (fg: "#3f3f3f" bg: "#f0dfaf" bold: #t))
    (mismatch . (fg: "#dca3a3" bg: "#5f5f5f" bold: #t))
    (isearch . (fg: "#3f3f3f" bg: "#ffcfaf"))
    (error . (fg: "#dca3a3"))

    ;; Org-mode headings
    (org-heading-1 . (fg: "#8cd0d3" bold: #t))
    (org-heading-2 . (fg: "#f0dfaf" bold: #t))
    (org-heading-3 . (fg: "#cc9393" bold: #t))
    (org-heading-4 . (fg: "#dfdfbf" bold: #t))
    (org-heading-5 . (fg: "#93e0e3" bold: #t))
    (org-heading-6 . (fg: "#ffcfaf" bold: #t))
    (org-heading-7 . (fg: "#dca3a3" bold: #t))
    (org-heading-8 . (fg: "#8cd0d3" bold: #t))

    ;; Org-mode elements
    (org-todo . (fg: "#dca3a3" bold: #t))
    (org-done . (fg: "#7f9f7f" bold: #t))
    (org-link . (fg: "#8cd0d3" underline: #t))
    (org-code . (fg: "#93e0e3"))
    (org-verbatim . (fg: "#93e0e3"))
    (org-table . (fg: "#93e0e3"))
    (org-comment . (fg: "#7f9f7f" italic: #t))
    (org-tag . (fg: "#dfdfbf"))
    (org-date . (fg: "#dfdfbf"))
    (org-property . (fg: "#7f7f7f"))
    (org-block-delimiter . (fg: "#ffcfaf"))
    (org-block-body . (fg: "#7f7f7f"))

    ;; Tab bar
    (tab-active . (fg: "#dcdccc" bg: "#5f5f5f"))
    (tab-inactive . (fg: "#7f7f7f" bg: "#3f3f3f"))

    ;; Window borders
    (window-border-active . (fg: "#8cd0d3"))
    (window-border-inactive . (fg: "#5f5f5f"))

    ;; Legacy UI chrome keys
    (bg . "#3f3f3f")
    (fg . "#dcdccc")
    (selection . "#5f5f5f")
    (modeline-bg . "#2b2b2b")
    (modeline-fg . "#dcdccc")
    (echo-bg . "#2b2b2b")
    (echo-fg . "#dcdccc")
    (gutter-bg . "#3f3f3f")
    (gutter-fg . "#7f7f7f")
    (split . "#2b2b2b")
    (tab-bg . "#3f3f3f")
    (tab-border . "#2b2b2b")
    (tab-active-bg . "#5f5f5f")
    (tab-active-fg . "#dcdccc")
    (tab-inactive-bg . "#3f3f3f")
    (tab-inactive-fg . "#7f7f7f")))

;;; ============================================================================
;;; Theme Registry
;;; ============================================================================

;; Global theme registry: theme-name -> face-alist
(def *themes* (make-hash-table-eq))

(def (register-theme! name face-alist)
  "Register a theme by name. FACE-ALIST is an alist of (face-name . face-props)."
  (hash-put! *themes* name face-alist))

(def (theme-get name)
  "Look up a theme by name. Returns the face alist or #f if not found."
  (hash-get *themes* name))

(def (theme-names)
  "Return a list of all registered theme names."
  (hash-keys *themes*))

;; Register all built-in themes
(register-theme! 'dark theme-dark)
(register-theme! 'light theme-light)
(register-theme! 'solarized-dark theme-solarized-dark)
(register-theme! 'solarized-light theme-solarized-light)
(register-theme! 'monokai theme-monokai)
(register-theme! 'gruvbox-dark theme-gruvbox-dark)
(register-theme! 'gruvbox-light theme-gruvbox-light)
(register-theme! 'dracula theme-dracula)
(register-theme! 'nord theme-nord)
(register-theme! 'zenburn theme-zenburn)
