# Font Sizes / Per-Style Font Support in Gemacs Qt Editor

## Summary

QScintilla supports per-style font sizes, but it is **not currently implemented** in this project. Only colors, bold, and italic are set per-style. Global font size is adjusted widget-wide.

---

## What QScintilla Can Do

QScintilla uses a **style-based** model. Each style ID can have independent:
- Font family (`SCI_STYLESETFONT` / message 2056)
- Font size (`SCI_STYLESETSIZE` / message 2055)
- Bold, italic, underline
- Foreground/background color

This is similar to Emacs faces, but you assign text to a style number — you can't apply arbitrary font sizes to arbitrary ranges directly.

---

## Current State

| Feature | Status | Location |
|---------|--------|----------|
| Per-style colors | ✅ Used | `qt/highlight.ss:284-612` |
| Per-style bold/italic | ✅ Used | `qt/highlight.ss` |
| Per-style font size | ❌ Not implemented | — |
| Global font size change | ✅ `cmd-increase/decrease-font-size` | `qt/commands-shell.ss:40-58` |

### Global font setup
`qt/window.ss:72-73`:
```scheme
(sci-send/string ed SCI_STYLESETFONT "Monospace" STYLE_DEFAULT)
(sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT 11)
```

### Org heading styles (color + bold, no size)
`qt/highlight.ss:555-612` (`qt-setup-org-styles!`):
```scheme
(sci-send ed SCI_STYLESETFORE style (car cs))
(sci-send ed SCI_STYLESETBOLD style 1)
;; SCI_STYLESETSIZE never called here
```

Org heading styles use IDs 33–40.

---

## How to Add Per-Style Font Sizes

The infrastructure is already in place. Example — make org headings progressively larger:

```scheme
;; In qt/highlight.ss, inside qt-setup-org-styles!
(sci-send ed SCI_STYLESETSIZE 33 16)  ;; h1: 16pt
(sci-send ed SCI_STYLESETSIZE 34 14)  ;; h2: 14pt
(sci-send ed SCI_STYLESETSIZE 35 13)  ;; h3: 13pt
```

`SCI_STYLESETSIZE` (2055) is already available from `:gerbil-scintilla/constants`.

**Caveat:** The global font-size commands (`cmd-increase-font-size` etc.) call `qt-widget-set-font-size!` which resets the whole widget. If per-style sizes are added, those commands would need to scale them proportionally.

---

## Limitation vs Emacs

Emacs can apply faces to arbitrary text ranges via overlays or text properties. QScintilla's style system is primarily designed for **syntax highlighting** — regions get their style from the lexer. Ad-hoc per-region font size (like `text-scale-increase` on a selection) would require custom indicator/annotation hacks.

Common use cases (bigger org headings, distinct fonts for doc comments, etc.) are fully achievable with `SCI_STYLESETSIZE`.
