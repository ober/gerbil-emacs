;;; -*- Gerbil -*-
;;; VT100 virtual terminal screen buffer — libvterm backend.
;;;
;;; Uses libvterm (via begin-ffi / c-declare + -lvterm link) for complete
;;; VT100/VT220/xterm terminal emulation with:
;;;   - C-speed byte parsing (no per-byte Scheme overhead)
;;;   - Full SGR color support (16/256/RGB per-cell)
;;;   - Proper alt screen buffer with save/restore
;;;   - Unicode + combining characters + wide chars
;;;   - Scrollback ring buffer (10,000 lines)
;;;   - Row-level damage tracking for efficient rendering
;;;   - Resize with line reflow
;;;
;;; API is backwards-compatible with the old pure-Scheme vtscreen.

(export new-vtscreen
        vtscreen-feed!
        vtscreen-render
        vtscreen-resize!
        vtscreen-rows
        vtscreen-cols
        vtscreen-cursor-row
        vtscreen-cursor-col
        vtscreen-alt-screen?
        ;; Row-level damage tracking for batched updates
        vtscreen-has-damage?
        vtscreen-row-dirty?
        vtscreen-clear-damage!
        vtscreen-mark-all-dirty!
        ;; Per-row text extraction
        vtscreen-get-row-text
        ;; Scrollback access
        vtscreen-scrollback-len
        vtscreen-scrollback-line
        vtscreen-scrollback-clear!
        ;; Per-cell color queries (packed 0x00RRGGBB, -1 = default)
        vtscreen-cell-fg
        vtscreen-cell-bg
        vtscreen-cell-attrs
        ;; Free resources
        vtscreen-free!)

(import :std/foreign
        :std/sugar)

;;;============================================================================
;;; FFI: libvterm via begin-ffi (compiled C code + -lvterm link flag)
;;;============================================================================

(begin-ffi (ffi-jvt-new
            ffi-jvt-free
            ffi-jvt-write
            ffi-jvt-resize
            ffi-jvt-get-row-text
            ffi-jvt-get-text
            ffi-jvt-is-altscreen
            ffi-jvt-get-rows
            ffi-jvt-get-cols
            ffi-jvt-get-cursor-row
            ffi-jvt-get-cursor-col
            ffi-jvt-has-damage
            ffi-jvt-row-dirty
            ffi-jvt-clear-damage
            ffi-jvt-mark-all-dirty
            ffi-jvt-get-cell-fg
            ffi-jvt-get-cell-bg
            ffi-jvt-get-cell-attrs
            ffi-jvt-scrollback-len
            ffi-jvt-scrollback-line
            ffi-jvt-scrollback-clear)

  (c-declare #<<END-C

/*
 * vterm_shim — Thin C wrapper bridging libvterm to Gambit FFI.
 *
 * Wraps libvterm's VTerm/VTermScreen with:
 *   - Scrollback ring buffer (sb_pushline/sb_popline callbacks)
 *   - Row-level damage tracking (dirty bitmask)
 *   - Alt-screen detection
 *   - Per-row text extraction (UTF-8)
 *   - Per-cell color + attribute queries
 */

#include <vterm.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ============================================================
 * Scrollback ring buffer
 * ============================================================ */

#define JVT_DEFAULT_SCROLLBACK 10000

typedef struct {
    char *text;
    int   text_len;
} JvtScrollLine;

typedef struct {
    JvtScrollLine *lines;
    int            max_lines;
    int            count;
    int            head;
} JvtScrollback;

static void scrollback_init(JvtScrollback *sb, int max_lines) {
    sb->max_lines = max_lines;
    sb->count = 0;
    sb->head = 0;
    sb->lines = (JvtScrollLine *)calloc(max_lines, sizeof(JvtScrollLine));
}

static void scrollback_free(JvtScrollback *sb) {
    if (!sb->lines) return;
    for (int i = 0; i < sb->max_lines; i++) free(sb->lines[i].text);
    free(sb->lines);
    sb->lines = NULL;
}

static void scrollback_push(JvtScrollback *sb, const char *text, int len) {
    int idx;
    if (sb->count < sb->max_lines) {
        idx = (sb->head + sb->count) % sb->max_lines;
        sb->count++;
    } else {
        idx = sb->head;
        sb->head = (sb->head + 1) % sb->max_lines;
        free(sb->lines[idx].text);
    }
    sb->lines[idx].text = (char *)malloc(len + 1);
    memcpy(sb->lines[idx].text, text, len);
    sb->lines[idx].text[len] = '\0';
    sb->lines[idx].text_len = len;
}

static int scrollback_pop(JvtScrollback *sb, char *buf, int buflen) {
    if (sb->count == 0) return -1;
    sb->count--;
    int idx = (sb->head + sb->count) % sb->max_lines;
    int len = sb->lines[idx].text_len;
    if (buf && buflen > 0) {
        int copy = len < buflen ? len : buflen - 1;
        memcpy(buf, sb->lines[idx].text, copy);
        buf[copy] = '\0';
    }
    free(sb->lines[idx].text);
    sb->lines[idx].text = NULL;
    sb->lines[idx].text_len = 0;
    return len;
}

static JvtScrollLine *scrollback_get(JvtScrollback *sb, int idx) {
    if (idx < 0 || idx >= sb->count) return NULL;
    int ring_idx = (sb->head + sb->count - 1 - idx) % sb->max_lines;
    return &sb->lines[ring_idx];
}

/* ============================================================
 * Main JVT wrapper struct
 * ============================================================ */

typedef struct {
    VTerm       *vt;
    VTermScreen *screen;
    int          rows;
    int          cols;
    uint8_t     *dirty_rows;
    int          any_damage;
    JvtScrollback scrollback;
    int          is_altscreen;
    char        *text_buf;
    int          text_buf_size;
} JvtState;

/* ============================================================
 * libvterm screen callbacks
 * ============================================================ */

static int jvt_cb_damage(VTermRect rect, void *user) {
    JvtState *st = (JvtState *)user;
    for (int r = rect.start_row; r < rect.end_row && r < st->rows; r++)
        st->dirty_rows[r] = 1;
    st->any_damage = 1;
    return 0;
}

static int jvt_cb_sb_pushline(int cols, const VTermScreenCell *cells, void *user) {
    JvtState *st = (JvtState *)user;
    int max_len = cols * VTERM_MAX_CHARS_PER_CELL * 4;
    char *buf = (char *)malloc(max_len + 1);
    int pos = 0;
    for (int c = 0; c < cols; c++) {
        for (int ci = 0; ci < VTERM_MAX_CHARS_PER_CELL && cells[c].chars[ci]; ci++) {
            uint32_t cp = cells[c].chars[ci];
            if (cp < 0x80) {
                buf[pos++] = (char)cp;
            } else if (cp < 0x800) {
                buf[pos++] = 0xC0 | (cp >> 6);
                buf[pos++] = 0x80 | (cp & 0x3F);
            } else if (cp < 0x10000) {
                buf[pos++] = 0xE0 | (cp >> 12);
                buf[pos++] = 0x80 | ((cp >> 6) & 0x3F);
                buf[pos++] = 0x80 | (cp & 0x3F);
            } else {
                buf[pos++] = 0xF0 | (cp >> 18);
                buf[pos++] = 0x80 | ((cp >> 12) & 0x3F);
                buf[pos++] = 0x80 | ((cp >> 6) & 0x3F);
                buf[pos++] = 0x80 | (cp & 0x3F);
            }
        }
        if (cells[c].chars[0] == 0) buf[pos++] = ' ';
    }
    while (pos > 0 && buf[pos - 1] == ' ') pos--;
    buf[pos] = '\0';
    scrollback_push(&st->scrollback, buf, pos);
    free(buf);
    return 0;
}

static int jvt_cb_sb_popline(int cols, VTermScreenCell *cells, void *user) {
    JvtState *st = (JvtState *)user;
    if (st->scrollback.count == 0) return 0;
    for (int c = 0; c < cols; c++) {
        memset(&cells[c], 0, sizeof(VTermScreenCell));
        cells[c].chars[0] = ' ';
        cells[c].width = 1;
    }
    char tmpbuf[8192];
    int len = scrollback_pop(&st->scrollback, tmpbuf, sizeof(tmpbuf));
    if (len > 0) {
        int c = 0, i = 0;
        while (i < len && c < cols) {
            unsigned char b = (unsigned char)tmpbuf[i];
            uint32_t cp; int bytes;
            if (b < 0x80) { cp = b; bytes = 1; }
            else if (b < 0xE0) { cp = b & 0x1F; bytes = 2; }
            else if (b < 0xF0) { cp = b & 0x0F; bytes = 3; }
            else { cp = b & 0x07; bytes = 4; }
            for (int j = 1; j < bytes && (i + j) < len; j++)
                cp = (cp << 6) | (tmpbuf[i + j] & 0x3F);
            cells[c].chars[0] = cp; cells[c].width = 1;
            i += bytes; c++;
        }
    }
    return 1;
}

static int jvt_cb_settermprop(VTermProp prop, VTermValue *val, void *user) {
    JvtState *st = (JvtState *)user;
    if (prop == VTERM_PROP_ALTSCREEN) st->is_altscreen = val->boolean;
    return 1;
}

static int jvt_cb_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    (void)pos; (void)oldpos; (void)visible; (void)user; return 1;
}

static int jvt_cb_bell(void *user) { (void)user; return 0; }
static int jvt_cb_resize(int rows, int cols, void *user) {
    (void)rows; (void)cols; (void)user; return 1;
}

static VTermScreenCallbacks jvt_screen_cbs = {
    .damage      = jvt_cb_damage,
    .moverect    = NULL,
    .movecursor  = jvt_cb_movecursor,
    .settermprop = jvt_cb_settermprop,
    .bell        = jvt_cb_bell,
    .resize      = jvt_cb_resize,
    .sb_pushline = jvt_cb_sb_pushline,
    .sb_popline  = jvt_cb_sb_popline,
    .sb_clear    = NULL,
};

/* ============================================================
 * Public API
 * ============================================================ */

static void *jvt_new(int rows, int cols) {
    JvtState *st = (JvtState *)calloc(1, sizeof(JvtState));
    if (!st) return NULL;
    st->vt = vterm_new(rows, cols);
    if (!st->vt) { free(st); return NULL; }
    vterm_set_utf8(st->vt, 1);
    st->screen = vterm_obtain_screen(st->vt);
    st->rows = rows; st->cols = cols;
    st->dirty_rows = (uint8_t *)calloc(rows, 1);
    st->any_damage = 0;
    scrollback_init(&st->scrollback, JVT_DEFAULT_SCROLLBACK);
    st->text_buf_size = cols * 4 + 16;
    st->text_buf = (char *)malloc(st->text_buf_size);
    vterm_screen_set_callbacks(st->screen, &jvt_screen_cbs, st);
    vterm_screen_enable_altscreen(st->screen, 1);
    vterm_screen_enable_reflow(st->screen, 1);
    vterm_screen_set_damage_merge(st->screen, VTERM_DAMAGE_ROW);
    vterm_screen_reset(st->screen, 1);
    return st;
}

static void jvt_free(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return;
    scrollback_free(&st->scrollback);
    free(st->dirty_rows);
    free(st->text_buf);
    if (st->vt) vterm_free(st->vt);
    free(st);
}

static void jvt_write(void *handle, const char *data, int len) {
    JvtState *st = (JvtState *)handle;
    if (!st || !data || len <= 0) return;
    vterm_input_write(st->vt, data, (size_t)len);
    vterm_screen_flush_damage(st->screen);
}

static void jvt_resize(void *handle, int rows, int cols) {
    JvtState *st = (JvtState *)handle;
    if (!st || rows <= 0 || cols <= 0) return;
    vterm_set_size(st->vt, rows, cols);
    free(st->dirty_rows);
    st->dirty_rows = (uint8_t *)calloc(rows, 1);
    memset(st->dirty_rows, 1, rows);
    st->any_damage = 1;
    st->rows = rows; st->cols = cols;
    free(st->text_buf);
    st->text_buf_size = cols * 4 + 16;
    st->text_buf = (char *)malloc(st->text_buf_size);
}

static int jvt_get_row_text(void *handle, int row, char *buf, int buflen) {
    JvtState *st = (JvtState *)handle;
    if (!st || row < 0 || row >= st->rows || !buf || buflen <= 0) return -1;
    VTermRect rect = { .start_row = row, .end_row = row + 1,
                       .start_col = 0,   .end_col = st->cols };
    size_t n = vterm_screen_get_text(st->screen, buf, (size_t)buflen - 1, rect);
    buf[n] = '\0';
    while (n > 0 && buf[n - 1] == ' ') { n--; buf[n] = '\0'; }
    return (int)n;
}

static int jvt_get_text(void *handle, char *buf, int buflen, int start_row, int end_row) {
    JvtState *st = (JvtState *)handle;
    if (!st || !buf || buflen <= 0) return -1;
    if (start_row < 0) start_row = 0;
    if (end_row > st->rows) end_row = st->rows;
    int row_buf_size = st->cols * 4 + 4;
    char *row_buf = st->text_buf;
    if (row_buf_size > st->text_buf_size) row_buf = (char *)malloc(row_buf_size);
    int last_nonempty = start_row - 1;
    for (int r = end_row - 1; r >= start_row; r--) {
        VTermRect rect = { .start_row = r, .end_row = r + 1,
                           .start_col = 0, .end_col = st->cols };
        size_t n = vterm_screen_get_text(st->screen, row_buf, (size_t)row_buf_size - 1, rect);
        while (n > 0 && row_buf[n - 1] == ' ') n--;
        if (n > 0) { last_nonempty = r; break; }
    }
    if (last_nonempty < start_row) {
        buf[0] = '\0';
        if (row_buf != st->text_buf) free(row_buf);
        return 0;
    }
    int pos = 0;
    for (int r = start_row; r <= last_nonempty; r++) {
        if (r > start_row && pos < buflen - 1) buf[pos++] = '\n';
        VTermRect rect = { .start_row = r, .end_row = r + 1,
                           .start_col = 0, .end_col = st->cols };
        size_t n = vterm_screen_get_text(st->screen, row_buf, (size_t)row_buf_size - 1, rect);
        row_buf[n] = '\0';
        while (n > 0 && row_buf[n - 1] == ' ') n--;
        int copy = (int)n;
        if (pos + copy >= buflen) copy = buflen - pos - 1;
        if (copy > 0) { memcpy(buf + pos, row_buf, copy); pos += copy; }
    }
    if (row_buf != st->text_buf) free(row_buf);
    buf[pos] = '\0';
    return pos;
}

static int jvt_is_altscreen(void *handle) {
    JvtState *st = (JvtState *)handle;
    return st ? st->is_altscreen : 0;
}

static int jvt_get_rows(void *handle) {
    return handle ? ((JvtState *)handle)->rows : 0;
}

static int jvt_get_cols(void *handle) {
    return handle ? ((JvtState *)handle)->cols : 0;
}

static int jvt_get_cursor_row(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return 0;
    VTermPos pos;
    vterm_state_get_cursorpos(vterm_obtain_state(st->vt), &pos);
    return pos.row;
}

static int jvt_get_cursor_col(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return 0;
    VTermPos pos;
    vterm_state_get_cursorpos(vterm_obtain_state(st->vt), &pos);
    return pos.col;
}

static int jvt_has_damage(void *handle) {
    return handle ? ((JvtState *)handle)->any_damage : 0;
}

static int jvt_row_dirty(void *handle, int row) {
    JvtState *st = (JvtState *)handle;
    if (!st || row < 0 || row >= st->rows) return 0;
    return st->dirty_rows[row];
}

static void jvt_clear_damage(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return;
    memset(st->dirty_rows, 0, st->rows);
    st->any_damage = 0;
}

static void jvt_mark_all_dirty(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return;
    memset(st->dirty_rows, 1, st->rows);
    st->any_damage = 1;
}

static int jvt_get_cell_fg(void *handle, int row, int col) {
    JvtState *st = (JvtState *)handle;
    if (!st || row < 0 || row >= st->rows || col < 0 || col >= st->cols) return -1;
    VTermPos pos = { .row = row, .col = col };
    VTermScreenCell cell;
    vterm_screen_get_cell(st->screen, pos, &cell);
    if (VTERM_COLOR_IS_DEFAULT_FG(&cell.fg)) return -1;
    if (VTERM_COLOR_IS_INDEXED(&cell.fg))
        vterm_screen_convert_color_to_rgb(st->screen, &cell.fg);
    return (cell.fg.rgb.red << 16) | (cell.fg.rgb.green << 8) | cell.fg.rgb.blue;
}

static int jvt_get_cell_bg(void *handle, int row, int col) {
    JvtState *st = (JvtState *)handle;
    if (!st || row < 0 || row >= st->rows || col < 0 || col >= st->cols) return -1;
    VTermPos pos = { .row = row, .col = col };
    VTermScreenCell cell;
    vterm_screen_get_cell(st->screen, pos, &cell);
    if (VTERM_COLOR_IS_DEFAULT_BG(&cell.bg)) return -1;
    if (VTERM_COLOR_IS_INDEXED(&cell.bg))
        vterm_screen_convert_color_to_rgb(st->screen, &cell.bg);
    return (cell.bg.rgb.red << 16) | (cell.bg.rgb.green << 8) | cell.bg.rgb.blue;
}

static int jvt_get_cell_attrs(void *handle, int row, int col) {
    JvtState *st = (JvtState *)handle;
    if (!st || row < 0 || row >= st->rows || col < 0 || col >= st->cols) return 0;
    VTermPos pos = { .row = row, .col = col };
    VTermScreenCell cell;
    vterm_screen_get_cell(st->screen, pos, &cell);
    int attrs = 0;
    if (cell.attrs.bold)      attrs |= (1 << 0);
    if (cell.attrs.underline) attrs |= (1 << 1);
    if (cell.attrs.italic)    attrs |= (1 << 2);
    if (cell.attrs.blink)     attrs |= (1 << 3);
    if (cell.attrs.reverse)   attrs |= (1 << 4);
    if (cell.attrs.strike)    attrs |= (1 << 5);
    if (cell.attrs.conceal)   attrs |= (1 << 6);
    return attrs;
}

static int jvt_scrollback_len(void *handle) {
    return handle ? ((JvtState *)handle)->scrollback.count : 0;
}

static int jvt_scrollback_line(void *handle, int idx, char *buf, int buflen) {
    JvtState *st = (JvtState *)handle;
    if (!st || !buf || buflen <= 0) return -1;
    JvtScrollLine *line = scrollback_get(&st->scrollback, idx);
    if (!line || !line->text) { buf[0] = '\0'; return 0; }
    int copy = line->text_len < buflen - 1 ? line->text_len : buflen - 1;
    memcpy(buf, line->text, copy);
    buf[copy] = '\0';
    return copy;
}

static void jvt_scrollback_clear(void *handle) {
    JvtState *st = (JvtState *)handle;
    if (!st) return;
    scrollback_free(&st->scrollback);
    scrollback_init(&st->scrollback, JVT_DEFAULT_SCROLLBACK);
}

END-C
  )

  ;; Core lifecycle
  (define-c-lambda ffi-jvt-new (int int) (pointer void)
    "jvt_new")
  (define-c-lambda ffi-jvt-free ((pointer void)) void
    "jvt_free")
  ;; Write: take u8vector body via scheme-object
  (define-c-lambda ffi-jvt-write ((pointer void) scheme-object int) void
    "jvt_write(___arg1, ___CAST(const char*, ___BODY(___arg2)), ___arg3);")
  (define-c-lambda ffi-jvt-resize ((pointer void) int int) void
    "jvt_resize")
  ;; Text extraction: output into pre-allocated u8vector
  (define-c-lambda ffi-jvt-get-row-text ((pointer void) int scheme-object int) int
    "___return(jvt_get_row_text(___arg1, ___arg2, ___CAST(char*, ___BODY(___arg3)), ___arg4));")
  (define-c-lambda ffi-jvt-get-text ((pointer void) scheme-object int int int) int
    "___return(jvt_get_text(___arg1, ___CAST(char*, ___BODY(___arg2)), ___arg3, ___arg4, ___arg5));")
  ;; State queries
  (define-c-lambda ffi-jvt-is-altscreen ((pointer void)) int
    "jvt_is_altscreen")
  (define-c-lambda ffi-jvt-get-rows ((pointer void)) int
    "jvt_get_rows")
  (define-c-lambda ffi-jvt-get-cols ((pointer void)) int
    "jvt_get_cols")
  (define-c-lambda ffi-jvt-get-cursor-row ((pointer void)) int
    "jvt_get_cursor_row")
  (define-c-lambda ffi-jvt-get-cursor-col ((pointer void)) int
    "jvt_get_cursor_col")
  ;; Damage tracking
  (define-c-lambda ffi-jvt-has-damage ((pointer void)) int
    "jvt_has_damage")
  (define-c-lambda ffi-jvt-row-dirty ((pointer void) int) int
    "jvt_row_dirty")
  (define-c-lambda ffi-jvt-clear-damage ((pointer void)) void
    "jvt_clear_damage")
  (define-c-lambda ffi-jvt-mark-all-dirty ((pointer void)) void
    "jvt_mark_all_dirty")
  ;; Per-cell colors
  (define-c-lambda ffi-jvt-get-cell-fg ((pointer void) int int) int
    "jvt_get_cell_fg")
  (define-c-lambda ffi-jvt-get-cell-bg ((pointer void) int int) int
    "jvt_get_cell_bg")
  (define-c-lambda ffi-jvt-get-cell-attrs ((pointer void) int int) int
    "jvt_get_cell_attrs")
  ;; Scrollback
  (define-c-lambda ffi-jvt-scrollback-len ((pointer void)) int
    "jvt_scrollback_len")
  (define-c-lambda ffi-jvt-scrollback-line ((pointer void) int scheme-object int) int
    "___return(jvt_scrollback_line(___arg1, ___arg2, ___CAST(char*, ___BODY(___arg3)), ___arg4));")
  (define-c-lambda ffi-jvt-scrollback-clear ((pointer void)) void
    "jvt_scrollback_clear")
)

;;;============================================================================
;;; Vtscreen wrapper — opaque handle
;;;============================================================================

;; The vtscreen is a box holding the void* handle from jvt_new.
;; We use a defstruct so the handle can be set to #f after free.

(defstruct vtscreen
  (handle)   ; (pointer void) — the JvtState pointer, or #f if freed
  transparent: #t)

(def (new-vtscreen (rows 24) (cols 80))
  "Create a new virtual terminal screen backed by libvterm."
  (let ((h (ffi-jvt-new rows cols)))
    (if h
      (make-vtscreen h)
      (error "new-vtscreen: jvt_new failed"))))

(def (vtscreen-free! vt)
  "Free the libvterm resources."
  (let ((h (vtscreen-handle vt)))
    (when h
      (ffi-jvt-free h)
      (set! (vtscreen-handle vt) #f))))

;;;============================================================================
;;; Core API (backwards-compatible)
;;;============================================================================

(def (vtscreen-normalize-input data)
  "Pre-process terminal data before feeding to libvterm (UTF-8 mode).

   Two normalizations applied:
   1. Bare LF (not preceded by CR) → CR+LF.
      libvterm in UTF-8 mode treats \\n as pure linefeed (cursor col unchanged),
      but apps like dmesg that lack a PTY rely on newline mode (LF implies CR).
   2. C1 control characters (U+0080..U+009F) → 2-char ESC + letter.
      libvterm disables C1 parsing when utf8=1 (it only recognises raw 0x9B etc.
      in 8-bit mode).  Converting U+009B → ESC [ ensures 8-bit CSI sequences
      generated by applications are dispatched correctly."
  (let ((esc (integer->char 27))
        (len (string-length data)))
    (let loop ((i 0) (out '()))
      (if (>= i len)
        (list->string (reverse out))
        (let* ((c  (string-ref data i))
               (cp (char->integer c)))
          (cond
            ;; C1 control U+0080..U+009F → ESC + (cp - 0x40)
            ((and (>= cp #x80) (<= cp #x9f))
             (loop (+ i 1)
                   (cons (integer->char (- cp #x40))
                         (cons esc out))))
            ;; Bare LF without preceding CR → insert CR
            ((and (char=? c #\newline)
                  (or (= i 0)
                      (not (char=? (string-ref data (- i 1)) #\return))))
             (loop (+ i 1) (cons c (cons #\return out))))
            (else
             (loop (+ i 1) (cons c out)))))))))

(def (vtscreen-feed! vt data)
  "Process terminal output through libvterm. data: string from PTY."
  (let ((h (vtscreen-handle vt)))
    (when h
      (let ((bv (string->utf8 (vtscreen-normalize-input data))))
        (ffi-jvt-write h bv (u8vector-length bv))))))

(def *render-buf-size* 131072)  ;; 128KB — enough for 24×80 with Unicode
(def *render-buf* (make-u8vector *render-buf-size* 0))

(def (vtscreen-render vt)
  "Render the entire screen to a string (trailing blank rows trimmed).
   Compatible with old vtscreen-render API."
  (let ((h (vtscreen-handle vt)))
    (if h
      (let ((n (ffi-jvt-get-text h *render-buf* *render-buf-size* 0
                                 (ffi-jvt-get-rows h))))
        (if (> n 0)
          (utf8->string (subu8vector *render-buf* 0 n))
          ""))
      "")))

(def (vtscreen-resize! vt new-rows new-cols)
  "Resize the virtual screen."
  (let ((h (vtscreen-handle vt)))
    (when h (ffi-jvt-resize h new-rows new-cols))))

(def (vtscreen-rows vt)
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-rows h) 0)))

(def (vtscreen-cols vt)
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cols h) 0)))

(def (vtscreen-cursor-row vt)
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cursor-row h) 0)))

(def (vtscreen-cursor-col vt)
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cursor-col h) 0)))

(def (vtscreen-alt-screen? vt)
  "Check if alt screen buffer is active (e.g. top, vim, less)."
  (let ((h (vtscreen-handle vt)))
    (if h (not (= 0 (ffi-jvt-is-altscreen h))) #f)))

;;;============================================================================
;;; Row-level damage tracking
;;;============================================================================

(def (vtscreen-has-damage? vt)
  "Check if any row has changed since last vtscreen-clear-damage!."
  (let ((h (vtscreen-handle vt)))
    (if h (not (= 0 (ffi-jvt-has-damage h))) #f)))

(def (vtscreen-row-dirty? vt row)
  "Check if a specific row has changed."
  (let ((h (vtscreen-handle vt)))
    (if h (not (= 0 (ffi-jvt-row-dirty h row))) #f)))

(def (vtscreen-clear-damage! vt)
  "Clear all damage flags."
  (let ((h (vtscreen-handle vt)))
    (when h (ffi-jvt-clear-damage h))))

(def (vtscreen-mark-all-dirty! vt)
  "Mark all rows as dirty (e.g. for initial render or after resize)."
  (let ((h (vtscreen-handle vt)))
    (when h (ffi-jvt-mark-all-dirty h))))

;;;============================================================================
;;; Per-row text extraction
;;;============================================================================

(def *row-buf-size* 4096)
(def *row-buf* (make-u8vector *row-buf-size* 0))

(def (vtscreen-get-row-text vt row)
  "Get the text content of a single row as a string (trailing spaces trimmed)."
  (let ((h (vtscreen-handle vt)))
    (if h
      (let ((n (ffi-jvt-get-row-text h row *row-buf* *row-buf-size*)))
        (if (> n 0)
          (utf8->string (subu8vector *row-buf* 0 n))
          ""))
      "")))

;;;============================================================================
;;; Scrollback access
;;;============================================================================

(def (vtscreen-scrollback-len vt)
  "Get number of scrollback lines."
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-scrollback-len h) 0)))

(def *sb-buf-size* 4096)
(def *sb-buf* (make-u8vector *sb-buf-size* 0))

(def (vtscreen-scrollback-line vt idx)
  "Get scrollback line by index (0 = most recent). Returns string."
  (let ((h (vtscreen-handle vt)))
    (if h
      (let ((n (ffi-jvt-scrollback-line h idx *sb-buf* *sb-buf-size*)))
        (if (> n 0)
          (utf8->string (subu8vector *sb-buf* 0 n))
          ""))
      "")))

(def (vtscreen-scrollback-clear! vt)
  "Clear all scrollback lines."
  (let ((h (vtscreen-handle vt)))
    (when h (ffi-jvt-scrollback-clear h))))

;;;============================================================================
;;; Per-cell color and attribute queries
;;;============================================================================

(def (vtscreen-cell-fg vt row col)
  "Get foreground color as packed 0x00RRGGBB. Returns -1 for default."
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cell-fg h row col) -1)))

(def (vtscreen-cell-bg vt row col)
  "Get background color as packed 0x00RRGGBB. Returns -1 for default."
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cell-bg h row col) -1)))

(def (vtscreen-cell-attrs vt row col)
  "Get cell attributes as packed bits (bold=0, underline=1, italic=2, etc.)."
  (let ((h (vtscreen-handle vt)))
    (if h (ffi-jvt-get-cell-attrs h row col) 0)))
