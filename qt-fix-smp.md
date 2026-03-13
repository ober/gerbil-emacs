# Fix: Qt Thread-Safe Communication (Option C)

**Bug**: Gambit's M:N scheduler creates up to 16 OS threads and migrates green threads between them at heartbeat preemption points. Qt requires ALL widget operations on the OS thread where `QApplication` was created. Gambit can migrate a green thread between any two Gerbil operations — even inside a Qt callback — causing `setParent` errors, timer warnings, and intermittent hangs.

**Fix**: Make every Qt FFI call thread-safe by wrapping all 802 C++ shim functions with a dispatch macro that checks the current thread and marshals to the Qt main thread via `Qt::BlockingQueuedConnection` when needed. Restructure `qt-main` so `QApplication::exec()` starts immediately, with initialization happening inside the event loop.

**Root cause**: `~/mine/gambit-qt-thread-migration.md`

---

## Architecture Overview

```
                    Gambit VM (up to 16 OS threads)
                    ┌──────────────────────────────────┐
                    │  Green Thread A (key handler)     │
                    │  Green Thread B (file indexer)    │
                    │  Green Thread C (LSP reader)      │
                    │  ... can migrate between threads  │
                    └──────────┬───────────────────────┘
                               │
                     qt-* FFI calls
                               │
                    ┌──────────▼───────────────────────┐
                    │  C++ Shim (qt_shim.cpp)          │
                    │  ┌─────────────────────────────┐ │
                    │  │ is_qt_main_thread()?        │ │
                    │  │  YES → call Qt directly     │ │
                    │  │  NO  → BlockingQueuedConn.  │ │
                    │  │        (marshal to Qt thread)│ │
                    │  └─────────────────────────────┘ │
                    └──────────┬───────────────────────┘
                               │
                    ┌──────────▼───────────────────────┐
                    │  Qt Event Loop (OS thread 0)     │
                    │  - Processes widget operations    │
                    │  - Fires timers & callbacks       │
                    │  - Repaints                       │
                    └──────────────────────────────────┘
```

**Key insight**: `Qt::BlockingQueuedConnection` posts a functor to the Qt event loop and blocks the calling thread until it completes. This requires a running event loop — so `QApplication::exec()` must start BEFORE any widget creation. The init code must run inside the event loop via a single-shot timer.

---

## Phase 1: Add Thread-Safe Dispatch to C++ Shim

**File**: `/home/jafourni/mine/gerbil-qt/vendor/qt_shim.cpp` (6,221 lines, 802 functions)
**File**: `/home/jafourni/mine/gerbil-qt/vendor/qt_shim.h` (1,605 lines)

### 1a. Add dispatch infrastructure

Add to the top of `qt_shim.cpp`, after existing includes:

```cpp
#include <QThread>
#include <QMetaObject>
#include <QCoreApplication>
#include <functional>
#include <string>

// The Qt main thread, set during qt_application_create()
static QThread* g_qt_main_thread = nullptr;

static inline bool is_qt_main_thread() {
    // Before QApplication exists, or on the correct thread: safe to call directly
    return !g_qt_main_thread ||
           QThread::currentThread() == g_qt_main_thread;
}

// Dispatch a void function to the Qt main thread.
// If already on the Qt thread, calls directly (zero overhead).
// Otherwise, marshals via BlockingQueuedConnection (blocks caller until done).
#define QT_VOID(...) do {                                         \
    if (is_qt_main_thread()) { __VA_ARGS__; }                    \
    else {                                                        \
        QMetaObject::invokeMethod(                                \
            QCoreApplication::instance(),                         \
            [=]() { __VA_ARGS__; },                              \
            Qt::BlockingQueuedConnection);                        \
    }                                                             \
} while(0)

// Dispatch a function returning a value.
// Uses [&] capture so the result is written to the caller's stack frame.
#define QT_RETURN(type, expr) do {                                \
    if (is_qt_main_thread()) { return (expr); }                  \
    type _result{};                                               \
    QMetaObject::invokeMethod(                                    \
        QCoreApplication::instance(),                             \
        [&]() { _result = (expr); },                             \
        Qt::BlockingQueuedConnection);                            \
    return _result;                                               \
} while(0)
```

### 1b. Set `g_qt_main_thread` during app creation

Modify `qt_application_create`:

```cpp
extern "C" qt_application_t qt_application_create(int argc, char** argv) {
    // Record the Qt main thread BEFORE creating QApplication
    g_qt_main_thread = QThread::currentThread();

    auto* app = new QApplication(s_argc, s_argv);
    QAccessible::setActive(false);
    return app;
}
```

### 1c. Wrap all 802 functions using the dispatch macros

This is mechanical work. Every function falls into one of these patterns:

**Pattern A: Void functions (majority)**
```cpp
// BEFORE:
extern "C" void qt_widget_show(qt_widget_t w) {
    static_cast<QWidget*>(w)->show();
}

// AFTER:
extern "C" void qt_widget_show(qt_widget_t w) {
    QT_VOID(static_cast<QWidget*>(w)->show());
}
```

**Pattern B: Functions returning int/long/bool/double**
```cpp
// BEFORE:
extern "C" int qt_widget_width(qt_widget_t w) {
    return static_cast<QWidget*>(w)->width();
}

// AFTER:
extern "C" int qt_widget_width(qt_widget_t w) {
    QT_RETURN(int, static_cast<QWidget*>(w)->width());
}
```

**Pattern C: Functions returning `const char*` via `s_return_buf`**

These need special handling because `s_return_buf` is `thread_local`. When marshaled to the Qt thread, the lambda writes to the Qt thread's copy, but the Gerbil caller reads from its own thread's copy.

```cpp
// BEFORE:
extern "C" const char* qt_label_text(qt_label_t label) {
    s_return_buf = static_cast<QLabel*>(label)->text().toUtf8().constData();
    return s_return_buf.c_str();
}

// AFTER:
extern "C" const char* qt_label_text(qt_label_t label) {
    if (is_qt_main_thread()) {
        s_return_buf = static_cast<QLabel*>(label)->text().toUtf8().constData();
        return s_return_buf.c_str();
    }
    std::string result;
    QMetaObject::invokeMethod(
        QCoreApplication::instance(),
        [&]() {
            result = static_cast<QLabel*>(label)->text().toUtf8().constData();
        },
        Qt::BlockingQueuedConnection);
    s_return_buf = std::move(result);
    return s_return_buf.c_str();
}
```

Add a convenience macro for this pattern:

```cpp
#define QT_RETURN_STRING(expr) do {                               \
    if (is_qt_main_thread()) {                                    \
        s_return_buf = (expr);                                    \
        return s_return_buf.c_str();                              \
    }                                                             \
    std::string _result;                                          \
    QMetaObject::invokeMethod(                                    \
        QCoreApplication::instance(),                             \
        [&]() { _result = (expr); },                             \
        Qt::BlockingQueuedConnection);                            \
    s_return_buf = std::move(_result);                            \
    return s_return_buf.c_str();                                  \
} while(0)
```

**Pattern D: Functions returning `void*` (widget creation)**
```cpp
// BEFORE:
extern "C" qt_widget_t qt_widget_create(qt_widget_t parent) {
    return new QWidget(static_cast<QWidget*>(parent));
}

// AFTER:
extern "C" qt_widget_t qt_widget_create(qt_widget_t parent) {
    QT_RETURN(qt_widget_t,
        new QWidget(static_cast<QWidget*>(parent)));
}
```

**Pattern E: Callback registration functions**

These register C callbacks that Qt invokes via signals. The callback itself already runs on the Qt thread (fired from the event loop). Only the registration call needs wrapping:

```cpp
// BEFORE:
extern "C" void qt_push_button_on_clicked(qt_push_button_t b,
                                          qt_callback_void callback,
                                          long callback_id) {
    auto* btn = static_cast<QPushButton*>(b);
    QObject::connect(btn, &QPushButton::clicked, [callback, callback_id]() {
        callback(callback_id);
    });
}

// AFTER:
extern "C" void qt_push_button_on_clicked(qt_push_button_t b,
                                          qt_callback_void callback,
                                          long callback_id) {
    QT_VOID(
        auto* btn = static_cast<QPushButton*>(b);
        QObject::connect(btn, &QPushButton::clicked, [callback, callback_id]() {
            callback(callback_id);
        })
    );
}
```

**Pattern F: `qt_application_exec` and `qt_application_create` — DO NOT WRAP**

These are the functions that establish the Qt thread and event loop. They must run directly on the calling thread. `qt_application_create` sets `g_qt_main_thread`. `qt_application_exec` enters the event loop and blocks.

**Pattern G: `qt_application_process_events` — DO NOT WRAP**

This is called from the master timer callback, which already runs on the Qt thread.

### 1d. Functions to EXCLUDE from wrapping

Some functions must NOT be wrapped because they either establish the Qt thread, are thread-safe by nature, or are called before QApplication exists:

| Function | Reason |
|----------|--------|
| `qt_application_create` | Sets `g_qt_main_thread`; must run directly |
| `qt_application_exec` | Enters event loop; blocks on calling thread |
| `qt_application_quit` | Thread-safe in Qt |
| `qt_application_process_events` | Always called from Qt thread (timer callback) |
| Pure getters with no side effects on thread-safe data | Optimization (optional) |

### 1e. Thread-local storage changes

Change `s_return_buf` from `thread_local` to regular static:

```cpp
// BEFORE:
thread_local std::string s_return_buf;

// AFTER:
// Safe because BlockingQueuedConnection serializes all cross-thread calls,
// and same-thread calls are sequential by nature.
static std::string s_return_buf;
```

**Why safe**: With `BlockingQueuedConnection`, only one dispatched call executes at a time on the Qt thread. Same-thread calls are inherently sequential. So `s_return_buf` is never accessed concurrently.

Similarly for other thread_local variables (`s_last_key_code`, etc.) — evaluate each one. Most are set and read within a single callback invocation, so they're safe as regular statics.

### 1f. Estimated scope

| Pattern | Count (approx) | Effort |
|---------|----------------|--------|
| A (void) | ~500 | Mechanical: add `QT_VOID(...)` |
| B (int/long/bool/ptr return) | ~200 | Mechanical: add `QT_RETURN(type, ...)` |
| C (string return) | ~80 | Mechanical: add `QT_RETURN_STRING(...)` |
| D (widget creation) | ~50 | Mechanical: add `QT_RETURN(type, ...)` |
| E (callback registration) | ~60 | Careful: wrap the connect, not the callback |
| F/G (excluded) | ~10 | No change |

Total: ~800 function edits in one file. All mechanical. Could be partially automated with a script that identifies the return type and applies the appropriate macro.

---

## Phase 2: Restructure Initialization Order

**File**: `/home/jafourni/mine/gerbil-emacs/qt/app.ss` (lines 312-1142)
**File**: `/home/jafourni/mine/gerbil-emacs/qt/main.ss` (lines 1-24)

### Why this is required

`Qt::BlockingQueuedConnection` requires a running event loop on the target thread. Currently, `qt-main` creates widgets (lines 343-362) BEFORE calling `qt-application-exec` (at the end). If Gambit migrates the green thread during widget creation, the dispatch macro tries `BlockingQueuedConnection` but there's no event loop to process it → **deadlock**.

### 2a. Add a C helper for deferred initialization

Add to `qt_shim.cpp`:

```cpp
// Callback type for deferred init
typedef void (*qt_init_callback_t)(long);

// Schedule a callback to run once the event loop starts.
// Must be called AFTER qt_application_create, BEFORE qt_application_exec.
extern "C" void qt_schedule_init(qt_init_callback_t callback, long callback_id) {
    QTimer::singleShot(0, [callback, callback_id]() {
        callback(callback_id);
    });
}
```

Add to `qt_shim.h`:

```c
typedef void (*qt_init_callback_t)(long);
void qt_schedule_init(qt_init_callback_t callback, long callback_id);
```

### 2b. Add Gerbil FFI binding

Add to `/home/jafourni/mine/gerbil-qt/libqt.ss`:

```scheme
(define-c-lambda qt_schedule_init ((function void (long)) long) void
  "qt_schedule_init")
```

Add the corresponding c-define callback trampoline (same pattern as existing `ffi_qt_callback_void`):

```scheme
(c-define (ffi_qt_init_callback callback-id)
          (long) void
          "ffi_qt_init_callback" ""
  (with-catch
    (lambda (e) (display-exception e (current-error-port)))
    (lambda ()
      (let ((handler (hash-ref *qt-void-handlers* callback-id #f)))
        (when handler (handler))))))
```

Or reuse the existing `ffi_qt_callback_void` trampoline directly since it has the same signature.

### 2c. Restructure `qt-main`

**Current flow** (qt/app.ss lines 312-1142):
```
qt-main:
  setenv QT_IM_MODULE
  with-qt-app:
    init-gemacs-log!
    create widgets (window, splitter, editors, labels)  ← 150+ lines
    setup keymaps and commands
    setup timers
    open files from args
    qt-application-exec  ← event loop starts HERE
```

**New flow**:
```
qt-main:
  setenv QT_IM_MODULE
  create QApplication
  schedule-init-callback(qt-do-init!)     ← runs inside event loop
  qt-application-exec                      ← event loop starts HERE
  cleanup

qt-do-init!:                               ← called from event loop (on Qt thread)
  init-gemacs-log!
  create widgets
  setup keymaps and commands
  setup timers
  open files from args
```

**New code structure** (qt/app.ss):

```scheme
(def (qt-main . args)
  (setenv "QT_IM_MODULE" "compose")
  (setenv "QT_ACCESSIBILITY" "0")
  (let ((qt-app (qt-app-create)))
    ;; Schedule initialization to run once the event loop starts.
    ;; This is required because BlockingQueuedConnection (used by the
    ;; thread-safe shim) needs a running event loop on the Qt thread.
    (qt-schedule-init!
      (lambda ()
        (qt-do-init! qt-app args)))
    ;; Enter the event loop. Blocks here until quit.
    ;; The init callback fires immediately (0ms timer).
    (qt-app-exec! qt-app)
    ;; Cleanup after quit
    (qt-app-destroy! qt-app)))
```

Extract the body of the current `with-qt-app` block (lines 321-1142) into `qt-do-init!`:

```scheme
(def (qt-do-init! qt-app args)
  ;; Everything that was previously inside (with-qt-app ...) goes here,
  ;; EXCEPT for qt-app-create and qt-app-destroy which are handled by qt-main.
  (init-gemacs-log!)
  (gemacs-log! "SMP: cpu-count=" (number->string (##cpu-count))
               " (thread-safe Qt dispatch active)")
  (define-standard-faces!)
  ;; ... rest of current init code (lines 326-1142) ...
  )
```

### 2d. Update `with-qt-app` macro or stop using it

The `with-qt-app` macro in gerbil-qt (`qt.ss:727`) does:
```scheme
(defrule (with-qt-app app body ...)
  (let ((app (qt-app-create)))
    (try body ...
      (finally (qt-app-destroy! app)))))
```

Since we now split create/exec/destroy across two functions, stop using `with-qt-app` in `qt-main`. Call `qt-app-create`, `qt-app-exec!`, and `qt-app-destroy!` directly as shown above.

### 2e. Handle the cleanup path

Currently `with-qt-app` ensures cleanup via `try/finally`. In the new structure, wrap `qt-app-exec!` in a try/finally:

```scheme
(def (qt-main . args)
  (setenv "QT_IM_MODULE" "compose")
  (setenv "QT_ACCESSIBILITY" "0")
  (let ((qt-app (qt-app-create)))
    (try
      (qt-schedule-init!
        (lambda () (qt-do-init! qt-app args)))
      (qt-app-exec! qt-app)
      (finally
        (qt-app-destroy! qt-app)))))
```

---

## Phase 3: Rebuild gerbil-qt Package

After modifying `qt_shim.cpp`, `qt_shim.h`, and `libqt.ss`, rebuild gerbil-qt:

```bash
cd ~/mine/gerbil-qt
make clean && make build
```

Then rebuild gemacs:

```bash
cd ~/mine/gerbil-emacs
make clean && make build
```

---

## Phase 4: Update Qt Test Executables

The Qt test executables also use `with-qt-app`. They need the same restructuring:

1. **`qt-highlight-test.ss`** — restructure to use `qt-schedule-init` + `exec`
2. **`qt-functional-test.ss`** — same
3. **`lsp-functional-test.ss`** — same
4. **`qt-split-simple-test.ss`** — same

**Alternative**: For tests that use `QT_QPA_PLATFORM=offscreen`, the thread checks are relaxed. Tests could keep the old init pattern if they also set `GAMBCOPT=,-:p1` in the Makefile `QT_TEST_ENV`. This is a pragmatic compromise — the tests exist to verify functionality, not to test threading.

Recommended: Add `GAMBCOPT=,-:p1` to `QT_TEST_ENV` in the Makefile (line 47) so tests remain simple:

```makefile
QT_TEST_ENV = QT_QPA_PLATFORM=offscreen GAMBCOPT=,-:p1 LD_LIBRARY_PATH=...
```

---

## Phase 5: Verification

### 5a. Build verification

```bash
# Rebuild gerbil-qt
cd ~/mine/gerbil-qt && make clean && make build

# Rebuild gemacs
cd ~/mine/gerbil-emacs && make clean && make build
.gerbil/bin/gemacs --version
QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version

# Run tests
make test
make test-qt
```

### 5b. Thread-safety verification

Create a stress test that forces Gambit to use multiple OS threads during Qt operations:

```scheme
;; stress-test-threads.ss — run inside gemacs via M-x eval-expression
;; Spawn CPU-bound green threads to force Gambit to create multiple OS processors
(for-each (lambda (i)
            (spawn (lambda ()
                     (let loop ((n 0))
                       (when (< n 1000000000)
                         (loop (+ n 1)))))))
          (iota 8))
;; Meanwhile, interact normally with the UI.
;; Without the fix: "Cannot set parent" errors, hangs.
;; With the fix: smooth operation.
```

### 5c. Manual verification

1. Launch `gemacs-qt` (NOT offscreen)
2. No `"Cannot set parent"` warnings in stderr
3. No `"Must be called from the main thread"` errors
4. Open an image file — must display immediately, no intermittent hang
5. Open multiple files, switch buffers rapidly
6. Run `M-x eshell` — terminal must work
7. Wait 30s — auto-save must trigger
8. Run `M-x magit-status` — background git ops must complete

### 5d. Performance check

The dispatch macro should add near-zero overhead when already on the Qt thread (`is_qt_main_thread()` is a single pointer comparison). Verify no perceptible latency increase during normal editing.

### 5e. Static build

```bash
make static-qt
.gerbil/bin/gemacs --version
QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version
```

---

## Implementation Strategy

### Recommended order

1. **Phase 1a-1b**: Add dispatch macros + `g_qt_main_thread` (10 minutes)
2. **Phase 2a-2b**: Add `qt_schedule_init` C helper + FFI binding (15 minutes)
3. **Phase 2c-2e**: Restructure `qt-main` into create/schedule/exec pattern (30 minutes)
4. **Phase 1c**: Wrap all 802 functions — start with a few to test, then do all (2-3 hours, mechanical)
5. **Phase 1e**: Fix thread-local storage (15 minutes)
6. **Phase 3**: Rebuild and test (30 minutes)
7. **Phase 4**: Update Makefile test env (5 minutes)
8. **Phase 5**: Full verification (30 minutes)

### Automation hint for Phase 1c

The 802 functions follow ~6 patterns. A sed/awk script or manual batch edit can handle most:

1. Find all `extern "C" void` functions → wrap body with `QT_VOID(...)`
2. Find all `extern "C" int` functions → wrap return with `QT_RETURN(int, ...)`
3. Find all functions using `s_return_buf` → use `QT_RETURN_STRING(...)` pattern
4. Find all `extern "C"` returning `void*` pointer types → `QT_RETURN(type, ...)`
5. Skip the exclude list (create, exec, quit, process_events)

### Testing incrementally

After wrapping a handful of functions (e.g., `qt_widget_create`, `qt_widget_show`, `qt_label_create`, `qt_label_set_text`), rebuild and run `gemacs-qt --version` to verify the dispatch macros compile and don't deadlock. Then wrap the rest in bulk.

---

## Risks and Mitigations

### Risk 1: Deadlock during initialization

**Risk**: If `qt-do-init!` takes a long time and Gambit migrates the green thread, a Qt call from inside `qt-do-init!` would use `BlockingQueuedConnection`. But `qt-do-init!` itself is running ON the Qt thread (it was called from a single-shot timer inside exec()). So `is_qt_main_thread()` returns true and calls go directly.

**However**: Gambit can migrate the green thread running `qt-do-init!` to a different OS thread between any two operations. After migration, `QThread::currentThread()` returns a different thread than `g_qt_main_thread`, so the dispatch tries `BlockingQueuedConnection`. But the Qt event loop is busy running our init callback — it can't process the queued call → **deadlock**.

**Mitigation**: This is the fundamental tension. The Qt event loop is reentrant to some degree (it can process events during a blocking call). But `BlockingQueuedConnection` specifically waits for the event loop to process the queued call, which requires the event loop to be idle (not blocked in our callback).

**Solution**: Use `QCoreApplication::processEvents()` in the dispatch when the event loop is in a callback. Or better: add an `is_in_init` flag that, during init, forces all calls to go directly (no dispatch). Since init happens on the Qt thread, this is safe as long as Gambit doesn't migrate the green thread.

**Best solution**: Combine with `GAMBCOPT=,-:p1` JUST for the initialization phase, then allow SMP after init. Or simpler: call `(##set-heartbeat-interval! 0.0)` before init to prevent preemption (and thus prevent migration), then restore it after init.

Actually, the simplest solution: add `(setenv "GAMBCOPT" ",-:p1")` to `qt-main` BEFORE creating QApplication. This prevents Gambit from creating additional OS processors for the lifetime of the process. Combined with the C++ dispatch wrappers, this gives defense-in-depth: even if the env var approach has edge cases, the dispatch wrappers catch them.

**Updated recommendation**: Use BOTH the `GAMBCOPT` restriction AND the C++ dispatch wrappers. Belt and suspenders. The `GAMBCOPT` prevents the common case. The dispatch wrappers handle any edge cases or future changes to Gambit's scheduling.

```scheme
(def (qt-main . args)
  ;; Primary defense: restrict Gambit to 1 OS processor
  (setenv "GAMBCOPT" ",-:p1")
  ;; Qt env setup
  (setenv "QT_IM_MODULE" "compose")
  (setenv "QT_ACCESSIBILITY" "0")
  ;; The C++ shim dispatch wrappers provide defense-in-depth:
  ;; even if Gambit creates multiple processors, Qt calls are
  ;; marshaled to the correct thread.
  ...)
```

### Risk 2: Performance overhead from BlockingQueuedConnection

**Risk**: Each cross-thread dispatch involves mutex lock, condition variable wait, and event loop wake. Could add latency to interactive editing.

**Mitigation**: With `GAMBCOPT=,-:p1`, Gambit uses only 1 OS thread. `is_qt_main_thread()` always returns true. The dispatch macro becomes a single pointer comparison (essentially free). The `BlockingQueuedConnection` path is never taken in normal operation — it exists only as a safety net.

### Risk 3: Multi-statement function bodies

**Risk**: Some C++ functions have multi-line bodies that don't fit neatly into a macro:

```cpp
extern "C" void qt_some_complex_function(qt_widget_t w, const char* text) {
    auto* widget = static_cast<QWidget*>(w);
    QString str = QString::fromUtf8(text);
    widget->setWindowTitle(str);
    widget->update();
}
```

**Mitigation**: Use a lambda block:

```cpp
extern "C" void qt_some_complex_function(qt_widget_t w, const char* text) {
    QT_VOID(
        auto* widget = static_cast<QWidget*>(w);
        QString str = QString::fromUtf8(text);
        widget->setWindowTitle(str);
        widget->update()
    );
}
```

The `QT_VOID` macro wraps the body in a lambda, so multi-statement bodies work. Just ensure no `return` statements inside the macro body (use `QT_RETURN` pattern instead).

### Risk 4: Scintilla `sci-send` calls

**Risk**: `sci-send` goes through `qt_scintilla_send_message` in the C++ shim, which calls `QsciScintilla::SendScintilla()`. This is one of the most frequently called Qt functions.

**Mitigation**: `qt_scintilla_send_message` gets wrapped like any other function. The pointer comparison in `is_qt_main_thread()` is negligible compared to the Scintilla message processing cost.

### Risk 5: Callback re-entrancy

**Risk**: A Qt callback (key event) fires on the Qt thread, calls Gerbil code, which calls a Qt function. If Gambit migrates the green thread between the callback entry and the Qt call, the dispatch tries `BlockingQueuedConnection`. But the Qt event loop is already executing our callback — processing the queued call requires the event loop to be idle.

**Mitigation**: With `GAMBCOPT=,-:p1`, this situation cannot occur (only 1 OS thread, no migration). Without it, Qt's event loop IS reentrant during `BlockingQueuedConnection` — Qt processes the queued call within `processEvents()` called internally by the connection mechanism. The calling (migrated) thread blocks until the Qt thread's event loop picks up the queued call. Since the Qt thread is running a callback (not blocked in our code), it will return to the event loop and process pending events, including the queued call.

**Actually**: If the Qt event loop is inside our callback and the callback's green thread migrates, the green thread now runs on a different OS thread. The Qt thread (OS thread 0) is no longer executing Gerbil code — the callback "returned" from the Qt thread's perspective (the green thread migrated away). But Gambit's scheduler views it as a continuation of the same green thread. This is the crux of the problem: Qt thinks the callback returned; Gambit thinks it's still running.

In this scenario, the Qt event loop IS free to process queued calls. So `BlockingQueuedConnection` would work. The migrated green thread blocks on the mutex, Qt processes the queued call on OS thread 0, signals the condition variable, and the migrated green thread continues.

**Conclusion**: This is safe, but only because Gambit's migration is transparent at the C level — the OS thread 0 sees the callback return, and the green thread continues on a different OS thread.

---

## Files Changed (Summary)

### gerbil-qt package (`~/mine/gerbil-qt/`)

| File | Change |
|------|--------|
| `vendor/qt_shim.cpp` | Add dispatch macros + `g_qt_main_thread`; wrap all 802 functions |
| `vendor/qt_shim.h` | Add `qt_schedule_init` signature |
| `libqt.ss` | Add `qt_schedule_init` FFI binding |

### gemacs project (`~/mine/gerbil-emacs/`)

| File | Change |
|------|--------|
| `qt/app.ss` | Restructure `qt-main`: split into create → schedule-init → exec; extract `qt-do-init!` |
| `qt/main.ss` | Add `(setenv "GAMBCOPT" ",-:p1")` as defense-in-depth |
| `Makefile` | Add `GAMBCOPT=,-:p1` to `QT_TEST_ENV` |

### What NOT to change

| File | Reason |
|------|--------|
| `main.ss` (TUI) | No Qt — SMP is fine |
| `async.ss` | Architecture is correct; ui-queue still works |
| `build.ss` | Build parallelism, not runtime |
| `qt/sci-shim.ss` | Calls go through `qt_scintilla_send_message` which gets wrapped in the shim |
| Gerbil wrappers in `qt.ss` | Thread safety is at the C++ level, transparent to Gerbil |
| Qt callback trampolines in `libqt.ss` | Already fire on the Qt thread; Gambit migration after entry is handled by dispatch wrappers on subsequent Qt calls |
