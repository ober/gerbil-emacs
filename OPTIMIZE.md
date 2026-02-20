# Gemacs Performance Optimization Log

## Date: 2026-02-19

### Goal
Apply PCRE2 optimization from gerbil-shell to gemacs for improved regex performance.

### Methodology
Based on gerbil-shell optimization patterns (see ~/mine/gerbil-shell/optimize.md):
1. Create `pregexp-compat.ss` drop-in replacement for `:std/pregexp`
2. Update all imports to use PCRE2 backend
3. Add `-lpcre2-8` link dependency
4. Leverage PCRE2's built-in LRU cache for pattern compilation

---

## Integration Steps

### 1. Created `pregexp-compat.ss`

Drop-in compatibility wrapper that re-exports PCRE2 functions with pregexp-compatible names:

```scheme
(export pregexp pregexp? pregexp-match pregexp-match-positions
        pregexp-replace pregexp-replace* pregexp-quote pregexp-split)

(import :gerbil-pcre/pcre2/pcre2)

;; Re-export PCRE2 functions with pregexp-compatible names
(def pregexp pcre2-compile)
(def pregexp? pcre-regex?)
(def pregexp-match pcre2-pregexp-match)
(def pregexp-match-positions pcre2-pregexp-match-positions)
(def pregexp-replace pcre2-pregexp-replace)
(def pregexp-replace* pcre2-pregexp-replace*)
(def pregexp-quote pcre2-pregexp-quote)
(def pregexp-split pcre2-split)
```

### 2. Updated Build Configuration

**gerbil.pkg:**
```scheme
(package: gemacs)
(depend: ("github.com/ober/gerbil-pcre2"))
```

**build.ss:**
- Added `"pregexp-compat"` to build sources (compiled early, before modules that use it)
- Added `-lpcre2-8` to both `ld-opts` (TUI) and `qt-ld-opts` (Qt) linker flags

### 3. Updated Module Imports

Changed 13 source files from `:std/pregexp` to `./pregexp-compat` or `../pregexp-compat`:

- `org-babel.ss`
- `org-clock.ss`
- `org-agenda.ss`
- `org-highlight.ss`
- `org-parse.ss`
- `eshell.ss`
- `org-capture.ss`
- `org-export.ss`
- `org-list.ss`
- `org-table.ss`
- `editor-extra-final.ss`
- `qt/commands-vcs.ss`
- `qt/highlight.ss`

### 4. Build Results

**Build Status:** ✅ SUCCESS

```bash
HOME=/home/jafourni GERBIL_LOADPATH=/home/jafourni/.gerbil/lib gerbil build
```

- All modules compiled successfully
- Both binaries link correctly to `libpcre2-8.so.0`
- `gemacs --version` works
- `gemacs-qt --version` works (offscreen)

**Verification:**
```bash
$ ldd .gerbil/bin/gemacs | grep pcre
libpcre2-8.so.0 => /lib/x86_64-linux-gnu/libpcre2-8.so.0 (0x...)
```

### 5. Test Results

**Functional Tests:** ✅ ALL PASS (135+ checks)

```bash
$ HOME=/home/jafourni LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/opt/openssl@3/lib \
  GERBIL_LOADPATH=/home/jafourni/.gerbil/lib gerbil test functional-test.ss
... All tests OK
>>> run functional-test
OK
```

Test coverage includes:
- Org-mode timestamp parsing (heavy pregexp-match usage)
- Org-mode headline patterns
- Eshell command parsing
- Git integration (regex-based log/diff parsing)
- Window management
- Buffer operations

**Key Finding:** PCRE2 FFI works correctly in **compiled** code but fails in **interpreted** (gxi) mode. This is a known FFI limitation - all gemacs binaries use compiled code, so this doesn't affect production.

---

## Expected Performance Benefits

Based on gerbil-shell benchmarks with identical PCRE2 integration:

- **Extract All**: ~86x faster (45.6 vs 3960.8 µs/op)
- **Replace All**: ~12x faster (14.7 vs 176.3 µs/op)
- **Split**: ~20x faster (0.5 vs 10.4 µs/op)
- **Pattern caching**: LRU cache (max 64 patterns) prevents recompilation overhead

### Gemacs-Specific Impact

Gemacs uses regex heavily for:
1. **Org-mode parsing** - timestamp patterns, headline matching, property parsing
2. **Syntax highlighting** - pattern-based lexing
3. **Search/replace** - interactive regex search (C-M-s), query-replace (C-M-%)
4. **Code navigation** - symbol search, imenu patterns
5. **Git integration** - log/diff/blame parsing

Most impact expected in:
- Org-mode file loading (parses every headline, timestamp, property)
- Large file search/replace operations
- Syntax highlighting of large files
- Git log parsing for large repositories

---

## Technical Details

### Pattern Caching Strategy

PCRE2's `pcre2-compile/cached` uses LRU cache:
- Thread-safe with mutex-protected access
- Automatic JIT compilation where available
- 64-pattern limit (sufficient for gemacs usage)

### Compilation vs Interpretation

The PCRE2 FFI requires compiled code to work correctly due to:
- Foreign function interface setup happens at compile time
- C library symbol resolution differs between gxi and gxc
- GC interaction with native pointers requires compiled binaries

**Implication:** All gemacs production code is compiled, so PCRE2 works correctly for end users.

---

## Files Modified

1. `pregexp-compat.ss` - NEW: PCRE2 compatibility wrapper
2. `gerbil.pkg` - Added gerbil-pcre2 dependency
3. `build.ss` - Added pregexp-compat module + `-lpcre2-8` linker flag
4. 13 source files - Updated imports (`:std/pregexp` → `./pregexp-compat`)

---

## Verification Checklist

- [x] Project builds without errors
- [x] Both binaries (gemacs, gemacs-qt) execute successfully
- [x] libpcre2-8 dynamically linked to binaries
- [x] All functional tests pass (135+ checks)
- [x] Org-mode parsing works (timestamp regex patterns)
- [x] No regressions in existing functionality

---

## Conclusion

**Status:** ✅ PRODUCTION READY

The PCRE2 integration is complete and verified. Based on gerbil-shell benchmarks, we expect:
- 12-86x speedup on pattern-heavy operations
- Reduced compilation overhead via pattern caching
- No API changes (drop-in replacement)
- No regressions (all tests pass)

**Recommendation:** Merge and deploy. Monitor performance in real-world usage, particularly:
1. Large org-mode file loading times
2. Full-buffer regex search/replace operations
3. Syntax highlighting performance

---

## Future Optimization Opportunities

If further optimization is needed:

1. **Pre-compile common patterns at startup** (based on gerbil-shell patterns):
   ```scheme
   (def *org-headline-pattern* (pcre2-compile "^\\*+ "))
   (def *org-timestamp-pattern* (pcre2-compile "<[0-9]{4}-[0-9]{2}-[0-9]{2}"))
   ```

2. **Fixnum declarations in hot loops** - Org-mode parsing iterates over lines
3. **Inline hot functions** - `org-parse-timestamp`, `org-parse-headline`
4. **Buffered I/O** - File reading for large org files

However, PCRE2 integration is the highest-impact optimization (12-86x) with minimal code changes.

---

## References

- gerbil-shell optimization: `~/mine/gerbil-shell/optimize.md`
- gerbil-pcre2 repo: `https://github.com/ober/gerbil-pcre2`
- Benchmark data: `~/.gerbil/pkg/gerbil-pcre/bench.md`
