#!/usr/bin/env python3
"""Patch gerbil-shell ffi.ss to add missing FFI symbols.
Called by `make deps` after cloning and resetting gerbil-shell to commit 4341a14.
Idempotent: checks if already patched before modifying.
"""
import re, sys, os

pkg_base = os.environ.get('PKG_BASE', '')
if not pkg_base:
    print("ERROR: PKG_BASE not set", file=sys.stderr)
    sys.exit(1)

ffi_path = os.path.join(pkg_base, 'gerbil-shell', 'ffi.ss')
rec_path = os.path.join(pkg_base, 'gerbil-shell', 'recorder.ss')

# --- Patch ffi.ss ---
src = open(ffi_path).read()
if 'ffi_clock_monotonic_ns' in src:
    print("ffi.ss already patched, skipping.")
else:
    # Add to begin-ffi export list
    src = src.replace(
        '            ffi-has-embedded-headers\n            )',
        '            ffi-has-embedded-headers\n'
        '            ffi-clock-monotonic-ns\n'
        '            ffi-clock-realtime-sec\n'
        '            ffi-fdread\n'
        '            )')

    insert = r"""
  ;; --- Missing FFI additions (patched by make deps) ---
  (c-declare #<<END-EXTRAS
static long long ffi_clock_monotonic_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}
static long long ffi_clock_realtime_sec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (long long)ts.tv_sec;
}
static int ffi_fdread_into(int fd, ___SCMOBJ bv, int count) {
    return (int)read(fd, ___CAST(char*, ___BODY(bv)), count);
}
END-EXTRAS
  )
  (define-c-lambda ffi-clock-monotonic-ns () long-long "ffi_clock_monotonic_ns")
  (define-c-lambda ffi-clock-realtime-sec () long-long "ffi_clock_realtime_sec")
  (define-c-lambda _ffi-fdread-into (int scheme-object int) int "ffi_fdread_into")
  (define (ffi-fdread fd count)
    (let* ((bv (make-u8vector count 0))
           (n (_ffi-fdread-into fd bv count)))
      (cond
        ((<= n 0) (u8vector))
        ((= n count) bv)
        (else (subu8vector bv 0 n)))))

"""
    src = re.sub(r'(END-C\n  \)\n)', r'\1' + insert, src, count=1)
    open(ffi_path, 'w').write(src)
    print("Patched ffi.ss: added ffi-fdread, ffi-clock-monotonic-ns, ffi-clock-realtime-sec")

# --- Patch recorder.ss (may not exist in all commits) ---
if not os.path.exists(rec_path):
    print("recorder.ss not present in this commit, skipping.")
elif 'create-directory dir' in open(rec_path).read():
    print("recorder.ss already patched, skipping.")
else:
    rec = open(rec_path).read()
    rec = rec.replace('(mkdir dir)', '(create-directory dir)')
    open(rec_path, 'w').write(rec)
    print("Patched recorder.ss: mkdir -> create-directory")
