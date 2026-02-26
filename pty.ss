;;; -*- Gerbil -*-
;;; PTY (pseudo-terminal) subprocess support via forkpty(3).
;;;
;;; Provides async subprocess execution with a real PTY, enabling
;;; interactive programs like top, vim, less to work in terminal mode.
;;;
;;; Gambit's open-process with pseudo-terminal: #t is broken in headless
;;; environments (TIOCSCTTY fails with EBADF when no controlling terminal),
;;; so we use forkpty() directly via FFI.

(export pty-spawn
        pty-read
        pty-write
        pty-close!
        pty-kill!
        pty-resize!
        pty-waitpid
        pty-child-alive?)

(import :std/foreign
        :std/sugar
        :std/srfi/13)

(begin-ffi (ffi-pty-spawn
            ffi-pty-get-master-fd
            ffi-pty-get-child-pid
            ffi-pty-read
            ffi-pty-write
            ffi-pty-close
            ffi-pty-kill
            ffi-pty-resize
            ffi-pty-waitpid
            ffi-pty-waitpid-status)

  (c-declare #<<END-C
#include <pty.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

/* Per-call state (single PTY spawn at a time from Scheme) */
static int    g_master_fd  = -1;
static pid_t  g_child_pid  = -1;
static int    g_wait_status = 0;

/*
 * Spawn a child process in a new PTY.
 * cmd:  shell command string (passed to /bin/sh -c)
 * envp: environment as "KEY=VALUE\n..." string (newline-separated)
 *       Empty string or NULL means inherit parent environment.
 * rows, cols: initial terminal size
 * Returns: child PID on success, -errno on failure.
 * The master fd is stored in g_master_fd (retrieve via ffi_pty_get_master_fd).
 */
static int ffi_pty_spawn_impl(const char *cmd, const char *envp,
                               int rows, int cols) {
    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_row = rows;
    ws.ws_col = cols;

    g_master_fd = -1;
    g_child_pid = forkpty(&g_master_fd, NULL, NULL, &ws);

    if (g_child_pid < 0) {
        int err = errno;
        g_master_fd = -1;
        return -err;
    }

    if (g_child_pid == 0) {
        /* Child process */

        /* Set environment variables from the envp string */
        if (envp && envp[0]) {
            /* Clear inherited env, set from scratch */
            clearenv();
            const char *p = envp;
            while (*p) {
                const char *nl = strchr(p, '\n');
                if (!nl) nl = p + strlen(p);
                int len = nl - p;
                if (len > 0) {
                    char *entry = (char *)alloca(len + 1);
                    memcpy(entry, p, len);
                    entry[len] = '\0';
                    putenv(entry); /* putenv doesn't copy, but we exec soon */
                    /* Actually, use setenv-style approach for safety */
                    char *eq = strchr(entry, '=');
                    if (eq) {
                        *eq = '\0';
                        setenv(entry, eq + 1, 1);
                    }
                }
                p = *nl ? nl + 1 : nl;
            }
        }

        /* Set TERM if not already set */
        if (!getenv("TERM"))
            setenv("TERM", "xterm-256color", 1);

        execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
        _exit(127);
    }

    /* Parent: make master fd non-blocking for polling reads */
    int flags = fcntl(g_master_fd, F_GETFL, 0);
    if (flags >= 0)
        fcntl(g_master_fd, F_SETFL, flags | O_NONBLOCK);

    return g_child_pid;
}

static int ffi_pty_get_master_fd_impl(void) { return g_master_fd; }
static int ffi_pty_get_child_pid_impl(void) { return g_child_pid; }

/*
 * Non-blocking read from PTY master fd.
 * Returns bytes read (>0), 0 if EAGAIN (no data), -1 on EOF/error.
 */
static int ffi_pty_read_impl(int fd, char *buf, int maxlen) {
    int n = read(fd, buf, maxlen);
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
            return 0;   /* no data available */
        if (errno == EIO)
            return -1;  /* child exited, PTY closed */
        return -1;      /* other error */
    }
    if (n == 0)
        return -1;      /* EOF */
    return n;
}

/*
 * Write to PTY master fd (sends data to child's stdin).
 * Returns bytes written, or -1 on error.
 */
static int ffi_pty_write_impl(int fd, const char *data, int len) {
    int n = write(fd, data, len);
    return n;
}

/* Close master fd and clean up. */
static void ffi_pty_close_impl(int fd) {
    if (fd >= 0) close(fd);
}

/* Send signal to child's process group (negative PID). */
static int ffi_pty_kill_impl(int pid, int sig) {
    if (pid > 0) return kill(-pid, sig);  /* negative = process group */
    return -1;
}

/* Set PTY window size. */
static int ffi_pty_resize_impl(int fd, int rows, int cols) {
    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_row = rows;
    ws.ws_col = cols;
    return ioctl(fd, TIOCSWINSZ, &ws);
}

/*
 * Wait for child process. nohang: 1 for non-blocking.
 * Returns: child PID if exited/stopped, 0 if NOHANG and still running, -1 error.
 * Exit status stored in g_wait_status.
 */
static int ffi_pty_waitpid_impl(int pid, int nohang) {
    g_wait_status = 0;
    int result = waitpid(pid, &g_wait_status, nohang ? WNOHANG : 0);
    return result;
}

static int ffi_pty_get_wait_status(void) {
    if (WIFEXITED(g_wait_status))
        return WEXITSTATUS(g_wait_status);
    if (WIFSIGNALED(g_wait_status))
        return 128 + WTERMSIG(g_wait_status);
    return -1;
}

END-C
  )

  (define-c-lambda ffi-pty-spawn (char-string char-string int int) int
    "ffi_pty_spawn_impl")
  (define-c-lambda ffi-pty-get-master-fd () int
    "ffi_pty_get_master_fd_impl")
  (define-c-lambda ffi-pty-get-child-pid () int
    "ffi_pty_get_child_pid_impl")
  (define-c-lambda ffi-pty-read (int scheme-object int) int
    "___return(ffi_pty_read_impl(___arg1, ___CAST(char*,___BODY(___arg2)), ___arg3));")
  (define-c-lambda ffi-pty-write (int char-string int) int
    "ffi_pty_write_impl")
  (define-c-lambda ffi-pty-close (int) void
    "ffi_pty_close_impl")
  (define-c-lambda ffi-pty-kill (int int) int
    "ffi_pty_kill_impl")
  (define-c-lambda ffi-pty-resize (int int int) int
    "ffi_pty_resize_impl")
  (define-c-lambda ffi-pty-waitpid (int int) int
    "ffi_pty_waitpid_impl")
  (define-c-lambda ffi-pty-waitpid-status () int
    "ffi_pty_get_wait_status")
)

;;;============================================================================
;;; Scheme-level API
;;;============================================================================

(def (pty-spawn cmd env-alist rows cols)
  "Spawn a child process in a new PTY.
   cmd: shell command string
   env-alist: list of (name . value) pairs for environment
   rows, cols: initial terminal size
   Returns (values master-fd child-pid) or (values #f #f) on error."
  (let* ((env-str (env-alist->string env-alist))
         (result (ffi-pty-spawn cmd env-str rows cols)))
    (if (> result 0)
      (values (ffi-pty-get-master-fd) result)
      (values #f #f))))

(def (pty-read master-fd)
  "Non-blocking read from PTY master. Returns:
   - string with data (may contain ANSI escapes)
   - #f if no data available (EAGAIN)
   - 'eof if child exited / PTY closed"
  (let* ((buf (make-u8vector 4096 0))
         (n (ffi-pty-read master-fd buf 4095)))
    (cond
      ((> n 0) (utf8->string (subu8vector buf 0 n)))
      ((= n 0) #f)       ;; EAGAIN — no data
      (else 'eof))))      ;; EOF or error

(def (pty-write master-fd str)
  "Write string to PTY master (sends to child's stdin).
   Returns bytes written or -1 on error."
  (ffi-pty-write master-fd str (string-length str)))

(def (pty-close! master-fd child-pid)
  "Close master fd, send SIGTERM to child, reap with WNOHANG."
  (ffi-pty-close master-fd)
  (when (and child-pid (> child-pid 0))
    (with-catch void
      (lambda ()
        (ffi-pty-kill child-pid 15)  ;; SIGTERM
        (ffi-pty-waitpid child-pid 1)))))  ;; WNOHANG

(def (pty-kill! child-pid signal)
  "Send signal to child's process group.
   Common signals: 2=SIGINT, 9=SIGKILL, 15=SIGTERM."
  (when (and child-pid (> child-pid 0))
    (ffi-pty-kill child-pid signal)))

(def (pty-resize! master-fd rows cols)
  "Set PTY window size. Child receives SIGWINCH."
  (when (and master-fd (>= master-fd 0))
    (ffi-pty-resize master-fd rows cols)))

(def (pty-waitpid child-pid nohang?)
  "Wait for child. Returns (values exit-status exited?) where:
   exit-status: 0-255 for normal exit, 128+N for signal N
   exited?: #t if child has exited, #f if still running (when nohang)"
  (let ((result (ffi-pty-waitpid child-pid (if nohang? 1 0))))
    (cond
      ((> result 0) (values (ffi-pty-waitpid-status) #t))
      ((= result 0) (values 0 #f))  ;; still running (WNOHANG)
      (else (values -1 #t)))))       ;; error

(def (pty-child-alive? child-pid)
  "Check if child process is still running."
  (let-values (((status exited?) (pty-waitpid child-pid #t)))
    (not exited?)))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (env-alist->string alist)
  "Convert ((name . value) ...) to newline-separated KEY=VALUE string."
  (if (or (not alist) (null? alist))
    ""
    (let loop ((pairs alist) (acc []))
      (if (null? pairs)
        (string-join (reverse acc) "\n")
        (let ((p (car pairs)))
          (loop (cdr pairs)
                (cons (string-append (car p) "=" (cdr p)) acc)))))))
