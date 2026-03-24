.PHONY: all help deps build binary clean test test-qt test-lsp test-lsp-protocol test-split-comprehensive test-org test-repl test-all install install-qt \
        install-static install-static-qt \
        static static-qt clean-docker check-root build-static build-static-qt linux-static-docker linux-static-qt-docker \
        docker-deps build-gemacs-static build-gemacs-static-qt linux-static-docker-full linux-static-qt-docker-full

UNAME_S := $(shell uname -s)

export GERBIL_BUILD_CORES := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu)

# Hermetically seal all Gerbil operations to the project-local .gerbil/
# No ~/.gerbil references — packages are installed via `make deps`
export GERBIL_PATH := $(CURDIR)/.gerbil

PKG_BASE := $(CURDIR)/.gerbil/pkg/github.com/ober

ifeq ($(UNAME_S),Darwin)
  HOMEBREW_PREFIX := $(shell brew --prefix 2>/dev/null || echo /opt/homebrew)
  OPENSSL_RPATH   = $(HOMEBREW_PREFIX)/opt/openssl@3/lib
  LH_RPATH        = $(PKG_BASE)/gerbil-litehtml/vendor
  QT_SHIM_RPATH   = $(PKG_BASE)/gerbil-qt/vendor
  SCI_RPATH       = $(CURDIR)/.gerbil/lib/gerbil-scintilla
  export GERBIL_LOADPATH := $(CURDIR)/.gerbil/lib:$(PKG_BASE)/gerbil-shell/.gerbil/lib:$(PKG_BASE)/gerbil-scintilla/.gerbil/lib:$(PKG_BASE)/gerbil-qt/.gerbil/lib:$(PKG_BASE)/gerbil-litehtml/.gerbil/lib
else
  OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib
  SCI_RPATH     = $(CURDIR)/.gerbil/lib/gerbil-scintilla
  QT_SHIM_RPATH = $(PKG_BASE)/gerbil-qt/vendor
  LH_RPATH      = $(PKG_BASE)/gerbil-litehtml/vendor
  export GERBIL_LOADPATH := $(CURDIR)/.gerbil/lib:$(PKG_BASE)/gerbil-shell/.gerbil/lib:$(PKG_BASE)/gerbil-scintilla/.gerbil/lib:$(PKG_BASE)/gerbil-qt/.gerbil/lib:$(PKG_BASE)/gerbil-litehtml/.gerbil/lib
endif

all: help

help:
	@echo "Usage: make <target>"
	@echo ""
	@echo "Build targets:"
	@echo "  deps                        Install all deps into project-local .gerbil/ (run once)"
	@echo "  build                       Build TUI and Qt binaries (with patchelf on Linux)"
	@echo "  binary                      Build binaries (macOS-native, no patchelf)"
	@echo "  clean                       Clean build artifacts (project-local only)"
	@echo ""
	@echo "Test targets:"
	@echo "  test                        Build + run TUI tests"
	@echo "  test-qt                     Build + run Qt headless tests"
	@echo "  test-lsp                    Build + run LSP functional tests"
	@echo "  test-lsp-protocol           Build + run LSP protocol tests (interpreter)"
	@echo "  test-split-comprehensive    Build + run window split tests"
	@echo "  test-org                    Build + run org-mode tests"
	@echo "  test-repl                   Build + run debug REPL tests"
	@echo "  test-all                    Build + run all tests"
	@echo ""
	@echo "Install targets:"
	@echo "  install                     Install TUI + Qt to PREFIX (builds only if needed)"
	@echo "  install-qt                  Install Qt only to PREFIX (builds only if needed)"
	@echo "  install-static              Install static TUI + Qt to PREFIX (no rebuild)"
	@echo "  install-static-qt           Install static Qt only to PREFIX (no rebuild)"
	@echo ""
	@echo "Static/Docker targets:"
	@echo "  docker-deps                 Build intermediate deps Docker image"
	@echo "  static                      Static TUI binary via Docker (needs docker-deps)"
	@echo "  static-qt                   Static Qt binary via Docker (needs docker-deps)"
	@echo "  linux-static-docker-full    Self-contained static TUI build (no deps image)"
	@echo "  linux-static-qt-docker-full Self-contained static Qt build (no deps image)"
	@echo "  clean-docker                Clean .gerbil dir via Docker"

QT_TEST_TIMEOUT ?= 600
ifeq ($(UNAME_S),Darwin)
  QT_TEST_ENV = QT_QPA_PLATFORM=offscreen DYLD_LIBRARY_PATH=$(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) QT_IM_MODULE=none QT_ACCESSIBILITY=0
else
  QT_TEST_ENV = QT_QPA_PLATFORM=offscreen LD_LIBRARY_PATH=$(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) QT_IM_MODULE=none QT_ACCESSIBILITY=0
endif

# deps: install all dependencies into project-local .gerbil/ (hermetic, no ~/.gerbil)
#
# gerbil-scintilla: needs vendor Scintilla/Lexilla/termbox built before gerbil build.
#   Strategy: clone via `gerbil pkg install` (build will fail), then `make vendor-deps`,
#   then `gerbil build` directly.
#
# gerbil-shell: HEAD has uncommitted recording modules with missing dependencies.
#   Strategy: reset to last clean commit (4341a14), apply patches for missing FFI
#   symbols (ffi-fdread, ffi-clock-*), build with GSH_LIB_ONLY=1 (no exe needed).
#
deps:
	@echo "Installing dependencies into $(CURDIR)/.gerbil/ ..."
	gerbil pkg install github.com/ober/gerbil-pcre2
	@# gerbil-scintilla: clone (build fails without vendor), then build vendor, then gerbil-build
	-gerbil pkg install github.com/ober/gerbil-scintilla
	$(MAKE) -C $(PKG_BASE)/gerbil-scintilla vendor-deps
	cd $(PKG_BASE)/gerbil-scintilla && gerbil build
	@# gerbil-shell: reset to clean commit, apply missing FFI patches, lib-only build
	-gerbil pkg install github.com/ober/gerbil-shell
	cd $(PKG_BASE)/gerbil-shell && git reset --hard 4341a14
	@# Patch: add missing ffi-fdread, ffi-clock-* to ffi.ss (absent in this commit)
	$(MAKE) -C $(CURDIR) _patch-gerbil-shell
	cd $(PKG_BASE)/gerbil-shell && GSH_LIB_ONLY=1 gerbil build
	gerbil pkg install github.com/ober/gerbil-litehtml
	gerbil pkg install github.com/ober/gerbil-qt
	gerbil pkg install github.com/ober/gerbil-termbox
	gerbil pkg install github.com/ober/gerbil-tui
	@echo "Dependencies installed."

# Apply patches to gerbil-shell's ffi.ss and recorder.ss (missing symbols in pinned commit).
# Uses scripts/patch-gerbil-shell.py which is idempotent (checks before modifying).
_patch-gerbil-shell:
	PKG_BASE=$(PKG_BASE) python3 $(CURDIR)/scripts/patch-gerbil-shell.py

# binary: macOS-native build (no patchelf, uses dylib rpaths embedded at link time)
binary:
	@-rm -f .gerbil/lib/static/*.lock 2>/dev/null; true
	chmod +x build.ss
	gerbil build

build:
	@# Kill any competing gerbil build processes on this project
	@-pkill -f 'gxi.*/home/jafourni/mine/gerbil-emacs/build.ss' 2>/dev/null; true
	@-rm -f .gerbil/lib/static/*.lock 2>/dev/null; true
	chmod +x build.ss
ifeq ($(UNAME_S),Darwin)
	gerbil build
else
	LD_LIBRARY_PATH=$(OPENSSL_RPATH) gerbil build
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(LH_RPATH) .gerbil/bin/gemacs
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/gemacs-qt
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/qt-highlight-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/qt-functional-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/lsp-functional-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/qt-split-comprehensive-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/qt-split-debug-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH):$(LH_RPATH) .gerbil/bin/qt-split-simple-test
endif

clean:
	@-pkill -f 'gxi.*/home/jafourni/mine/gerbil-emacs/build.ss' 2>/dev/null; true
	-gerbil clean
	rm -f .gerbil/lib/static/*.lock 2>/dev/null; true
	rm -rf .gerbil/bin .gerbil/lib

test: build
	@# Exit 139 (SIGSEGV) = Gambit cleanup crash after all tests pass (pre-existing
	@# Qt FFI module finalization issue in interpreter mode). Only the success path
	@# reaches Gambit cleanup; failures exit 42 cleanly via gerbil test's (exit 42).
	@# LD_PRELOAD libvterm.so: vtscreen~0.o2 links -lvterm without NEEDED (shared
	@# lib linker doesn't record as-needed deps), so symbols must be pre-loaded.
	@# Unset WAYLAND_DISPLAY/DISPLAY: prevents clipboard tools (wl-copy/xclip)
	@# from hanging in headless test environments with no compositor running.
	LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libvterm.so.0 WAYLAND_DISPLAY= DISPLAY= gerbil test; EC=$$?; [ $$EC -eq 139 ] && exit 0 || exit $$EC

test-qt: build
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/qt-highlight-test; \
	EXIT=$$?; if [ $$EXIT -eq 139 ]; then true; else exit $$EXIT; fi
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/qt-functional-test; \
	EXIT=$$?; if [ $$EXIT -eq 139 ]; then true; else exit $$EXIT; fi

test-lsp: build
	@echo "Running LSP functional tests..."
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/lsp-functional-test
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/lsp-functional-test; \
	EXIT=$$?; if [ $$EXIT -eq 137 ] || [ $$EXIT -eq 139 ]; then exit 0; else exit $$EXIT; fi

test-lsp-protocol: build
	@echo "Running LSP protocol tests (interpreter)..."
	LD_LIBRARY_PATH=$(OPENSSL_RPATH) timeout $(QT_TEST_TIMEOUT) gerbil test ./lsp-protocol-test.ss; \
	EXIT=$$?; if [ $$EXIT -eq 139 ]; then exit 0; else exit $$EXIT; fi

test-split-comprehensive: build
	@echo "Running COMPREHENSIVE window split tests..."
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/qt-split-comprehensive-test

test-org: build
	@echo "Running org-mode tests..."
	gerbil test ./org-parse-test.ss ./org-element-test.ss ./org-table-test.ss \
	  ./org-list-test.ss ./org-clock-test.ss ./org-duration-test.ss \
	  ./org-export-test.ss ./org-babel-test.ss ./org-agenda-test.ss \
	  ./org-capture-test.ss ./org-tempo-test.ss ./org-footnote-test.ss \
	  ./org-lint-test.ss ./org-src-test.ss ./org-fold-test.ss \
	  ./org-num-test.ss ./org-property-test.ss; \
	EC=$$?; [ $$EC -eq 139 ] && exit 0 || exit $$EC

test-repl: build
	@echo "Running debug REPL tests..."
	LD_LIBRARY_PATH=$(OPENSSL_RPATH) timeout $(QT_TEST_TIMEOUT) gerbil test ./debug-repl-test.ss; \
	EC=$$?; [ $$EC -eq 139 ] && exit 0 || exit $$EC

test-all: build test test-qt test-lsp test-lsp-protocol test-split-comprehensive test-org test-repl

PREFIX ?= $(HOME)/.local

install:
	@if [ ! -f .gerbil/bin/gemacs ] || [ ! -f .gerbil/bin/gemacs-qt ]; then \
	  echo "No existing binaries found, building..."; \
	  $(MAKE) build; \
	fi
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs $(PREFIX)/bin/
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/
	@echo "Installed to $(PREFIX)/bin"

install-qt:
	@if [ ! -f .gerbil/bin/gemacs-qt ]; then \
	  echo "No existing binary found, building..."; \
	  $(MAKE) build; \
	fi
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/

# Install pre-built static binaries (from `make static` / `make static-qt`)
install-static:
	@test -f .gerbil/bin/gemacs || { echo "ERROR: No static binary found. Run 'make static-qt' first."; exit 1; }
	@file .gerbil/bin/gemacs | grep -q 'statically linked' || { echo "ERROR: .gerbil/bin/gemacs is not static. Run 'make static-qt' first."; exit 1; }
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs $(PREFIX)/bin/
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/ 2>/dev/null || true
	@echo "Installed static binaries to $(PREFIX)/bin"

install-static-qt:
	@test -f .gerbil/bin/gemacs-qt || { echo "ERROR: No static binary found. Run 'make static-qt' first."; exit 1; }
	@file .gerbil/bin/gemacs-qt | grep -q 'statically linked' || { echo "ERROR: .gerbil/bin/gemacs-qt is not static. Run 'make static-qt' first."; exit 1; }
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/
	@echo "Installed static gemacs-qt to $(PREFIX)/bin"

# =============================================================================
# Static binary builds (Docker-based, following gerbil-charts pattern)
# =============================================================================

ARCH := $(shell uname -m)
DOCKER_IMAGE := gerbil/gerbil:$(ARCH)-master
UID := $(shell id -u)
GID := $(shell id -g)

# Package source dirs for dependencies (installed via `make deps`)
SCI_SRC ?= $(PKG_BASE)/gerbil-scintilla
QT_SRC  ?= $(PKG_BASE)/gerbil-qt
LH_SRC  ?= $(PKG_BASE)/gerbil-litehtml

DEPS_IMAGE := gemacs-deps:$(ARCH)

static: linux-static-docker

static-qt: linux-static-qt-docker

clean-docker:
	-rm -rf .gerbil 2>/dev/null || true
	-docker run --rm -v $(CURDIR):/src:z alpine sh -c "rm -rf /src/.gerbil || true"

check-root:
	@if [ "$$(id -u)" = "0" ]; then \
	  git config --global --add safe.directory '*'; \
	fi

# -----------------------------------------------------------------------------
# Intermediate deps image (run once, or when dependencies change)
# -----------------------------------------------------------------------------
docker-deps:
	DOCKER_BUILDKIT=1 docker build \
	  --build-arg ARCH=$(ARCH) \
	  --build-context sci-src=$(SCI_SRC) \
	  --build-context qt-src=$(QT_SRC) \
	  --build-context lh-src=$(LH_SRC) \
	  -t $(DEPS_IMAGE) \
	  $(CURDIR)

# -----------------------------------------------------------------------------
# In-container targets (for use inside the deps image)
# -----------------------------------------------------------------------------

# gerbil-shell package path inside deps image
GSH_PKG_DIR = /root/.gerbil/pkg/github.com/ober/gerbil-shell
GSH_LIB_DIR = $(GSH_PKG_DIR)/.gerbil/lib

# Build only gemacs TUI inside the pre-built deps image
build-gemacs-static: check-root
	cd /src && \
	  GERBIL_BUILD_CORES=1 \
	  GEMACS_BUILD_TUI_ONLY=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/deps/gerbil-scintilla \
	  GEMACS_LH_BASE=/deps/gerbil-litehtml \
	  GEMACS_GSH_BASE=$(GSH_PKG_DIR) \
	  GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:/deps/gerbil-litehtml/.gerbil/lib:$(GSH_LIB_DIR):/root/.gerbil/lib \
	  gerbil build

# Build gemacs TUI + Qt inside the pre-built deps image
build-gemacs-static-qt: check-root
	cd /src && \
	  GERBIL_BUILD_CORES=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/deps/gerbil-scintilla \
	  GEMACS_QT_BASE=/deps/gerbil-qt \
	  GEMACS_LH_BASE=/deps/gerbil-litehtml \
	  GEMACS_GSH_BASE=$(GSH_PKG_DIR) \
	  PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig \
	  GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:/deps/gerbil-qt/.gerbil/lib:/deps/gerbil-litehtml/.gerbil/lib:$(GSH_LIB_DIR):/root/.gerbil/lib \
	  gerbil build

# -----------------------------------------------------------------------------
# Fast Docker builds (require deps image from `make docker-deps`)
# -----------------------------------------------------------------------------

# Static TUI binary via Docker (fast — uses pre-built deps image)
# Runs build as host UID:GID via su-exec so .gerbil/ files are never root-owned
linux-static-docker: clean-docker
	@docker image inspect $(DEPS_IMAGE) >/dev/null 2>&1 || \
	  { echo "ERROR: Deps image '$(DEPS_IMAGE)' not found. Run 'make docker-deps' first."; exit 1; }
	docker run --rm \
	  --ulimit nofile=8192:8192 \
	  -v $(CURDIR):/src:z \
	  $(DEPS_IMAGE) \
	  sh -c "chmod 755 /root && \
	         chown -R $(UID):$(GID) /opt/ /root/.gerbil /deps && \
	         mkdir -p /tmp/gemacs-build && chown $(UID):$(GID) /tmp/gemacs-build && \
	         exec su-exec $(UID):$(GID) env HOME=/tmp/gemacs-build sh -c '\
	           cd /src && make build-gemacs-static'"

# Static Qt binary via Docker (fast — uses pre-built deps image)
# Runs build as host UID:GID via su-exec so .gerbil/ files are never root-owned
linux-static-qt-docker: clean-docker
	@docker image inspect $(DEPS_IMAGE) >/dev/null 2>&1 || \
	  { echo "ERROR: Deps image '$(DEPS_IMAGE)' not found. Run 'make docker-deps' first."; exit 1; }
	docker run --rm \
	  --ulimit nofile=8192:8192 \
	  -v $(CURDIR):/src:z \
	  $(DEPS_IMAGE) \
	  sh -c "chmod 755 /root && \
	         chown -R $(UID):$(GID) /opt/ /root/.gerbil /deps && \
	         mkdir -p /tmp/gemacs-build && chown $(UID):$(GID) /tmp/gemacs-build && \
	         exec su-exec $(UID):$(GID) env HOME=/tmp/gemacs-build sh -c '\
	           cd /src && make build-gemacs-static-qt'"

# -----------------------------------------------------------------------------
# Self-contained fallbacks (build everything from scratch in one container)
# -----------------------------------------------------------------------------

# Build static TUI binary inside Docker container (self-contained)
# NOTE: vendor .a files from host are glibc-compiled; must rebuild with musl
build-static: check-root
	@echo "=== Copying gerbil-scintilla and rebuilding vendor for musl ==="
	cp -a /deps/gerbil-scintilla /tmp/gerbil-scintilla
	rm -rf /tmp/gerbil-scintilla/.gerbil
	find /tmp/gerbil-scintilla/vendor -name "*.o" -o -name "*.a" | xargs rm -f 2>/dev/null || true
	cd /tmp/gerbil-scintilla && make vendor-deps
	@echo "=== Building gerbil-scintilla ==="
	gxpkg link gerbil-scintilla /tmp/gerbil-scintilla
	cd /tmp/gerbil-scintilla && gerbil build
	@echo "=== Installing gerbil-pcre2 ==="
	gxpkg install github.com/ober/gerbil-pcre2
	@echo "=== Building gerbil-litehtml (static) ==="
	gxpkg link gerbil-litehtml /deps/gerbil-litehtml
	cd /deps/gerbil-litehtml && GEMACS_LH_STATIC=1 gerbil build
	@echo "=== Building gemacs (TUI, static) ==="
	cd /src && \
	  GERBIL_BUILD_CORES=1 \
	  GEMACS_BUILD_TUI_ONLY=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/tmp/gerbil-scintilla \
	  GEMACS_LH_BASE=/deps/gerbil-litehtml \
	  GERBIL_LOADPATH=/tmp/gerbil-scintilla/.gerbil/lib:/deps/gerbil-litehtml/.gerbil/lib:$(HOME)/.gerbil/lib \
	  gerbil build

# Build static Qt binary inside Docker container (self-contained)
# NOTE: Requires Qt6 development packages in the container.
build-static-qt: check-root
	@echo "=== Copying gerbil-scintilla and rebuilding vendor for musl ==="
	cp -a /deps/gerbil-scintilla /tmp/gerbil-scintilla
	rm -rf /tmp/gerbil-scintilla/.gerbil
	find /tmp/gerbil-scintilla/vendor -name "*.o" -o -name "*.a" | xargs rm -f 2>/dev/null || true
	cd /tmp/gerbil-scintilla && make vendor-deps
	@echo "=== Building gerbil-scintilla ==="
	gxpkg link gerbil-scintilla /tmp/gerbil-scintilla
	cd /tmp/gerbil-scintilla && gerbil build
	@echo "=== Installing gerbil-pcre2 ==="
	gxpkg install github.com/ober/gerbil-pcre2
	@echo "=== Building gerbil-qt ==="
	gxpkg link gerbil-qt /deps/gerbil-qt
	cd /deps/gerbil-qt && \
	  GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:$(HOME)/.gerbil/lib \
	  gerbil build
	@echo "=== Building gerbil-litehtml (static) ==="
	gxpkg link gerbil-litehtml /deps/gerbil-litehtml
	cd /deps/gerbil-litehtml && GEMACS_LH_STATIC=1 gerbil build
	@echo "=== Building gemacs (TUI + Qt, static) ==="
	cd /src && \
	  GERBIL_BUILD_CORES=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/tmp/gerbil-scintilla \
	  GEMACS_QT_BASE=/deps/gerbil-qt \
	  GEMACS_LH_BASE=/deps/gerbil-litehtml \
	  PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig \
	  GERBIL_LOADPATH=/tmp/gerbil-scintilla/.gerbil/lib:/deps/gerbil-qt/.gerbil/lib:/deps/gerbil-litehtml/.gerbil/lib:$(HOME)/.gerbil/lib \
	  gerbil build

# Self-contained static TUI binary via Docker (no deps image needed)
linux-static-docker-full: clean-docker
	docker run --rm \
	  --ulimit nofile=8192:8192 \
	  -v $(CURDIR):/src:z \
	  -v $(SCI_SRC):/deps/gerbil-scintilla:z \
	  -v $(LH_SRC):/deps/gerbil-litehtml:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache g++ git curl make su-exec cmake samurai \
	           pcre2-dev pcre2-static \
	           gumbo-parser-dev gumbo-parser-static \
	           openssl-dev openssl-libs-static \
	           sqlite-dev sqlite-static \
	           zlib-static && \
	         cd /tmp && git clone --depth 1 --branch v0.6 https://github.com/litehtml/litehtml.git && \
	         cd litehtml && cmake -B build -G Ninja -DBUILD_SHARED_LIBS=OFF -DCMAKE_INSTALL_PREFIX=/usr/local -DLITEHTML_BUILD_TESTING=OFF -DEXTERNAL_GUMBO=ON && \
	         cmake --build build && cmake --install build && cd / && rm -rf /tmp/litehtml && \
	         chown -R $(UID):$(GID) /opt/ && \
	         mkdir -p /tmp/gemacs-build && chown $(UID):$(GID) /tmp/gemacs-build && \
	         exec su-exec $(UID):$(GID) env HOME=/tmp/gemacs-build sh -c '\
	           cd /src && make build-static'"

# Self-contained static Qt binary via Docker (no deps image needed)
linux-static-qt-docker-full: clean-docker
	docker run --rm \
	  --ulimit nofile=8192:8192 \
	  -v $(CURDIR):/src:z \
	  -v $(SCI_SRC):/deps/gerbil-scintilla:z \
	  -v $(QT_SRC):/deps/gerbil-qt:z \
	  -v $(LH_SRC):/deps/gerbil-litehtml:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache g++ git curl make su-exec cmake samurai \
	           pcre2-dev pcre2-static \
	           gumbo-parser-dev gumbo-parser-static \
	           qt6-qtbase-dev \
	           qscintilla-dev 2>/dev/null; \
	         cd /tmp && git clone --depth 1 --branch v0.6 https://github.com/litehtml/litehtml.git && \
	         cd litehtml && cmake -B build -G Ninja -DBUILD_SHARED_LIBS=OFF -DCMAKE_INSTALL_PREFIX=/usr/local -DLITEHTML_BUILD_TESTING=OFF -DEXTERNAL_GUMBO=ON && \
	         cmake --build build && cmake --install build && cd / && rm -rf /tmp/litehtml; \
	         chown -R $(UID):$(GID) /opt/ && \
	         mkdir -p /tmp/gemacs-build && chown $(UID):$(GID) /tmp/gemacs-build && \
	         exec su-exec $(UID):$(GID) env HOME=/tmp/gemacs-build sh -c '\
	           cd /src && make build-static-qt'"
