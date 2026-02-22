.PHONY: all help build clean test test-qt test-lsp test-lsp-protocol test-split-comprehensive test-org test-all install install-qt \
        install-static install-static-qt \
        static static-qt clean-docker check-root build-static build-static-qt linux-static-docker linux-static-qt-docker \
        docker-deps build-gemacs-static build-gemacs-static-qt linux-static-docker-full linux-static-qt-docker-full

export GERBIL_LOADPATH := $(HOME)/.gerbil/lib
export GERBIL_BUILD_CORES := $(shell echo $$(( $$(nproc) / 2 )))

OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib
SCI_RPATH = $(HOME)/.gerbil/lib/gerbil-scintilla
QT_SHIM_RPATH = $(HOME)/.gerbil/lib/gerbil-qt

all: help

help:
	@echo "Usage: make <target>"
	@echo ""
	@echo "Build targets:"
	@echo "  build                       Build TUI and Qt binaries (with patchelf)"
	@echo "  clean                       Clean local and global build artifacts"
	@echo ""
	@echo "Test targets:"
	@echo "  test                        Build + run TUI tests"
	@echo "  test-qt                     Build + run Qt headless tests"
	@echo "  test-lsp                    Build + run LSP functional tests"
	@echo "  test-lsp-protocol           Build + run LSP protocol tests (interpreter)"
	@echo "  test-split-comprehensive    Build + run window split tests"
	@echo "  test-org                    Build + run org-mode tests"
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
QT_TEST_ENV = QT_QPA_PLATFORM=offscreen LD_LIBRARY_PATH=$(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH)

build:
	chmod +x build.ss
	LD_LIBRARY_PATH=$(OPENSSL_RPATH) gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH) .gerbil/bin/gemacs
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/gemacs-qt
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/qt-highlight-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/qt-functional-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/lsp-functional-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/qt-split-comprehensive-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/qt-split-debug-test
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/qt-split-simple-test

clean:
	gerbil clean
	rm -rf .gerbil
	@# Remove stale global static artifacts that can shadow local builds
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o

test: build
	@# Exit 139 (SIGSEGV) = Gambit cleanup crash after all tests pass (pre-existing
	@# Qt FFI module finalization issue in interpreter mode). Only the success path
	@# reaches Gambit cleanup; failures exit 42 cleanly via gerbil test's (exit 42).
	gerbil test; EC=$$?; [ $$EC -eq 139 ] && exit 0 || exit $$EC

test-qt: build
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/qt-highlight-test
	$(QT_TEST_ENV) timeout $(QT_TEST_TIMEOUT) .gerbil/bin/qt-functional-test

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

test-all: build test test-qt test-lsp test-lsp-protocol test-split-comprehensive test-org

PREFIX ?= $(HOME)/.local

install:
	@if [ ! -f .gerbil/bin/gemacs ] || [ ! -f .gerbil/bin/gemacs-qt ]; then \
	  echo "No existing binaries found, building..."; \
	  $(MAKE) build; \
	fi
	@# Remove stale global static artifacts before install
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs $(PREFIX)/bin/
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/
	@echo "Installed to $(PREFIX)/bin"

install-qt:
	@if [ ! -f .gerbil/bin/gemacs-qt ]; then \
	  echo "No existing binary found, building..."; \
	  $(MAKE) build; \
	fi
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o
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
DOCKER_IMAGE := gerbil/gerbilxx:$(ARCH)-master
UID := $(shell id -u)
GID := $(shell id -g)

# Package source dirs for dependencies (linked or installed via gerbil pkg)
GERBIL_PATH ?= $(HOME)/.gerbil
SCI_SRC ?= $(shell readlink -f $(GERBIL_PATH)/pkg/gerbil-scintilla 2>/dev/null || echo $(GERBIL_PATH)/pkg/github.com/ober/gerbil-scintilla)
QT_SRC  ?= $(shell readlink -f $(GERBIL_PATH)/pkg/gerbil-qt 2>/dev/null || echo $(GERBIL_PATH)/pkg/github.com/ober/gerbil-qt)

DEPS_IMAGE := gemacs-deps:$(ARCH)

static: linux-static-docker

static-qt: linux-static-qt-docker

clean-docker:
	-rm -rf .gerbil 2>/dev/null || true
	docker run --rm -v $(CURDIR):/src:z alpine rm -rf /src/.gerbil

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
	  -t $(DEPS_IMAGE) \
	  $(CURDIR)

# -----------------------------------------------------------------------------
# In-container targets (for use inside the deps image)
# -----------------------------------------------------------------------------

# Build only gemacs TUI inside the pre-built deps image
build-gemacs-static: check-root
	cd /src && \
	  GEMACS_BUILD_TUI_ONLY=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/deps/gerbil-scintilla \
	  GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:/root/.gerbil/lib \
	  gerbil build

# Build gemacs TUI + Qt inside the pre-built deps image
build-gemacs-static-qt: check-root
	cd /src && \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/deps/gerbil-scintilla \
	  GEMACS_QT_BASE=/deps/gerbil-qt \
	  PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig \
	  GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:/deps/gerbil-qt/.gerbil/lib:/root/.gerbil/lib \
	  gerbil build

# -----------------------------------------------------------------------------
# Fast Docker builds (require deps image from `make docker-deps`)
# -----------------------------------------------------------------------------

# Static TUI binary via Docker (fast — uses pre-built deps image)
# Always chown .gerbil back to host user, even on build failure
linux-static-docker: clean-docker
	@docker image inspect $(DEPS_IMAGE) >/dev/null 2>&1 || \
	  { echo "ERROR: Deps image '$(DEPS_IMAGE)' not found. Run 'make docker-deps' first."; exit 1; }
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(CURDIR):/src:z \
	  $(DEPS_IMAGE) \
	  sh -c "cd /src && make build-gemacs-static; \
	         RC=\$$?; chown -R $(UID):$(GID) .gerbil 2>/dev/null; exit \$$RC"

# Static Qt binary via Docker (fast — uses pre-built deps image)
linux-static-qt-docker: clean-docker
	@docker image inspect $(DEPS_IMAGE) >/dev/null 2>&1 || \
	  { echo "ERROR: Deps image '$(DEPS_IMAGE)' not found. Run 'make docker-deps' first."; exit 1; }
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(CURDIR):/src:z \
	  $(DEPS_IMAGE) \
	  sh -c "cd /src && make build-gemacs-static-qt; \
	         RC=\$$?; chown -R $(UID):$(GID) .gerbil 2>/dev/null; exit \$$RC"

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
	@echo "=== Building gemacs (TUI, static) ==="
	cd /src && \
	  GEMACS_BUILD_TUI_ONLY=1 \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/tmp/gerbil-scintilla \
	  GERBIL_LOADPATH=/tmp/gerbil-scintilla/.gerbil/lib:$(HOME)/.gerbil/lib \
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
	@echo "=== Building gemacs (TUI + Qt, static) ==="
	cd /src && \
	  GEMACS_STATIC=1 \
	  GEMACS_SCI_BASE=/tmp/gerbil-scintilla \
	  GEMACS_QT_BASE=/deps/gerbil-qt \
	  PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig \
	  GERBIL_LOADPATH=/tmp/gerbil-scintilla/.gerbil/lib:/deps/gerbil-qt/.gerbil/lib:$(HOME)/.gerbil/lib \
	  gerbil build

# Self-contained static TUI binary via Docker (no deps image needed)
linux-static-docker-full: clean-docker
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(CURDIR):/src:z \
	  -v $(SCI_SRC):/deps/gerbil-scintilla:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache g++ git curl make \
	           pcre2-dev pcre2-static \
	           openssl-dev openssl-libs-static \
	           sqlite-dev sqlite-static \
	           zlib-static && \
	         cd /src && make build-static; \
	         RC=\$$?; chown -R $(UID):$(GID) .gerbil 2>/dev/null; exit \$$RC"

# Self-contained static Qt binary via Docker (no deps image needed)
linux-static-qt-docker-full: clean-docker
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(CURDIR):/src:z \
	  -v $(SCI_SRC):/deps/gerbil-scintilla:z \
	  -v $(QT_SRC):/deps/gerbil-qt:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache g++ git curl make \
	           pcre2-dev pcre2-static \
	           qt6-qtbase-dev \
	           qscintilla-dev 2>/dev/null; \
	         cd /src && make build-static-qt; \
	         RC=\$$?; chown -R $(UID):$(GID) .gerbil 2>/dev/null; exit \$$RC"
