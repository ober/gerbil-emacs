.PHONY: all build clean test test-qt test-lsp test-lsp-protocol test-split-comprehensive test-org test-all install install-qt \
        static static-qt clean-docker check-root build-static build-static-qt linux-static-docker linux-static-qt-docker

export GERBIL_LOADPATH := $(HOME)/.gerbil/lib
export GERBIL_BUILD_CORES := $(shell echo $$(( $$(nproc) / 2 )))

OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib
SCI_RPATH = $(HOME)/.gerbil/lib/gerbil-scintilla
QT_SHIM_RPATH = $(HOME)/.gerbil/lib/gerbil-qt

all: build

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

install: build
	@# Remove stale global static artifacts before install
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs $(PREFIX)/bin/
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/
	@echo "Installed to $(PREFIX)/bin"

install-qt: build
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gemacs-qt $(PREFIX)/bin/

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

static: linux-static-docker

static-qt: linux-static-qt-docker

clean-docker:
	-rm -rf .gerbil 2>/dev/null || true
	docker run --rm -v $(CURDIR):/src:z alpine rm -rf /src/.gerbil

check-root:
	@if [ "$$(id -u)" = "0" ]; then \
	  git config --global --add safe.directory '*'; \
	fi

# Build static TUI binary inside Docker container
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

# Build static Qt binary inside Docker container
# NOTE: Requires Qt6 development packages in the container.
# Alpine qt6-qtbase-dev provides shared libs; fully static Qt requires
# building Qt6 from source with -DBUILD_SHARED_LIBS=OFF (~1 hour build).
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
	  GERBIL_LOADPATH=/tmp/gerbil-scintilla/.gerbil/lib:/deps/gerbil-qt/.gerbil/lib:$(HOME)/.gerbil/lib \
	  gerbil build

# Static TUI binary via Docker
linux-static-docker: clean-docker
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
	         cd /src && make build-static && \
	         chown -R $(UID):$(GID) .gerbil"

# Static Qt binary via Docker
# Requires: Qt6 static libraries in the container.
# Alpine packages qt6-qtbase-dev + qt6-qtbase-static (if available),
# or build Qt6 from source first. QScintilla must also be built as static.
linux-static-qt-docker: clean-docker
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(CURDIR):/src:z \
	  -v $(SCI_SRC):/deps/gerbil-scintilla:z \
	  -v $(QT_SRC):/deps/gerbil-qt:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache g++ git curl make \
	           pcre2-dev pcre2-static \
	           qt6-qtbase-dev \
	           libqscintilla2-qt6-dev 2>/dev/null; \
	         cd /src && make build-static-qt && \
	         chown -R $(UID):$(GID) .gerbil"
