# Intermediate Docker image for gemacs static builds.
# Bakes static Qt6 + QScintilla from source, plus gerbil-scintilla +
# gerbil-pcre2 + gerbil-qt so that subsequent gemacs builds only compile
# gemacs itself (~2-5 min instead of ~10-15 min).
#
# Build with:
#   make docker-deps
#
# Uses BuildKit --build-context to pull dependency sources without
# copying them into the project directory.

ARG ARCH=x86_64
FROM gerbil/gerbilxx:${ARCH}-master

# ── Phase 1: Alpine build deps ──────────────────────────────────────────
RUN apk add --no-cache \
    cmake ninja-build perl python3 linux-headers \
    libxcb-dev xcb-util-dev xcb-util-image-dev \
    xcb-util-keysyms-dev xcb-util-renderutil-dev \
    xcb-util-wm-dev xcb-util-cursor-dev \
    libx11-dev libxkbcommon-dev \
    fontconfig-dev freetype-dev harfbuzz-dev \
    libpng-dev zlib-dev mesa-dev \
    pcre2-dev pcre2-static \
    at-spi2-core-dev libdrm-dev \
    zlib-static libxcb-static

# ── Phase 2: Build Qt6 qtbase static (~1-3 hours, cached) ──────────────
ARG QT6_VERSION=6.8.3
RUN wget -q https://download.qt.io/official_releases/qt/6.8/${QT6_VERSION}/submodules/qtbase-everywhere-src-${QT6_VERSION}.tar.xz && \
    tar xf qtbase-everywhere-src-${QT6_VERSION}.tar.xz && \
    rm qtbase-everywhere-src-${QT6_VERSION}.tar.xz && \
    cmake -S qtbase-everywhere-src-${QT6_VERSION} -B qt6-build -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=/opt/qt6-static \
      -DBUILD_SHARED_LIBS=OFF \
      -DQT_BUILD_EXAMPLES=OFF \
      -DQT_BUILD_TESTS=OFF \
      -DQT_BUILD_BENCHMARKS=OFF \
      -DFEATURE_xcb=ON \
      -DFEATURE_sql=OFF \
      -DFEATURE_network=OFF \
      -DFEATURE_testlib=OFF \
      -DFEATURE_printsupport=OFF \
      -DFEATURE_dbus=OFF \
      -DFEATURE_opengl=OFF \
      -DFEATURE_vulkan=OFF && \
    cmake --build qt6-build --parallel && \
    cmake --install qt6-build && \
    rm -rf qtbase-everywhere-src-${QT6_VERSION} qt6-build

# ── Phase 3: Build QScintilla static (~5-15 min) ───────────────────────
ARG QSCI_VERSION=2.14.1
RUN wget -q https://www.riverbankcomputing.com/static/Downloads/QScintilla/${QSCI_VERSION}/QScintilla_src-${QSCI_VERSION}.tar.gz && \
    tar xf QScintilla_src-${QSCI_VERSION}.tar.gz && \
    rm QScintilla_src-${QSCI_VERSION}.tar.gz && \
    cd QScintilla_src-${QSCI_VERSION}/src && \
    /opt/qt6-static/bin/qmake CONFIG+=staticlib && \
    make -j$(nproc) && \
    make install && \
    cd / && rm -rf QScintilla_src-${QSCI_VERSION}

# ── Phase 4: Gerbil dependencies ────────────────────────────────────────
ENV PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig

# Build gerbil-scintilla: strip glibc vendor artifacts, rebuild for musl
COPY --from=sci-src . /deps/gerbil-scintilla
RUN find /deps/gerbil-scintilla/vendor -name "*.o" -o -name "*.a" | xargs rm -f 2>/dev/null; \
    rm -rf /deps/gerbil-scintilla/.gerbil && \
    cd /deps/gerbil-scintilla && make vendor-deps
RUN gxpkg link gerbil-scintilla /deps/gerbil-scintilla && \
    cd /deps/gerbil-scintilla && gerbil build

# Install gerbil-pcre2
RUN gxpkg install github.com/ober/gerbil-pcre2

# Build gerbil-qt
COPY --from=qt-src . /deps/gerbil-qt
RUN gxpkg link gerbil-qt /deps/gerbil-qt && \
    cd /deps/gerbil-qt && \
    PKG_CONFIG_PATH=/opt/qt6-static/lib/pkgconfig \
    GERBIL_LOADPATH=/deps/gerbil-scintilla/.gerbil/lib:/root/.gerbil/lib \
    gerbil build

# Build static libqt_shim.a alongside the shared .so
RUN cd /deps/gerbil-qt && \
    QT_CFLAGS=$(pkg-config --cflags Qt6Widgets 2>/dev/null || echo "-I/opt/qt6-static/include -I/opt/qt6-static/include/QtCore -I/opt/qt6-static/include/QtGui -I/opt/qt6-static/include/QtWidgets") && \
    QSCI_FLAGS="-DQT_SCINTILLA_AVAILABLE $(pkg-config --cflags QScintilla 2>/dev/null || echo "-I/opt/qt6-static/include/Qsci -I/opt/qt6-static/include")" && \
    g++ -c -fPIC -std=c++17 \
      $QT_CFLAGS $QSCI_FLAGS \
      -I/deps/gerbil-scintilla/vendor/scintilla/include \
      vendor/qt_shim.cpp -o vendor/qt_shim_static.o && \
    ar rcs vendor/libqt_shim.a vendor/qt_shim_static.o

WORKDIR /src
