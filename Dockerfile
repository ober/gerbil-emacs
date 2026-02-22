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
FROM gerbil/gerbil:${ARCH}-master

# ── Phase 1: Alpine build deps ──────────────────────────────────────────
RUN apk add --no-cache \
    cmake samurai perl python3 linux-headers patchelf \
    libxcb-dev xcb-util-dev xcb-util-image-dev \
    xcb-util-keysyms-dev xcb-util-renderutil-dev \
    xcb-util-wm-dev xcb-util-cursor-dev \
    libx11-dev libxkbcommon-dev \
    fontconfig-dev freetype-dev harfbuzz-dev \
    libpng-dev zlib-dev mesa-dev \
    pcre2-dev pcre2-static \
    at-spi2-core-dev libdrm-dev \
    zlib-static libxcb-static \
    fontconfig-static freetype-static harfbuzz-static \
    libpng-static bzip2-static expat-static brotli-static \
    libx11-static graphite2-static libxkbcommon-static

# Build static libXau (no Alpine -static package available)
RUN apk add --no-cache libxau-dev && \
    cd /tmp && \
    wget -q https://xorg.freedesktop.org/releases/individual/lib/libXau-1.0.12.tar.xz && \
    tar xf libXau-1.0.12.tar.xz && \
    cd libXau-1.0.12 && \
    ./configure --prefix=/usr --enable-static --disable-shared && \
    make -j$(nproc) && make install && \
    cd / && rm -rf /tmp/libXau-1.0.12*

# Build static libxcb-util (no Alpine -static package; needed by xcb-image)
RUN cd /tmp && \
    wget -q https://xcb.freedesktop.org/dist/xcb-util-0.4.1.tar.xz && \
    tar xf xcb-util-0.4.1.tar.xz && \
    cd xcb-util-0.4.1 && \
    ./configure --prefix=/usr --enable-static --disable-shared && \
    make -j$(nproc) && make install && \
    cd / && rm -rf /tmp/xcb-util-0.4.1*

# ── Phase 2: Build Qt6 qtbase static (~7-10 min, cached) ────────────────
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
      -DFEATURE_printsupport=ON \
      -DFEATURE_dbus=OFF \
      -DFEATURE_opengl=OFF \
      -DFEATURE_vulkan=OFF \
      -DFEATURE_glib=OFF \
      -DFEATURE_icu=OFF && \
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

# ── Phase 3b: Generate pkg-config files with full transitive deps ─────
# Static Qt6 cmake doesn't generate .pc files.  We extract direct deps
# from .prl files, then add known transitive deps manually (because
# Alpine's system harfbuzz was built with glib, but our Qt6 was not).
RUN mkdir -p /opt/qt6-static/lib/pkgconfig && \
    # Helper: extract -l flags from .prl, skip Qt internal refs
    prl_libs() { \
      grep '^QMAKE_PRL_LIBS ' "$1" | \
        sed 's/^QMAKE_PRL_LIBS *= *//' | \
        tr ' ' '\n' | grep '^-l' | \
        grep -v '^-lQt6' | tr '\n' ' '; \
    } && \
    CORE_PRIVATE=$(prl_libs /opt/qt6-static/lib/libQt6Core.prl) && \
    GUI_PRL=$(prl_libs /opt/qt6-static/lib/libQt6Gui.prl) && \
    XCB_PRL=$(prl_libs /opt/qt6-static/plugins/platforms/libqxcb.prl) && \
    # Add transitive deps that .prl files don't include:
    #   harfbuzz → graphite2       freetype → bz2, brotlidec, brotlicommon
    #   fontconfig → expat         libpng → zlib
    #   xcb-image → xcb-util       xcb → Xau, Xdmcp
    # Filter out glib/intl (Qt6 built with -DFEATURE_glib=OFF)
    TRANSITIVE="-lgraphite2 -lbz2 -lbrotlidec -lbrotlicommon -lexpat -lXau -lXdmcp" && \
    GUI_PRIVATE="$GUI_PRL $TRANSITIVE" && \
    XCB_TRANSITIVE="-lxcb-util -lxcb -lXau -lXdmcp" && \
    XCB_PRIVATE="$XCB_PRL $TRANSITIVE $XCB_TRANSITIVE" && \
    # Debug: show resolved deps
    echo "Core deps: $CORE_PRIVATE" && \
    echo "Gui deps: $GUI_PRIVATE" && \
    echo "XCB deps: $XCB_PRIVATE" && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      '' \
      'Name: Qt6Core' 'Description: Qt6 Core' 'Version: 6.8.3' \
      'Cflags: -I${includedir} -I${includedir}/QtCore' \
      "Libs: -L\${libdir} -lQt6Core" \
      "Libs.private: $CORE_PRIVATE" \
      > /opt/qt6-static/lib/pkgconfig/Qt6Core.pc && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      '' \
      'Name: Qt6Gui' 'Description: Qt6 Gui' 'Version: 6.8.3' \
      'Requires: Qt6Core' \
      'Cflags: -I${includedir} -I${includedir}/QtGui' \
      "Libs: -L\${libdir} -lQt6Gui" \
      "Libs.private: $GUI_PRIVATE" \
      > /opt/qt6-static/lib/pkgconfig/Qt6Gui.pc && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      '' \
      'Name: Qt6Widgets' 'Description: Qt6 Widgets' 'Version: 6.8.3' \
      'Requires: Qt6Gui' \
      'Cflags: -I${includedir} -I${includedir}/QtWidgets -I${includedir}/QtGui -I${includedir}/QtCore' \
      "Libs: -L\${libdir} -lQt6Widgets" \
      > /opt/qt6-static/lib/pkgconfig/Qt6Widgets.pc && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      '' \
      'Name: Qt6PrintSupport' 'Description: Qt6 PrintSupport' 'Version: 6.8.3' \
      'Requires: Qt6Widgets' \
      'Cflags: -I${includedir} -I${includedir}/QtPrintSupport' \
      "Libs: -L\${libdir} -lQt6PrintSupport" \
      > /opt/qt6-static/lib/pkgconfig/Qt6PrintSupport.pc && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      'plugindir=${prefix}/plugins' \
      '' \
      'Name: Qt6XcbPlugin' 'Description: Qt6 XCB platform plugin' 'Version: 6.8.3' \
      'Requires: Qt6Gui' \
      "Libs: -L\${libdir} -L\${plugindir}/platforms -lqxcb -lQt6XcbQpa" \
      "Libs.private: $XCB_PRIVATE" \
      > /opt/qt6-static/lib/pkgconfig/Qt6XcbPlugin.pc && \
    printf '%s\n' \
      'prefix=/opt/qt6-static' \
      'includedir=${prefix}/include' \
      'libdir=${prefix}/lib' \
      '' \
      'Name: QScintilla' 'Description: QScintilla for Qt6' 'Version: 2.14.1' \
      'Requires: Qt6Widgets Qt6PrintSupport' \
      'Cflags: -I${includedir} -I${includedir}/Qsci' \
      "Libs: -L\${libdir} -lqscintilla2_qt6" \
      > /opt/qt6-static/lib/pkgconfig/QScintilla.pc && \
    echo "Generated .pc files:" && ls /opt/qt6-static/lib/pkgconfig/

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

# Build static libqt_shim.a + qt_static_plugins.o (kept separate)
# The plugins .o must NOT be in the .a archive — it contains Q_IMPORT_PLUGIN
# static constructors that the linker would drop from an archive unless
# --whole-archive is used (which pulls in unwanted transitive deps).
RUN cd /deps/gerbil-qt && \
    QT_CFLAGS=$(pkg-config --cflags Qt6Widgets 2>/dev/null || echo "-I/opt/qt6-static/include -I/opt/qt6-static/include/QtCore -I/opt/qt6-static/include/QtGui -I/opt/qt6-static/include/QtWidgets") && \
    QSCI_FLAGS="-DQT_SCINTILLA_AVAILABLE $(pkg-config --cflags QScintilla 2>/dev/null || echo "-I/opt/qt6-static/include/Qsci -I/opt/qt6-static/include")" && \
    g++ -c -fPIC -std=c++17 \
      $QT_CFLAGS $QSCI_FLAGS \
      -I/deps/gerbil-scintilla/vendor/scintilla/include \
      vendor/qt_shim.cpp -o vendor/qt_shim_static.o && \
    ar rcs vendor/libqt_shim.a vendor/qt_shim_static.o && \
    printf '#include <QtPlugin>\nQ_IMPORT_PLUGIN(QXcbIntegrationPlugin)\n' \
      > vendor/qt_static_plugins.cpp && \
    g++ -c -fPIC -std=c++17 $QT_CFLAGS \
      vendor/qt_static_plugins.cpp -o vendor/qt_static_plugins.o

WORKDIR /src
