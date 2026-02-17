.PHONY: all build clean test install install-qt

export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib
SCI_RPATH = $(HOME)/.gerbil/lib/gerbil-scintilla
QT_SHIM_RPATH = $(HOME)/.gerbil/lib/gerbil-qt

all: build

build:
	chmod +x build.ss
	LD_LIBRARY_PATH=$(OPENSSL_RPATH) gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH) .gerbil/bin/gemacs
	-patchelf --set-rpath $(OPENSSL_RPATH):$(SCI_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/gemacs-qt

clean:
	gerbil clean
	rm -rf .gerbil
	@# Remove stale global static artifacts that can shadow local builds
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.scm
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.c
	rm -f $(HOME)/.gerbil/lib/static/gemacs__*.o

test: build
	gerbil test

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
