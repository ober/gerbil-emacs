.PHONY: all build clean test install install-qt

export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib
QT_SHIM_RPATH = $(HOME)/.gerbil/lib/gerbil-qt

all: build

build:
	chmod +x build.ss
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) .gerbil/bin/gerbil-emacs
	patchelf --set-rpath $(OPENSSL_RPATH):$(QT_SHIM_RPATH) .gerbil/bin/gerbil-emacs-qt

clean:
	gerbil clean
	rm -rf .gerbil

test: build
	gerbil test

PREFIX ?= $(HOME)/.local

install: build
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gerbil-emacs $(PREFIX)/bin/
	cp -f .gerbil/bin/gerbil-emacs-qt $(PREFIX)/bin/
	@echo "Installed to $(PREFIX)/bin"

install-qt: build
	mkdir -p $(PREFIX)/bin
	cp -f .gerbil/bin/gerbil-emacs-qt $(PREFIX)/bin/
