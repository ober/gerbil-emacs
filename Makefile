.PHONY: all build clean test install build-qt install-qt

# Dependencies: gerbil-scintilla (TUI) and gerbil-qt (Qt) must be built first
SCINTILLA_DIR = $(HOME)/mine/gerbil-scintilla
QT_DIR = $(HOME)/mine/gerbil-qt
export GERBIL_LOADPATH = $(SCINTILLA_DIR)/.gerbil/lib:$(QT_DIR)/.gerbil/lib

all: build

build:
	chmod +x build.ss
	gerbil build

clean:
	gerbil clean
	rm -rf .gerbil

test: build
	gerbil test

PREFIX ?= $(HOME)/.local

install: build
	mkdir -p $(PREFIX)/bin
	cp .gerbil/bin/gerbil-emacs $(PREFIX)/bin/gerbil-emacs
	cp .gerbil/bin/gerbil-emacs-qt $(PREFIX)/bin/gerbil-emacs-qt

install-qt: build
	mkdir -p $(PREFIX)/bin
	cp .gerbil/bin/gerbil-emacs-qt $(PREFIX)/bin/gerbil-emacs-qt
