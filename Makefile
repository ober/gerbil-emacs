.PHONY: all build clean test

# gerbil-scintilla must be built first; point GERBIL_LOADPATH to it
SCINTILLA_DIR = $(HOME)/mine/gerbil-scintilla
export GERBIL_LOADPATH = $(SCINTILLA_DIR)/.gerbil/lib

all: build

build:
	chmod +x build.ss
	gerbil build

clean:
	gerbil clean
	rm -rf .gerbil

test: build
	gerbil test
