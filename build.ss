#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Build script for gerbil-emacs

(import :std/build-script)

(defbuild-script
  `("keymap"
    "buffer"
    "window"
    "modeline"
    "echo"
    "editor"
    "app"
    "emacs-test"))
