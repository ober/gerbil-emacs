;;; -*- Gerbil -*-
;;; Executable entry point for gerbil-emacs Qt backend

(export main)
(import (only-in :gerbil-emacs/qt/app qt-main))

(def (main . args)
  (apply qt-main args))
