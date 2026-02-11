;;; -*- Gerbil -*-
;;; Executable entry point for gerbil-emacs

(export main)
(import :gerbil-emacs/editor
        :gerbil-emacs/window
        :gerbil-scintilla/tui
        (only-in :gerbil-emacs/app app-init! app-run!))

(def (main . args)
  (let ((app (app-init! args)))
    (try
      (app-run! app)
      (finally
        (frame-shutdown! (app-state-frame app))
        (tui-shutdown!)))))
