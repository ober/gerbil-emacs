;;; -*- Gerbil -*-
;;; Image viewing for gerbil-emacs Qt backend
;;;
;;; Supports two modes:
;;; 1. Inline display: images shown inside the editor area via QStackedWidget
;;; 2. Dialog display: legacy popup dialog (qt-view-image!)

(export image-file?
        qt-view-image!
        qt-ensure-image-widget!
        qt-show-image-buffer!
        qt-hide-image-buffer!
        qt-image-zoom!
        qt-image-cleanup!)

(import :std/sugar
        :std/srfi/13
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core
        :gerbil-emacs/qt/window)

;;;============================================================================
;;; Image file detection
;;;============================================================================

(def *image-extensions*
  '(".png" ".jpg" ".jpeg" ".gif" ".bmp" ".webp" ".svg" ".ico" ".tiff" ".tif"))

(def (image-file? path)
  "Check if path has an image file extension."
  (and path
       (let ((ext (string-downcase (path-extension path))))
         (member ext *image-extensions*))))

;;;============================================================================
;;; Inline image display (QStackedWidget approach)
;;;============================================================================

(def (qt-ensure-image-widget! win)
  "Ensure the qt-edit-window has an image widget at stacked index 1.
   Creates QScrollArea + QLabel lazily on first call."
  (unless (qt-edit-window-image-scroll win)
    (let* ((container (qt-edit-window-container win))
           (scroll (qt-scroll-area-create parent: container))
           (label (qt-label-create "" parent: scroll)))
      (qt-scroll-area-set-widget! scroll label)
      (qt-scroll-area-set-widget-resizable! scroll #f)
      (qt-widget-set-style-sheet! scroll "background: #202020;")
      (qt-label-set-alignment! label (bitwise-ior QT_ALIGN_CENTER QT_ALIGN_CENTER))
      (qt-stacked-widget-add-widget! container scroll)
      (set! (qt-edit-window-image-scroll win) scroll)
      (set! (qt-edit-window-image-label win) label))))

(def (qt-show-image-buffer! editor buf)
  "Display an image buffer inline. Loads pixmap into the label and
   switches the stacked widget to the image view (index 1)."
  (let ((win (hash-get *editor-window-map* editor)))
    (when win
      (qt-ensure-image-widget! win)
      (let* ((state (hash-get *image-buffer-state* buf))
             (container (qt-edit-window-container win))
             (label (qt-edit-window-image-label win)))
        (when state
          (let* ((pixmap (list-ref state 0))
                 (zoom-ref (list-ref state 1))
                 (orig-w (list-ref state 2))
                 (orig-h (list-ref state 3))
                 (zoom (unbox zoom-ref))
                 (new-w (inexact->exact (round (* orig-w zoom))))
                 (new-h (inexact->exact (round (* orig-h zoom)))))
            (when (and (> new-w 0) (> new-h 0))
              (let ((scaled (qt-pixmap-scaled pixmap new-w new-h)))
                (qt-label-set-pixmap! label scaled)
                (qt-widget-set-minimum-size! label new-w new-h)))))
        (qt-stacked-widget-set-current-index! container 1)))))

(def (qt-hide-image-buffer! editor)
  "Switch the stacked widget back to the editor view (index 0)."
  (let ((win (hash-get *editor-window-map* editor)))
    (when win
      (let ((container (qt-edit-window-container win)))
        (qt-stacked-widget-set-current-index! container 0)))))

(def (qt-image-zoom! app editor buf factor)
  "Zoom the image buffer by FACTOR. Use 'fit for fit-to-window, 'reset for 100%."
  (let* ((state (hash-get *image-buffer-state* buf))
         (win (hash-get *editor-window-map* editor)))
    (when (and state win)
      (let* ((pixmap (list-ref state 0))
             (zoom-ref (list-ref state 1))
             (orig-w (list-ref state 2))
             (orig-h (list-ref state 3))
             (new-zoom
              (cond
                ((eq? factor 'fit)
                 (let* ((scroll (qt-edit-window-image-scroll win))
                        (cw (qt-widget-width scroll))
                        (ch (qt-widget-height scroll)))
                   (if (and (> orig-w 0) (> orig-h 0) (> cw 0) (> ch 0))
                     (min (/ (exact->inexact cw) orig-w)
                          (/ (exact->inexact ch) orig-h))
                     1.0)))
                ((eq? factor 'reset) 1.0)
                (else (min 10.0 (max 0.1 (* (unbox zoom-ref) factor))))))
             (label (qt-edit-window-image-label win))
             (new-w (inexact->exact (round (* orig-w new-zoom))))
             (new-h (inexact->exact (round (* orig-h new-zoom)))))
        (set-box! zoom-ref new-zoom)
        (when (and (> new-w 0) (> new-h 0))
          (let ((scaled (qt-pixmap-scaled pixmap new-w new-h)))
            (qt-label-set-pixmap! label scaled)
            (qt-widget-set-minimum-size! label new-w new-h)))
        (echo-message! (app-state-echo app)
          (string-append (number->string
                           (inexact->exact (round (* new-zoom 100)))) "%"))))))

(def (qt-image-cleanup! buf)
  "Destroy pixmap and clean up state for an image buffer being killed."
  (let ((state (hash-get *image-buffer-state* buf)))
    (when state
      (qt-pixmap-destroy! (list-ref state 0))
      (hash-remove! *image-buffer-state* buf))))

;;;============================================================================
;;; Image viewing in a dialog (legacy)
;;;============================================================================

(def (qt-view-image! app parent filename)
  "Open an image file in a dialog window with zoom controls."
  (let* ((pixmap (qt-pixmap-load filename)))
    (if (qt-pixmap-null? pixmap)
      (begin
        (qt-pixmap-destroy! pixmap)
        (echo-error! (app-state-echo app)
                     (string-append "Failed to load image: " filename))
        #f)
      ;; Create dialog
      (let* ((dialog (qt-dialog-create parent: parent))
             (layout (qt-vbox-layout-create dialog))
             ;; Zoom control buttons
             (btn-zoom-in (qt-push-button-create "+" parent: dialog))
             (btn-zoom-out (qt-push-button-create "-" parent: dialog))
             (btn-zoom-fit (qt-push-button-create "Fit" parent: dialog))
             (btn-zoom-100 (qt-push-button-create "100%" parent: dialog))
             (zoom-label (qt-label-create "100%" parent: dialog))
             ;; Scroll area for image
             (scroll-area (qt-scroll-area-create parent: dialog))
             (img-label (qt-label-create "" parent: scroll-area))
             ;; State
             (orig-w (qt-pixmap-width pixmap))
             (orig-h (qt-pixmap-height pixmap))
             (zoom-ref (box 1.0)))

        ;; Helper to update display
        (def (update-image!)
          (let* ((zoom (unbox zoom-ref))
                 (new-w (inexact->exact (round (* orig-w zoom))))
                 (new-h (inexact->exact (round (* orig-h zoom)))))
            (when (and (> new-w 0) (> new-h 0))
              (let ((scaled (qt-pixmap-scaled pixmap new-w new-h)))
                (qt-label-set-pixmap! img-label scaled)
                (qt-widget-set-minimum-size! img-label new-w new-h)
                (qt-label-set-text! zoom-label
                  (string-append (number->string (inexact->exact (round (* zoom 100)))) "%"))))))

        ;; Configure scroll area
        (qt-scroll-area-set-widget! scroll-area img-label)
        (qt-scroll-area-set-widget-resizable! scroll-area #f)
        (qt-widget-set-style-sheet! img-label "background: #202020;")

        ;; Set initial image and size label to match
        (qt-label-set-pixmap! img-label pixmap)
        (qt-widget-set-minimum-size! img-label orig-w orig-h)

        ;; Configure dialog
        (qt-dialog-set-title! dialog (string-append "Image: " (path-strip-directory filename)))
        (qt-widget-resize! dialog (min 1200 (+ orig-w 40)) (min 800 (+ orig-h 80)))

        ;; Zoom handlers
        (qt-on-clicked! btn-zoom-in
          (lambda ()
            (set-box! zoom-ref (min 10.0 (* (unbox zoom-ref) 1.25)))
            (update-image!)))

        (qt-on-clicked! btn-zoom-out
          (lambda ()
            (set-box! zoom-ref (max 0.1 (/ (unbox zoom-ref) 1.25)))
            (update-image!)))

        (qt-on-clicked! btn-zoom-100
          (lambda ()
            (set-box! zoom-ref 1.0)
            (update-image!)))

        (qt-on-clicked! btn-zoom-fit
          (lambda ()
            (let* ((container-w (qt-widget-width scroll-area))
                   (container-h (qt-widget-height scroll-area)))
              (when (and (> orig-w 0) (> orig-h 0)
                         (> container-w 0) (> container-h 0))
                (let* ((scale-w (/ (exact->inexact container-w) orig-w))
                       (scale-h (/ (exact->inexact container-h) orig-h))
                       (new-zoom (min scale-w scale-h)))
                  (set-box! zoom-ref new-zoom)
                  (update-image!))))))

        ;; Add widgets to main layout
        (let* ((btn-row (qt-widget-create parent: dialog))
               (btn-layout (qt-hbox-layout-create btn-row)))
          (qt-layout-add-widget! btn-layout btn-zoom-out)
          (qt-layout-add-widget! btn-layout btn-zoom-in)
          (qt-layout-add-widget! btn-layout btn-zoom-fit)
          (qt-layout-add-widget! btn-layout btn-zoom-100)
          (qt-layout-add-widget! btn-layout zoom-label)
          (qt-layout-add-stretch! btn-layout)
          (qt-layout-add-widget! layout btn-row))

        (qt-layout-add-widget! layout scroll-area)
        (qt-layout-set-margins! layout 5 5 5 5)

        ;; Install key handler for +/- zoom
        (qt-on-key-press! dialog
          (lambda ()
            (let ((key (qt-last-key-code)))
              (cond
                ((= key QT_KEY_ESCAPE)
                 (qt-dialog-reject! dialog))
                ((or (= key 43) ; + key
                     (= key 61)) ; = key (for + without shift)
                 (set-box! zoom-ref (min 10.0 (* (unbox zoom-ref) 1.25)))
                 (update-image!))
                ((= key 45) ; - key
                 (set-box! zoom-ref (max 0.1 (/ (unbox zoom-ref) 1.25)))
                 (update-image!))
                ((= key 48) ; 0 key - fit
                 (let* ((container-w (qt-widget-width scroll-area))
                        (container-h (qt-widget-height scroll-area)))
                   (when (and (> orig-w 0) (> orig-h 0)
                              (> container-w 0) (> container-h 0))
                     (let* ((scale-w (/ (exact->inexact container-w) orig-w))
                            (scale-h (/ (exact->inexact container-h) orig-h))
                            (new-zoom (min scale-w scale-h)))
                       (set-box! zoom-ref new-zoom)
                       (update-image!)))))
                ((= key 49) ; 1 key - 100%
                 (set-box! zoom-ref 1.0)
                 (update-image!))))))

        ;; Show dialog
        (qt-widget-show! dialog)
        (qt-dialog-exec! dialog)

        ;; Cleanup
        (qt-pixmap-destroy! pixmap)
        (qt-widget-destroy! dialog)
        #t))))
