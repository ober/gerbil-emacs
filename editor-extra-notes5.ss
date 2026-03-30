;;; -*- Gerbil -*-
;;; Real implementations: tetris, snake, dunnet, 2048, animate,
;;; type-break, highlight-changes, glasses-mode, solitaire, blackbox.
;;; Replaces stubs. New module for 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/ports
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        (only-in :gerbil-scintilla/ffi scintilla-send-message-string scintilla-editor-handle)
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers)

;;;============================================================================
;;; Helper
;;;============================================================================
(def (n5-get-text ed start len)
  (if (<= len 0) ""
    (begin
      (send-message ed SCI_SETTARGETSTART start 0)
      (send-message ed SCI_SETTARGETEND (+ start len) 0)
      (let ((buf (make-string (+ len 1) #\nul)))
        (scintilla-send-message-string
          (scintilla-editor-handle ed) SCI_GETTARGETTEXT 0 buf)
        (let ((nul-pos (string-index buf #\nul)))
          (if nul-pos (substring buf 0 nul-pos) buf))))))

;;;============================================================================
;;; 1. Tetris — Real turn-based Tetris
;;;============================================================================

(def *tetris-width* 10)
(def *tetris-height* 18)
(def *tetris-board* #f)
(def *tetris-piece* #f) ;; list of (x . y) offsets
(def *tetris-px* 4)     ;; piece position
(def *tetris-py* 0)
(def *tetris-score* 0)
(def *tetris-game-over* #f)

(def *tetris-pieces*
  '(;; I
    ((0 . 0) (1 . 0) (2 . 0) (3 . 0))
    ;; O
    ((0 . 0) (1 . 0) (0 . 1) (1 . 1))
    ;; T
    ((0 . 0) (1 . 0) (2 . 0) (1 . 1))
    ;; S
    ((1 . 0) (2 . 0) (0 . 1) (1 . 1))
    ;; Z
    ((0 . 0) (1 . 0) (1 . 1) (2 . 1))
    ;; L
    ((0 . 0) (0 . 1) (0 . 2) (1 . 2))
    ;; J
    ((1 . 0) (1 . 1) (1 . 2) (0 . 2))))

(def (tetris-init!)
  (set! *tetris-board* (make-vector (* *tetris-width* *tetris-height*) #f))
  (set! *tetris-score* 0)
  (set! *tetris-game-over* #f)
  (tetris-new-piece!))

(def (tetris-new-piece!)
  (set! *tetris-piece* (list-ref *tetris-pieces* (random-integer (length *tetris-pieces*))))
  (set! *tetris-px* 3) (set! *tetris-py* 0)
  (when (tetris-collision? *tetris-px* *tetris-py* *tetris-piece*)
    (set! *tetris-game-over* #t)))

(def (tetris-get x y)
  (and (>= x 0) (< x *tetris-width*) (>= y 0) (< y *tetris-height*)
       (vector-ref *tetris-board* (+ x (* y *tetris-width*)))))

(def (tetris-set! x y v)
  (when (and (>= x 0) (< x *tetris-width*) (>= y 0) (< y *tetris-height*))
    (vector-set! *tetris-board* (+ x (* y *tetris-width*)) v)))

(def (tetris-collision? px py piece)
  (any (lambda (p)
         (let ((x (+ px (car p))) (y (+ py (cdr p))))
           (or (< x 0) (>= x *tetris-width*) (>= y *tetris-height*)
               (and (>= y 0) (tetris-get x y)))))
       piece))

(def (tetris-lock-piece!)
  (for-each (lambda (p)
              (tetris-set! (+ *tetris-px* (car p)) (+ *tetris-py* (cdr p)) #t))
    *tetris-piece*)
  (tetris-clear-lines!)
  (tetris-new-piece!))

(def (tetris-clear-lines!)
  (let loop ((y (- *tetris-height* 1)) (cleared 0))
    (when (>= y 0)
      (if (every (lambda (x) (tetris-get x y)) (iota *tetris-width*))
        (begin
          ;; Shift everything down
          (let yy ((row y))
            (when (> row 0)
              (let xx ((x 0))
                (when (< x *tetris-width*)
                  (tetris-set! x row (tetris-get x (- row 1)))
                  (xx (+ x 1))))
              (yy (- row 1))))
          (let xx ((x 0))
            (when (< x *tetris-width*) (tetris-set! x 0 #f) (xx (+ x 1))))
          (set! *tetris-score* (+ *tetris-score* 100))
          (loop y (+ cleared 1)))
        (loop (- y 1) cleared)))))

(def (tetris-rotate-piece)
  (map (lambda (p) (cons (- (cdr p)) (car p))) *tetris-piece*))

(def (tetris-render)
  (with-output-to-string
    (lambda ()
      (display "  TETRIS")
      (display (if *tetris-game-over* "  -- GAME OVER --" ""))
      (display "\n  Score: ") (display *tetris-score*) (display "\n\n")
      (display "  +") (display (make-string (* *tetris-width* 2) #\-)) (display "+\n")
      (let yloop ((y 0))
        (when (< y *tetris-height*)
          (display "  |")
          (let xloop ((x 0))
            (when (< x *tetris-width*)
              (let ((is-piece (and *tetris-piece* (not *tetris-game-over*)
                                  (any (lambda (p)
                                         (and (= x (+ *tetris-px* (car p)))
                                              (= y (+ *tetris-py* (cdr p)))))
                                       *tetris-piece*))))
                (display (cond (is-piece "[]")
                               ((tetris-get x y) "##")
                               (else "  "))))
              (xloop (+ x 1))))
          (display "|\n")
          (yloop (+ y 1))))
      (display "  +") (display (make-string (* *tetris-width* 2) #\-)) (display "+\n")
      (display "\n  tetris-left/right/rotate/drop/down to play\n"))))

(def (tetris-refresh! app)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (editor-set-text ed (tetris-render))
    (editor-goto-pos ed 0)))

(def (cmd-tetris-real app)
  "Play Tetris — real turn-based game."
  (tetris-init!)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Tetris*") (buffer-create! "*Tetris*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (tetris-refresh! app)
    (echo-message! (app-state-echo app) "Tetris! Use tetris-left/right/rotate/drop/down")))

(def (cmd-tetris-left app)
  (when (and *tetris-board* (not *tetris-game-over*))
    (unless (tetris-collision? (- *tetris-px* 1) *tetris-py* *tetris-piece*)
      (set! *tetris-px* (- *tetris-px* 1))))
  (tetris-refresh! app))

(def (cmd-tetris-right app)
  (when (and *tetris-board* (not *tetris-game-over*))
    (unless (tetris-collision? (+ *tetris-px* 1) *tetris-py* *tetris-piece*)
      (set! *tetris-px* (+ *tetris-px* 1))))
  (tetris-refresh! app))

(def (cmd-tetris-rotate app)
  (when (and *tetris-board* (not *tetris-game-over*))
    (let ((rotated (tetris-rotate-piece)))
      (unless (tetris-collision? *tetris-px* *tetris-py* rotated)
        (set! *tetris-piece* rotated))))
  (tetris-refresh! app))

(def (cmd-tetris-down app)
  (when (and *tetris-board* (not *tetris-game-over*))
    (if (tetris-collision? *tetris-px* (+ *tetris-py* 1) *tetris-piece*)
      (tetris-lock-piece!)
      (set! *tetris-py* (+ *tetris-py* 1))))
  (tetris-refresh! app))

(def (cmd-tetris-drop app)
  "Drop piece to bottom."
  (when (and *tetris-board* (not *tetris-game-over*))
    (let loop ()
      (if (tetris-collision? *tetris-px* (+ *tetris-py* 1) *tetris-piece*)
        (begin (set! *tetris-score* (+ *tetris-score* 10)) (tetris-lock-piece!))
        (begin (set! *tetris-py* (+ *tetris-py* 1)) (loop)))))
  (tetris-refresh! app))

;;;============================================================================
;;; 2. Snake — Real turn-based Snake
;;;============================================================================

(def *snake-w* 20) (def *snake-h* 10)
(def *snake-body* '()) ;; list of (x . y), head first
(def *snake-dir* 'right)
(def *snake-food* #f)
(def *snake-score* 0)
(def *snake-dead* #f)

(def (snake-init!)
  (set! *snake-body* (list (cons 5 5) (cons 4 5) (cons 3 5)))
  (set! *snake-dir* 'right) (set! *snake-score* 0) (set! *snake-dead* #f)
  (snake-place-food!))

(def (snake-place-food!)
  (let loop ()
    (let ((x (random-integer *snake-w*)) (y (random-integer *snake-h*)))
      (if (any (lambda (s) (and (= (car s) x) (= (cdr s) y))) *snake-body*)
        (loop)
        (set! *snake-food* (cons x y))))))

(def (snake-step!)
  (when (not *snake-dead*)
    (let* ((head (car *snake-body*))
           (nx (+ (car head) (case *snake-dir* ((left) -1) ((right) 1) (else 0))))
           (ny (+ (cdr head) (case *snake-dir* ((up) -1) ((down) 1) (else 0)))))
      (cond
        ;; Wall collision
        ((or (< nx 0) (>= nx *snake-w*) (< ny 0) (>= ny *snake-h*))
         (set! *snake-dead* #t))
        ;; Self collision
        ((any (lambda (s) (and (= (car s) nx) (= (cdr s) ny))) *snake-body*)
         (set! *snake-dead* #t))
        ;; Eat food
        ((and *snake-food* (= nx (car *snake-food*)) (= ny (cdr *snake-food*)))
         (set! *snake-body* (cons (cons nx ny) *snake-body*))
         (set! *snake-score* (+ *snake-score* 10))
         (snake-place-food!))
        ;; Normal move
        (else
         (set! *snake-body*
           (cons (cons nx ny)
             (let loop ((b *snake-body*))
               (if (null? (cdr b)) '()
                 (cons (car b) (loop (cdr b))))))))))))

(def (snake-render)
  (with-output-to-string
    (lambda ()
      (display "  SNAKE") (when *snake-dead* (display "  -- GAME OVER --"))
      (display "\n  Score: ") (display *snake-score*)
      (display "  Length: ") (display (length *snake-body*)) (display "\n\n")
      (display "  +") (display (make-string *snake-w* #\-)) (display "+\n")
      (let yloop ((y 0))
        (when (< y *snake-h*)
          (display "  |")
          (let xloop ((x 0))
            (when (< x *snake-w*)
              (display
                (cond
                  ((and (= x (caar *snake-body*)) (= y (cdar *snake-body*)))
                   (case *snake-dir* ((up) "^") ((down) "v") ((left) "<") ((right) ">")))
                  ((any (lambda (s) (and (= (car s) x) (= (cdr s) y))) (cdr *snake-body*)) "@")
                  ((and *snake-food* (= x (car *snake-food*)) (= y (cdr *snake-food*))) "*")
                  (else " ")))
              (xloop (+ x 1))))
          (display "|\n") (yloop (+ y 1))))
      (display "  +") (display (make-string *snake-w* #\-)) (display "+\n")
      (display "\n  snake-up/down/left/right to move, snake-step to advance\n"))))

(def (snake-refresh! app)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (editor-set-text ed (snake-render)) (editor-goto-pos ed 0)))

(def (cmd-snake-real app)
  "Play Snake — turn-based."
  (snake-init!)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Snake*") (buffer-create! "*Snake*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (snake-refresh! app)
    (echo-message! (app-state-echo app) "Snake! Move with snake-up/down/left/right, advance with snake-step")))

(def (cmd-snake-up app) (set! *snake-dir* 'up) (snake-step!) (snake-refresh! app))
(def (cmd-snake-down app) (set! *snake-dir* 'down) (snake-step!) (snake-refresh! app))
(def (cmd-snake-left app) (set! *snake-dir* 'left) (snake-step!) (snake-refresh! app))
(def (cmd-snake-right app) (set! *snake-dir* 'right) (snake-step!) (snake-refresh! app))
(def (cmd-snake-step app) (snake-step!) (snake-refresh! app))

;;;============================================================================
;;; 3. Dunnet — Real text adventure with rooms and items
;;;============================================================================

(def *dunnet-room* 'dead-end)
(def *dunnet-inventory* '())
(def *dunnet-history* "")

(def *dunnet-rooms* (make-hash-table))
(def *dunnet-room-items* (make-hash-table))
(begin
  (hash-put! *dunnet-rooms* 'dead-end
    '("Dead End"
      "You are at a dead end of a dirt road. The road goes to the east.\nIn the distance you can see that it will eventually fork off.\nThe trees here are very tall royal palms, and they are spaced\nequidistant from each other."
      ((east . fork))))
  (hash-put! *dunnet-rooms* 'fork
    '("Fork"
      "You are at a fork of two roads. To the southwest is a large\nfield, and a road goes to the northeast. There is a stream to\nthe southeast."
      ((west . dead-end) (southwest . field) (northeast . cave-entrance) (southeast . stream))))
  (hash-put! *dunnet-rooms* 'field
    '("Field"
      "You are in a large open field. There is a barn to the north.\nThe road goes to the northeast."
      ((northeast . fork) (north . barn))))
  (hash-put! *dunnet-rooms* 'barn
    '("Barn"
      "You are inside an old wooden barn. There is hay everywhere.\nA ladder leads to a loft above."
      ((south . field) (up . loft))))
  (hash-put! *dunnet-rooms* 'loft
    '("Loft"
      "You are in the loft of the barn. There is a window overlooking\nthe field. Dusty boxes are stacked in the corner."
      ((down . barn))))
  (hash-put! *dunnet-rooms* 'cave-entrance
    '("Cave Entrance"
      "You are at the entrance to a dark cave. The road continues\nto the southwest. A narrow passage leads into darkness."
      ((southwest . fork) (north . dark-cave))))
  (hash-put! *dunnet-rooms* 'dark-cave
    '("Dark Cave"
      "It is very dark here. You can barely see the walls of the cave.\nThere is a faint glow to the east."
      ((south . cave-entrance) (east . crystal-room))))
  (hash-put! *dunnet-rooms* 'crystal-room
    '("Crystal Room"
      "You are in a magnificent crystal cavern. Crystals of all colors\nline the walls, casting rainbow reflections."
      ((west . dark-cave))))
  (hash-put! *dunnet-rooms* 'stream
    '("Stream"
      "You are beside a peaceful stream. The water is crystal clear.\nA path leads northwest back to the fork."
      ((northwest . fork))))
  ;; Room items
  (hash-put! *dunnet-room-items* 'dead-end '("shovel"))
  (hash-put! *dunnet-room-items* 'field '("key"))
  (hash-put! *dunnet-room-items* 'loft '("lantern"))
  (hash-put! *dunnet-room-items* 'crystal-room '("diamond"))
  (hash-put! *dunnet-room-items* 'stream '("fish")))

(def (dunnet-describe-room)
  (let* ((info (hash-ref *dunnet-rooms* *dunnet-room*))
         (name (car info))
         (desc (cadr info))
         (items (hash-ref *dunnet-room-items* *dunnet-room* '())))
    (string-append name "\n\n" desc
      (if (null? items) ""
        (string-append "\n\nYou can see: " (string-join items ", ") "."))
      "\n")))

(def (dunnet-process-command cmd)
  (let* ((parts (string-split (string-downcase (string-trim-both cmd)) #\space))
         (verb (if (pair? parts) (car parts) ""))
         (noun (if (and (pair? parts) (pair? (cdr parts))) (cadr parts) "")))
    (cond
      ((member verb '("n" "north" "s" "south" "e" "east" "w" "west"
                       "ne" "northeast" "nw" "northwest" "se" "southeast"
                       "sw" "southwest" "up" "down"))
       (let* ((dir-sym (cond
                        ((member verb '("n" "north")) 'north)
                        ((member verb '("s" "south")) 'south)
                        ((member verb '("e" "east")) 'east)
                        ((member verb '("w" "west")) 'west)
                        ((member verb '("ne" "northeast")) 'northeast)
                        ((member verb '("nw" "northwest")) 'northwest)
                        ((member verb '("se" "southeast")) 'southeast)
                        ((member verb '("sw" "southwest")) 'southwest)
                        ((string=? verb "up") 'up)
                        ((string=? verb "down") 'down)
                        (else #f)))
              (exits (caddr (hash-ref *dunnet-rooms* *dunnet-room*)))
              (dest (assq dir-sym exits)))
         (if dest
           (begin (set! *dunnet-room* (cdr dest))
                  (dunnet-describe-room))
           "You can't go that way.\n")))
      ((string=? verb "look")
       (dunnet-describe-room))
      ((string=? verb "inventory")
       (if (null? *dunnet-inventory*)
         "You are empty-handed.\n"
         (string-append "You have: " (string-join *dunnet-inventory* ", ") ".\n")))
      ((member verb '("take" "get"))
       (let ((items (hash-ref *dunnet-room-items* *dunnet-room* '())))
         (if (member noun items)
           (begin
             (set! *dunnet-inventory* (cons noun *dunnet-inventory*))
             (hash-put! *dunnet-room-items* *dunnet-room*
               (filter (lambda (i) (not (string=? i noun))) items))
             (string-append "Taken: " noun "\n"))
           "I don't see that here.\n")))
      ((string=? verb "drop")
       (if (member noun *dunnet-inventory*)
         (begin
           (set! *dunnet-inventory* (filter (lambda (i) (not (string=? i noun))) *dunnet-inventory*))
           (hash-put! *dunnet-room-items* *dunnet-room*
             (cons noun (hash-ref *dunnet-room-items* *dunnet-room* '())))
           (string-append "Dropped: " noun "\n"))
         "You don't have that.\n"))
      ((string=? verb "help")
       "Commands: north/south/east/west (or n/s/e/w), look, take <item>,\ndrop <item>, inventory, help, quit\n")
      ((string=? verb "quit")
       "Thanks for playing!\n")
      (else "I don't understand that. Type 'help' for commands.\n"))))

(def (cmd-dunnet-real app)
  "Play Dunnet text adventure — real room exploration with items."
  (set! *dunnet-room* 'dead-end)
  (set! *dunnet-inventory* '())
  ;; Reset room items
  (hash-put! *dunnet-room-items* 'dead-end '("shovel"))
  (hash-put! *dunnet-room-items* 'field '("key"))
  (hash-put! *dunnet-room-items* 'loft '("lantern"))
  (hash-put! *dunnet-room-items* 'crystal-room '("diamond"))
  (hash-put! *dunnet-room-items* 'stream '("fish"))
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Dunnet*") (buffer-create! "*Dunnet*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (set! *dunnet-history* (string-append (dunnet-describe-room) "\n> "))
    (editor-set-text ed *dunnet-history*)
    (editor-goto-pos ed (string-length *dunnet-history*))
    (echo-message! (app-state-echo app) "Dunnet: type commands with M-x dunnet-command")))

(def (cmd-dunnet-command app)
  "Enter a command in the Dunnet text adventure."
  (let* ((cmd (app-read-string app "> "))
         (result (dunnet-process-command (or cmd "")))
         (fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (set! *dunnet-history*
      (string-append *dunnet-history* (or cmd "") "\n\n" result "\n> "))
    (editor-set-text ed *dunnet-history*)
    (editor-goto-pos ed (string-length *dunnet-history*))))

;;;============================================================================
;;; 4. 2048 — Number tile puzzle
;;;============================================================================

(def *2048-board* #f) ;; 4x4 vector of integers (0=empty)
(def *2048-score* 0)
(def *2048-won* #f)

(def (2048-init!)
  (set! *2048-board* (make-vector 16 0))
  (set! *2048-score* 0) (set! *2048-won* #f)
  (2048-spawn!) (2048-spawn!))

(def (2048-get x y) (vector-ref *2048-board* (+ x (* y 4))))
(def (2048-set! x y v) (vector-set! *2048-board* (+ x (* y 4)) v))

(def (2048-spawn!)
  (let ((empties '()))
    (let loop ((i 0))
      (when (< i 16)
        (when (= (vector-ref *2048-board* i) 0)
          (set! empties (cons i empties)))
        (loop (+ i 1))))
    (when (pair? empties)
      (let ((pos (list-ref empties (random-integer (length empties)))))
        (vector-set! *2048-board* pos (if (< (random-integer 10) 9) 2 4))))))

(def (2048-slide-row row)
  "Slide a row left, merging equal adjacent tiles. Returns (new-row . points)."
  (let* ((filtered (filter (lambda (x) (not (= x 0))) row))
         (merged '()) (pts 0))
    ;; Merge adjacent equal tiles
    (let loop ((lst filtered))
      (cond
        ((null? lst) #t)
        ((and (pair? (cdr lst)) (= (car lst) (cadr lst)))
         (let ((v (* 2 (car lst))))
           (set! merged (cons v merged))
           (set! pts (+ pts v))
           (when (= v 2048) (set! *2048-won* #t))
           (loop (cddr lst))))
        (else (set! merged (cons (car lst) merged))
              (loop (cdr lst)))))
    (let* ((result (reverse merged))
           (padded (append result (make-list (- 4 (length result)) 0))))
      (cons padded pts))))

(def (2048-move! direction)
  "Move tiles. Direction: 'left 'right 'up 'down. Returns #t if board changed."
  (let ((old (vector-copy *2048-board*)) (total-pts 0))
    (case direction
      ((left)
       (let loop ((y 0))
         (when (< y 4)
           (let* ((row (list (2048-get 0 y) (2048-get 1 y) (2048-get 2 y) (2048-get 3 y)))
                  (result (2048-slide-row row))
                  (new-row (car result)))
             (set! total-pts (+ total-pts (cdr result)))
             (2048-set! 0 y (list-ref new-row 0)) (2048-set! 1 y (list-ref new-row 1))
             (2048-set! 2 y (list-ref new-row 2)) (2048-set! 3 y (list-ref new-row 3)))
           (loop (+ y 1)))))
      ((right)
       (let loop ((y 0))
         (when (< y 4)
           (let* ((row (list (2048-get 3 y) (2048-get 2 y) (2048-get 1 y) (2048-get 0 y)))
                  (result (2048-slide-row row))
                  (new-row (car result)))
             (set! total-pts (+ total-pts (cdr result)))
             (2048-set! 3 y (list-ref new-row 0)) (2048-set! 2 y (list-ref new-row 1))
             (2048-set! 1 y (list-ref new-row 2)) (2048-set! 0 y (list-ref new-row 3)))
           (loop (+ y 1)))))
      ((up)
       (let loop ((x 0))
         (when (< x 4)
           (let* ((col (list (2048-get x 0) (2048-get x 1) (2048-get x 2) (2048-get x 3)))
                  (result (2048-slide-row col))
                  (new-col (car result)))
             (set! total-pts (+ total-pts (cdr result)))
             (2048-set! x 0 (list-ref new-col 0)) (2048-set! x 1 (list-ref new-col 1))
             (2048-set! x 2 (list-ref new-col 2)) (2048-set! x 3 (list-ref new-col 3)))
           (loop (+ x 1)))))
      ((down)
       (let loop ((x 0))
         (when (< x 4)
           (let* ((col (list (2048-get x 3) (2048-get x 2) (2048-get x 1) (2048-get x 0)))
                  (result (2048-slide-row col))
                  (new-col (car result)))
             (set! total-pts (+ total-pts (cdr result)))
             (2048-set! x 3 (list-ref new-col 0)) (2048-set! x 2 (list-ref new-col 1))
             (2048-set! x 1 (list-ref new-col 2)) (2048-set! x 0 (list-ref new-col 3)))
           (loop (+ x 1))))))
    (set! *2048-score* (+ *2048-score* total-pts))
    ;; Check if board changed
    (let ((changed #f))
      (let loop ((i 0))
        (when (< i 16)
          (when (not (= (vector-ref old i) (vector-ref *2048-board* i)))
            (set! changed #t))
          (loop (+ i 1))))
      changed)))

(def (2048-render)
  (with-output-to-string
    (lambda ()
      (display "  2048")
      (when *2048-won* (display "  -- YOU WIN! --"))
      (display "\n  Score: ") (display *2048-score*) (display "\n\n")
      (display "  +------+------+------+------+\n")
      (let yloop ((y 0))
        (when (< y 4)
          (display "  |")
          (let xloop ((x 0))
            (when (< x 4)
              (let ((v (2048-get x y)))
                (if (= v 0)
                  (display "      |")
                  (let ((s (number->string v)))
                    (display (make-string (- 6 (string-length s)) #\space))
                    (display s) (display "|"))))
              (xloop (+ x 1))))
          (display "\n  +------+------+------+------+\n")
          (yloop (+ y 1))))
      (display "\n  2048-up/down/left/right to play\n"))))

(def (2048-refresh! app)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (editor-set-text ed (2048-render)) (editor-goto-pos ed 0)))

(def (cmd-2048 app)
  "Play 2048 number tile puzzle."
  (2048-init!)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*2048*") (buffer-create! "*2048*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (2048-refresh! app)
    (echo-message! (app-state-echo app) "2048! Use 2048-up/down/left/right")))

(def (cmd-2048-up app) (when (and *2048-board* (2048-move! 'up)) (2048-spawn!)) (2048-refresh! app))
(def (cmd-2048-down app) (when (and *2048-board* (2048-move! 'down)) (2048-spawn!)) (2048-refresh! app))
(def (cmd-2048-left app) (when (and *2048-board* (2048-move! 'left)) (2048-spawn!)) (2048-refresh! app))
(def (cmd-2048-right app) (when (and *2048-board* (2048-move! 'right)) (2048-spawn!)) (2048-refresh! app))

;;;============================================================================
;;; 5. Animate — Text animation (like M-x animate-birthday-present)
;;;============================================================================

(def (cmd-animate-birthday app)
  "Display animated birthday greeting like Emacs animate-birthday-present."
  (let* ((name (app-read-string app "Name: "))
         (name (or name "World"))
         (fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Animate*") (buffer-create! "*Animate*" ed)))
         (lines (list
                  ""
                  (string-append "Happy Birthday, " name "!")
                  ""
                  "   ___        _   _     _             "
                  "  / _ \\      | | | |   | |            "
                  " | | | |_ __ | |_| |__ | |_ __   ___  "
                  " | | | | '_ \\|  _  '_ \\| | '_ \\ / _ \\ "
                  " | |_| | | | | | | | | | | |_) |  __/ "
                  "  \\___/|_| |_|_| |_| |_|_| .__/ \\___| "
                  "                          | |          "
                  "                          |_|          "
                  ""
                  "    *  *  *  *  *  *  *  *  *  *  *"
                  (string-append "  Happy Birthday to " name "!")
                  "    *  *  *  *  *  *  *  *  *  *  *")))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-join lines "\n"))
    (editor-goto-pos ed 0)
    (echo-message! (app-state-echo app) (string-append "Happy Birthday, " name "!"))))

(def (cmd-animate-string app)
  "Animate a string across the screen."
  (let* ((text (app-read-string app "Text to animate: "))
         (text (or text "Hello, World!"))
         (fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Animate*") (buffer-create! "*Animate*" ed)))
         (lines (let loop ((i 0) (acc '()))
                  (if (>= i (string-length text)) (reverse acc)
                    (loop (+ i 1)
                      (cons (string-append
                              (make-string (max 0 (- 30 i)) #\space)
                              (substring text 0 (+ i 1)))
                            acc))))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-join lines "\n"))
    (editor-goto-pos ed 0)))

;;;============================================================================
;;; 6. Type-break — Real RSI prevention timer
;;;============================================================================

(def *type-break-interval* 3600)  ;; seconds between breaks (1 hour)
(def *type-break-duration* 300)   ;; break duration in seconds (5 min)
(def *type-break-start* #f)
(def *type-break-keystroke-count* 0)
(def *type-break-keystroke-threshold* 5000)

(def (cmd-type-break-mode-real app)
  "Enable RSI prevention timer — reminds you to take breaks."
  (if *type-break-start*
    (begin (set! *type-break-start* #f)
           (echo-message! (app-state-echo app) "Type-break mode disabled"))
    (begin (set! *type-break-start* (time->seconds (current-time)))
           (set! *type-break-keystroke-count* 0)
           (echo-message! (app-state-echo app)
             (string-append "Type-break mode enabled: break every "
               (number->string (quotient *type-break-interval* 60)) " minutes")))))

(def (cmd-type-break-status app)
  "Show type-break timer status."
  (if (not *type-break-start*)
    (echo-message! (app-state-echo app) "Type-break mode is off")
    (let* ((elapsed (- (time->seconds (current-time)) *type-break-start*))
           (remaining (max 0 (- *type-break-interval* elapsed)))
           (mins (quotient (inexact->exact (floor remaining)) 60))
           (secs (modulo (inexact->exact (floor remaining)) 60)))
      (if (<= remaining 0)
        (echo-message! (app-state-echo app)
          "*** TAKE A BREAK! *** You've been typing too long. Stand up and stretch!")
        (echo-message! (app-state-echo app)
          (string-append "Next break in " (number->string mins) ":"
            (if (< secs 10) "0" "") (number->string secs)
            " | Keystrokes: " (number->string *type-break-keystroke-count*)))))))

(def (cmd-type-break-reset app)
  "Reset the type-break timer (start a new work session)."
  (set! *type-break-start* (time->seconds (current-time)))
  (set! *type-break-keystroke-count* 0)
  (echo-message! (app-state-echo app) "Type-break timer reset"))

;;;============================================================================
;;; 7. Highlight-changes — Real change tracking with indicators
;;;============================================================================

(def *hl-changes-enabled* #f)
(def *hl-changes-positions* (make-hash-table)) ;; buffer-name -> list of (start . end)
(def *hl-changes-indicator* 8)  ;; Scintilla indicator 8

(def (cmd-highlight-changes-real app)
  "Toggle highlight-changes-mode — track and highlight modifications."
  (set! *hl-changes-enabled* (not *hl-changes-enabled*))
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (if *hl-changes-enabled*
      (begin
        ;; Set up indicator for change highlighting
        (send-message ed SCI_INDICSETSTYLE *hl-changes-indicator* INDIC_FULLBOX)
        (send-message ed SCI_INDICSETFORE *hl-changes-indicator* #xFF8800) ;; orange
        (send-message ed 2523 *hl-changes-indicator* 60) ;; SCI_INDICSETALPHA
        (echo-message! (app-state-echo app) "Highlight-changes mode ON"))
      (begin
        ;; Clear all highlights
        (let ((len (send-message ed SCI_GETLENGTH 0 0)))
          (send-message ed SCI_SETINDICATORCURRENT *hl-changes-indicator* 0)
          (send-message ed SCI_INDICATORCLEARRANGE 0 len))
        (echo-message! (app-state-echo app) "Highlight-changes mode OFF")))))

(def (cmd-highlight-changes-mark-region app)
  "Mark the current selection as a change."
  (when *hl-changes-enabled*
    (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
           (sel-start (send-message ed SCI_GETSELECTIONSTART 0 0))
           (sel-end (send-message ed SCI_GETSELECTIONEND 0 0)))
      (when (> sel-end sel-start)
        (send-message ed SCI_SETINDICATORCURRENT *hl-changes-indicator* 0)
        (send-message ed SCI_INDICATORFILLRANGE sel-start (- sel-end sel-start))
        (echo-message! (app-state-echo app) "Region marked as change")))))

;;;============================================================================
;;; 8. Glasses-mode — Real CamelCase word separator
;;;============================================================================

(def *glasses-separator* "_")

(def (glasses-transform text)
  "Insert separator before uppercase letters in CamelCase words."
  (let ((out '()) (prev-lower #f))
    (string-for-each
      (lambda (c)
        (when (and prev-lower (char-upper-case? c))
          (set! out (cons (string-ref *glasses-separator* 0) out)))
        (set! prev-lower (char-lower-case? c))
        (set! out (cons (char-downcase c) out)))
      text)
    (list->string (reverse out))))

(def (cmd-glasses-mode-real app)
  "Apply glasses-mode — transform CamelCase to underscore_separated in selection."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (sel-start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= sel-start sel-end)
      (echo-error! (app-state-echo app) "Select text to transform")
      (let* ((text (n5-get-text ed sel-start (- sel-end sel-start)))
             (transformed (glasses-transform text)))
        (editor-replace-selection ed transformed)
        (echo-message! (app-state-echo app)
          (string-append "Glasses: " text " -> " transformed))))))

;;;============================================================================
;;; 9. Solitaire — Peg solitaire (like Emacs M-x solitaire)
;;;============================================================================

(def *sol-board-template*
  '("    o o o"
    "    o o o"
    "o o o o o o o"
    "o o o . o o o"
    "o o o o o o o"
    "    o o o"
    "    o o o"))

(def *sol-board* #f)  ;; 7x7 vector: 'peg, 'empty, 'wall
(def *sol-cx* 3) (def *sol-cy* 3)

(def (sol-init!)
  (set! *sol-board* (make-vector 49 'wall))
  (set! *sol-cx* 3) (set! *sol-cy* 3)
  ;; Set up standard English cross board
  (let ((valid '((2 0) (3 0) (4 0) (2 1) (3 1) (4 1)
                 (0 2) (1 2) (2 2) (3 2) (4 2) (5 2) (6 2)
                 (0 3) (1 3) (2 3) (3 3) (4 3) (5 3) (6 3)
                 (0 4) (1 4) (2 4) (3 4) (4 4) (5 4) (6 4)
                 (2 5) (3 5) (4 5) (2 6) (3 6) (4 6))))
    (for-each (lambda (p) (sol-set! (car p) (cadr p) 'peg)) valid)
    (sol-set! 3 3 'empty)))

(def (sol-get x y) (and (>= x 0) (< x 7) (>= y 0) (< y 7)
                        (vector-ref *sol-board* (+ x (* y 7)))))
(def (sol-set! x y v) (when (and (>= x 0) (< x 7) (>= y 0) (< y 7))
                        (vector-set! *sol-board* (+ x (* y 7)) v)))

(def (sol-can-jump? x y dx dy)
  "Can peg at (x,y) jump over (x+dx,y+dy) to (x+2dx,y+2dy)?"
  (and (eq? (sol-get x y) 'peg)
       (eq? (sol-get (+ x dx) (+ y dy)) 'peg)
       (eq? (sol-get (+ x (* 2 dx)) (+ y (* 2 dy))) 'empty)))

(def (sol-jump! x y dx dy)
  (sol-set! x y 'empty)
  (sol-set! (+ x dx) (+ y dy) 'empty)
  (sol-set! (+ x (* 2 dx)) (+ y (* 2 dy)) 'peg))

(def (sol-count-pegs)
  (let ((n 0))
    (let loop ((i 0))
      (when (< i 49) (when (eq? (vector-ref *sol-board* i) 'peg) (set! n (+ n 1)))
        (loop (+ i 1))))
    n))

(def (sol-render)
  (with-output-to-string
    (lambda ()
      (display "  PEG SOLITAIRE\n")
      (display "  Pegs remaining: ") (display (sol-count-pegs)) (display "\n\n")
      (let yloop ((y 0))
        (when (< y 7)
          (display "  ")
          (let xloop ((x 0))
            (when (< x 7)
              (let ((cell (sol-get x y)))
                (cond
                  ((and (= x *sol-cx*) (= y *sol-cy*))
                   (display (if (eq? cell 'peg) "O " ". ")))
                  ((eq? cell 'peg) (display "o "))
                  ((eq? cell 'empty) (display ". "))
                  (else (display "  "))))
              (xloop (+ x 1))))
          (display "\n") (yloop (+ y 1))))
      (display "\n  solitaire-up/down/left/right to move cursor\n")
      (display "  solitaire-jump-up/down/left/right to jump\n"))))

(def (sol-refresh! app)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (editor-set-text ed (sol-render)) (editor-goto-pos ed 0)))

(def (cmd-solitaire app)
  "Play peg solitaire."
  (sol-init!)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Solitaire*") (buffer-create! "*Solitaire*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (sol-refresh! app) (echo-message! (app-state-echo app) "Peg Solitaire! Jump pegs to clear the board.")))

(def (cmd-solitaire-up app) (when (> *sol-cy* 0) (set! *sol-cy* (- *sol-cy* 1))) (sol-refresh! app))
(def (cmd-solitaire-down app) (when (< *sol-cy* 6) (set! *sol-cy* (+ *sol-cy* 1))) (sol-refresh! app))
(def (cmd-solitaire-left app) (when (> *sol-cx* 0) (set! *sol-cx* (- *sol-cx* 1))) (sol-refresh! app))
(def (cmd-solitaire-right app) (when (< *sol-cx* 6) (set! *sol-cx* (+ *sol-cx* 1))) (sol-refresh! app))

(def (cmd-solitaire-jump-up app)
  (if (sol-can-jump? *sol-cx* *sol-cy* 0 -1)
    (begin (sol-jump! *sol-cx* *sol-cy* 0 -1) (set! *sol-cy* (- *sol-cy* 2)) (sol-refresh! app))
    (echo-error! (app-state-echo app) "Can't jump up")))
(def (cmd-solitaire-jump-down app)
  (if (sol-can-jump? *sol-cx* *sol-cy* 0 1)
    (begin (sol-jump! *sol-cx* *sol-cy* 0 1) (set! *sol-cy* (+ *sol-cy* 2)) (sol-refresh! app))
    (echo-error! (app-state-echo app) "Can't jump down")))
(def (cmd-solitaire-jump-left app)
  (if (sol-can-jump? *sol-cx* *sol-cy* -1 0)
    (begin (sol-jump! *sol-cx* *sol-cy* -1 0) (set! *sol-cx* (- *sol-cx* 2)) (sol-refresh! app))
    (echo-error! (app-state-echo app) "Can't jump left")))
(def (cmd-solitaire-jump-right app)
  (if (sol-can-jump? *sol-cx* *sol-cy* 1 0)
    (begin (sol-jump! *sol-cx* *sol-cy* 1 0) (set! *sol-cx* (+ *sol-cx* 2)) (sol-refresh! app))
    (echo-error! (app-state-echo app) "Can't jump right")))

;;;============================================================================
;;; 10. Blackbox — Deduction puzzle (like Emacs M-x blackbox)
;;;============================================================================

(def *bb-size* 8)
(def *bb-balls* '())     ;; hidden ball positions
(def *bb-guesses* '())   ;; player guesses
(def *bb-rays* (make-hash-table)) ;; (side . pos) -> result
(def *bb-num-balls* 4)

(def (bb-init!)
  (set! *bb-balls* '()) (set! *bb-guesses* '()) (set! *bb-rays* (make-hash-table))
  (let loop ((n 0))
    (when (< n *bb-num-balls*)
      (let try ()
        (let ((x (random-integer *bb-size*)) (y (random-integer *bb-size*)))
          (if (member (cons x y) *bb-balls*)
            (try)
            (set! *bb-balls* (cons (cons x y) *bb-balls*)))))
      (loop (+ n 1)))))

(def (bb-has-ball? x y)
  (member (cons x y) *bb-balls*))

(def (bb-trace-ray side pos)
  "Trace a ray from edge. Returns 'hit, 'reflect, or (exit-side . exit-pos)."
  (let* ((dx (case side ((left) 1) ((right) -1) (else 0)))
         (dy (case side ((top) 1) ((bottom) -1) (else 0)))
         (x (case side ((left) 0) ((right) (- *bb-size* 1)) (else pos)))
         (y (case side ((top) 0) ((bottom) (- *bb-size* 1)) (else pos))))
    ;; Check immediate hit
    (if (bb-has-ball? x y) 'hit
      ;; Check immediate reflection (ball at corners of entry)
      (let ((reflect? (or (and (not (= dx 0)) ;; entering horizontally
                               (or (bb-has-ball? x (- y 1))
                                   (bb-has-ball? x (+ y 1))))
                          (and (not (= dy 0)) ;; entering vertically
                               (or (bb-has-ball? (- x 1) y)
                                   (bb-has-ball? (+ x 1) y))))))
        (if reflect? 'reflect
          ;; Trace the ray
          (let loop ((rx (+ x dx)) (ry (+ y dy)) (rdx dx) (rdy dy))
            (cond
              ;; Exit the box
              ((or (< rx 0) (>= rx *bb-size*) (< ry 0) (>= ry *bb-size*))
               (cond ((< rx 0) (cons 'left ry))
                     ((>= rx *bb-size*) (cons 'right ry))
                     ((< ry 0) (cons 'top rx))
                     ((>= ry *bb-size*) (cons 'bottom rx))))
              ;; Hit a ball
              ((bb-has-ball? rx ry) 'hit)
              ;; Check deflection
              (else
               (let* ((deflect-up (and (not (= rdx 0)) (bb-has-ball? rx (- ry 1))))
                      (deflect-down (and (not (= rdx 0)) (bb-has-ball? rx (+ ry 1))))
                      (deflect-left (and (not (= rdy 0)) (bb-has-ball? (- rx 1) ry)))
                      (deflect-right (and (not (= rdy 0)) (bb-has-ball? (+ rx 1) ry))))
                 (cond
                   ;; Both sides deflect = reflect back
                   ((or (and deflect-up deflect-down) (and deflect-left deflect-right))
                    (loop (- rx rdx) (- ry rdy) (- rdx) (- rdy)))
                   ;; Single deflection
                   ((and (not (= rdx 0)) deflect-up)
                    (loop rx (- ry 1) 0 -1))
                   ((and (not (= rdx 0)) deflect-down)
                    (loop rx (+ ry 1) 0 1))
                   ((and (not (= rdy 0)) deflect-left)
                    (loop (- rx 1) ry -1 0))
                   ((and (not (= rdy 0)) deflect-right)
                    (loop (+ rx 1) ry 1 0))
                   ;; No deflection, continue straight
                   (else (loop (+ rx rdx) (+ ry rdy) rdx rdy))))))))))))

(def (bb-render)
  (with-output-to-string
    (lambda ()
      (display "  BLACKBOX — Find the hidden balls!\n")
      (display "  ") (display *bb-num-balls*) (display " balls hidden in the grid.\n")
      (display "  Guesses: ") (display (length *bb-guesses*)) (display "/") (display *bb-num-balls*)
      (display "\n\n    ")
      ;; Top edge labels
      (let loop ((i 0))
        (when (< i *bb-size*)
          (let ((key (cons 'top i)))
            (display (cond ((hash-key? *bb-rays* key)
                            (let ((r (hash-ref *bb-rays* key)))
                              (cond ((eq? r 'hit) "H") ((eq? r 'reflect) "R") (else "."))))
                           (else (number->string i)))))
          (display " ") (loop (+ i 1))))
      (display "\n")
      ;; Board
      (let yloop ((y 0))
        (when (< y *bb-size*)
          ;; Left edge
          (let ((key (cons 'left y)))
            (display (cond ((hash-key? *bb-rays* key)
                            (let ((r (hash-ref *bb-rays* key)))
                              (cond ((eq? r 'hit) " H") ((eq? r 'reflect) " R") (else " ."))))
                           (else (string-append " " (number->string y))))))
          (display "  ")
          ;; Grid
          (let xloop ((x 0))
            (when (< x *bb-size*)
              (display
                (if (member (cons x y) *bb-guesses*) "X "
                  ". "))
              (xloop (+ x 1))))
          ;; Right edge
          (let ((key (cons 'right y)))
            (display (cond ((hash-key? *bb-rays* key)
                            (let ((r (hash-ref *bb-rays* key)))
                              (cond ((eq? r 'hit) "H") ((eq? r 'reflect) "R") (else "."))))
                           (else (number->string y)))))
          (display "\n") (yloop (+ y 1))))
      (display "    ")
      ;; Bottom edge labels
      (let loop ((i 0))
        (when (< i *bb-size*)
          (let ((key (cons 'bottom i)))
            (display (cond ((hash-key? *bb-rays* key)
                            (let ((r (hash-ref *bb-rays* key)))
                              (cond ((eq? r 'hit) "H") ((eq? r 'reflect) "R") (else "."))))
                           (else (number->string i)))))
          (display " ") (loop (+ i 1))))
      (display "\n\n  blackbox-shoot <side> <pos>, blackbox-guess <x> <y>, blackbox-reveal\n"))))

(def (bb-refresh! app)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
    (editor-set-text ed (bb-render)) (editor-goto-pos ed 0)))

(def (cmd-blackbox app)
  "Play Blackbox deduction puzzle."
  (bb-init!)
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Blackbox*") (buffer-create! "*Blackbox*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (bb-refresh! app)
    (echo-message! (app-state-echo app) "Blackbox! Shoot rays to find hidden balls.")))

(def (cmd-blackbox-shoot app)
  "Shoot a ray into the blackbox from an edge."
  (let* ((input (app-read-string app "Shoot from (top/bottom/left/right pos): "))
         (parts (if input (string-split (string-trim-both input) #\space) '())))
    (if (not (= (length parts) 2))
      (echo-error! (app-state-echo app) "Usage: side pos (e.g. 'top 3')")
      (let* ((side (string->symbol (car parts)))
             (pos (string->number (cadr parts))))
        (if (or (not pos) (< pos 0) (>= pos *bb-size*)
                (not (memq side '(top bottom left right))))
          (echo-error! (app-state-echo app) "Invalid side or position")
          (let ((result (bb-trace-ray side pos)))
            (hash-put! *bb-rays* (cons side pos) result)
            (bb-refresh! app)
            (echo-message! (app-state-echo app)
              (cond ((eq? result 'hit) "HIT! Ray hit a ball.")
                    ((eq? result 'reflect) "REFLECT! Ray bounced back.")
                    (else (string-append "Ray exits at "
                            (symbol->string (car result)) " " (number->string (cdr result))))))))))))

(def (cmd-blackbox-guess app)
  "Guess a ball position in blackbox."
  (let* ((input (app-read-string app "Guess position (x y): "))
         (parts (if input (string-split (string-trim-both input) #\space) '())))
    (if (not (= (length parts) 2))
      (echo-error! (app-state-echo app) "Usage: x y (e.g. '3 5')")
      (let ((x (string->number (car parts))) (y (string->number (cadr parts))))
        (if (or (not x) (not y) (< x 0) (>= x *bb-size*) (< y 0) (>= y *bb-size*))
          (echo-error! (app-state-echo app) "Invalid position")
          (begin
            (if (member (cons x y) *bb-guesses*)
              (set! *bb-guesses* (filter (lambda (g) (not (equal? g (cons x y)))) *bb-guesses*))
              (set! *bb-guesses* (cons (cons x y) *bb-guesses*)))
            (bb-refresh! app)))))))

(def (cmd-blackbox-reveal app)
  "Reveal the hidden balls and show score."
  (let* ((correct (length (filter (lambda (g) (member g *bb-balls*)) *bb-guesses*)))
         (wrong (- (length *bb-guesses*) correct))
         (missed (- *bb-num-balls* correct))
         (fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (text (string-append (editor-get-text ed)
                 "\n\n--- REVEALED ---\n"
                 "Balls were at: "
                 (string-join (map (lambda (b)
                   (string-append "(" (number->string (car b)) "," (number->string (cdr b)) ")"))
                   *bb-balls*) " ")
                 "\nCorrect: " (number->string correct)
                 " | Wrong: " (number->string wrong)
                 " | Missed: " (number->string missed)
                 "\n")))
    (editor-set-text ed text)
    (echo-message! (app-state-echo app)
      (string-append "Score: " (number->string correct) "/" (number->string *bb-num-balls*) " correct"))))
