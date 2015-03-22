(in-package :aark)

(defstruct ball
  x y dx dy sprite)

(defstruct board
  length x dx base-length r g b a)

(defun game-init ()
  (unless (gethash 'game *storage* nil)
    (setf (gethash 'game *storage*) (make-hash-table)))
  (let ((game-hash (gethash 'game *storage*)))
    (mapcar
     (lambda (pair)
       (setf (gethash (car pair) game-hash) (cdr pair)))
     `((lifes  . 3)
       (running . t)
       (scores . 0)
       (bricks . ,(level-1))
       (brick-sprite . ,(sdl2:load-bmp "/home/ex/pro/lisp/aark/kirpich.bmp"))
       (board . ,(make-board
                  :base-length 20
                  :length 2
                  :x 40 :dx 0
                  :r 60 :g 150 :b 90 :a 255))
       (balls . ,(list
                  (make-ball
                   :sprite
                   (sdl2:load-bmp "/home/ex/pro/lisp/aark/ball.bmp")
                   :x 320 :y 240 :dx -1.0 :dy 1.0)))
       (bonuses . '())))))

(defun level-1 ()
  (loop
     for i from 0 to 9
     append
       (loop
          for j from 0 to 9
          collect (cons i j))))

(defun game-update (win)
  (with-state-storage
      (game
       brick-sprite
       bricks
       ball
       board
       running)
    (when running
      (setf (board-x board)
            (+ (board-x board)
               (board-dx board)))
      (setf (board-dx board) 0)
      (cond ((> (board-x board)
                (- 640 (* (board-length board)
                          (board-base-length board))))
             (setf (board-x board)
                   (- 640 (* (board-length board)
                             (board-base-length board)))))
            ((< (board-x board) 0)
             (setf (board-x board) 0))))))

(defun game-idle (win)
  (with-state-storage
      (game
       brick-sprite
       bricks
       ball
       board)
    (let* ((surf (sdl2:get-window-surface win))
           (ball-sprite (ball-sprite ball))
           (bw (sdl2-ffi.accessors:sdl-surface.w brick-sprite))
           (bh (sdl2-ffi.accessors:sdl-surface.h brick-sprite)))
      (sdl2:fill-rect surf 0 0 640 480
                      0 0 0 255)
      (loop
         for b in bricks
         do (sdl2:draw-sprite surf
                              brick-sprite
                              (+ 120 (* (car b) bw))
                              (+ 40 (* (cdr b) bh))))
      (sdl2:draw-sprite surf
                        ball-sprite
                        (ball-x ball)
                        (ball-y ball))
      (sdl2:fill-rect surf
                      (board-x board)
                      (- 480 20)
                      (* (board-length board)
                         (board-base-length board))
                      10
                      (board-r board) (board-g board)
                      (board-b board) (board-a board)))))


(defun game-input (win direction keysym)
  (if (eq direction :keydown)
      (game-keydown win keysym)
      (game-keyup win keysym)))

(defun game-keyup (win keysym))

(defun game-keydown (win keysym)
  (let ((game (gethash 'game *storage*)))
    (cond ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-escape)
           (setf (gethash 'running game) nil)
           (show-game-menu))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-left)
           (setf (board-dx (gethash 'board game)) -10))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-right)
           (setf (board-dx (gethash 'board game)) 10)))))
