(in-package :aark)

(defstruct ball
  x y dx dy sprite)

(defstruct board
  length sprite)

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
                  :sprite
                  (sdl2:load-bmp "/home/ex/pro/lisp/aark/board.bmp")
                  :length 2))
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
                        (ball-y ball)))))


(defun game-input (win direction keysym)
  (if (eq direction :keydown)
      (menu-keydown win keysym)
      (menu-keyup win keysym)))
