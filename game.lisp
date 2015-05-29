(in-package :aark)

(defstruct ball
  x y dx dy)

(defstruct board
  length x dx base-length r g b a)

(defclass game-state (state)
  ((lifes    :initform 3)
   (runningp :initform t)
   (score    :initform 0)
   (board    :initform (make-board
                        :base-length 20
                        :length 2
                        :x 40 :dx 0
                        :r 60 :g 150 :b 90 :a 255))
   (brick-list :initform '())
   (ball-list  :initform (list (make-ball :x 320 :y 240 :dx -1.0 :dy 1.0)))
   (bonus-list :initform '())
   (running    :initform nil)))

(defmethod init ((state state))
  (setf (slot-value state 'brick-list) (level-1))
  (setf (slot-value state 'running) t)
  (start-ball (car (slot-value state 'ball-list))))

(defmethod update ((game game-state))
  (with-slots (running board ball-list) game
    (when running
      (update-board board)
      (mapcar #'update-ball ball-list))))

(defmethod draw ((game game-state))
  (with-slots (renderer brick-list ball-list board) game
    (with-slots (sprites) renderer
      (let* ((ball (car ball-list))
             (ball-texture (get-sprite-texture renderer :ball))
             (brick-texture (get-sprite-texture renderer :brick))
             (bw (sdl2:texture-width brick-texture))
             (bh (sdl2:texture-height brick-texture)))
        (draw-rect renderer 0 0 640 480
                   0 0 0 255)
        (draw-rect renderer 0 0 640 480
                   69 69 69 255)
        (loop
           for b in brick-list
           do (draw-sprite renderer
                           :brick
                           (+ 120 (* (car b) bw))
                           (+ 40 (* (cdr b) bh))))
        (draw-sprite renderer
                     :ball
                     (ball-x ball)
                     (ball-y ball))
        (draw-rect renderer
                   (board-x board)
                   (- 480 20)
                   (* (board-length board)
                      (board-base-length board))
                   10
                   (board-r board) (board-g board)
                   (board-b board) (board-a board))))))

(defmethod process-input ((game game-state)
                          direction
                          keysym)
  (if (eq direction :keydown)
      (game-keydown game keysym)
      (game-keyup game keysym)))

(defun game-keyup (game keysym))

(defun game-keydown (game keysym)
  (with-slots (board running (app application))
      game
    (cond ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-escape)
           (setf running nil)
           (set-state app :game-menu))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-left)
           (setf (board-dx board) -10))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-right)
           (setf (board-dx board) 10)))))

(defun level-1 ()
  (loop
     for i from 0 to 9
     append
       (loop
          for j from 0 to 9
          collect (cons i j))))

(defun update-board (board)
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
         (setf (board-x board) 0))))

(defun update-ball (ball)
  (let* ((dx (ball-dx ball))
         (dy (ball-dy ball))
         (x (+ (ball-x ball) dx))
         (y (+ (ball-y ball) dy)))
    (cond ((> x 630) (setf x 630)
           (setf dx (- dx)))
          ((> y 470) (setf y 470)
           (setf dy (- dy)))
          ((< x 0) (game-over x)
           (setf dx (- dx)))
          ((< y 0) (setf y 0)
           (setf dy (- dy))))
    (setf (ball-x ball) x)
    (setf (ball-y ball) y)
    (setf (ball-dx ball) dx)
    (setf (ball-dy ball) dy)))

(defun game-over (x)
  (setf x 0))

(defun game-unpause (game)
  (setf (slot-value game 'running) t))

(defun start-ball (ball)
  (setf (ball-dx ball) (- 10 (random 20)))
  (setf (ball-dy ball) -5))
