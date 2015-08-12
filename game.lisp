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

(defmethod init ((game game-state))
  (with-slots (brick-list running ball-list) game
    (setf brick-list (level-1))
    (setf running t)
    (start-ball (car ball-list))))

(defmethod update ((game game-state))
  (with-slots (renderer score running board ball-list brick-list) game
    (when running
      (update-board board)
      (mapcar #'update-ball ball-list)
      (mapcar
       (lambda (ball)
         (mapcar
          (lambda (brick)
            (mapc
             (lambda (collision)
               (process-collision renderer
                                  ball
                                  brick-list
                                  collision))
             (let ((collision-list (detect-collision ball brick
                                                     renderer)))
               (incf score (length collision-list))
               (delete-if-not (lambda (a)
                                (equal
                                 a
                                 (cadar collision-list)))
                              collision-list
                              :key #'cadr))))
          brick-list))
       ball-list))))

(defmethod draw ((game game-state))
  (with-slots (renderer brick-list ball-list board lifes score) game
    (with-slots (sprites) renderer
      (let* ((ball (car ball-list))
             (ball-texture (get-sprite-texture renderer :ball))
             (brick-texture (get-sprite-texture renderer :brick))
             (bw (sdl2:texture-width brick-texture))
             (bh (sdl2:texture-height brick-texture))
             (ball-side (sdl2:texture-height ball-texture)))
        (draw-rect renderer 0 0 640 480
                   69 69 69 255)
        ;; draw all bricks
        (loop
           for b in brick-list
           do (draw-sprite renderer
                           :brick
                           (* (car b) bw)
                           (+ 40 (* (cdr b) bh))))
        ;; draw ball
        (draw-sprite renderer
                     :ball
                     (ball-x ball)
                     (ball-y ball))
        ;; draw remaining lifes
        (loop for l from 0 to (1- lifes) do
             (draw-sprite renderer
                          :ball
                          (+ 10 (* (+ ball-side 3) l))
                          10))
        ;; draw score
        (write-text renderer (format nil "ОЧКИ: ~3d" score)
                   :small :x 540 :y 10)
        ;; draw board
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
     for i from 0 to 15
     append
       (loop
          for j from 0 to 7
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
           (setf x (- x dx))
           (setf dx (- dx)))
          ((< y 0)
           (setf y (- y dy))
           (setf dy (- dy))))
    (setf (ball-x ball) x)
    (setf (ball-y ball) y)
    (setf (ball-dx ball) dx)
    (setf (ball-dy ball) dy)))

(defun game-over (x)
  )

(defun game-unpause (game)
  (setf (slot-value game 'running) t))

(defun start-ball (ball)
  (setf (ball-dx ball) (- 10 (random 20)))
  (setf (ball-dy ball) -5))

(defun detect-collision (ball brick renderer)
  (let ((brick.w (sprite-width renderer :brick))
        (brick.h (sprite-height renderer :brick)))
    (let* ((brick.lt (cons (* brick.w (car brick))
                           (+ 40 (* brick.h (cdr brick)))))
           (brick.lb (cons (car brick.lt)
                           (+ brick.h (cdr brick.lt))))
           (brick.rt (cons (+ brick.w (car brick.lt))
                           (cdr brick.lt)))
           (brick.rb (cons (car brick.rt)
                           (+ brick.h (cdr brick.rt))))
           ;; Sides
           (left   (cons brick.lt brick.lb))
           (right  (cons brick.rt brick.rb))
           (top    (cons brick.lt brick.rt))
           (bottom (cons brick.lb brick.rb))

           (ball-side (sprite-width renderer :ball))
           (ball-x (- (ball-x ball) (/ ball-side 2)))
           (ball-y (- (ball-y ball) (/ ball-side 2)))
           (ball-path (cons
                       (cons ball-x ball-y)
                       (cons (- ball-x
                                (ball-dx ball))
                             (- ball-y
                                (ball-dy ball))))))
      (remove-if-not (lambda (side) (intersectp ball-path side))
                     (mapcar (lambda (int side)
                               (list int brick side))
                             (list left right top bottom)
                             (list :left :right :top :bottom))
                     :key #'car))))

(defun intersectp (line1 line2)
  (flet ((x (c) (car c))
         (y (c) (cdr c))
         (n/e (a b) (not (eql a b))))
    (flet ((orientation (p q r)
             (let ((val (- (* (- (y q) (y p))
                              (- (x r) (x q)))
                           (* (- (x q) (x p))
                              (- (y r) (y q))))))
               (cond ((= val 0) :collinear)
                     ((> val 0) :cw)
                     ((< val 0) :ccw))))
           (on-segment (p q r)
             (and (<= (x q) (max (x p) (x r)))
                  (>= (x q) (min (x p) (x r)))
                  (<= (y q) (max (y p) (y r)))
                  (>= (y q) (min (y p) (y r))))))
      (let ((p1 (car line1))
            (q1 (cdr line1))
            (p2 (car line2))
            (q2 (cdr line2)))
        (let ((o1 (orientation p1 q1 p2))
              (o2 (orientation p1 q1 q2))
              (o3 (orientation p2 q2 p1))
              (o4 (orientation p2 q2 q1)))
          (or (and (n/e o1 o2) (n/e o3 o4))
              (and (eql o1 :collinear) (on-segment p1 p2 q1))
              (and (eql o2 :collinear) (on-segment p1 q2 q1))
              (and (eql o3 :collinear) (on-segment p2 p1 q2))
              (and (eql o4 :collinear) (on-segment p2 q1 q2))))))))

(defun process-collision (renderer ball brick-list collision)
  (destructuring-bind (int brick side) collision
    (let* ((x (caar int))
           (y (cdar int))
           (w (- (cadr int) x))
           (h (- (cddr int) y)))
      (draw-rect renderer x y
                 (or (and (= w 0) 4) w)
                 (or (and (= h 0) 6) h)
                 255 0 0 255)
      (delete brick brick-list)
      (setf (ball-x ball) (- (ball-x ball)
                             (ball-dx ball))
            (ball-y ball) (- (ball-y ball)
                             (ball-dy ball)))
      (cond ((or (eql side :left)
                 (eql side :right))
             (setf (ball-dx ball)
                   (- (ball-dx ball))))
            ((or (eql side :top)
                 (eql side :bottom))
             (setf (ball-dy ball)
                   (- (ball-dy ball))))))))
