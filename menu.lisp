(in-package :aark)

;;; Menu

(defun menu-init ()
  (unless (gethash 'menu *storage* nil)
    (setf (gethash 'menu *storage*) (make-hash-table)))
  (let ((menu-hash (gethash 'menu *storage*)))
    (setf (gethash 'current-choise menu-hash) 2)))

(defun menu-input (win direction keysym)
  (if (eq direction :keydown)
      (menu-keydown win keysym)
      (menu-keyup win keysym)))

(defun menu-keyup (win keysym)
  (cond ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-escape)
         (sdl2:push-event :quit))
        ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-return)
         (let ((choise (gethash 'current-choise
                                (gethash 'menu *storage*))))
           (cond ((= choise 2)
                  (sdl2:push-event :quit))
                 ((= choise 1)
                  nil)
                 ((= choise 0)
                  (game-init)
                  (setf *idle-fun* 'game-idle)
                  (setf *process-input-fun* 'game-input)))))))

(defun menu-keydown (win keysym)
  (let ((menu-hash (gethash 'menu *storage*)))
    (cond ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-down)
           (setf (gethash 'current-choise menu-hash)
                 (mod (1+ (gethash 'current-choise menu-hash))
                      3)))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-up)
           (setf (gethash 'current-choise menu-hash)
                 (mod (1- (gethash 'current-choise menu-hash))
                      3))))))

(defun menu-idle (win)
  (let* ((menu-hash (gethash 'menu *storage*))
         (font (gethash 'font *storage*))
         (surf (sdl2:get-window-surface win))
         (choise (gethash 'current-choise menu-hash)))
    (sdl2:fill-rect surf 0 0 640 480 83 3 116 255)
    (if (= choise 0)
        (sdl2:fill-rect surf 60 305 520 35
                        180 95 215 255)
        (sdl2:fill-rect surf 60 305 520 35
                        110 44 138 255))
    (write-on-surface "СТАРТ" font surf
                      :centered t
                      :x 640
                      :y 300
                      :cell-w 40
                      :cell-h 40)
    (if (= choise 1)
        (sdl2:fill-rect surf 60 345 520 35
                        180 95 215 255)
        (sdl2:fill-rect surf 60 345 520 35
                        110 44 138 255))
    (write-on-surface "ВЫСОКИЕ ОЧКИ" font surf
                      :centered t
                      :x 640
                      :y 340
                      :cell-w 40
                      :cell-h 40)
    (if (= choise 2)
        (sdl2:fill-rect surf 60 385 520 35
                        180 95 215 255)
        (sdl2:fill-rect surf 60 385 520 35
                        110 44 138 255))
    (write-on-surface "ВЫХОД" font surf
                      :centered t
                      :x 640
                      :y 380
                      :cell-w 40
                      :cell-h 40)))
