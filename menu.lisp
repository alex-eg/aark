(in-package :aark)

;;; Menu

(defun menu-init ()
  (unless (gethash 'menu *storage* nil)
    (setf (gethash 'menu *storage*) (make-hash-table)))
  (let ((menu-hash (gethash 'menu *storage*)))
    (setf (gethash 'current-choise menu-hash) 0)))

(defun menu-input (win ren direction keysym)
  (if (eq direction :keydown)
      (menu-keydown win ren keysym)
      (menu-keyup win ren keysym)))

(defun menu-keyup (win ren keysym)
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
                  (game-init ren)
                  (setf *update-fun* 'game-update)
                  (setf *idle-fun* 'game-idle)
                  (setf *process-input-fun* 'game-input)))))))

(defun menu-keydown (win ren keysym)
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

(defun menu-idle (ren)
  (let* ((menu-hash (gethash 'menu *storage*))
         (font (gethash 'font *storage*))
         (choise (gethash 'current-choise menu-hash)))
    (draw-rect ren 0 0 640 480 83 3 116 255)
    (if (= choise 0)
	(draw-rect ren 60 305 520 35
		   180 95 215 255)
	(draw-rect ren 60 305 520 35
		   110 44 138 255))
    (write-on-surface ren "СТАРТ" font
		      :centered t
		      :x 640
		      :y 300)
    (if (= choise 1)
	(draw-rect ren 60 345 520 35
		   180 95 215 255)
	(draw-rect ren 60 345 520 35
		   110 44 138 255))
    (write-on-surface ren "ВЫСОКИЕ ОЧКИ" font
		      :centered t
		      :x 640
		      :y 340)
    (if (= choise 2)
	(draw-rect ren 60 385 520 35
		   180 95 215 255)
	(draw-rect ren 60 385 520 35
		   110 44 138 255))
    (write-on-surface ren "ВЫХОД" font
		      :centered t
		      :x 640
		      :y 380)))
