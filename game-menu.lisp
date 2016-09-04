(in-package #:aark)

(defclass game-menu-state (state)
  ((choise :initform 0)))

(defmethod draw ((menu game-menu-state))
  (with-slots ((app application)
               choise) menu
    (draw (get-state app :game))
    (draw-rect 0 0 640 480 255 255 255 12)
    (draw-rect 0 180 640 120 255 255 255 32)

    (if (= choise 0)
        (draw-rect 60 205 520 35
                   180 95 215 255)
        (draw-rect 60 205 520 35
                   110 44 138 255))
    (write-text "ПРОДОЛЖИТЬ" :default
                :centered t :x 640 :y 200)
    (if (= choise 1)
        (draw-rect 60 245 520 35
                   180 95 215 255)
        (draw-rect 60 245 520 35
                   110 44 138 255))

    (write-text "ВЫХОД" :default
                :centered t :x 640 :y 240)))

(defmethod process-input ((menu game-menu-state) direction keysym)
  (if (eq direction :keydown)
      (game-menu-keydown menu keysym)
      (game-menu-keyup menu keysym)))

(defun game-menu-keyup (menu keysym)
  (cond ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-return)
         (with-slots ((app application)
                      choise) menu
           (cond ((= choise 1)
                  (if (highscores-check)
                      ;; switch to highscores, else
                      (set-state app :main-menu)))
                 ((= choise 0)
                  (game-unpause (get-state app :game))
                  (set-state app :game :no-reinit t)))))))

(defun game-menu-keydown (menu keysym)
  (with-slots (choise) menu
    (cond ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-down)
           (setf choise (mod (1+ choise)
                             2)))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-up)
           (setf choise (mod (1- choise)
                             2))))))

(defun highscores-check ()
  t)
