(in-package #:aark)

(defun show-game-menu ()
  (unless (gethash 'game-menu *storage* nil)
    (setf (gethash 'game-menu *storage*) (make-hash-table)))
  (let ((game-menu-hash (gethash 'game-menu *storage*)))
    (setf (gethash 'current-choise game-menu-hash) 0))
  (setf *idle-fun* 'game-menu-idle)
  (setf *process-input-fun* 'game-menu-input)
  (setf *update-fun* (lambda (&rest rest))))

(defun game-menu-idle (win)
  (let ((surf (sdl2:get-window-surface win))
        (font (gethash 'font *storage*))
        (hash (gethash 'game-menu *storage*)))
    (game-idle win)
    (sdl2:fill-rect surf 0 0 640 480 0 0 0 72)
    (sdl2:fill-rect surf 0 180 640 120 255 255 255 192)

    (let ((choise (gethash 'current-choise hash)))

      (if (= choise 0)
          (sdl2:fill-rect surf 60 205 520 35
                          180 95 215 255)
          (sdl2:fill-rect surf 60 205 520 35
                          110 44 138 255))
      (write-on-surface "ПРОДОЛЖИТЬ" font surf
                        :centered t :x 640 :y 200)
      (if (= choise 1)
          (sdl2:fill-rect surf 60 245 520 35
                          180 95 215 255)
          (sdl2:fill-rect surf 60 245 520 35
                          110 44 138 255))

      (write-on-surface "ВЫХОД" font surf
                        :centered t :x 640 :y 240))))

(defun game-menu-input (win direction keysym)
  (if (eq direction :keydown)
      (game-menu-keydown win keysym)
      (game-menu-keyup win keysym)))

(defun game-menu-keyup (win keysym)
  (cond ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-return)
         (let ((choise (gethash 'current-choise
                                (gethash 'game-menu *storage*))))
           (cond ((= choise 1)
                  (if (highscores-check)
                      ;; switch to highscores, else
                      (progn
                        (setf *idle-fun* 'menu-idle)
                        (setf *process-input-fun* 'menu-input)
                        (menu-init))))
                 ((= choise 0)
                  (setf (gethash 'runnin (gethash 'game *storage*)) t)
                  (setf *update-fun* 'game-update)
                  (setf *idle-fun* 'game-idle)
                  (setf *process-input-fun* 'game-input)))))))

(defun game-menu-keydown (win keysym)
  (let ((game-menu-hash (gethash 'game-menu *storage*)))
    (cond ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-down)
           (setf (gethash 'current-choise game-menu-hash)
                 (mod (1+ (gethash 'current-choise game-menu-hash))
                      2)))
          ((sdl2:scancode=
            (sdl2:scancode-value keysym)
            :scancode-up)
           (setf (gethash 'current-choise game-menu-hash)
                 (mod (1- (gethash 'current-choise game-menu-hash))
                      2))))))



(defun highscores-check ()
  t)
