(in-package #:aark)

(defvar *main-window*)

(defun start ()
  (create-window 640 480)
  (initialize-start-state)
  (main-loop)
  (clean-up))
 
(defun create-window (w h)
  (sdl2:init :video
             :audio)
  (setf *main-window*
        (sdl2:create-window :title "Aarkanoid"
                            :w w
                            :h h)))

(defun main-loop ()
  (aark.state:process-input)
  (aark.state:update-state)
  (aark.state:render))

(defun clean-up ()
  (sdl2:quit))
