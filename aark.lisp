(in-package #:aark)

(defparameter +delay+ (/ 1000.0 30.0))	; fps

(defun start ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Aark"
                           :w 640
                           :h 480)
      (sdl2:with-renderer (ren win
                               :flags '(:sdl-renderer-accelerated
                                        :sdl-renderer-presentvsync))
        (let* ((renderer (make-instance 'renderer :renderer ren))
               (state (make-instance    'menu :renderer renderer))))
        (init renderer)
        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (funcall *process-input-fun* win ren :keydown keysym))
          (:keyup
           (:keysym keysym)
           (funcall *process-input-fun* win ren :keyup keysym))
          (:idle
           ()
           (setf current-frame (sdl2:get-ticks))
           (funcall *update-fun* win)
           (sdl2:render-clear ren)
           (funcall *idle-fun* ren)
           (sdl2:render-present ren)
           (let ((current-speed (- (sdl2:get-ticks)
                                   current-frame)))
             (if (< current-speed +delay+)
                 (progn
                   (sdl2:delay (round (- +delay+ current-speed)))))))
          (:quit () t))))))

(defun init (ren)
  (setf (gethash 'font *storage*)
        (init-font ren
		   "/home/ex/programming/lisp/aark/font3.bmp"
                   "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                   40 40
		   :r 0 :g 0 :b 0)))
