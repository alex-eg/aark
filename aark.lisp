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
               (state (make-instance    'menu-state
                                        :renderer renderer
                                        :name "main menu")))
          (add-font renderer :default
                    "./font3.bmp"
                    "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                    40 40
                    :r 0 :g 0 :b 0)
          (add-sprite renderer :ball  "./ball.bmp")
          (add-sprite renderer :brick "./kirpich.bmp")
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (process-input state :keydown keysym))
            (:keyup
             (:keysym keysym)
             (process-input state :keyup keysym))
            (:idle
             ()
             (setf current-frame (sdl2:get-ticks))
             (update state)
             (sdl2:render-clear ren)
             (draw state)
             (sdl2:render-present ren)
             (let ((current-speed (- (sdl2:get-ticks)
                                     current-frame)))
               (if (< current-speed +delay+)
                   (progn
                     (sdl2:delay (round (- +delay+ current-speed)))))))
            (:quit () t)))))))
