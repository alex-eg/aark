(in-package #:aark)

(defvar *process-input-fun* (lambda (&rest rest)
                              (sdl2:push-event :quit)))
(defvar *idle-fun* (lambda (&rest rest)))
(defvar *update-fun* (lambda (&rest rest)))
(defvar *storage* (make-hash-table))

(defparameter +delay+ (/ 1000.0 30.0))

(defun start ()
  (let ((current-frame 0))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title "Aark"
                             :w 640
                             :h 480)
        (init win)
        (menu-init)

        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (funcall *process-input-fun* win :keydown keysym))
          (:keyup
           (:keysym keysym)
           (funcall *process-input-fun* win :keyup keysym))
          (:idle
           ()
           (setf current-frame (sdl2:get-ticks))
           (funcall *update-fun* win)
           (funcall *idle-fun* win)
           (sdl2:update-window win)
           (let ((current-speed (- (sdl2:get-ticks)
                                   current-frame)))
             (format t "delay: ~s current-speed: ~s~%"
                     +delay+ current-speed)
             (if (< current-speed +delay+)
                 (progn
                   (sdl2:delay (round (- +delay+ current-speed)))))))
          (:quit () t))))))


(defun init (win)
  (setf (gethash 'font *storage*)
        (init-font "/home/ex/pro/lisp/aark/font3.bmp"
                   "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                   40 40 :r 0 :g 0 :b 0))
  (sdl2-ffi.functions:sdl-set-surface-blend-mode
   (sdl2-ffi.functions:sdl-get-window-surface win)
   sdl2-ffi:+sdl-blendmode-blend+)
  (setf *idle-fun* 'menu-idle)
  (setf *process-input-fun* 'menu-input))
