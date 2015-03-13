(in-package #:aark)

(defvar *process-input-fun* (lambda (&rest rest)
                              (sdl2:push-event :quit)))
(defvar *idle-fun* (lambda (&rest rest)))
(defvar *storage* (make-hash-table))

(defun start ()
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
         (funcall *idle-fun* win)
         (sdl2:update-window win))
        (:quit () t)))))


(defun init (win)
  (setf (gethash 'font *storage*)
        (init-font "/home/ex/pro/lisp/aark/font2.bmp"
                   "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                   40 40))
  (setf *idle-fun* 'menu-idle)
  (setf *process-input-fun* 'menu-input))
