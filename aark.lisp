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
      (init)
      (sdl2:with-event-loop (:method :poll)
        (:keyup
         (:keysym keysym)
         (funcall *process-input-fun* win keysym))
        (:idle
         ()
         (funcall *idle-fun* win)
         (sdl2:update-window win))
        (:quit () t)))))


(defun init ()
  (setf (gethash 'font *storage*)
        (init-font "/home/ex/pro/lisp/aark/font2.bmp"
                   "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                   40 40))
  (setf *idle-fun* 'menu-idle)
  (setf *process-input-fun* 'menu-input))

;;; Menu

(defun menu-init ()
  (unless (gethash 'menu *storage* nil)
    (setf (gethash 'menu *storage*) (make-hash-table)))
  (let ((menu-hash (gethash 'menu *storage*)))
    (declare (ignore menu-hash))))

(defun menu-input (win keysym)
  (cond ((sdl2:scancode= (sdl2:scancode-value keysym)
                         :scancode-escape)
         (sdl2:push-event :quit))))

(defun menu-idle (win)
  (let ((font (gethash 'font *storage*))
        (surf (sdl2:get-window-surface win)))
    (sdl2-ffi.functions:sdl-fill-rect
     surf
     (sdl2:make-rect 0 0 640 480)
     (sdl2-ffi.functions:sdl-map-rgba
      (sdl2-ffi.accessors:sdl-surface.format surf)
      128 200 255 255))

    (write-on-surface "СТАРТ" font surf :centered t :x 640 :y 300
                      :cell-w 40
                      :cell-h 40)
    (write-on-surface "НАСТРОЙКИ" font surf :centered t :x 640 :y 340
                      :cell-w 40
                      :cell-h 40)
    (write-on-surface "ВЫХОД" font surf :x 640 :centered t :y 430
                      :cell-w 40
                      :cell-h 40)))
