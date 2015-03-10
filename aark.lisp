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
  (cffi:with-foreign-string (p "/home/ex/pro/lisp/aark/font.bmp")
    (let ((b (sdl2:load-bmp p)))
      (setf (gethash 'font *storage*)
            b)))
  (setf *idle-fun* 'menu-idle)
  (setf *process-input-fun* 'menu-input))

;;; Menu
(defun get-font-symbol (num)
  (multiple-value-bind (col row)
      (floor num 6)
    (sdl2:make-rect (* 10 row)
                    (* 10 col)
                    10
                    10)))

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

    (sdl2:blit-surface-scaled font (get-font-symbol 16)
                              surf (sdl2:make-rect 10 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 0)
                              surf (sdl2:make-rect 30 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 25)
                              surf (sdl2:make-rect 50 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 0)
                              surf (sdl2:make-rect 70 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 54)
                              surf (sdl2:make-rect 90 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 16)
                              surf (sdl2:make-rect 110 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 9)
                              surf (sdl2:make-rect 130 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 18)
                              surf (sdl2:make-rect 150 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 29)
                              surf (sdl2:make-rect 170 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 11)
                              surf (sdl2:make-rect 190 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 0)
                              surf (sdl2:make-rect 210 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 54)
                              surf (sdl2:make-rect 230 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 52)
                              surf (sdl2:make-rect 250 10 20 20))
    (sdl2:blit-surface-scaled font (get-font-symbol 8)
                              surf (sdl2:make-rect 270 10 20 20))))
