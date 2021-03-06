(in-package :aark)

;;; State interface (state.lisp):
;; (defclass state ()
;;   ((name     :initform (error "Name must be set")
;;              :initarg :name)
;;    (application :initform (error "Application must be set")
;;                 :initarg :application)
;;    (renderer    :initform (error "Renderer must be set")
;;                 :initarg :renderer)))
;;
;; (defgeneric init (state))
;; (defgeneric process-input (state direction keysym))
;; (defgeneric update (state))
;; (defgeneric draw (state))
;; (defgeneric cleanup (state))
;;
;; (defmethod init ((state state)))
;; (defmethod update ((state state)))

;;; Menu
(defclass menu-state (state)
  ((choise :initform 0)))

(defmethod process-input ((menu menu-state)
                          direction
                          keysym)
  (if (eq direction :keydown)
      (menu-keydown menu keysym)
      (menu-keyup menu keysym)))

(defun menu-keyup (menu keysym)
  (cond ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-escape)
         (sdl2:push-event :quit))
        ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-return)
         (let ((choise (slot-value menu 'choise )))
           (cond ((= choise 2)
                  (sdl2:push-event :quit))
                 ((= choise 1)
                  nil)
                 ((= choise 0)
                  nil
                  (with-slots ((app application)) menu
                    (set-state app :game))))))))

(defun menu-keydown (menu keysym)
  (cond ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-down)
         (setf (slot-value menu 'choise)
               (mod (1+ (slot-value menu 'choise))
                    3)))
        ((sdl2:scancode=
          (sdl2:scancode-value keysym)
          :scancode-up)
         (setf (slot-value menu 'choise)
               (mod (1- (slot-value menu 'choise))
                    3)))))

(defmethod draw ((menu menu-state))
  (with-slots (choise) menu
    (draw-rect 0 0 640 480 83 3 116 255)
    (if (= choise 0)
        (draw-rect 60 305 520 35
                   180 95 215 255)
        (draw-rect 60 305 520 35
                   110 44 138 255))
    (write-text "СТАРТ" :default
                :centered t
                :x 640
                :y 300)
    (if (= choise 1)
        (draw-rect 60 345 520 35
                   180 95 215 255)
        (draw-rect 60 345 520 35
                   110 44 138 255))
    (write-text "ВЫСОКИЕ ОЧКИ" :default
                :centered t
                :x 640
                :y 340)
    (if (= choise 2)
        (draw-rect 60 385 520 35
                   180 95 215 255)
        (draw-rect 60 385 520 35
                   110 44 138 255))
    (write-text "ВЫХОД" :default
                :centered t
                :x 640
                :y 380)))

(defmethod update ((menu menu-state)))
