(in-package :aark)

(defparameter +delay+ (/ 1000.0 30.0))	; fps

(defclass application ()
  ((state-hash    :initform (make-hash-table))
   (current-state :initform nil)
   (renderer      :initform nil)
   (running       :initform nil)))

(defun add-state (app state-class state-name)
  (with-slots ((states state-hash)
               (ren renderer)
               current-state) app
    (if (gethash state-name states)
        (error "State with name ~a already exists" state-name))
    (let ((state (make-instance state-class
                                :application app
                                :name state-name
                                :renderer ren)))
      (unless current-state
        (setf current-state state))
      (setf (gethash state-name states) state))))

(defun set-state (app state-name &key (no-reinit nil))
  (with-slots ((states state-hash)
               current-state)
      app
    (let ((state (gethash state-name states)))
      (unless state
        (error "State with name ~a does not exist" state-name))
      (setf current-state state)
      (unless no-reinit (init state)))))

(defun get-state (app state-name)
  (with-slots ((states state-hash))
      app
    (or (gethash state-name states nil)
        (error "No name state with name ~S" state-name))))

(defmethod start ((app application))
  (sdl2-image:init '(:png))
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Aark"
                           :w 640
                           :h 480)
      (sdl2:with-renderer (ren win
                               :flags '(:accelerated
                                        :presentvsync))
        (let* ((renderer (make-instance 'renderer :renderer ren))
               current-frame)
          (setf (slot-value app 'renderer) renderer)
          (add-state app 'menu-state :main-menu)
          (add-state app 'game-state :game)
          (add-state app 'game-menu-state :game-menu)
          (add-font renderer :default
                    "./font3.png"
                    "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "
                    40 40
                    :r 0 :g 0 :b 0)
          (add-sprite renderer :ball "./ball.png")
          (add-sprite renderer :brick "./kirpich.bmp")
          (with-slots ((state current-state)) app
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
               (sdl2:render-clear ren)
               (draw state)
               (update state)
               (sdl2:render-present ren)
               (let ((current-speed (- (sdl2:get-ticks)
                                       current-frame)))
                 (if (< current-speed +delay+)
                     (progn
                       (sdl2:delay (round (- +delay+ current-speed)))))))
              (:quit ()
                     (sdl2-image:quit)
                     t))))))))
