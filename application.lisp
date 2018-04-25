(in-package :aark)

(defparameter +delay+ (/ 1000.0 30.0)
  "Frames per second.")

;; TODO: provide something like config package with resource path
;;       mappings and path-related routines. Now I can only hardcode them.
(defun res (resource-file-name)
  "Creates absolute path to resource by it's local filename."
  (merge-pathnames resource-file-name +root+))

(defclass application ()
  ((state-hash :initform (make-hash-table))
   (current-state :initform nil)
   (renderer :initform nil)
   (running :initform nil)))

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
          (set-current-renderer renderer)
          (setf (slot-value app 'renderer) renderer)
          (let ((alphabet
                 "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ1234567890.,-!?\"№<>:; "))
            (add-font :default
                      (res "font3.png")
                      alphabet
                      40 40)
            (add-font :small
                      (res "font-small.png")
                      alphabet
                      10 10))
          (add-sprite :ball (res "ball.png"))
          (add-sprite :brick (res "kirpich.bmp"))

          (add-state app 'menu-state :main-menu)
          (add-state app 'game-state :game)
          (add-state app 'game-menu-state :game-menu)

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
