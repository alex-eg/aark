(in-package #:aark.state)

;; variables storing current states functions
(defvar *init* nil)
(defvar *process-input* nil)
(defvar *update* nil)
(defvar *render* nil)
(defvar *cleanup* nil)

;; variable storing all needed information, e.g. in a hash table
(defvar *state*)

;; maybe there should be a grand defmacro?..
(defun set-state (init input update render cleanup)
  (setf *init* init)
  (setf *process-input* input)
  (setf *update* update)
  (setf *render* render)
  (setf *cleanup* cleanup))

(defun init (&rest args)
  (apply *init* args))

(defun process-input (&rest args)
  (apply *process-input* args))

(defun update (&rest args)
  (apply *process-input* args))

(defun render (&rest args)
  (apply *render* args))

(defun cleanup (&rest args)
  (apply *cleanup* args))


;;; TEMP

(defun initialize-game-state ()
  (labels
      ((init ())
       (input ()
         (sdl2:with-event-loop (:method :poll)
           (:keyup
            (:keysym keysym)
            (when (sdl2:scancode=
                   (sdl2:scancode-value keysym)
                   :scancode-escape)
              (sdl2:push-event :quit)))))
       (update ())
       (render (mainwin)
         ())
       (cleanup ()))))

