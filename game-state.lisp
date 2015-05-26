(in-package :aark)

(defclass state ()
  ((name :initform (error "Name must be set")
         :initarg :name)))

(defgeneric init (state))
(defgeneric process-input (state))
(defgeneric update (state))
(defgeneric draw (state))
(defgeneric cleanup (state))

