(in-package :aark)

(defun main (&key (core-is-root nil))
  (defparameter +root+
    (if core-is-root
        *default-pathname-defaults*
        (asdf:system-source-directory :aark))
    "Project root absolute path.")

  (let ((aark (make-instance 'application)))
    (start aark)))
