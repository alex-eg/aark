(in-package #:cl-user)

(defpackage #:aark-asd
  (:use #:cl
        #:asdf))

(in-package #:aark-asd)

(defsystem :aark
  :components ((:file "aark")
               (:file "state")
               (:file "collisions")
               (:file "packages")))
