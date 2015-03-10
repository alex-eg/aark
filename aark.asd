(in-package #:cl-user)

(asdf:defsystem :aark
  :depends-on (:sdl2-dev)
  :components ((:file "packages")
               (:file "util")
               (:file "aark")))
