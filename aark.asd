(in-package #:cl-user)

(asdf:defsystem :aark
  :depends-on (:sdl2)
  :components ((:file "packages")
               (:file "renderer")
               (:file "state")
               (:file "game")
               (:file "game-menu")
               (:file "menu")
               (:file "aark")))
