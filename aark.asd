(in-package #:cl-user)

(asdf:defsystem :aark
  :depends-on (:sdl2
               :sdl2-image)
  :components ((:file "packages")
               (:file "application")
               (:file "renderer")
               (:file "state")
               (:file "game")
               (:file "game-menu")
               (:file "menu")
               (:file "aark")))
