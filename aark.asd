(in-package #:cl-user)

(asdf:defsystem :aark
  :depends-on (:sdl2)
  :components ((:file "packages")
               (:file "util")
               (:file "font")
               (:file "game")
               (:file "game-menu")
               (:file "menu")
               (:file "aark")))
