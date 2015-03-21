(in-package #:cl-user)

(asdf:defsystem :aark
  :depends-on (:sdl2-dev
               :cffi)
  :components ((:file "packages")
               (:file "game")
               (:file "game-menu")
               (:file "menu")
               (:file "util")
               (:file "font")
               (:file "aark")))
