(in-package #:cl-user)

(defpackage #:aark
  (:documentation "Arkanoid game main package")
  (:use :cl)
  (:export
   #:start))

(defpackage #:aark.state
  (:documentation "Pacakge for game states and related functions")
  (:use :cl)
  (:export #:init
           #:process-input
           #:update
           #:render
           #:cleanup))
