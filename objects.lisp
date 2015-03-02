(defstruct rect
  x y w h image)
    
(defstruct ball
  (:include rect))

(defstruct block
  (:include rect))

