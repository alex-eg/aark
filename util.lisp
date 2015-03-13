(in-package :sdl2)

(defun get-window-surface (window)
  (sdl2-ffi.functions:sdl-get-window-surface window))

(defun blit-surface (src srcrect dst dstrect)
  (sdl2-ffi.functions:sdl-upper-blit src srcrect
                                     dst dstrect))

(defun blit-surface-scaled (src srcrect dst dstrect)
  (sdl2-ffi.functions:sdl-upper-blit-scaled src srcrect
                                            dst dstrect))

(defun fill-rect (surface x y w h r g b a)
  (sdl2-ffi.functions:sdl-fill-rect
   surface
   (sdl2:make-rect x y w h)
   (sdl2-ffi.functions:sdl-map-rgba
    (sdl2-ffi.accessors:sdl-surface.format surface)
    r g b a)))

(export 'fill-rect)
(export 'get-window-surface)
(export 'blit-surface)
(export 'blit-surface-scaled)
