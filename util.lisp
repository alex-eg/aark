(in-package :sdl2)

(defun get-window-surface (window)
  (sdl2-ffi.functions:sdl-get-window-surface window))

(defun blit-surface (src srcrect dst dstrect)
  (sdl2-ffi.functions:sdl-upper-blit src srcrect
                                     dst dstrect))

(defun blit-surface-scaled (src srcrect dst dstrect)
  (sdl2-ffi.functions:sdl-upper-blit-scaled src srcrect
                                            dst dstrect))

(export 'get-window-surface)
(export 'blit-surface)
(export 'blit-surface-scaled)
