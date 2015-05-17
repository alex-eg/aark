(in-package :aark)

(defun load-texture (renderer path-to-file)
  (sdl2:create-texture-from-surface renderer
				    (sdl2:load-bmp path-to-file)))

(defun draw-rect (renderer x y w h r g b a)
  (sdl2:set-render-draw-color renderer r g b a)
  (sdl2:render-fill-rect renderer
			 (sdl2:make-rect x y w h)))

(defun draw-sprite (renderer texture x y)
  (let ((w (sdl2:texture-width texture))
	(h (sdl2:texture-height texture)))
    (sdl2:render-copy renderer texture
		      :dest-rect (sdl2:make-rect x y w h))))

(defmacro with-state-storage ((storage-hash &rest entries) &body body)
  (let* ((storage-hash-var (gensym))
         (entries-list (loop for e in entries
                          collect `(,e (gethash ',e ,storage-hash-var)))))
    `(let* ((,storage-hash-var (gethash ',storage-hash *storage*))
            ,@entries-list)
       ,@body)))
