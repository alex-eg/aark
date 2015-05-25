(in-package :aark)

(defclass renderer ()
  ((sdl2-renderer :initform (error "Renderer must be set")
                  :initarg :renderer)
   (sprites       :initform '())
   (fonts         :initform '())))

(defun load-texture (renderer path-to-file)
  (sdl2:create-texture-from-surface renderer
				    (sdl2:load-bmp path-to-file)))

(defun add-sprite (renderer sprite-name path-to-file)
  (etypecase sprite-name
    (keyword 
     (with-slots (sdl2-renderer sprites) renderer
       (if (assoc sprite-name sprites)
           (error "Sprite with name ~a already registered" sprite-name))
       (setf sprites
             (acons sprite-name
                    (load-texture sdl2-renderer path-to-file)
                    sprites))
       sprite-name))))

(defun get-sprite-texture (renderer sprite-name)
  (etypecase sprite-name
    (keyword
     (cdr (assoc sprite-name (slot-value renderer 'sprites))))))

(defun remove-srpite (renderer sprite-name)
  (etypecase sprite-name
    (keyword
     (with-slots (sprites) renderer
       (setf sprites
             (remove sprite-name sprites
                     :key #'car))
       t))))

(defun sprite-width (renderer sprite-name)
  (sdl2:texture-width (get-sprite-texture renderer sprite-name)))

(defun sprite-heigth (renderer sprite-name)
  (sdl2:texture-height (get-sprite-texture renderer sprite-name)))

(defun draw-rect (renderer x y w h r g b a)
  (with-slots (sdl2-renderer) renderer 
    (sdl2:set-render-draw-color sdl2-renderer r g b a)
    (sdl2:render-fill-rect sdl2-renderer
                           (sdl2:make-rect x y w h))))

(defun draw-sprite (renderer sprite-name x y)
  (let ((w (sdl2:texture-width texture))
	(h (sdl2:texture-height texture)))
    (sdl2:render-copy (slot-value renderer 'sdl2-renderer)
                      (get-sprite-texture renderer sprite-name)
		      :dest-rect (sdl2:make-rect x y w h))))

(defstruct font
  sprite
  alphabet
  rows
  cols
  cell-w
  cell-h)

(define-condition load-font-failed (error)
  ((path :initarg :path :reader path))
  (:report (lambda (condition stream)
             (format stream "Failed loading font bmp: ~S~%"
                     (sdl2-ffi.functions:sdl-get-error)))))

(defun add-font (renderer font-name path-to-file alphabet cell-w cell-h
                 &key (r 0) (g 0) (b 0))
  (let* ((font-sprite (add-sprite renderer font-name path-to-file))
	 (width (sprite-width renderer font-sprite))
	 (height (sprite-heigth renderer font-sprite))
	 (rows (floor height cell-h))
	 (cols (floor width cell-w)))
    (with-slots (fonts) renderer
      (setf fonts (acons
                   font-name
                   (make-font :sprite font-sprite
                              :alphabet (loop for a across alphabet
                                           with alphabet-hash = (make-hash-table)
                                           counting a into i
                                           do (setf (gethash a alphabet-hash) (1- i))
                                           finally (return alphabet-hash))
                              :rows rows
                              :cols cols
                              :cell-w cell-w
                              :cell-h cell-h)))
      t)))

(defun get-font (renderer font-name))

(defun get-font-symbol (num font)
  (multiple-value-bind (col row)
      (floor num (font-cols font))
    (sdl2:make-rect (* (font-cell-w font) row)
                    (* (font-cell-h font) col)
                    (font-cell-w font)
                    (font-cell-h font))))

(defun write-text (renderer text font-name
                   &key
                     (centered nil)
                     (x 0) (y 0)
                     (cell-w (font-cell-w (get-font renderer font-name)))
                     (cell-h (font-cell-h (get-font renderer font-name))))
  (let ((text-list (loop for c across text
                      collect c))
        (new-x (if centered
                   (/ (- x (* cell-w (length text)))
		      2)
                   x)))
    (loop for char in text-list
       count char into i
       do (sdl2:render-copy
	   (slot-value renderer 'sdl2-renderer)
	   (get-sprite-texture renderer (font-sprite font))
	   :source-rect
	   (get-font-symbol (gethash char
				     (font-alphabet font))
			    font)
	   :dest-rect
	   (sdl2:make-rect 
	    (+ new-x (* cell-w (1- i))) y
	    cell-w cell-h)))))
