(in-package :aark)

(defclass renderer ()
  ((sdl2-renderer :initform (error "Renderer must be set")
                  :initarg :renderer
                  :accessor sdl2-renderer)
   (sprites :initform (list) :accessor renderer-sprites)
   (fonts :initform (list) :accessor renderer-fonts)))

(defvar *renderer* nil
  "Global variable holding current renderer")

(defun set-current-renderer (renderer)
  (setf *renderer* renderer))

(defun load-texture (renderer path-to-file)
  (sdl2:create-texture-from-surface
   renderer
   (sdl2-image:load-image path-to-file)))

(defun add-sprite (sprite-name path-to-file &key (blend-mode :none))
  (etypecase sprite-name
    (keyword
     (with-slots (sdl2-renderer sprites) *renderer*
       (if (assoc sprite-name sprites)
           (error "Sprite with name ~a already registered" sprite-name))
       (let ((texture (load-texture sdl2-renderer path-to-file)))
         (sdl2:set-texture-blend-mode texture blend-mode)
         (setf sprites
               (acons sprite-name
                      texture
                      sprites))
         sprite-name)))))

(defun get-sprite-texture (sprite-name)
  (etypecase sprite-name
    (keyword
     (cdr (assoc sprite-name
                 (renderer-sprites
                  *renderer*))))))

(defun remove-srpite (sprite-name)
  (etypecase sprite-name
    (keyword
     (with-slots (sprites) *renderer*
       (setf sprites
             (remove sprite-name sprites
                     :key #'car))
       t))))

(defun sprite-width (sprite-name)
  (sdl2:texture-width (get-sprite-texture sprite-name)))

(defun sprite-height (sprite-name)
  (sdl2:texture-height (get-sprite-texture sprite-name)))

(defun draw-rect (x y w h r g b a &key (blendmode :blend))
  (with-slots (sdl2-renderer) *renderer*
    (sdl2:set-render-draw-blend-mode sdl2-renderer blendmode)
    (sdl2:set-render-draw-color sdl2-renderer r g b a)
    (sdl2:render-fill-rect sdl2-renderer
                           (sdl2:make-rect x y w h))))

(defun draw-sprite (sprite-name x y &key (blendmode :blend)
                                               (scale 1.0))
  (let* ((texture (get-sprite-texture sprite-name))
         (sdl2-renderer (slot-value *renderer* 'sdl2-renderer))
         (w (round (* scale (sdl2:texture-width texture))))
         (h (round (* scale (sdl2:texture-height texture)))))
    (sdl2:set-texture-blend-mode texture blendmode)
    (sdl2:render-copy sdl2-renderer
                      texture
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
             (declare (ignore condition))
             (format stream "Failed loading font bmp: ~S~%"
                     (sdl2-ffi.functions:sdl-get-error)))))

(defun add-font (font-name path-to-file alphabet cell-w cell-h
                 &key (blend-mode :blend))
  (let* ((font-sprite (add-sprite font-name
                                  path-to-file
                                  :blend-mode blend-mode))
         (width (sprite-width font-sprite))
         (height (sprite-height font-sprite))
         (rows (floor height cell-h))
         (cols (floor width cell-w)))
    (with-slots (fonts) *renderer*
      (setf fonts (acons
                   font-name
                   (make-font
                    :sprite font-sprite
                    :alphabet (loop for a across alphabet
                                 with alphabet-hash = (make-hash-table)
                                 counting a into i
                                 do (setf (gethash a alphabet-hash) (1- i))
                                 finally (return alphabet-hash))
                    :rows rows
                    :cols cols
                    :cell-w cell-w
                    :cell-h cell-h)
                   fonts))
      t)))

(defun get-font (font-name)
  (cdr (assoc font-name (renderer-fonts *renderer*))))

(defun get-font-symbol (num font)
  (multiple-value-bind (col row)
      (floor num (font-cols font))
    (sdl2:make-rect (* (font-cell-w font) row)
                    (* (font-cell-h font) col)
                    (font-cell-w font)
                    (font-cell-h font))))

(defun write-text (text font-name
                   &key
                     (centered nil)
                     (x 0) (y 0)
                     (cell-w (font-cell-w (get-font font-name)))
                     (cell-h (font-cell-h (get-font font-name))))
  (let ((text-list (loop for c across text
                      collect c))
        (new-x (if centered
                   (/ (- x (* cell-w (length text)))
                      2)
                   x))
        (font (get-font font-name)))
    (loop for char in text-list
       count char into i
       do (sdl2:render-copy
           (sdl2-renderer *renderer*)
           (get-sprite-texture (font-sprite font))
           :source-rect
           (get-font-symbol (gethash char
                                     (font-alphabet font))
                            font)
           :dest-rect
           (sdl2:make-rect
            (+ new-x (* cell-w (1- i))) y
            cell-w cell-h)))))
