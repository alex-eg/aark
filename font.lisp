(in-package #:aark)

(defstruct font
  texture
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

(defun init-font (ren path-to-file alphabet cell-w cell-h
                  &key (r 0) (g 0) (b 0))
  (let* ((font-surface (sdl2:load-bmp path-to-file))
	 (font-texture (load-texture ren path-to-file))
	 (width (sdl2:surface-width font-surface))
	 (height (sdl2:surface-height font-surface))
	 (rows (floor height cell-h))
	 (cols (floor width cell-w)))
    (make-font :texture font-texture
	       :alphabet (loop for a across alphabet
			    with alphabet-hash = (make-hash-table)
			    counting a into i
			    do (setf (gethash a alphabet-hash) (1- i))
			    finally (return alphabet-hash))
	       :rows rows
	       :cols cols
	       :cell-w cell-w
	       :cell-h cell-h)))

(defun get-font-symbol (num font)
  (multiple-value-bind (col row)
      (floor num (font-cols font))
    (sdl2:make-rect (* (font-cell-w font) row)
                    (* (font-cell-h font) col)
                    (font-cell-w font)
                    (font-cell-h font))))

(defun write-on-surface (ren text font
			 &key
			   (centered nil)
			   (x 0) (y 0)
			   (cell-w (font-cell-w font))
			   (cell-h (font-cell-h font)))
  (let ((text-list (loop for c across text
                      collect c))
        (new-x (if centered
                   (/ (- x (* cell-w (length text)))
		      2)
                   x)))
    (loop for char in text-list
       count char into i
       do (sdl2:render-copy
	   ren
	   (font-texture font)
	   :source-rect
	   (get-font-symbol (gethash char
				     (font-alphabet font))
			    font)
	   :dest-rect
	   (sdl2:make-rect 
	    (+ new-x (* cell-w (1- i))) y
	    cell-w cell-h)))))
