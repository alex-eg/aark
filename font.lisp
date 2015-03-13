(in-package #:aark)

(defstruct font
  surface
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

(defun init-font (path-to-file alphabet cell-w cell-h)
  (cffi:with-foreign-string (foreign-string-path path-to-file)
    (let ((font-surface (sdl2:load-bmp foreign-string-path)))
      (if (autowrap:wrapper-null-p font-surface)
          (error 'load-font-failed :path path-to-file))
      (let* ((width (sdl2-ffi.accessors:sdl-surface.w font-surface))
             (height (sdl2-ffi.accessors:sdl-surface.h font-surface))
             (rows (floor height cell-h))
             (cols (floor width cell-w)))
        (make-font :surface font-surface
                   :alphabet (loop for a across alphabet
                                with alphabet-hash = (make-hash-table)
                                counting a into i
                                do (setf (gethash a alphabet-hash) (1- i))
                                finally (return alphabet-hash))
                   :rows rows
                   :cols cols
                   :cell-w cell-w
                   :cell-h cell-h)))))

(defun get-font-symbol (num font)
  (multiple-value-bind (col row)
      (floor num (font-cols font))
    (sdl2:make-rect (* (font-cell-w font) row)
                    (* (font-cell-h font) col)
                    (font-cell-w font)
                    (font-cell-h font))))

(defmacro write-on-surface (text font dest
                            &key
                              (centered nil)
                              (x 0) (y 0)
                              (blit-func ''sdl2:blit-surface-scaled)
                              (cell-w `(font-cell-w ,font))
                              (cell-h `(font-cell-h ,font)))
  (let ((text-list (loop for c across text
                      collect c))
        (char (gensym))
        (i (gensym))
        (new-x (if centered
                   (/ (- x (* cell-w (length text)))
                      2)
                   x)))
    `(loop for ,char in ',text-list
        count ,char into ,i
        do (funcall
            ,blit-func
            (font-surface ,font)
            (get-font-symbol (gethash ,char (font-alphabet ,font))
                             ,font)
            ,dest
            (sdl2:make-rect 
             (+ ,new-x (* ,cell-w (1- ,i))) ,y
             ,cell-w ,cell-h)))))
