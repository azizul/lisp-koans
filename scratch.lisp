(setf test (make-array '(3 3)))
(dotimes (x 3)
  (dotimes (y 3)
    (setf (aref test x y) (if (evenp (+ x y)) :black :white))))

(setf (aref test 0 1 2) :red
      (aref test 2 1 0) :white)
