(setf test (make-array '(3 3)))
(dotimes (x 3)
  (dotimes (y 3)
    (setf (aref test x y) (if (evenp (+ x y)) :black :white))))

(setf (aref test 0 1 2) :red
      (aref test 2 1 0) :white)


(labels ((test (a b)
           (if (or (= 0 a) (= 0 b))
               1
               (+ (* a b) (test (1- a) (1- b))))))
  (test 4 5))
;; 20/4 5, 12/3 4, 6/2 3, 2/1 2, 1
;;
(defun test (&optional (a 2 a-provided-p) (b 3 b-provided-p))
  (list a a-provided-p b b-provided-p ))
