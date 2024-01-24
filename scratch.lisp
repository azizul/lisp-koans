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

(+ 2 1)
(+ 1 (- 10 2))
(symbol-value (find-symbol "*PACKAGE*" "COMMON-LISP"))

(defvar test-var-a '("Mary had a little lamb"
                   "Old McDonald had a farm"
                   "Happy birthday to you"))
test-var-a

(mapcar (lambda (x) (subseq x 4 12)) test-var-a)

(setf test-var-a "lorem ipsum")
(map 'string #'char-upcase test-var-a)
(map 'list #'char-upcase test-var-a)
(length (map 'list #'char-upcase test-var-a))

(map '(vector t) #'char-upcase test-var-a)

(setf test-var-a '((1 2 3)
                   (4 5 6)
                   (7 8 9)))

(apply #'mapcar #'list test-var-a)
(expt 8 3)
(reduce #'expt '(1 2 3 4 5))
(cons 1 2 3)
(reduce #'cons '(1 2 3 4 5))
(reduce #'cons '(1 2 3 4 5) :FROM-END t)
(equalp '(1 2 3 4 . 5) (reduce #'cons '(1 2 3 4 5) :FROM-END t))
(reduce #'expt '(2 9) )
(reduce #'expt '(2 3 2) :FROM-END t)
(apply #'* '(1 2 3 4 5))
(mapcar #'* '(1 2 3) '(4 5 6))
(when (= 1 1)
  (format t "hello")
  (format t "bye"))
(loop for x in '((test) (pen)) append x)
(loop for x on '((test) (pen)) append x)
(loop for letter in '(:a :b :c :d)
      for i from 1 to 1000
      collect (list i letter))


(loop for x in '(:a :b :c) collect x)
(loop for x on '(:a :b :c) collect x)
(loop for i from 0 to 30 by 5 collect i)
(loop for i from 5 downto -5 collect i)
(loop for i in '(:a :b :c) by #'cdr collect i)
(loop for i in '(:a :b :c :d :e :f) by #'cddr collect i)

(loop for i across '#(1 2 3 4 5) collect i)
(setf test-var-a (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
(array-dimension test-var-a 0)
(let ((result  (loop with max-i = (array-dimension test-var-a 0)
                     for i from 0 below max-i
                     collect (loop with max-i = (array-dimension test-var-a 1)
                              for j from 0 below max-i
                              collect (expt (aref test-var-a i j) 2)))))
  result)

; return multiple values
(let ((book-heroes (make-hash-table :test 'equal)))
    (setf (gethash "The Hobbit" book-heroes) "Bilbo"
          (gethash "Where The Wild Things Are" book-heroes) "Max"
          (gethash "The Wizard Of Oz" book-heroes) "Dorothy"
          (gethash "The Great Gatsby" book-heroes) "James Gatz")
  (let ((pairs-in-table (loop for key being the hash-keys of book-heroes
                              using (hash-value value)
                              collect (list key value))))
   (values (length pairs-in-table) pairs-in-table)))

(multiple-value-bind (v-hash-length v-pairs-in-table)
    (let ((book-heroes (make-hash-table :test 'equal)))
      (setf (gethash "The Hobbit" book-heroes) "Bilbo"
            (gethash "Where The Wild Things Are" book-heroes) "Max"
            (gethash "The Wizard Of Oz" book-heroes) "Dorothy"
            (gethash "The Great Gatsby" book-heroes) "James Gatz")
      (let ((pairs-in-table (loop for key being the hash-keys of book-heroes
                                  using (hash-value value)
                                  collect (list key value))))
        (values (length pairs-in-table) pairs-in-table)))
  (find '("The Hobbit" "Bilbo") v-pairs-in-table :test #'equal))

(let* ((count 0)
      (result (loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
               do (incf count)
               collect (+ a b))))
  (values count result))

(let ((numbers '(1 1 2 3 5 8 13 21)))
  (let ((result (loop for x in numbers
                      when (evenp x) sum x)))
    (format t "result 1 is :~d~&" result))
  (let ((result (loop for x in numbers
                      unless (evenp x) sum x)))
    (format t "result 2 is :~d~&" result))
  (flet ((greater-than-10-p (x) (> x 10)))
    (let ((result (loop for x in numbers
                        when (greater-than-10-p x) sum x)))
      (format t "result 3 is :~d~&" result))))

(score)
(score 5)
(score 1 5 5 1) ; 300
(score 5 1 1 5) ; 300
(score 2 3 4 6) ; 0 
(score 5 5 5) ; 500
(score 1 1 1) ; 1000
(score 2 1 1) ; 200
(score 2 2 2) ; 200
(score 3 3 3) ; 300
(score 4 4 4) ; 400
(score 6 6 6) ; 600sc
(format t "~c[2J" #\Esc)
(format nil "~A evaluates to ~A"
                             '(/ 24 (- 3 (/ 8 3)))
                             (/ 24 (- 3 (/ 8 3))))
(defparameter (form '(/ 24 (- 3 (/ 8 3))))
        (result (/ 24 (- 3 (/ 8 3)))))

(let ((form '(/ 24 (- 3 (/ 8 3))))
      (result (/ 24 (- 3 (/ 8 3)))))
  (format t "~B evaluates to ~B~&" form result)
  (format t "~O evaluates to ~O~&" form result)
  (format t "~D evaluates to ~D~&" form result)
  (format t "~X evaluates to ~X~&" form result)
  (format t "~3R evaluates to ~3R~&" form result))
