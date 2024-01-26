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

(defvar jack (make-instance 'person :name :jack))
(defvar bob (make-instance 'lisp-programmer
                            :name :bob
                            :favorite-lisp-implementation :sbcl))
(defvar adam (make-instance 'c-programmer
                             :name :adam
                             :favorite-c-compiler :clang))
(person-name jack)
(person-name bob)
(favorite-lisp-implementation bob)
(person-name adam)
(favorite-c-compiler adam)
(defvar zenon (make-instance 'clisp-programmer
                              :name :zenon
                              :favorite-lisp-implementation :clisp
                              :favorite-c-compiler :gcc))
(typep zenon 'person)
(typep zenon 'lisp-programmer)
(typep zenon 'c-programmer)
(typep zenon 'clisp-programmer)

(defvar chatbot (make-instance 'greeting-chatbot :version "1.0.0"))
(typep chatbot 'greeting-mixin)
(typep chatbot 'chatbot)
(typep chatbot 'greeting-chatbot)

(defvar james (make-instance 'american))
(defvar antonio (make-instance 'italian))
(defvar roy (make-instance 'stereotypical-person))
(defvar mary (make-instance 'another-stereotypical-person))


;; implementing sort of DI method (before & after)
;; accessor, reader and writer are generic method, hence we could use
;; the implementation method with class specific to modify the values
(defclass access-counter ()
  ((value :accessor value :initarg :value)
   (access-count :reader access-count :initform 0)))

;; inject access count to value slot
(defmethod value :after ((object access-counter))
  (incf (slot-value object 'access-count)))

(defvar counter (make-instance 'access-counter :value 0))
(access-count counter) ; read the value access counter
(value counter); read the value
(setf (value counter) 24)


;; defined an implementation of generic grab-lollipop
(defgeneric grab-lollipop ()
  (:method () :lollipop))
;; defined a list of implementation with :around, which when exist
;; executed first with the option to continue with original method
(defgeneric grab-lollipop-while-mom-is-nearby (was-nice-p)
  (:method :around (was-nice-p) (if was-nice-p (call-next-method) :no-lollipop))
  (:method (was-nice-p) (declare (ignore was-nice-p)) :lollipop))


(defclass countdown ()
  ;; The countdown object represents an ongoing countdown. Each time the
  ;; REMAINING-TIME function is called, it should return a number one less than
  ;; the previous time that it returned. If the countdown hits zero, :BANG
  ;; should be returned instead.
  ((remaining-time :reader remaining-time :initarg :time)))

(defmethod remaining-time :around ((object countdown))
  (let ((time (call-next-method)))
    (if (< 0 time)
        ;; PROG1 returns the value of the first expression in the sequence.
        ;; DECF is similar to INCF. It decreases the value stored in the place
        ;; and returns the decreased value.
        (prog1
          time
          (decf (slot-value object 'remaining-time)))
        :bang)))

(defvar countdown (make-instance 'countdown :time 4))

(remaining-time countdown)
(remaining-time countdown)
(remaining-time countdown)
(remaining-time countdown)
(remaining-time countdown)
(remaining-time countdown)


(defvar object (make-instance 'object))
;; actual initialization of generic methods to a specific class type, 
;; for all the slots functions
(frobnicate object) 
(counter object)
;; since the following object inherits object, the generic methods binded
;; are all executed (subtype first, around, before, actual method, after)
(defvar bigger-object (make-instance 'bigger-object))
(frobnicate bigger-object) 
(counter bigger-object)

;; currently it seems we could use defgeneric as following
;; 1. As a method switch-case based on parameters (call-next-method)
;;    - method definition redefined the switch-case mechanism, either:
;;      - "+" to indicate sum of addition according to class type
;;      - "*" to indicate multiplaction based on type hierarchy
;;      - "progn" to indicate list of programs execution
;; 2. As a method declaration of generic function; requires defmethod to be 
;;    declared specific to the generic method
;; 3. As a "override" to the generated/macro definition of class slots functions
;; 4. As DI for all the class slots functions call with the defgeneric called type,
;;    in this case slot are not named in the defgeneric declaration


(defgeneric calculate (x)
  (:method :around ((x bigger-object))
    (setf (counter x) 40)
    (call-next-method))
  (:method :around ((x object))
    (incf (counter x) 24)
    (call-next-method))
  (:method :before ((x bigger-object))
    (setf (counter x) (mod (counter x) 6)))
  (:method :before ((x object))
    (setf (counter x) (/ (counter x) 4)))
  (:method ((x bigger-object))
    (setf (counter x) (* (counter x) (counter x)))
    (call-next-method))
  (:method ((x object))
    (decf (counter x) 100))
  (:method :after ((x object))
    (setf (counter x) (/ 1 (counter x))))
  (:method :after ((x bigger-object))
    (incf (counter x) 2)))

(defvar calc-object (make-instance 'object))
(defvar calc-bigger-object (make-instance 'bigger-object))
(calculate calc-object)
(counter calc-object)
(calculate calc-bigger-object)
(counter calc-bigger-object)


;; condition object or define-condition seems to be similar to exception
;; but more general class. Hence need to create via make-condition
;; will use with handler-bind to associate condition with handler method,
;; a function with condition definition as argument
(define-condition my-condition () ())
(define-condition my-warning (warning) ())
(define-condition my-serious-condition (serious-condition) ())
(define-condition my-error (error) ())
(type-of var-condition)
(class-of var-condition)
(defvar var-condition (make-condition 'my-condition))
(type-of var-condition)
(class-of var-condition)
(typep var-condition 'my-condition)
(typep var-condition 'condition)
(typep var-condition 'warning)
(typep var-condition 'error))

(defvar *list*)

(define-condition silly-condition () ())

(define-condition very-silly-condition (silly-condition) ())

(define-condition most-silly-condition (very-silly-condition) ())

(defun handle-silly-condition (condition)
  (declare (ignore condition))
  (push :silly-condition *list*))

(defun handle-very-silly-condition (condition)
  (declare (ignore condition))
  (push :very-silly-condition *list*))

(defun handle-most-silly-condition (condition)
  (declare (ignore condition))
  (push :most-silly-condition *list*))

(let ((*list* '()))
    (handler-bind ((very-silly-condition #'handle-very-silly-condition)
                   (silly-condition #'handle-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (signal (make-condition 'most-silly-condition)))
    *list*)

;; binding in sequence
(let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (handler-bind ((very-silly-condition #'handle-very-silly-condition))
        (signal (make-condition 'most-silly-condition))))
    *list*)


;; duplicate binding stills trigger
(let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (silly-condition #'handle-silly-condition))
      (handler-bind ((very-silly-condition #'handle-very-silly-condition)
                     (silly-condition #'handle-silly-condition)
                     (very-silly-condition #'handle-very-silly-condition))
        (signal (make-condition 'most-silly-condition))))
    *list*)

;; signal condition has to match type hierarchy
(let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (very-silly-condition #'handle-very-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (signal (make-condition 'very-silly-condition)))
     *list*)

;; condition handler transfering control, i.e. to escape related condition handlers
(let ((*list* '()))
    (block my-block
      (handler-bind ((silly-condition #'handle-silly-condition)
                     (silly-condition (lambda (condition)
                                        (declare (ignore condition))
                                        (return-from my-block)))
                     (silly-condition #'handle-silly-condition))
        (signal (make-condition 'silly-condition))))
    *list*)

;; handler case is more like a try-catch block; where as when we use handler-bind
;; the signal portion is triggered in the scope handler-bind is in; handler case
;; handle in the scope of the handler-case block switching control to appropriate handler-bind
(let ((*list* '()))
  (handler-case (signal (make-condition 'my-error))
    (error (condition) (handle-error condition))
    (my-error (condition) (handle-my-error condition)))
  *list*)

;; handler-case orders matter
(let ((*list* '()))
    (handler-case (signal (make-condition 'my-error))
      (my-error (condition) (handle-my-error condition))
      (error (condition) (handle-error condition)))
    *list*)

;; case must match
(let ((*list* '()))
    (handler-case (signal (make-condition 'warning))
      (my-error (condition) (handle-my-error condition))
      (error (condition) (handle-error condition)))
     *list*)


(defun divide (numerator denominator)
  (/ numerator denominator))

(flet ((try-to-divide (numerator denominator)
           ;; In code outside Lisp Koans, HANDLER-CASE should be used.
           (handler-case (divide numerator denominator)
             (division-by-zero () :division-by-zero)
             (type-error () :type-error))))
   (try-to-divide 6 2))
   ;;(try-to-divide 6 0)
  ;;  (try-to-divide 6 :zero))


;; extractin meta data of the condition
(let ((condition (handler-case (divide 6 0) (division-by-zero (c) c))))
    (arithmetic-error-operands condition)) ;; operand raised by division-by-zero

;; retrive back the function signed in the condition
(let ((condition (handler-case (divide 6 0) (division-by-zero (c) c))))
    (let ((operation (arithmetic-error-operation condition)))
      (funcall operation 12 4)))

;; retrieve meta data on type error: get the error operand
(let ((condition (handler-case (divide 6 :zero) (type-error (c) c))))
    (type-error-datum condition))

;; retrieve the expected error operand
(let ((condition (handler-case (divide 6 :zero) (type-error (c) c))))
    (type-error-datum condition)
    (let ((expected-type (type-error-expected-type condition)))
      (class-of expected-type)))
      ;;(typep :zero expected-type)))
      ;;(typep 0 expected-type)
      ;;(typep "zero" expected-type)
      ;;(typep 0.0 expected-type)))


;; condition with slots defined
(define-condition parse-log-line-error (parse-error)
  ((line :initarg :line :reader line)
   (reason :initarg :reason :reader reason)))

(defun log-line-type (line)
  ;; The macro CHECK-TYPE signals a TYPE-ERROR if the object is not of the
  ;; specified type.
  (check-type line string)
  (cond ((eql 0 (search "TIMESTAMP" line)) :timestamp)
        ((eql 0 (search "HTTP" line)) :http)
        ((eql 0 (search "LOGIN" line)) :login)
        ;; The function ERROR should be used for signaling serious conditions
        ;; and errors: if the condition is not handled, it halts program
        ;; execution and starts the Lisp debugger.
        (t (error 'parse-log-line-error :line line
                                        :reason :unknown-log-line-type))))

(defun triangle (a b c)
  ;; Fill in the blank with a function that satisfies the below tests.
 (let* ((min (min a b c))
        (max (max a b c))
        (mid (car (remove min (remove max (list a b c) :count 1) :count 1))))
   (cond  ((= min mid max) :equilateral)
          ((= min mid) :isosceles)
          ((= min max) :isosceles)
          (t :scalene)
     )
   ))
(triangle 1 1 1)
triangle
(defvar x '(123))
(defvar z '(7 8 9))

(let ((variable 'x))
  ;; Fill in the blank without using backquote/unquote notation.
  (format t "~A"
   `(if (typep ,variable 'string)
        (format nil "The value of ~A is ~A" ',variable ,variable)
        (error 'type-error :datum ,variable
               :expected-type 'string)))
  :COMPLETED)

;;; even weirder since limit becomes 3 after macro
;;; nreverse result is '(0 1 2 3)
(macrolet ((for ((var start stop) &body body)
               `(do ((,var ,start (1+ ,var))
                     (limit ,stop))
                    ((> ,var limit))
                  ,@body)))
    (let ((limit 10)
          (result '()))
      (for (i 0 3)
           (push i result)
           (format t "limit is : ~A~&" limit))
      (format t "nreverse result is :~{~A~}"(nreverse result)))
  :COMPLETED)

;;; don't know how this work, since side effect are inserted 5 times
;; both nreverse; result is '(0 1 2 3), side-effects is '(0 3 3 3 3 3)
(macrolet ((for ((var start stop) &body body)
               `(do ((,var ,start (1+ ,var)))
                    ((> ,var ,stop))
                  ,@body)))
    (let ((side-effects '())
          (result '()))
      ;; Our functions RETURN-0 and RETURN-3 have side effects.
      (flet ((return-0 () (push 0 side-effects) 0)
             (return-3 () (push 3 side-effects) 3))
        (for (i (return-0) (return-3))
          (push i result)))
      (format t "nreverse result is :~{~A ~}~&" (nreverse result))
      (format t "nreverse sid-effects is :~{~A ~}~&" (nreverse side-effects))
      :COMPLETED))

;;; should say this doesn't respect macro subform evaluation order
;;; both nreverse; result is '(0 1 2 3) and side-effects is '(3 0)
(macrolet ((for ((var start stop) &body body)
               ;; The function GENSYM creates GENerated SYMbols, guaranteed to
               ;; be unique in the whole Lisp system. Because of that, they
               ;; cannot capture other symbols, preventing variable capture.
               (let ((limit (gensym "LIMIT")))
                 `(do ((,limit ,stop)
                       (,var ,start (1+ ,var)))
                      ((> ,var ,limit))
                    ,@body))))
    (let ((side-effects '())
          (result '()))
      (flet ((return-0 () (push 0 side-effects) 0)
             (return-3 () (push 3 side-effects) 3))
        (for (i (return-0) (return-3))
          (push i result)))
      (format t "nreverse result is : ~{ ~A ~}~&" (nreverse result))
      (format t "nreverse side-effects is : ~{ ~A ~}~&" (nreverse side-effects))
      :COMPLETED))

;;; the correct working one; fixing all the aboves
;;; both nreverse; result is '(0 1 2 3) and side-effects is '(0 3)
( macrolet ((for ((var start stop) &body body)
              ;; Fill in the blank with a correct FOR macroexpansion that is
              ;; not affected by the three macro pitfalls mentioned above.
              (let ((limit (gensym "LIMIT")))
                `(do ((,var ,start (1+ ,var))
                      (,limit ,stop))
                     ((> ,var ,limit))
                   ,@body))))
  (let ((side-effects '())
        (result '()))
    (flet ((return-0 () (push 0 side-effects) 0)
           (return-3 () (push 3 side-effects) 3))
      (for (i (return-0) (return-3))
           (push i result)))
    (format t "nreverse result is : ~{ ~A ~}~&" (nreverse result))
    (format t "nreverse side-effects is : ~{ ~A ~}~&" (nreverse side-effects))
    :COMPLETED))
