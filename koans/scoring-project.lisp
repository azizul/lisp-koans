;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Greed is a dice game played among 2 or more players, using 5
;;; six-sided dice.
;;;
;;; Each player takes a turn consisting of one or more rolls of the dice.
;;; On the first roll of the game, a player rolls all five dice which are
;;; scored according to the following:
;;;
;;;   Three 1's => 1000 points
;;;   Three 6's =>  600 points
;;;   Three 5's =>  500 points
;;;   Three 4's =>  400 points
;;;   Three 3's =>  300 points
;;;   Three 2's =>  200 points
;;;   One   1   =>  100 points
;;;   One   5   =>   50 points
;;;
;;; A single die can only be counted once in each roll.  For example,
;;; a "5" can only count as part of a triplet (contributing to the 500
;;; points) or as a single 50 points, but not both in the same roll.
;;;
;;; Example Scoring
;;;
;;;    Throw       Score
;;;    ---------   ------------------
;;;    5 1 3 4 1   50 + 2 * 100 = 250
;;;    1 1 1 3 1   1000 + 100 = 1100
;;;    2 4 4 5 4   400 + 50 = 450
;;;
;;; The dice not contributing to the score are called the non-scoring
;;; dice.  "3" and "4" are non-scoring dice in the first example.  "3" is
;;; a non-scoring die in the second, and "2" is a non-score die in the
;;; final example.
;;;
;;; More scoring examples are given in the tests below.
;;;
;;; Your goal is to write the scoring function for Greed.

;; using iteration to track multiple state is hard, better use
;; hash table to store state changes; so the key would be the number
;; then the value would be list of occurence of the the data
;; for dice rolls; 
;; '(1 1 2 1 1) => [1] = '(2, 2)
;; '(2 1 1 1 1) => [1] = '(4)
;; '(2 1 1 1 3) => [1] = '(3)
;; '(1 2 3 4 5) => [1] = '()

(defun score-once (&rest dice)
  (let ((sorted (sort (copy-list dice) #'<)))
    (cond ((search '(1 1 1) sorted) (list 1000 (remove 1 sorted :count 3)))
          ((search '(2 2 2) sorted) (list 200 (remove 2 sorted :count 3)))
          ((search '(3 3 3) sorted) (list 300 (remove 3 sorted :count 3)))
          ((search '(4 4 4) sorted) (list 400 (remove 4 sorted :count 3)))
          ((search '(5 5 5) sorted) (list 500 (remove 5 sorted :count 3)))
          ((search '(6 6 6) sorted) (list 600 (remove 6 sorted :count 3)))
          ((find 5 sorted) (list 50 (remove 5 sorted :count 1)))
          ((find 1 sorted) (list 100 (remove 1 sorted :count 1)))
          (t (list 0 '())))))

(defun score (&rest dice)
  (loop for current-dice = dice then remaining-dice
        for (score remaining-dice) = (apply #'score-once current-dice)
        sum score
        while remaining-dice))

(defun score_not_working (&rest dice)
  (let ((states (make-hash-table :test 'equal))
        (score 0)
        (previous-roll 0))
    ;; Get the occurence states
    (dolist (x dice)
      ;; for each roll, track the occurence of the data in a list of each
      ;; dice value
      (if (and (gethash states x) (equal x previous-roll))
          ;; iterates for non-empty list
          (dolist (value (gethash states x))
            ;; maximum occurence is 3, else add new occurence in the list 
            (setf (car value) (+ (car value) 1))
            )
          ;; else the initial empty list
          (setf (gethash states x) '()))

      ;; set previous rolls
      (setf previous-roll x))
    ;; count the states
    (loop for key being the hash-keys of states
          using (hash-value value)
          do (format t "key: ~a value: ~a~%" key value)
          ;;(progn 
          ;;    ((format t "key: ~a value: ~a~%" key value)
          ;;    (cond 
          ;;     ;; for die = one
          ;;     ((and (= key 1) (< (car value) 3)) 
          ;;      (setf score (+ score (* value 100))))
          ;;     ((and (= key 1) (= (car value) 3))
          ;;      (setf score (+ score 1000)))
          ;;     ((and (= key 1) (> (car value) 3))
          ;;      (setf score (+ score (+ 1000 (* (- 5 (car value)) 100)))))
          ;;     ;; for die = five
          ;;     ((and (= key 5) (< (car value) 3))
          ;;      (setf score (+ score (* value 50))))
          ;;     ((and (= key 5) (= (car value) 3))
          ;;      (setf score (+ score 500)))
          ;;     ((and (= key 5) (> (car value) 3))
          ;;      (setf score (+ score (+ 500 (* (- 5 (car value)) 50)))))
          ;;     ;; for others
          ;;     ((t)
          ;;      (setf score (+ score (* (- 5 value) 100)))
          ;;      )
          ;;     )))
          )
    score)
  )

(defun score_hard (&rest dice)
  (let ((prev-roll 0)
        (occurence 0)
        (rolls 0)
        (score 0)
        (score_per_roll '())
        (rolls-count (length dice)))
    ;; empty rolls returns zero
    (when (or (not dice) (= rolls-count 0))
      (format t "empty rolls"))

    (format t "dice:~d~&" dice)
    (dolist (x dice)
      (setf rolls (+ rolls 1))
      (when (= x prev-roll)
        (setf occurence (+ occurence 1)))
      (format t "rolls:~d, occurence:~d, prev-roll:~d~%, current roll:~d~&" rolls occurence prev-roll x)
      (when (and (= prev-roll 1) (= occurence 2))
        (setf score (+ score 1000)))
      (when (or (and (= prev-roll 1) (= occurence 0)) 
                (and (= rolls rolls-count) (= x 1) (= occurence 0))
                (and (= rolls rolls-count) (= x 1) (not (= x prev-roll)) (> occurence 0)))
        (setf score (+ score 100)))
      (when (or (and (= prev-roll 1) (= occurence 1) (not (= x 1)))
                (and (= rolls rolls-count) (= x prev-roll) (= x 1) (= occurence 1)))
        (setf score (+ score (* 100 2))))
      (when (and (= prev-roll 5) (= occurence 2))
        (setf score (+ score 500)))
      (when (or (and (= prev-roll 5) (= occurence 0)) 
                (and (= rolls rolls-count) (= x 5) (= occurence 0))
                (and (= rolls rolls-count) (= x 5) (not (= x prev-roll)) (> occurence 0)))
        (setf score (+ score 50)))
      (when (or (and (= prev-roll 5) (= occurence 1) (not (= x 5)))
                (and (= rolls rolls-count) (= x 5) (= occurence 5)))
        (setf score (+ score (* 50 2))))
      (when (and (= prev-roll 2) (= occurence 2))
        (setf score (+ score 200)))
      (when (and (= prev-roll 3) (= occurence 2))
        (setf score (+ score 300)))
      (when (and (= prev-roll 4) (= occurence 2))
        (setf score (+ score 400)))
      (when (and (= prev-roll 6) (= occurence 2))
        (setf score (+ score 600)))
      ;; reset occurence
      (when (and (> rolls 1) (not (= x prev-roll)))
        (setf occurence 0))
      (setf prev-roll x)
      (format t "score:~d~&" score))
    (format t "list score~d~&" (reduce #'+ score_per_roll))
    score))

(define-test score-of-an-empty-list-is-zero
  (assert-equal 0 (score)))

(define-test score-of-a-single-roll-of-5-is-50
  (assert-equal 50 (score 5)))

(define-test score-of-a-single-roll-of-1-is-100
  (assert-equal 100 (score 1)))

(define-test score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores
  (assert-equal 300 (score 1 5 5 1)))

(define-test score-of-single-2s-3s-4s-and-6s-are-zero
  (assert-equal 0 (score 2 3 4 6)))

(define-test score-of-a-triple-1-is-1000
  (assert-equal 1000  (score 1 1 1)))

(define-test score-of-other-triples-is-100x
  (assert-equal 200  (score 2 2 2))
  (assert-equal 300  (score 3 3 3))
  (assert-equal 400  (score 4 4 4))
  (assert-equal 500  (score 5 5 5))
  (assert-equal 600  (score 6 6 6)))

(define-test score-of-mixed-is-sum
  (assert-equal 250  (score 2 5 2 2 3))
  (assert-equal 550  (score 5 5 5 5)))
