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

;;; EXTRA CREDIT:
;;;
;;; Create a program that will play the Greed game.
;;; The full rules for the game are in the file extra-credit.txt.
;;;
;;; You already have a DICE-SET class and a score function you can use.
;;; Write a PLAYER class and a GAME class to complete the project.
;;;
;;; This is a free form assignment, so approach it however you desire.

;; returns the random number when thrown 
(defclass dice ()
  ((roll :reader roll)))

;; overidde the accessor, to return the random dice value
(defmethod roll ((object dice))
    (1+ (random 6)))

;; player with total dice thrown
(defclass player ()
  ((name 
     :initarg :name 
     :accessor name )
   (dices 
     :accessor dices 
     :initform '())
   (complete-roll 
     :reader complete-roll 
     :initform 'false)))

;; override the roll method for player, will have to check if the dices
;; set in the players requires rolls
(defmethod complete-roll ((object player))
    ;; check if the rolls require rolling 
  (if (player-roll-required)
      (let ((dice (make-instance 'dice))) 
        (roll dice)
        (append dice (dices object))
        )
      nil)
  )

;; skip error checking first
(defun player-roll-required(player)
    (let* ((dices (dices player))
           (required nil))
      (when (<= (length dices) 5) (setf required t))
      ;; check for occurence of triple 1, 5 
      (when (>= (length dices) 0) 
        (progn 
          (loop for dice in dices
                do ())
          (setf required t)))
      required) 
  )
;; test the value
;;(let* ((dice (make-instance 'dice)))
;;  (roll dice))

;;(let ((player (make-instance 'player :name "azizul")))
;;  (format t "player name is ~A" (name player)))

;;(let ((player (make-instance 'player :name "azizul")))
;;  (format t "player score is ~A" (calculate-score player)))
;; play the game, given a list of players
(defun play-greed ()
    (let* ((robert (make-instance 'player :name "Robert"))
           (julia (make-instance 'player :name "Julia"))
           (edward (make-instance 'player :name "Edward"))
           (players `(,robert ,julia ,edward)))
      (loop for player in players 
            do (format t "player ~A~&" (name player)))))
;; FIXME later on complete this
;; test 
;;(play-greed)
(define-test play-greed
  (assert-true t))
