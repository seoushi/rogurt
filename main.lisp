;; World is a grid like this
;; X X X X X
;; X X X X X
;; X X X X X
;; X X X X X
;; X X X X X

;; Room is a object with 4 possible exits or connections to other rooms
;; X X D X X
;; D . . . D
;; X X D X X

(in-package "rogurt")


(defstruct world-room x-coordinate y-coordinate north-room east-room south-room west-room)

(defstruct world grid rooms)



(defparameter *world* (make-world :grid (make-array '(5 5))
                                  :rooms '()))


(defun fill-world (world)
  (let ((grid (world-grid world)))
    (dotimes (y (array-dimension grid 0))
    (dotimes (x (array-dimension grid 1))
      (setf (aref grid x y) :#)))))

(fill-world *world*)

(defun set-room-for-direction (parent-room direction child-room)
  (cond
    ((= direction 0) (setf (world-room-north-room parent-room) child-room))
    ((= direction 1) (setf (world-room-east-room parent-room) child-room))
    ((= direction 2) (setf (world-room-south-room parent-room) child-room))
    ((= direction 3) (setf (world-room-west-room parent-room) child-room))))

(defun get-coordinate-for-next-room (room direction)
  (if (not room)
      '(0 0)
      (let ((x (+ (world-room-x-coordinate room)
                  (cond
                    ((= direction 1) 1)
                    ((= direction 3) -1)
                    (t 0))))
            (y (+ (world-room-y-coordinate room)
                  (cond
                    ((= direction 2) -1)
                    ((= direction 0) 1)
                    (t 0)))))
        (list x y))))

(defun generate-rooms (world number-of-rooms &optional last-room)
  (if (<= number-of-rooms 0)
      world
      (let* ((direction (random 4))
             (coordinate (get-coordinate-for-next-room last-room direction))
             (new-room (make-world-room :x-coordinate (first coordinate)
                                        :y-coordinate (second coordinate)
                                        :north-room nil
                                        :east-room nil
                                        :south-room nil
                                        :west-room nil)))
        ;; TODO: need to check if a room already exists at the new-room-coordinate
        (if last-room
            (set-room-for-direction last-room direction new-room))
        (setf (world-rooms world) (cons (world-rooms world) new-room))
        (generate-rooms world (- number-of-rooms 1) new-room))))

(defun print-world (world):q
  (let ((grid (world-grid world)))
    (dotimes (y (array-dimension grid 0))
      (dotimes (x (array-dimension grid 1))
        (format t "~a " (aref grid x y)))
      (format t "~%"))))

(defun make-room (world start-x start-y width height)
  (let ((grid (world-grid world)))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref grid (+ x start-x)
                    (+ y start-y))
              :.)))))


(generate-rooms *world* 2)
(make-room *world* 1 1 2 2)

(defun clear-screen()
  (format t "~A[H~@*~A[J" #\escape))

(print-world *world*)
(format t "~a" (world-rooms *world*))
