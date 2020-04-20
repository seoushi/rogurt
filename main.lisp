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

(defun get-room-at-coordinate (coordinate rooms)
  (if (not rooms)
      nil
      (let* ((room (first rooms))
             (x (world-room-x-coordinate room))
             (y (world-room-y-coordinate room))
             (x2 (first coordinate))
             (y2 (second coordinate)))
        (if (and (equalp x x2)
                 (equalp y y2))
            room
            (get-room-at-coordinate coordinate (rest rooms))))))

(defun opposite-direction (direction)
  (cond
    ((= direction 0) 2) ;; north -> south
    ((= direction 1) 3) ;; east ->west
    ((= direction 2) 0)
    ((= direction 3) 1)))

(defun add-room (world new-room)
  (setf (world-rooms world) (append (world-rooms world) new-room)))

(defun connect-rooms (first-room second-room direction) ;; direction is from first->second
  (set-room-for-direction first-room direction second-room)
  (set-room-for-direction second-room (opposite-direction direction) first-room))

(defun generate-rooms (world number-of-rooms &optional last-room)
  (if (<= number-of-rooms 0)
      world
      (let* ((direction (random 4))
             (coordinate (get-coordinate-for-next-room last-room direction))
             (all-rooms (world-rooms world))
             (new-room (make-world-room :x-coordinate (first coordinate)
                                        :y-coordinate (second coordinate)
                                        :north-room nil
                                        :east-room nil
                                        :south-room nil
                                        :west-room nil))
             (existing-room (get-room-at-coordinate coordinate all-rooms)))
        ;; TODO: limit the direction depending on the surrounding rooms
        ;;  if all the room connects are full. then we need to pick a new room
        (if last-room
            (if existing-room
                (progn
                  (connect-rooms last-room existing-room direction)
                  (generate-rooms world number-of-rooms existing-room))
                (progn
                  (add-room world new-room)
                  (connect-rooms last-room new-room direction)
                  (generate-rooms world (- number-of-rooms 1) new-room)))
            (progn
              (setf (world-rooms world) (list new-room))
              (generate-rooms world (- number-of-rooms 1) new-room))))))


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

(defun print-room (room)
  (format t "X:~a, Y:~a - N:~a E:~a S:~a W:~a~%"
          (world-room-x-coordinate room)
          (world-room-y-coordinate room)
          (not (not (world-room-north-room room)))
          (not (not (world-room-east-room room)))
          (not (not (world-room-south-room room)))
          (not (not (world-room-west-room room)))))

(defun print-rooms (rooms)
  (if rooms
      (if (listp rooms)
          (progn
            (print-room (first rooms))
            (print-rooms (rest rooms)))
          (print-room rooms))))

(print-world *world*)
(print-rooms (world-rooms *world*))
