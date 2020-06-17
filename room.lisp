
(in-package #:rogurt)

(defstruct room
  (x-coordinate 0 :type integer)
  (y-coordinate 0 :type integer)
  (north-room nil :type (or null room))
  (east-room nil  :type (or null room))
  (south-room nil :type (or null room))
  (west-room nil  :type (or null room)))


(defun set-room-for-direction (parent-room direction child-room)
  "set a link to another room in a given direction"
  (cond
    ((eq direction :north) (setf (room-north-room parent-room) child-room))
    ((eq direction :east)  (setf (room-east-room parent-room) child-room))
    ((eq direction :south) (setf (room-south-room parent-room) child-room))
    ((eq direction :west)  (setf (room-west-room parent-room) child-room))))

(defun get-coordinate-for-next-room (room direction)
  "returns the coordinate for a room given a room and the direction to move"
  (if (not room)
      '(0 0)
      (let ((x (+ (room-x-coordinate room)
                  (cond
                    ((eq direction :east) 1)
                    ((eq direction :west) -1)
                    (t 0))))
            (y (+ (room-y-coordinate room)
                  (cond
                    ((eq direction :south) -1)
                    ((eq direction :north) 1)
                    (t 0)))))
        (list x y))))

(defun find-room-at-coordinate (coordinate rooms)
  "tries to find a room at a given coordinate in a list of rooms"
  (if (not rooms)
      nil
      (let* ((room (first rooms))
             (other-rooms (rest rooms))
             (x (room-x-coordinate room))
             (y (room-y-coordinate room))
             (x2 (first coordinate))
             (y2 (second coordinate)))
        (if (and (equalp x x2)
                 (equalp y y2))
            room
            (find-room-at-coordinate coordinate other-rooms)))))


(defun connect-rooms (first-room second-room direction)
  "connects the first room to the second room in the given direction"
  (set-room-for-direction first-room direction second-room)
  (set-room-for-direction second-room (opposite-direction direction) first-room))

(defun get-free-directions (room)
  "returns all the directions that the room isn't connected to another room"
  (let ((results '()))
    (unless (room-north-room room)
      (setf results (append results (list :north))))
    (unless (room-east-room room)
      (setf results (append results (list :east))))
    (unless (room-south-room room)
      (setf results (append results (list :south))))
    (unless (room-west-room room)
      (setf results (append results (list :west))))
    results))

(defun pick-random-room (rooms)
  "returns a random room from a list of rooms"
  (let* ((room-count (length rooms))
         (room-index (random room-count)))
    (nth room-index rooms)))


(defun generate-rooms (number-of-rooms &optional rooms current-room)
  "returns a list of connected rooms"
  (if (<= number-of-rooms 0)
      rooms
      (if (not current-room)
          (let ((new-room (make-room :x-coordinate 0
                                           :y-coordinate 0)))
            (generate-rooms (- number-of-rooms 1)
                            (list new-room)
                            new-room))
          (let* ((directions (get-free-directions current-room))
                 (direction-count (length directions)))
            (if (equalp direction-count 0)
                (generate-rooms number-of-rooms rooms (pick-random-room rooms))
                (let* ((direction (nth (random direction-count) directions))
                       (coordinate (get-coordinate-for-next-room current-room direction))
                       (existing-room (find-room-at-coordinate coordinate rooms)))
                  (if existing-room
                      (progn
                        (connect-rooms current-room existing-room direction)
                        (generate-rooms number-of-rooms rooms existing-room))
                      (let ((new-room (make-room :x-coordinate (first coordinate)
                                                       :y-coordinate (second coordinate))))
                        (connect-rooms current-room new-room direction)
                        (generate-rooms (- number-of-rooms 1) (cons new-room rooms) new-room)))))))))


(defun print-room (room)
  "print a room to stdout"
  (format t "X:~a, Y:~a - N:~a E:~a S:~a W:~a~%"
          (room-x-coordinate room)
          (room-y-coordinate room)
          (not (not (room-north-room room)))
          (not (not (room-east-room room)))
          (not (not (room-south-room room)))
          (not (not (room-west-room room)))))

(defun print-rooms (rooms)
  "prints all rooms in a list"
  (if rooms
      (if (listp rooms)
          (progn
            (print-room (first rooms))
            (print-rooms (rest rooms)))
          (print-room rooms))))

(defun get-max-coordinate (rooms &optional max-x max-y)
  "calculates the max x and y coordinate for all rooms -> (list x y)"
  (get-coordinate-helper rooms #'> nil nil))

(defun get-min-coordinate (rooms &optional max-x max-y)
  "calculates the min x and y coordinate for all rooms -> (list x y)"
  (get-coordinate-helper rooms #'< nil nil))

(defun get-coordinate-helper (rooms compare-function x y)
  "calculate a cooridinate given a compare function -> (list x y)"
  (if (not rooms)
      (list x y)
      (let* ((room (first rooms))
             (room-x (room-x-coordinate room))
             (room-y (room-y-coordinate room))
             (new-x x)
             (new-y y))
        (if (not x)
            (setf new-x room-x)
            (if (funcall compare-function room-x new-x)
                (setf new-x room-x)))
        (if (not y)
            (setf new-y room-y)
            (if (funcall compare-function room-y new-y)
                (setf new-y room-y)))
        (get-coordinate-helper (rest rooms) compare-function new-x new-y))))



(defun normalize-rooms (rooms)
  "make sure the room coordinates are all positive"
  (let* ((min-coord (get-min-coordinate rooms))
         (min-x (first min-coord))
         (min-y (second min-coord)))
    (map 'list #'(lambda (room)
                   (let ((x (room-x-coordinate room))
                         (y (room-y-coordinate room)))
                     (setf (room-x-coordinate room) (- x min-x))
                     (setf (room-y-coordinate room) (- y min-y))
                     room))
         rooms)))




(defun room-get-center (room room-size-x room-size-y)
  "calculates the center of a room"
  (let ((center-x (floor (/ room-size-x 2)))
        (center-y (floor (/ room-size-y 2))))
    (list center-x center-y)))

(defun room-get-starting-position (room room-size-x room-size-y)
  "gets the grid position of where a rooms should start given it's coordinate position"
  (let ((start-x (+ 1 (* (room-x-coordinate room) (+ 1 room-size-x))))
        (start-y (+ 1 (* (room-y-coordinate room) (+ 1 room-size-y)))))
    (list start-x start-y)))
