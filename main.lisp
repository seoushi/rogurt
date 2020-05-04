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




(defun fill-world (world)
  (let ((grid (world-grid world)))
    (dotimes (y (array-dimension grid 1))
    (dotimes (x (array-dimension grid 0))
      (setf (aref grid y y) :#)))))


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
             (other-rooms (rest rooms))
             (x (world-room-x-coordinate room))
             (y (world-room-y-coordinate room))
             (x2 (first coordinate))
             (y2 (second coordinate)))
        (if (and (equalp x x2)
                 (equalp y y2))
              room
              (get-room-at-coordinate coordinate other-rooms)))))



(defun opposite-direction (direction)
  (cond
    ((= direction 0) 2) ;; north -> south
    ((= direction 1) 3) ;; east ->west
    ((= direction 2) 0)
    ((= direction 3) 1)))

(defun add-room (world new-room)
  (setf (world-rooms world) (cons new-room (world-rooms world))))

(defun connect-rooms (first-room second-room direction) ;; direction is from first->second
  (set-room-for-direction first-room direction second-room)
  (set-room-for-direction second-room (opposite-direction direction) first-room))


(defun get-free-directions (room)
  (let ((north-free (not (world-room-north-room room)))
        (east-free (not (world-room-east-room room)))
        (south-free (not (world-room-south-room room)))
        (west-free (not (world-room-west-room room)))
        (results '()))

    (progn
      (if north-free
          (setf results (append results (list 0))))
      (if east-free
          (setf results (append results (list 1))))
      (if south-free
          (setf results (append results (list 2))))
      (if west-free
          (setf results (append results (list 3))))
      results)))

(defun pick-random-room (world)
  (let* ((rooms (world-rooms world))
         (room-count (length rooms))
         (room-index (random room-count)))
    (nth room-index rooms)))


(defun generate-rooms (world number-of-rooms &optional current-room)
  (if (> number-of-rooms 0)
    (if (not current-room)
        (let ((new-room (make-world-room :x-coordinate 0
                                        :y-coordinate 0
                                        :north-room nil
                                        :east-room nil
                                        :south-room nil
                                        :west-room nil)))
          (progn
            (setf (world-rooms world) (list new-room))
            (generate-rooms world (- number-of-rooms 1) new-room)))
        (let* ((directions (get-free-directions current-room))
              (direction-count (length directions)))
          (if (equalp direction-count 0)
              (generate-rooms world number-of-rooms (pick-random-room world))
              (let* ((direction (nth (random direction-count) directions))
                    (coordinate (get-coordinate-for-next-room current-room direction))
                    (new-room (get-room-at-coordinate coordinate (world-rooms world))))
                (if new-room
                  (progn
                    (connect-rooms current-room new-room direction)
                    (generate-rooms world number-of-rooms new-room))
                  (let ((new-room (make-world-room :x-coordinate (first coordinate)
                                                   :y-coordinate (second coordinate)
                                                   :north-room nil
                                                   :east-room nil
                                                   :south-room nil
                                                   :west-room nil)))
                    (progn
                      (add-room world new-room)
                      (connect-rooms current-room new-room direction)
                      (generate-rooms world (- number-of-rooms 1) new-room))))))))))


(defun print-grid(grid)
  (dotimes (y (array-dimension grid 1))
    (dotimes (x (array-dimension grid 0))
      (format t "~a " (aref grid x y)))
    (format t "~%")))

(defun make-grid-room (grid character start-x start-y width height)
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref grid (+ x start-x)
                    (+ y start-y))
              character))))


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

(defun get-max-coordinate (rooms &optional max-x max-y)
  (get-coordinate-helper rooms #'> nil nil))

(defun get-min-coordinate (rooms &optional max-x max-y)
  (get-coordinate-helper rooms #'< nil nil))

(defun get-coordinate-helper (rooms compare-function x y)
  (if (not rooms)
      (list x y)
      (let* ((room (first rooms))
             (room-x (world-room-x-coordinate room))
             (room-y (world-room-y-coordinate room))
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


(defun get-grid-size (rooms)
  (let ((min-coord (get-min-coordinate rooms))
        (max-coord (get-max-coordinate rooms)))
    (list (+ 1 (- (first max-coord) (first min-coord)))
          (+ 1 (- (second max-coord) (second min-coord))))))

(defun normalize-rooms (rooms)
  (let* ((min-coord (get-min-coordinate rooms))
         (min-x (first min-coord))
         (min-y (second min-coord)))
    (map nil #'(lambda (room)
                       (let ((x (world-room-x-coordinate room))
                             (y (world-room-y-coordinate room)))
                         (progn
                           (setf (world-room-x-coordinate room) (- x min-x))
                           (setf (world-room-y-coordinate room) (- y min-y)))))
         rooms)))


(defun build-room-grid (rooms x-size y-size)
  (let* ((grid-size (get-grid-size rooms))
         (room-size-x (+ 1 x-size))
         (room-size-y (+ 1 y-size))
         (x-max (* room-size-x (first grid-size)))
         (y-max (* room-size-y (second grid-size)))
         (grid (make-array (list x-max y-max)))
         (room-index 0))
    (progn
      (dotimes (y (array-dimension grid 1))
        (dotimes (x (array-dimension grid 0))
          (setf (aref grid x y) :#)))
      (map nil #'(lambda (room)
                   (make-grid-room grid
                                   :.
                                   (* (world-room-x-coordinate room) room-size-x)
                                   (* (world-room-y-coordinate room) room-size-y)
                                   x-size
                                   y-size)
                   (setf room-index (+ 1 room-index)))
           rooms)
      grid)))



(defparameter *world* (make-world :grid (make-array '(5 5))
                                  :rooms '()))

(fill-world *world*)
(generate-rooms *world* 10)
(normalize-rooms (world-rooms *world*))
(print-rooms (world-rooms *world*))
(print-grid (build-room-grid (world-rooms *world*) 4 3))

;; TODO: connect all the rooms
;; TODO: draw only a portion of the rooms
