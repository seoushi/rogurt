(in-package #:rogurt)


(defstruct grid-item display-character)


(defun fill-grid (grid character)
  "Fills the grid in every spot with the given character"
  (dotimes (y (array-dimension grid 1))
    (dotimes (x (array-dimension grid 0))
      (setf (aref grid x y) character))))


(defun print-grid(grid start-x start-y width height grid-items)
  (let ((grid-height (array-dimension grid 1))
        (grid-width (array-dimension grid 0)))
    (loop for y from start-y below (min (+ start-y height) grid-height) do
      (progn
        (loop for x from start-x below (min (+ start-x width) grid-width) do
          (let ((grid-item (aref grid-items x y)))
            (if grid-item
                (format t "~a " (grid-item-display-character grid-item))
                (format t "~a " (aref grid x y)))))
        (format t "~%")))))


(defun make-grid-room (grid character start-x start-y width height)
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref grid (+ x start-x)
                    (+ y start-y))
              character))))

(defun get-grid-size (rooms)
  (let ((min-coord (get-min-coordinate rooms))
        (max-coord (get-max-coordinate rooms)))
    (list (+ 2 (- (first max-coord) (first min-coord)))
          (+ 2 (- (second max-coord) (second min-coord))))))


(defun build-room-grid (rooms room-width room-height print-room-number)
  (let* ((grid-size (get-grid-size rooms))
         (x-max (* (+ room-width 1) (first grid-size)))
         (y-max (* (+ room-height 1) (second grid-size)))
         (grid (make-array (list x-max y-max)))
         (room-index 0))
    (progn
      (dotimes (y (array-dimension grid 1))
        (dotimes (x (array-dimension grid 0))
          (setf (aref grid x y) :#)))
      (map nil #'(lambda (room)
                   (let* ((room-center (room-get-center room room-width room-height))
                          (center-x (first room-center))
                          (center-y (second room-center))
                          (room-start (room-get-starting-position room room-width room-height))
                          (start-x (first room-start))
                          (start-y (second room-start)))
                   (make-grid-room grid
                                   :.
                                   start-x
                                   start-y
                                   room-width
                                   room-height)
                     (if (world-room-north-room room)
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y room-height)) :-))
                     (if (world-room-south-room room)
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y -1)) :-))
                     (if (world-room-east-room room)
                         (setf (aref grid
                                     (+ start-x room-width)
                                     (+ start-y center-y)) #\|))
                     (if (world-room-west-room room)
                         (setf (aref grid
                                     (+ start-x -1)
                                     (+ start-y center-y)) #\|))
                     (if print-room-number
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y center-y)) room-index))
                   (setf room-index (+ 1 room-index))))
           rooms)
      grid)))
