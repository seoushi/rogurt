;; X X X X X
;; X X X X X
;; X X X X X
;; X X X X X
;; X X X X X

(defvar *world* (make-array '(5 5)))

(print (type-of *world*))
(print (array-dimension *world* 0))

(defun fill-world (world)
  (dotimes (y (array-dimension world 0))
    (dotimes (x (array-dimension world 1))
      (setf (aref world x y) :#))))

(fill-world *world*)

(defun print-world (world)
  (dotimes (y (array-dimension world 0))
    (dotimes (x (array-dimension world 1))
      (format t "~a " (aref world x y)))
    (format t "~%")))

(defun make-room (world start-x start-y width height)
  (dotimes (x width)
    (dotimes (y height)
      (setf (aref world (+ x start-x)
                  (+ y start-y))
            :.))))

(make-room *world* 1 1 2 2)

(print-world *world*)
