(in-package #:rogurt)


(defstruct grid-item texture-id)


(defun fill-grid (grid character)
  "Fills the grid in every spot with the given character"
  (dotimes (y (array-dimension grid 1))
    (dotimes (x (array-dimension grid 0))
      (setf (aref grid x y) character))))

(defun render-grid(world renderer x-offset y-offset grid-items)
  (let* ((grid (world-grid world))
         (grid-height (array-dimension grid 1))
         (grid-width (array-dimension grid 0))
         (textures (world-textures world))
         (tile-width (world-tile-width world))
         (tile-height (world-tile-height world))
         (dest-rect (sdl2:make-rect 0 0 tile-width tile-height))
         (render-texture (lambda (texture-id)
                           (let ((texture (gethash texture-id textures)))
                             (if texture
                                 (sdl2:render-copy renderer texture :dest-rect dest-rect)
                                 (format t "Texture id not found: '~a'~%" texture-id))))))
    (loop for y from 0 below grid-height do
      (setf (sdl2:rect-x dest-rect) x-offset)
      (setf (sdl2:rect-y dest-rect) (+ y-offset (* tile-height y)))
      (loop for x from 0 below grid-width do
        (let* ((grid-item (aref grid-items x y))
               (grid-texture-id (aref grid x y)))
          (when grid-texture-id
              (funcall render-texture grid-texture-id))
          (when grid-item
              (funcall render-texture (grid-item-texture-id grid-item)))
          (setf (sdl2:rect-x dest-rect) (+ tile-width (sdl2:rect-x dest-rect))))))))


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
          (setf (aref grid x y) :wall)))
      (map nil #'(lambda (room)
                   (let* ((room-center (room-get-center room room-width room-height))
                          (center-x (first room-center))
                          (center-y (second room-center))
                          (room-start (room-get-starting-position room room-width room-height))
                          (start-x (first room-start))
                          (start-y (second room-start)))
                   (make-grid-room grid
                                   :floor
                                   start-x
                                   start-y
                                   room-width
                                   room-height)
                     (if (world-room-north-room room)
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y room-height)) :horizontal-door))
                     (if (world-room-south-room room)
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y -1)) :horizontal-door))
                     (if (world-room-east-room room)
                         (setf (aref grid
                                     (+ start-x room-width)
                                     (+ start-y center-y)) :vertical-door))
                     (if (world-room-west-room room)
                         (setf (aref grid
                                     (+ start-x -1)
                                     (+ start-y center-y)) :vertical-door))
                     (if print-room-number
                         (setf (aref grid
                                     (+ start-x center-x)
                                     (+ start-y center-y)) room-index))
                   (setf room-index (+ 1 room-index))))
           rooms)
      grid)))
