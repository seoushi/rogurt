(in-package #:rogurt)

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

(defstruct player x-coordinate y-coordinate)

(defstruct world grid rooms player textures tile-width tile-height)





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




(defun room-get-center (room room-size-x room-size-y)
  (let ((center-x (floor (/ room-size-x 2)))
        (center-y (floor (/ room-size-y 2))))
    (list center-x center-y)))

(defun room-get-starting-position (room room-size-x room-size-y)
  (let ((start-x (+ 1 (* (world-room-x-coordinate room) (+ 1 room-size-x))))
        (start-y (+ 1 (* (world-room-y-coordinate room) (+ 1 room-size-y)))))
    (list start-x start-y)))



(defun generate-world (number-of-rooms room-width room-height)
  (let ((world (make-world :grid nil :rooms nil :player (make-player :x-coordinate 0 :y-coordinate 0)
                           :textures (make-hash-table)
                           :tile-width 32
                           :tile-height 32)))
    (generate-rooms world number-of-rooms)
    (normalize-rooms (world-rooms world))
    (setf (world-grid world) (build-room-grid (world-rooms world) room-width room-height nil))
    (let* ((first-room (first (world-rooms world)))
          (room-center (room-get-center first-room room-width room-height))
          (center-x (first room-center))
          (center-y (second room-center))
          (player (world-player world))
          (room-start (room-get-starting-position first-room room-width room-height))
          (start-x (first room-start))
          (start-y (second room-start)))
      (setf (player-x-coordinate player) (+ start-x center-x))
      (setf (player-y-coordinate player) (+ start-y center-y)))
    world))

(defun world-make-grid-items (world)
  (let* ((grid (world-grid world))
         (grid-width (array-dimension grid 0))
         (grid-height (array-dimension grid 1))
         (grid-items (make-array (list grid-width grid-height)))
         (player (world-player world))
         (player-x (player-x-coordinate player))
         (player-y (player-y-coordinate player)))
    (fill-grid grid-items nil)
    (setf (aref grid-items player-x player-y) (make-grid-item :texture-id :player))
    grid-items))

(defun player-move (player x y)
  (let ((cur-x (player-x-coordinate player))
        (cur-y (player-y-coordinate player)))
    (setf (player-x-coordinate player) (+ cur-x x))
    (setf (player-y-coordinate player) (+ cur-y y))))




(defun render-world (&key world renderer)
  (let ((grid-items (world-make-grid-items world)))
    (render-grid :renderer renderer :world world :start-x 0 :start-y 0 :width 40 :height 40 :grid-items grid-items)))


(defun update-game (&key world renderer)
  ;; reset color to white
  (sdl2:set-render-draw-color renderer 255 255 255 255)

  (render-world :world world :renderer renderer)

  ;; set our clear color
  (sdl2:set-render-draw-color renderer 30 10 30 255)
      ;;(case (read-char)
      ;;  (#\w (progn
      ;;         (player-move player 0 -1)
      ;;         (setf world-changed T)))
      ;;  (#\s (progn
      ;;         (player-move player 0 1)
      ;;         (setf world-changed T)))
      ;;  (#\a (progn
      ;;         (player-move player -1 0)
      ;;         (setf world-changed T)))
      ;;  (#\d (progn
      ;;         (player-move player 1 0)
      ;;         (setf world-changed T)))
      ;; (#\e (setf is-game-running nil)))))
      )

(defun init-game (renderer)
  (let* ((world (generate-world 4 5 3))
         (textures (world-textures world))
         (load-texture (lambda (id filename)
                         (let ((surface (sdl2-image:load-image filename)))
                           (setf (gethash id textures) (sdl2:create-texture-from-surface renderer surface))
                           (sdl2:free-surface surface)))))
    (funcall load-texture :player "data/textures/player.png")
    (funcall load-texture :wall "data/textures/wall.png")
    (funcall load-texture :vertical-door "data/textures/vertical-door.png")
    (funcall load-texture :horizontal-door "data/textures/horizontal-door.png")
    (funcall load-texture :floor "data/textures/floor.png")
    world))

(defun shutdown-game (&key world renderer)
  (maphash (lambda (texture-key texture-value)
             (sdl2:destroy-texture texture-value))
           (world-textures world)))


(defun run-game ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "rogurt" :flags '(:shown) :w 480 :h 320)
      (sdl2:show-window win)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2-image:init '(:png))
        (let ((world (init-game renderer)))
          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))
            (:idle ()
                   (sdl2:show-window win)
                   (sdl2:render-clear renderer)
                   (update-game :world world :renderer renderer)
                   (sdl2:render-present renderer)
                   (sdl2:delay 16)
                   (update-slynk)
                   )
            (:quit ()
                   (shutdown-game :world world :renderer renderer)
                   (sdl2-image:quit)
                   t)))))))
