(in-package #:rogurt)

(defstruct player
  (x-coordinate 0 :type integer)
  (y-coordinate 0 :type integer))

(defstruct world
  (grid nil :type array)
  (grid-items nil :type array)
  (rooms nil :type list)
  (player nil :type player)
  (textures nil :type hash-table)
  (tile-width nil :type integer)
  (tile-height nil :type integer))

(defstruct grid-info
  (grid-type nil :type symbol)
  (grid-items nil :type list))


(defun opposite-direction (direction)
  "gets the opposite direction of the given direction"
  (cond
    ((eq direction :north) :south)
    ((eq direction :east)  :west)
    ((eq direction :south) :north)
    ((eq direction :west)  :east)))


(defun generate-world (number-of-rooms room-width room-height)
  "generates a world"
  (let* ((rooms (normalize-rooms (generate-rooms number-of-rooms)))
         (grid (build-room-grid rooms room-width room-height nil))
         (player (make-player :x-coordinate 0 :y-coordinate 0))
         (grid-items (world-make-grid-items grid rooms room-width room-height player))
         (world (make-world :grid grid
                            :grid-items grid-items
                            :rooms rooms
                            :player player
                            :textures (make-hash-table)
                            :tile-width 32
                            :tile-height 32))
         (first-room (first (world-rooms world)))
         (room-center (room-get-center first-room room-width room-height))
         (center-x (first room-center))
         (center-y (second room-center))
         (player (world-player world))
         (room-start (room-get-starting-position first-room room-width room-height))
         (start-x (first room-start))
         (start-y (second room-start))
         (player-x (+ start-x center-x))
         (player-y (+ start-y center-y)))
    (move-grid-item grid-items :player 0 0 player-x player-y)
    (setf (player-x-coordinate player) player-x)
    (setf (player-y-coordinate player) player-y)
    world))

(defun world-make-grid-items (grid rooms room-width room-height player)
  "makes all grid items for the world"
  (let*  ((grid-width (array-dimension grid 0))
          (grid-height (array-dimension grid 1))
          (grid-items (make-array (list grid-width grid-height)))
          (player-x (player-x-coordinate player))
          (player-y (player-y-coordinate player)))
    (fill-grid grid-items '())
    (setf (aref grid-items player-x player-y)
          (cons (make-grid-item :texture-id :player :obstructs-player nil) (aref grid-items player-x player-y)))
      (map nil #'(lambda (room)
                   (let* ((room-center (room-get-center room room-width room-height))
                          (center-x (first room-center))
                          (center-y (second room-center))
                          (room-start (room-get-starting-position room room-width room-height))
                          (start-x (first room-start))
                          (start-y (second room-start))
                          (add-door-to-grid-items #'(lambda (x y door-type)
                                                      (setf (aref grid-items x y)
                                                            (cons (make-grid-item :texture-id door-type :obstructs-player t)
                                                                  (aref grid-items x y))))))
                     (if (room-north-room room)
                         (funcall add-door-to-grid-items
                                  (+ start-x center-x)
                                  (+ start-y room-height)
                                  :horizontal-door))
                     (if (room-east-room room)
                         (funcall add-door-to-grid-items
                                  (+ start-x room-width)
                                  (+ start-y center-y)
                                  :vertical-door))))
           rooms)
    grid-items))

(defun world-coordinate-valid (world x y)
  "returns true when the coordinate is within bounds of the world"
  (let* ((grid (world-grid world))
         (grid-width (array-dimension grid 0))
         (grid-height (array-dimension grid 1)))
    (and (>= x 0)
         (>= y 0)
         (< x grid-width)
         (< y grid-height))))

(defun world-get-grid-info (world x y)
  "gets information about a grid coordinate"
  (let* ((grid (world-grid world))
         (grid-type (aref grid x y))
         (grid-items (aref (world-grid-items world) x y)))
    (make-grid-info :grid-type grid-type
                    :grid-items grid-items)))

(defun move-grid-item (grid-items item-type x y new-x new-y)
  "remove the item at x, y and replace the item in new-x, new-y with the item in x, y"
  (let ((items (aref grid-items x y)))
    (loop for item in items
          do (when (eq (grid-item-texture-id item) item-type)
               (setf (aref grid-items x y)
                     (remove item (aref grid-items x y)))
               (setf (aref grid-items new-x new-y)
                     (cons item (aref grid-items new-x new-y)))))))

(defun grid-info-obstructs-player (grid-info)
  "returns true when the grid-info has an item or type that doesn't allow the player to move through it"
  (not (eq :floor (grid-info-grid-type grid-info))))

(defun grid-items-obstruct-player (grid-items)
  (if (eq grid-items '())
      '()
      (if (grid-item-obstructs-player (first grid-items))
          t
          (grid-items-obstruct-player (rest grid-items)))))

(defun player-move (world x y)
  "moves the player by x and y on the grid"
  (let* ((player (world-player world))
         (cur-x (player-x-coordinate player))
         (cur-y (player-y-coordinate player))
         (new-x (+ cur-x x))
         (new-y (+ cur-y y)))
    (when (world-coordinate-valid world new-x new-y) ;; Checks world bounds
      (let ((grid-info (world-get-grid-info world new-x new-y))
            (grid-items (world-grid-items world)))
        (unless (or (grid-info-obstructs-player grid-info)
                    (grid-items-obstruct-player (aref grid-items new-x new-y)))
          (move-grid-item (world-grid-items world)
                          :player
                          cur-x cur-y new-x new-y)
          (setf (player-x-coordinate player) new-x)
          (setf (player-y-coordinate player) new-y))))))

(defun open-door (world)
  "try to open a door if the player is in range of a door (max of one space away)"
  (let* ((player (world-player world))
         (start-x (- (player-x-coordinate player) 1))
         (start-y (- (player-y-coordinate player) 1))
         (do-open-door #'(lambda (x y)
                           (let ((items (aref (world-grid-items world) x y)))
                             (loop for item in items do
                               (let ((item-type (grid-item-texture-id item)))
                                 (when (or (eq item-type :horizontal-door)
                                           (eq item-type :vertical-door))
                                   (setf (aref (world-grid-items world) x y)
                                         (remove item items)))))))))
    (dotimes (y 3)
      (dotimes (x 3)
        (let ((cur-x (+ x start-x))
              (cur-y (+ y start-y)))
          (when (world-coordinate-valid world cur-x cur-y)
            (funcall do-open-door cur-x cur-y)))))))


(defun render-world (world renderer x-offset y-offset)
  "renders the world to the screen with an offset in pixels"
    (render-grid world renderer x-offset y-offset (world-grid-items world)))


(defun update-game (world renderer screen-width screen-height)
  ;; reset color to white
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (let* ((player (world-player world))
         (screen-center-x (/ screen-width 2))
         (screen-center-y (/ screen-height 2))
         (x-offset (+ screen-center-x (* -1 (player-x-coordinate player) (world-tile-width world))))
         (y-offset (+ screen-center-y (* -1 (player-y-coordinate player) (world-tile-height world)))))
    (render-world world renderer x-offset y-offset))

  ;; set our clear color
  (sdl2:set-render-draw-color renderer 30 10 30 255))

(defun init-game (renderer)
  "initializes the game state"
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
  "cleans up the game state"
  (maphash (lambda (texture-key texture-value)
             (sdl2:destroy-texture texture-value))
           (world-textures world)))

(defun key-pressed (world scancode)
  "handles key input"
  (cond
    ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))
    ((sdl2:scancode= scancode :scancode-w) (player-move world 0 -1))
    ((sdl2:scancode= scancode :scancode-s) (player-move world 0 1))
    ((sdl2:scancode= scancode :scancode-a) (player-move world -1 0))
    ((sdl2:scancode= scancode :scancode-d) (player-move world 1 0))
    ((sdl2:scancode= scancode :scancode-space) (open-door world))))

(defun run-game ()
  "main entry point to run/play the game"
  (sdl2:with-init (:video :timer)
    (let ((screen-width 480)
          (screen-height 320))
      (sdl2:with-window (win :title "rogurt" :flags '(:shown) :w screen-width :h screen-height)
        (sdl2:show-window win)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (sdl2-image:init '(:png))
          (let ((world (init-game renderer)))
            (sdl2:with-event-loop (:method :poll)
              (:keyup (:keysym keysym)
                      (key-pressed world (sdl2:scancode-value keysym)))
              (:idle ()
                     (sdl2:show-window win)
                     (sdl2:render-clear renderer)
                     (update-game world renderer screen-width screen-height)
                     (sdl2:render-present renderer)
                     (sdl2:delay 16)
                     (update-slynk)
                     )
              (:quit ()
                     (shutdown-game :world world :renderer renderer)
                     (sdl2-image:quit)
                     t))))))))
