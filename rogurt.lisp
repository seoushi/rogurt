(in-package #:rogurt)

(defstruct player
  (x-coordinate 0 :type integer)
  (y-coordinate 0 :type integer))

(defstruct world
  (grid nil :type array)
  (rooms nil :type list)
  (player nil :type player)
  (textures nil :type hash-table)
  (tile-width nil :type integer)
  (tile-height nil :type integer))


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
         (world (make-world :grid grid
                            :rooms rooms
                            :player (make-player :x-coordinate 0 :y-coordinate 0)
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
         (start-y (second room-start)))
    (setf (player-x-coordinate player) (+ start-x center-x))
    (setf (player-y-coordinate player) (+ start-y center-y))
    world))

(defun world-make-grid-items (world)
  "makes all grid items for the world"
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
  "moves the player by x and y on the grid"
  (let ((cur-x (player-x-coordinate player))
        (cur-y (player-y-coordinate player)))
    (setf (player-x-coordinate player) (+ cur-x x))
    (setf (player-y-coordinate player) (+ cur-y y))))




(defun render-world (world renderer x-offset y-offset)
  "renders the world to the screen with an offset in pixels"
  (let ((grid-items (world-make-grid-items world)))
    (render-grid world renderer x-offset y-offset grid-items)))


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
  (let ((player (world-player world)))
    (cond
          ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))
          ((sdl2:scancode= scancode :scancode-w) (player-move player 0 -1))
          ((sdl2:scancode= scancode :scancode-s) (player-move player 0 1))
          ((sdl2:scancode= scancode :scancode-a) (player-move player -1 0))
          ((sdl2:scancode= scancode :scancode-d) (player-move player 1 0)))))

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
