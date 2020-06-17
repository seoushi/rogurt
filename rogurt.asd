;;;; rogurt.asd

(asdf:defsystem #:rogurt
  :description "Roguelike Game"
  :author "Sean Chapel <sean@seoushigames.com>"
  :license  "LGPL v3"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-image #:slynk)
  :components ((:file "package")
               (:file "rogurt")
               (:file "room")
               (:file "grid")
               (:file "slynk")))
