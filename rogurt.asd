;;;; rogurt.asd

(asdf:defsystem #:rogurt
  :description "Roguelike Game"
  :author "Sean Chapel <sean@seoushigames.com>"
  :license  "LGPL v3"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2)
  :components ((:file "package")
               (:file "rogurt")
               (:file "grid")))
