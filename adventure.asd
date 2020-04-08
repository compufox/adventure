;;;; adventure.asd

(asdf:defsystem #:adventure
  :description "Describe adventure here"
  :author "ava fox"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-ttf #:str)
  :components ((:file "package")
	       (:file "sdl")
               (:file "adventure")))
