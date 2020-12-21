(defpackage :trel-asd
  (:use :cl :asdf))

(in-package :trel-asd)

(defsystem trel
  :name "trel"
  :depends-on (:croatoan
               :croatoan-ncurses
               :dexador
               :cl-json
               :alexandria)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "api")
               (:file "collections")
               (:file "render")
               (:file "state")
               (:file "control")
               (:file "trel")))
