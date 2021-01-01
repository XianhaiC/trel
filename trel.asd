;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :trel-asd
  (:use :cl :asdf))

(in-package :trel-asd)

(defsystem trel
  :name "trel"
  :depends-on (:croatoan
               :croatoan-ncurses
               :dexador
               :cl-json
               :alexandria
               :uiop
               :vom)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "util")
               (:file "api")
               (:file "collections")
               (:file "render")
               (:file "state")
               (:file "control")
               (:file "trel")))
