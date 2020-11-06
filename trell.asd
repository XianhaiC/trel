(defpackage :trell-asd
  (:use :cl :asdf))

(in-package :trell-asd)

(defsystem trell
  :name "trell"
  :depends-on (:cl-charms)
  :serial t
  :components ((:file "package")
               (:file "greed")))
