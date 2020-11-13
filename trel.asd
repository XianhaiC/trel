(defpackage :trel-asd
  (:use :cl :asdf))

(in-package :trel-asd)

(defsystem trel
  :name "trel"
  :depends-on (:cl-charms)
  :serial t
  :components ((:file "package")
               (:file "greed")))
