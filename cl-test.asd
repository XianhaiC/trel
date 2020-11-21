(defpackage :cl-test-asd
  (:use :cl :asdf))

(in-package :cl-test-asd)

(defsystem cl-test
  :name "cl-test"
  :depends-on (:cl-charms :croatoan)
  :serial t
  :components ((:file "package-test")
               (:file "cl-test")))
