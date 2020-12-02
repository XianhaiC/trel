(defpackage :trel-test-asd
  (:use :cl :asdf))

(in-package :trel-test-asd)

(defsystem trel-test
  :name "trel-test"
  :depends-on (:croatoan :dexador :cl-json)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "api")
               (:file "collections")
               (:file "render")
               (:file "trel-test")))
