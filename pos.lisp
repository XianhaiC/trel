(in-package :trel)

(defun make-pos (x y)
  (cons x y))

(defun get-pos-x (pos) (car pos))
(defun get-pos-y (pos) (cdr pos))
