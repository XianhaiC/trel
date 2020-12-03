(in-package :trel)

;; setup application state
(defparameter *state-ui* (make-hash-table))
(setf (gethash :curs-pos *state-ui*) 0)
(setf (gethash :layer *state-ui*) :boards)
