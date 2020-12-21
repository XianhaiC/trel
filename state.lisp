(in-package :trel)

;; setup application state
(defparameter *state-ui* (make-hash-table))
(setf (gethash :layer *state-ui*) :boards)
(setf (gethash :selected-board-pos *state-ui*) 0)
(setf (gethash :selected-list-pos *state-ui*) 0)
(setf (gethash :selected-id-board *state-ui*) nil)
(setf (gethash :focused-id-board *state-ui*) nil)
(setf (gethash :focused-id-list *state-ui*) nil)

(defparameter *state-collections* (make-hash-table))
(setf (gethash :loaded-boards *state-collections*) nil)
