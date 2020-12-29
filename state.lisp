(in-package :trel)

;; setup application state
(defparameter *state-ui* (make-hash-table))
(setf (gethash :layer *state-ui*) :boards)

(setf (gethash :selected-board-pos *state-ui*) 0)
(setf (gethash :selected-list-pos *state-ui*) 0)
(setf (gethash :selected-card-pos *state-ui*) 0)

(setf (gethash :focused-id-board *state-ui*) nil)
(setf (gethash :focused-id-list *state-ui*) nil)
(setf (gethash :focused-id-card *state-ui*) nil)

(setf (gethash :rend-board-width *state-ui*) 20)
(setf (gethash :rend-list-width *state-ui*) 20)
(setf (gethash :rend-card-width *state-ui*) 40)
(setf (gethash :rend-card-height *state-ui*) 7)
(setf (gethash :rend-card-gap *state-ui*) 0)

(defparameter *state-wg* (make-hash-table))
