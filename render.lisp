(in-package :trel)

;; map collection id's to their render objects
(defparameter *rend-cards* (make-hash-table :test 'equal))

;; generic class to hold rendering information for a collection
(defclass rend ()
  ((pos
    :initarg :pos
    :initform (error "Must supply pos.")
    :accessor pos)
   (dim
    :initarg :dim
    :initform (error "Must supply dim.")
    :accessor dim)
   (win
    :initarg :win
    :initform nil
    :accessor win)))

(defmethod initialize-instance :after ((obj rend) &key)
  "Initialize the window for the rend."
  (with-accessors ((pos pos)
                   (dim dim)
                   (win win)) obj
    (when (null win)
      (setf win (make-instance 'croatoan:window
                               :position (list (get-pos-y pos)
                                               (get-pos-x pos))
                               :height (get-pos-y dim)
                               :width (get-pos-x dim))))))

;; TODO
;; Render the current board's lists
;; add the ability to select a new list, fetching the data from the api
(defclass rend-card (rend)
  ())

;;; board render functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rend-win-boards (win)
  "Render the boards window."
  (progn
    (croatoan:clear win)

    ;; sort the boards into alphabetical order
    (let ((boards (sort (alexandria:hash-table-values *trel-boards*)
                        #'string< :key #'(lambda (board) (name board)))))
      ;; iterate through each board
      (loop
        for i from 0
        for board in boards do
          (croatoan:move win i 0)

          ;; highlight the line if it's currently focused
          (when (= (gethash :selected-board-pos *state-ui*) i)
            (setf (croatoan:attributes win) '(:reverse)))

          (with-accessors ((name name)) board
            (croatoan:add-string win name))

          (when (= (gethash :selected-board-pos *state-ui*) i)
            (setf (croatoan:attributes win) '()))))

    (croatoan:refresh win)))


(defun rend-win-lists (win)
  "Render the lists window."
  (progn
    (croatoan:clear win)

    ;; sort the lists into alphabetical order
    (let ((lists (sort (mapcar #'(lambda (id-list) (gethash id-list *trel-lists*))
                               (alexandria:hash-table-keys
                                (gethash (gethash :focused-id-board *state-ui*)
                                         *board-to-lists*)))
                       #'string< :key #'(lambda (list) (name list)))))
      ;; iterate through each list
      (loop
        for i from 0
        for list in lists do
          (croatoan:move win i 0)

          ;; highlight the line if it's currently focused
          (when (= (gethash :selected-list-pos *state-ui*) i)
            (setf (croatoan:attributes win) '(:reverse)))

          (with-accessors ((name name)) list
            (croatoan:add-string win name))

          (when (= (gethash :selected-list-pos *state-ui*) i)
            (setf (croatoan:attributes win) '()))))

    (croatoan:refresh win)))

;; TODO finish renders
(defun rend-list (win pos) ())

(defun rend-card (win pos-x pos-y)
  ())

;; (defparameter *state-boards* (make-hash-table :test 'equal))
;; (setf (gethash "123" *state-boards*) 1)
;; (setf (gethash "456" *state-boards*) 2)
;; (hash-table-count *state-boards*)

;; (defparameter *state-ui* (make-hash-table))


;; (croatoan:submit (rend-board-screen *scr* *state-boards* *state-ui*))

;; (croatoan:bind *scr* #\j
;;                (lambda (win event)
;;                  (setf (gethash :selected-board-pos *state-ui*)
;;                        (mod (1+ (gethash :selected-board-pos *state-ui*))
;;                             (hash-table-count *state-boards*)))))


;; (croatoan:bind *scr* #\k
;;                (lambda (win event)
;;                  (setf (gethash :selected-board-pos *state-ui*)
;;                        (mod (1- (gethash :selected-board-pos *state-ui*))
;;                             (hash-table-count *state-boards*)))))
