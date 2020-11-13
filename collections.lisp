(in-package :trel)

;; we are using the collection's id's as string keys
;; hence the need for test to use equal
(defvar *trel-cards* (make-hash-table :test 'equal))
(defvar *trel-lists* (make-hash-table :test 'equal))
(defvar *trel-boards* (make-hash-table :test 'equal))

(defvar *board-to-lists* (make-hash-table :test 'equal))
(defvar *list-to-cards* (make-hash-table :test 'equal))

;; TODO can't iterate through hash table
(defun dump-hash (hash)
  (dolist (entry hash) (print entry)))

(dump-hash *trel-cards*)

(defclass collection ()
  ((id
     :initarg :id
     :initform (error "Must supply id.")
     :accessor id)
   (name
     :initarg :name
     :initform (error "Must supply name.")
     :accessor name)))

(defclass trel-card (collection)
  ((desc
     :initarg :desc
     :initform ""
     :accessor desc)))

(defclass trel-list (collection) ())

(defclass trel-board (collection) ())



(defgeneric delete-collection (collection)
  (:documentation "Delete a collection object, including its connections to other collections."))

(defmethod add-collection ((collection trel-card) &key)
  (setf (gethash (id collection) *trel-cards*) collection))

(defmethod add-collection ((collection trel-list) &key (cards '() cards-supplied-p))
  (with-accessors ((list-id id)) collection
    (setf (gethash (id collection) *trel-lists*) collection)
    (when cards-supplied-p
      (loop for card in cards do
            (with-accessors ((card-id id)) card
              (add-collection card)
              (push card-id (gethash list-id *list-to-cards*)))))))

;; TODO implement this just like how it was done for lists
(defmethod add-collection ((collection trel-board) &key)
  (setf (gethash (id collection) *trel-boards*) collection))



(defgeneric add-collection (collection &key)
  (:documentation "Delete a collection object, including its connections to other collections."))

;; TODO
(defmethod delete-collection ((collection trel-card))
  ())



(defparameter *cards* (list
                        (make-instance 'trel-card :id "1" :name "one")
                        (make-instance 'trel-card :id "2" :name "two")
                        (make-instance 'trel-card :id "3" :name "three")))

(defparameter *my-list* (make-instance 'trel-list :id "12" :name "my-list"))

(add-collection *my-list* :cards *cards*)
(id (gethash "12" *trel-lists*))
(gethash "12" *list-to-cards*)
(name (gethash "1" *trel-cards*))
(name (gethash "2" *trel-cards*))

(add-collection (make-instance 'trel-board :id "213" :name "hello") :cards *cards*)














(defparameter b '(1 2 3))
(nconc b '(4 5))
b
(defparameter *test-hash* (make-hash-table :test 'equal))
(setf (gethash "1" *test-hash*) '())
(push '(1 2 3) (gethash "1" *test-hash*))
(gethash "1" *test-hash*)
