(in-package :trel)

;; we are using the collection's id's as string keys
;; hence the need for test to use equal
(defvar *trel-cards* (make-hash-table :test 'equal))
(defvar *trel-lists* (make-hash-table :test 'equal))
(defvar *trel-boards* (make-hash-table :test 'equal))

(defvar *board-to-lists* (make-hash-table :test 'equal))
(defvar *board-to-cards* (make-hash-table :test 'equal))
(defvar *list-to-cards* (make-hash-table :test 'equal))

;; TODO can't iterate through hash table
(defun dump-hash (hash)
  (with-hash-table-iterator (entry hash)
    (loop
      (multiple-value-bind (more-p key val) (entry)
        (unless more-p (return))
        (print (format nil "~a: ~a" key val))))))

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
  ((id-board
     :initarg :id-board
     :initform (error "Must supply board id.")
     :accessor id-board)
   (id-list
     :initarg :id-list
     :initform (error "Must supply list id.")
     :accessor id-list)
   (desc
     :initarg :desc
     :initform ""
     :accessor desc)))

(defclass trel-list (collection)
  ((id-board
     :initarg :id-board
     :initform (error "Must supply board id.")
     :accessor id-board)))

(defclass trel-board (collection) ())


(defmacro ensure-hash-table (collection-id collection-hash)
  `(when (null (gethash ,collection-id ,collection-hash))
     (setf (gethash ,collection-id ,collection-hash)
           (make-hash-table :test 'equal))))

(defmacro set-under-collection (child parent hash-table)
  `(setf (gethash ,child (gethash ,parent ,hash-table)) t))

(macroexpand-1 '(ensure-hash-table id-list *list-to-cards*))
(macroexpand-1 '(set-under-collection id-card id-list *list-to-cards*))


;; what we want to do is to have the owner of a collection object when
;; seting that object to the global state
;; this means that cards should be passed in with its parent list and board
;; while lists should be passed in with its parent board
;; 
;; we should also have the relation mapping lists be hash tables instead
;; this way we can easily replace duplicate values without performing an O(n)
;; scan

(defgeneric set-collection (collection)
  (:documentation "Delete a collection object, including its connections to other collections."))

(defmethod set-collection ((collection trel-card))
  (with-accessors ((id-card id)
                   (id-board id-board)
                   (id-list id-list)) collection
    ;; set to the card table
    (setf (gethash id-card *trel-cards*) collection)

    ;; create a hash table for the list/board to cards mapping if it doesn't exist already
    (ensure-hash-table id-list *list-to-cards*)
    (ensure-hash-table id-board *board-to-cards*)

    ;; set the card under the list/board
    (set-under-collection id-card id-list *list-to-cards*)
    (set-under-collection id-card id-board *board-to-cards*)))

(defmethod set-collection ((collection trel-list))
  (with-accessors ((id-list id)
                   (id-board id-board)) collection
    ;; set to the list table
    (setf (gethash id-list *trel-lists*) collection)

    ;; create a hash table for the board to lists mapping if it doesn't exist already
    (ensure-hash-table id-board *board-to-lists*)

    ;; set the list under the board
    (set-under-collection id-list id-board *board-to-lists*)))


; (defmethod set-collection ((collection trel-list) &key (cards '() cards-supplied-p))
; (with-accessors ((id-list id)) collection
; (setf (gethash (id collection) *trel-lists*) collection)
; (when cards-supplied-p
; (loop for card in cards do
; (with-accessors ((id-card id)) card
; (set-collection card)
; (push id-card (gethash id-list *list-to-cards*)))))))

(defmethod set-collection ((collection trel-board))
  (setf (gethash (id collection) *trel-boards*) collection))

(defgeneric delete-collection (collection)
  (:documentation "Delete a collection object, including its connections to other collections."))

(defmethod delete-collection ((collection trel-card))
  (with-accessors ((id-card id)
                   (id-list id-list)
                   (id-board id-board)) collection

    ;; delete connection to board/list
    (remhash id-card (gethash id-list *list-to-cards*))
    (remhash id-card (gethash id-board *board-to-cards*))

    ;; delete from card table
    (remhash id-card *trel-cards*)))

(defmethod delete-collection ((collection trel-list))
  (with-accessors ((id-list id)
                   (id-board id-board)) collection

    ;; delete connection to board
    (remhash id-list (gethash id-board *board-to-lists*))

    ;; delete from list table
    (remhash id-list *trel-lists*)))

(defmethod delete-collection ((collection trel-board))
  (with-accessors ((id-board id)) collection
    ;; delete from list table
    (remhash (id collection) *trel-lists*)))

;;TODO add helper methods that turn api trello objects into collection objects
;; (defun card-to-collection ())
;; (defun cards-to-collections ())
;; (defun list-to-collection ())
;; (defun lists-to-collections ())










(defparameter *cards* (list
                        (make-instance 'trel-card :id "1" :name "one")
                        (make-instance 'trel-card :id "2" :name "two")
                        (make-instance 'trel-card :id "3" :name "three")))

(defparameter *my-list* (make-instance 'trel-list :id "12" :name "my-list"))

(set-collection *my-list* :cards *cards*)
(id (gethash "12" *trel-lists*))
(gethash "12" *list-to-cards*)
(name (gethash "1" *trel-cards*))
(name (gethash "2" *trel-cards*))

(defparameter my-list (make-instance 'trel-list :id "list 1" :id-board "board 1" :name "yo"))
(set-collection (make-instance 'trel-list :id "list 1" :id-board "board 1" :name "yo"))
(dump-hash (gethash "board 1" *board-to-lists*))
(name (gethash "list 1" *trel-lists*))
(delete-collection my-list)


(defparameter my-card (make-instance 'trel-card :id "345" :id-list "list 1" :id-board "board 1" :name "whatyyt" :desc "something"))
(set-collection my-card)
(name (gethash "345" *trel-cards*))
(gethash "345" (gethash "list 1" *list-to-cards*))

(delete-collection my-card)
(dump-hash *trel-cards*)
(dump-hash (gethash "list 1" *list-to-cards*))












(defparameter b '(1 2 3))
(nconc b '(4 5))
b
(defparameter *test-hash* (make-hash-table :test 'equal))
(setf (gethash "hello" *test-hash*) 1)
(setf (gethash "world" *test-hash*) 2)

(let ((my-hash *test-hash*))
  (setf (gethash "hello" my-hash) 23))
(gethash "hello" *test-hash*)

(with-hash-table-iterator (entry *test-hash*)
  (loop
    (multiple-value-bind (more-p key val) (entry)
      (unless more-p (return))
      (print (format nil "key ~a" key))
      (print (format nil "val ~a" val)))))

(setf (gethash "1" *test-hash*) '())
(push '(1 2 3) (gethash "1" *test-hash*))
(gethash "1" *test-hash*)
