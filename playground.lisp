;; load the following files
(load "package.lisp")
(load "config.lisp")
(load "api.lisp")
(load "collections.lisp")

(in-package :trel)
(ql:quickload :dexador)
(ql:quickload :cl-json)

;; api stuff
(defparameter res (dex:get (format nil
                                   "https://api.trello.com/1/members/me/boards?key=~a&token=~a"
                                   +api-key+ +api-token+)))
(defparameter res (dex:get "http://lisp.org/"))
(defparameter decoded (cl-json:decode-json (car res)))
(defparameter board (dex:get (format nil
                                     "https://api.trello.com/1/boards/5d76b52abfac6846630d7ada?key=~a&token=~a"
                                     +api-key+ +api-token+)))

(defconstant +test-board+ "5dffa7b061ac060653847490")
(defparameter new-board (get-board +test-board+))
new-board
(defparameter all-boards (get-boards :fields "name,url"))
all-boards

(update-board-name +test-board+ "new-scratch-4")

(defparameter board-lists (get-lists +test-board+))
board-lists

(defparameter test-list (get-list "5f8088b4add2c686d3fda2e6"))
test-list

(defparameter test-list-cards (get-cards "5f8088b4add2c686d3fda2e6"))
(first test-list-cards)
(assoc-cdr :id (first test-list-cards))

(defparameter got-card (get-card "5f809190eccfe338721d9f63"))
(assoc-cdr :id-board got-card)

(create-list "test-list-9" +test-board+)
(defparameter new-list (get-list "5fa6f0d8e4b326318afb3e6f"))
new-list

(create-board "test-board-4")
(defparameter new-board (get-board "5fa98e4c41ca6189e3536544"))
new-board

(create-card "5fa6f0d8e4b326318afb3e6f" :content '(("name" . "new card 5")
                                                   ("desc" . "my desc")))
(defparameter new-card (get-list "5fa6f0d8e4b326318afb3e6f"))
new-list





;; collection stuff

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
