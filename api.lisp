(ql:quickload :dexador)
(ql:quickload :cl-json)

(in-package :trel)

(defconstant +api-key+ "a160034de0f0562199c555cb9e4c2adf")
(defconstant +api-token+ "ead848fb75cf972668daa413d5247d3e7780dd8f27fe81575f9198dce9721d68")
(defconstant +api-domain+ "https://api.trello.com")

;;; request utils
;;;;;;;;;;;;;;;;;

(defun contains-char-p (char string) (search char string))

(defun assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; generates the api url for the specified path with parameters
;; automatically appends domain and authentication information
;; the params are parsed as a list of :key value pairs
(defun append-params (path params)
  (format nil "~a~a?~{~(~a~)=~a~^&~}" +api-domain+ path
          (append params
                  `(:key ,+api-key+ :token ,+api-token+))))

;; executes the api request via dex
(defmacro trello-req (method path params &rest body)
  (let ((method-func (read-from-string (concatenate 'string "dex:" (string method)))))
    `(,method-func (append-params ,path ,params) ,@body)))

;; create a post/put/patch request
;; NOTE: this is an incredibly leaky macro, as it assumes the keywords 'params' and
;; 'content' to be defined
(defmacro update-req (method path required-content)
  `(trello-req ,method
               ,path params
               :content (json:encode-json-to-string (append ,required-content content))
               :headers '(("Content-Type" . "application/json"))))

;; create a get request
(defmacro get-req (path)
  `(json:decode-json-from-string
     (trello-req :get ,path params)))

;;; board functions
;;;;;;;;;;;;;;;;;;;

(defun get-boards (&rest params)
  (get-req "/1/members/me/boards"))

(defun get-board (board-id &key (params '()))
  (get-req (format nil "/1/boards/~a" board-id)))

(defun update-board-name (board-id name &key (params '()) (content '()))
  (update-req :put (format nil "/1/boards/~a" board-id) `(("name" . ,name))))

(defun create-board (name &key (params '()) (content '()))
  (update-req :post "/1/boards" `(("name" . ,name))))

(defun update-list (board-id &key (params '()) (content '()))
  (update-req :put (format nil "/1/boards/~a" board-id) '()))
;;; list functions
;;;;;;;;;;;;;;;;;;

(defun get-lists (board-id &key (params '()))
  (get-req (format nil "/1/boards/~a/lists" board-id)))

(defun get-list (list-id &key (params '()))
  (get-req (format nil "/1/lists/~a" list-id)))

(defun create-list (name board-id &key (params '()) (content '()))
  (update-req :post "/1/lists" `(("name" . ,name) ("idBoard" . ,board-id))))

(defun update-list (list-id &key (params '()) (content '()))
  (update-req :put (format nil "/1/lists/~a" list-id) '()))

;;; card functions
;;;;;;;;;;;;;;;;;;

(defun get-cards (list-id &key (params '()))
  (get-req (format nil "/1/lists/~a/cards" list-id)))

(defun get-card (card-id &key (params '()))
  (get-req (format nil "/1/cards/~a" card-id)))

(defun create-card (list-id &key (params '()) (content '()))
  (update-req :post "/1/cards" `(("idList" . ,list-id))))

(defun update-card (card-id &key (params '()) (content '()))
  (update-req :put (format nil "/1/cards/~a" card-id) '()))





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
got-card

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
