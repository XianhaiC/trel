(ql:quickload :dexador)
(ql:quickload :cl-json)

(in-package :trel)

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

(defun get-board (id-board &key (params '()))
  (get-req (format nil "/1/boards/~a" id-board)))

(defun create-board (name &key (params '()) (content '()))
  (update-req :post "/1/boards" `(("name" . ,name))))

(defun update-board (id-board &key (params '()) (content '()))
  (update-req :put (format nil "/1/boards/~a" id-board) '()))

;;; board helpers

(defun update-board-name (id-board name &key (params '()) (content '()))
  (update-req :put (format nil "/1/boards/~a" id-board) `(("name" . ,name))))

;;; list functions
;;;;;;;;;;;;;;;;;;

(defun get-lists (id-board &key (params '()))
  (get-req (format nil "/1/boards/~a/lists" id-board)))

(defun get-list (id-list &key (params '()))
  (get-req (format nil "/1/lists/~a" id-list)))

(defun create-list (name id-board &key (params '()) (content '()))
  (update-req :post "/1/lists" `(("name" . ,name) ("idBoard" . ,id-board))))

(defun update-list (id-list &key (params '()) (content '()))
  (update-req :put (format nil "/1/lists/~a" id-list) '()))

;;; list helpers

(defun update-list-pos (id-list pos-new)
  "Updates the position of a list. If pos-new is <= 0.125, then positions of
  all lists in the board will be reset.
  When updating the position, set the new value to be the midpoint between
  the positions of the two lists surrounding the new location."
  (update-list id-list :content `(("pos" . ,pos-new))))

;;; card functions
;;;;;;;;;;;;;;;;;;

(defun get-cards (id-list &key (params '()))
  (get-req (format nil "/1/lists/~a/cards" id-list)))

(defun get-card (id-card &key (params '()))
  (get-req (format nil "/1/cards/~a" id-card)))

(defun create-card (id-list &key (params '()) (content '()))
  (update-req :post "/1/cards" `(("idList" . ,id-list))))

(defun update-card (id-card &key (params '()) (content '()))
  (update-req :put (format nil "/1/cards/~a" id-card) '()))
