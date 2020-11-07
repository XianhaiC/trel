(ql:quickload :dexador)
(ql:quickload :cl-json)

(defconstant +api-key+ "a160034de0f0562199c555cb9e4c2adf")
(defconstant +api-token+ "ead848fb75cf972668daa413d5247d3e7780dd8f27fe81575f9198dce9721d68")
(defconstant +api-domain+ "https://api.trello.com")

;;; request utils
;;;;;;;;;;;;;;;;;

(defun contains-char-p (char string) (search char string))

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

(append-params "/somewhere/hello" '(:hello "world" :some "where"))
(defun tester (mand &optional))

(defmacro trello-req (method path params &rest body)
  (let ((method-func (read-from-string (concatenate 'string "dex:" (string method)))))
    `(,method-func (append-params ,path ,params) ,@body)))


(macroexpand-1 '(trello-req :get "/1/members/me/boards?fields=name"
                            :content (json:encode-json-to-string `(("name" . ,name)))
                            :headers '(("Content-Type" . "application/json"))
                            :verbose t))

(macroexpand-1 '(trello-req :get (format nil "/1/boards/~a" board-id)))

(macroexpand-1 '
    (trello-req :get "/1/members/me/boards" '(:fields "name,url")))

;;; board functions
;;;;;;;;;;;;;;;;;;;

(defun get-boards (&rest params)
  (json:decode-json-from-string
    (trello-req :get "/1/members/me/boards" params)))

(defun get-board (board-id &rest params)
  (json:decode-json-from-string
    (trello-req :get (format nil "/1/boards/~a" board-id) params)))

(defun update-board-name (board-id name &rest params)
  (trello-req :put
              (format nil "/1/boards/~a" board-id) params
              :content (json:encode-json-to-string `(("name" . ,name)))
              :headers '(("Content-Type" . "application/json"))
              :verbose t))


;;; list functions
;;;;;;;;;;;;;;;;;;

(defun get-lists (board-id &rest params)
  (json:decode-json-from-string
    (trello-req :get (format nil "/1/boards/~a/lists" board-id) params)))

(defun get-list (list-id &rest params)
  (json:decode-json-from-string
    (trello-req :get (format nil "/1/lists/~a" list-id) params)))

(defun get-list-cards (list-id &rest params)
  (json:decode-json-from-string
    (trello-req :get (format nil "/1/lists/~a/cards" list-id) params)))

(defun create-list (name board-id &rest params)
  (trello-req :post
              "/1/lists" params
              :content (json:encode-json-to-string `(("name" . ,name)
                                                     ("idBoard" . ,board-id)))
              :headers '(("Content-Type" . "application/json"))))

;;; card functions
;;;;;;;;;;;;;;;;;;

(defun get-card ())
(defun create-card ())












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

;; TODO figure out how to get this to work
(update-board-name +test-board+ "new-scratch-3")

(defparameter board-lists (get-lists +test-board+))
board-lists

(defparameter test-list (get-list "5f8088b4add2c686d3fda2e6"))
test-list

(defparameter test-list-cards (get-list-cards "5f8088b4add2c686d3fda2e6"))
test-list-cards

(create-list "test-list-6" +test-board+)
(defparameter new-list (get-list "5fa6f0d8e4b326318afb3e6f"))
new-list
