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

(defmacro trello-req (method path &rest body)
  (let ((method-func (read-from-string (concatenate 'string "dex:" (string method)))))
    (with-gensyms (path-var contains-var)
      `(let* ((,path-var ,path)
              (,contains-var (contains-char-p "?" ,path-var))
              (url (concatenate 'string
                                +api-domain+
                                ,path-var
                                (if ,contains-var "&" "?")
                                (format nil "key=~a&token=~a" +api-key+ +api-token+))))

         (,method-func url ,@body)))))

(macroexpand-1 '(trello-req :get "/1/members/me/boards?fields=name"
                            :content (json:encode-json-to-string `(("name" . ,name)))
                            :headers '(("Content-Type" . "application/json"))
                            :verbose t))

(macroexpand-1 '(trello-req :get (format nil "/1/boards/~a" board-id)))

;;; board functions
;;;;;;;;;;;;;;;;;;;

(defun get-boards ()
  (json:decode-json-from-string
    (trello-req :get "/1/members/me/boards?fields=name,url")))
; (dex:get (format nil
; "https://api.trello.com/1/members/me/boards?fields=name,url&key=~a&token=~a"
; +api-key+ +api-token+))))

(defun get-board (board-id)
  (let ((path (format nil "/1/boards/~a" board-id)))
    (json:decode-json-from-string (trello-req :get path))))

(defun update-board-name (board-id name)
  (dex:put (format nil
                   "https://api.trello.com/1/boards/~a?key=~a&token=~a"
                   board-id +api-key+ +api-token+)
           :content (json:encode-json-to-string `(("name" . ,name)))
           :headers '(("Content-Type" . "application/json"))
           :verbose t))

(defun get-board-lists ())

;;; list functions
;;;;;;;;;;;;;;;;;;

(defun get-list (list-id)
  (json:decode-json-from-string
    (dex:get (format nil
                     "https://api.trello.com/1/boards/~a?key=~a&token=~a"
                     board-id +api-key+ +api-token+))))
(defun get-list-cards ())
(defun create-list ())

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
(defparameter all-boards (get-boards))
all-boards

;; TODO figure out how to get this to work
(update-board-name +test-board+ "new-scratch")
