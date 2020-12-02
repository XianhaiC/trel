(in-package :trel)

;;; board render functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rend-board-screen (win state-boards state-ui)
  (progn
    ;; iterate through each board
    (with-hash-table-iterator (entry state-boards)
      (loop
        for i from 0 do
          (multiple-value-bind (more-p id board) (entry)
            (unless more-p (return))
            (croatoan:move win i 0)

            ;; highlight the line if it's currently focused
            (when (= (gethash :curs-pos state-ui) i)
              (setf (croatoan:attributes win) '(:reverse)))

            (with-accessors ((name name)) board
                (croatoan:add-string win name))

            (when (= (gethash :curs-pos state-ui) i)
              (setf (croatoan:attributes win) '())))))

    (croatoan:refresh win)))

;; (defparameter *state-boards* (make-hash-table :test 'equal))
;; (setf (gethash "123" *state-boards*) 1)
;; (setf (gethash "456" *state-boards*) 2)
;; (hash-table-count *state-boards*)

;; (defparameter *state-ui* (make-hash-table))


;; (croatoan:submit (rend-board-screen *scr* *state-boards* *state-ui*))

;; (croatoan:bind *scr* #\j
;;                (lambda (win event)
;;                  (setf (gethash :curs-pos *state-ui*)
;;                        (mod (1+ (gethash :curs-pos *state-ui*))
;;                             (hash-table-count *state-boards*)))))


;; (croatoan:bind *scr* #\k
;;                (lambda (win event)
;;                  (setf (gethash :curs-pos *state-ui*)
;;                        (mod (1- (gethash :curs-pos *state-ui*))
;;                             (hash-table-count *state-boards*)))))
