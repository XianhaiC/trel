(in-package :trel)

;; setup data
(defparameter *state-ui* (make-hash-table))
(setf (gethash :curs-pos *state-ui*) 0)

(boards-to-collections (get-boards :fields "name,url"))

(defun cmd-select-up (win event)
  (progn
    (setf (gethash :curs-pos *state-ui*)
          (mod (1- (gethash :curs-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-board-screen win *trel-boards* *state-ui*)))

(defun cmd-select-down (win event)
  (progn
    (setf (gethash :curs-pos *state-ui*)
          (mod (1+ (gethash :curs-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-board-screen win *trel-boards* *state-ui*)))



(defun main ()
  (croatoan:with-screen (win :input-echoing nil :input-blocking t :enable-colors t)
    (croatoan:clear win)
    (rend-board-screen win *trel-boards* *state-ui*)
    (croatoan:refresh win)
    (croatoan:event-case (win event)
      (#\q (return-from croatoan:event-case))
      (#\j (cmd-select-down win event))
      (#\k (cmd-select-up win event))
      (otherwise (sleep 0.0166)))))



(main)
