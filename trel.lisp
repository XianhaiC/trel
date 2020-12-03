(in-package :trel)

;; fetch upstream resources
(boards-to-collections (get-boards :fields "name,url"))

(defun main ()
  (croatoan:with-screen (win :input-echoing nil :input-blocking t :enable-colors t)
    (croatoan:clear win)
    (rend-board-screen win *trel-boards* *state-ui*)
    (croatoan:refresh win)

    (loop
      (let ((event (croatoan:get-event win)))
        (case event
          (nil (sleep .0166))
          (#\q (return))
          (otherwise (execute-event win event)))))))


;; (croatoan:event-case (win event)
;;   (#\q (return-from croatoan:event-case))
;;   (#\j (cmd-select-down win event))
;;   (#\k (cmd-select-up win event))
;;   (otherwise (sleep 0.0166)))))


(main)
