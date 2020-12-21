(in-package :trel)

(defun init-state ()
  (update-boards))

(defun main ()
  (init-state)

  ;; start the screen
  (croatoan:with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
    (let ((scr-height (croatoan:height scr))
          (scr-width (croatoan:width scr))
          (state-win (make-hash-table)))
      (croatoan:with-windows ((win-boards :position (list 0 0)
                                          :height scr-height
                                          :width 20)
                              (win-lists :position (list 0 20)
                                         :height scr-height
                                         :width scr-width)
                              (win-cards :position (list 0 40)
                                         :height scr-height
                                         :width 30))

        (setf (gethash :win-boards state-win) win-boards)
        (setf (gethash :win-lists state-win) win-lists)
        (setf (gethash :win-cards state-win) win-cards)

        (croatoan:clear scr)
        (rend-win-boards scr)
        (croatoan:refresh scr)
        (loop
          (let ((event (croatoan:get-event scr)))

            ;; DEBUG
            (croatoan:move scr (1- scr-height) 0)
            (croatoan:add-string scr (format nil "      "))
            (croatoan:move scr (1- scr-height) 0)
            (croatoan:add-string scr (format nil "~a" (char-code event)))
            (croatoan:move scr (- scr-height 2) 0)
            (croatoan:add-string scr (format nil "~a" (gethash :selected-id-board *state-ui*)))

            ;; (croatoan:move win-lists 0 0)
            ;; (croatoan:add-string win-lists (format nil "LISTS"))
            ;; (croatoan:refresh win-lists)
            ;; (croatoan:move win-boards 0 0)
            ;; (croatoan:add-string win-boards (format nil "BOARDS"))
            ;; (croatoan:refresh win-boards)

            (case event
              (nil (sleep .0166))
              (#\q (return))
              (otherwise (execute-event state-win event)))
            ))))))

;; (croatoan:event-case (win event)
;;   (#\q (return-from croatoan:event-case))
;;   (#\j (cmd-select-down win event))
;;   (#\k (cmd-select-up win event))
;;   (otherwise (sleep 0.0166)))))


(main)
