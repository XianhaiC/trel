(in-package :trel)

(defmacro init-logging (&body body)
  (with-gensyms (stream)
    `(progn
       (vom:config t :debug)
       (with-open-file (,stream +log-file+
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
         (defparameter vom:*log-stream* ,stream)
         ,@body))))

(defun init-state ()
  (update-boards)
  (update-items (gethash :boards *state-wg*)))

(defun main ()

  (init-logging
   (vom:debug "Program start.")
   ;; start the screen
   (croatoan:with-screen (scr :input-echoing nil
                              :input-blocking t
                              :enable-colors t
                              :use-terminal-colors t
                              :cursor-visible nil)
     (let ((scr-height (croatoan:height scr))
           (scr-width (croatoan:width scr))
           (board-width (gethash :rend-board-width *state-ui*))
           (list-width (gethash :rend-list-width *state-ui*))
           (card-width (gethash :rend-card-width *state-ui*)))
       (croatoan:with-windows ((win-boards :position (list 0 0)
                                           :height scr-height
                                           :width board-width)
                               (win-lists :position (list 0 20)
                                          :height scr-height
                                          :width list-width)
                               (win-cards :position (list 0 40)
                                          :height scr-height
                                          :width card-width))

         (setf (gethash :boards *state-wg*)
               (make-instance 'wg-list-boards
                              :win win-boards))
         (setf (gethash :lists *state-wg*)
               (make-instance 'wg-list-lists
                              :win win-lists))
         (setf (gethash :cards *state-wg*)
               (make-instance 'wg-list-cards
                              :win win-cards))

         ;; TODO: implement error handling for network issues
         (init-state)

         ;; render the initial windows
         ;; TODO: figure out why we must refresh the main screen first
         (croatoan:refresh scr)
         (render (gethash :boards *state-wg*))
                                        ;(render (gethash :lists *state-wg*))

         (loop
           (let ((event (croatoan:get-event scr)))

             ;; DEBUG
             (croatoan:move scr (1- scr-height) 0)
             (croatoan:add-string scr (format nil "      "))
             (croatoan:move scr (1- scr-height) 0)
             (croatoan:add-string scr (format nil "~a" (char-code event)))
             (croatoan:move scr (- scr-height 2) 0)
             (croatoan:add-string scr (format nil "~a" (gethash :selected-id-board *state-ui*)))

             (if event
                 (case event
                   (#\q (return))
                   (otherwise (execute-event event)))
                 (nil (sleep .0166))))))))))

(main)
