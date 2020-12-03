(in-package :trel)

;;; command definitions
;;;;;;;;;;;;;;;;;;;;;;;

;; (defun cmd-exit (win event)
;;   (return))

(defun cmd-board-select-up (win event)
  "Move the board selection up."
  (progn
    (setf (gethash :curs-pos *state-ui*)
          (mod (1- (gethash :curs-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-board-screen win *trel-boards* *state-ui*)))

(defun cmd-board-select-down (win event)
  "Move the board selection down."
  (progn
    (setf (gethash :curs-pos *state-ui*)
          (mod (1+ (gethash :curs-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-board-screen win *trel-boards* *state-ui*)))

(defun demux-action (action layer)
  "Demux the layer to map the action to the correct command.
    If there is not mapping the command in the current layer,
    fall back to the default layer."
  (let ((cmd (case layer
               ((:boards) (gethash action *action-layer-boards*))
               ((:lists) (gethash action *action-layer-lists*)))))
    (if cmd
        cmd
        (gethash action *action-layer-default*))))

(defun execute-event (win event)
  "Execute the appropriate command connected to this event."
  (let ((action (gethash event *event-action-map*)))
    ;; return if there's no action mapped to the event
    (unless action (return-from execute-event))

    (funcall (demux-action action (gethash :layer *state-ui*))
           win event)))

;; define a mapping between event (keypress) and actions
;; an action is a set of commands that is decoupled from the
;; events that trigger them
;;
;; user defined remaps should modify this mapping
(defparameter *event-action-map* (make-hash-table))

;; (setf (gethash #\q *event-action-map*) 'exit)
(setf (gethash #\j *event-action-map*) 'dir-down)
(setf (gethash #\k *event-action-map*) 'dir-up)


;; define event to command mappings for each 'layer'
;; a state demuxer will select the command for an action based on the
;; current layer state

;; state layer mapping for default commands
(defparameter *action-layer-default* (make-hash-table))

;; (setf (gethash 'exit *action-layer-default*) #'cmd-exit)

;; state layer mapping for the user boards display
(defparameter *action-layer-boards* (make-hash-table))

(setf (gethash 'dir-up *action-layer-boards*) #'cmd-board-select-up)
(setf (gethash 'dir-down *action-layer-boards*) #'cmd-board-select-down)

;; state layer mapping for lists display
;; TODO
(defparameter *action-layer-lists* (make-hash-table))
