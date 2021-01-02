(in-package :trel)

;;; internal functions
;;;;;;;;;;;;;;;;;;;;;;

(defun update-boards ()
  "Fetch the the user's boards"
  (boards-to-collections (get-boards :fields "name,url")))

(defun update-board-lists (id-board)
  "Fetch the board's lists for the first time or when a refresh is mandated."
  (with-accessors ((loaded-p loaded-p))
      (gethash id-board *trel-boards*)
    (unless loaded-p
      (lists-to-collections (get-lists id-board))
      (setf loaded-p t))))

;; TODO: investigate why get-cards can return null
;; empty list? or non existent list? do a null check beforehand
(defun update-list-cards (id-list)
  "Fetch the list's lists for the first time or when a refresh is mandated."
  (with-accessors ((loaded-p loaded-p))
      (gethash id-list *trel-lists*)
    (unless loaded-p
      (cards-to-collections (get-cards id-list))
      (setf loaded-p t))))


;;; command definitions
;;;;;;;;;;;;;;;;;;;;;;;

;; movement commands
(defun cmd-select-up (event)
  (move-selection-up (gethash (gethash :layer *state-ui*) *state-wg*)))

(defun cmd-select-down (event)
  (move-selection-down (gethash (gethash :layer *state-ui*) *state-wg*)))

(defun cmd-enter-layer-boards (event)
  (setf (gethash :layer *state-ui*) :boards))

(defun cmd-enter-layer-lists (event)
  (setf (gethash :layer *state-ui*) :lists))

(defun cmd-enter-layer-cards (event)
  (setf (gethash :layer *state-ui*) :cards))


(defun cmd-execute (event)
  (declare (ignore event))
  (focus (gethash (gethash :layer *state-ui*) *state-wg*)))

;; collection interaction
(defun cmd-edit-selected (event)
  (declare (ignore event))
  (edit-selected (gethash (gethash :layer *state-ui*) *state-wg*)))


;;; helper methods for events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demux-action (action layer)
  "Demux the layer to map the action to the correct command.
    If there is not mapping the command in the current layer,
    fall back to the default layer."
  (let ((cmd (case layer
               ((:boards) (gethash action *action-layer-boards*))
               ((:lists) (gethash action *action-layer-lists*))
               ((:cards) (gethash action *action-layer-cards*)))))
    (if cmd
        cmd
        (gethash action *action-layer-default*))))

(defun execute-event (event)
  "Execute the appropriate command connected to this event."
  (let ((action (gethash event *event-action-map*)))
    ;; return if there's no action mapped to the event
    (unless action (return-from execute-event))

    (let ((cmd (demux-action action (gethash :layer *state-ui*))))
      (when cmd
        (funcall cmd event))))

  ;; rerender the flagged widgets
  (loop for wg in (alexandria:hash-table-values *state-wg*) do
    (with-accessors ((rerender rerender)) wg
      (when rerender (render wg)))))

;; define a mapping between event (keypress) and actions
;; an action is a set of commands that is decoupled from the
;; events that trigger them
;;
;; user defined remaps should modify this mapping
(defparameter *event-action-map* (make-hash-table))

;; (setf (gethash #\q *event-action-map*) 'exit)
(setf (gethash #\h *event-action-map*) 'dir-left)
(setf (gethash #\j *event-action-map*) 'dir-down)
(setf (gethash #\k *event-action-map*) 'dir-up)
(setf (gethash #\l *event-action-map*) 'dir-right)
(setf (gethash #\b *event-action-map*) 'to-layer-boards)
(setf (gethash #\e *event-action-map*) 'edit)
(setf (gethash #\Newline *event-action-map*) 'execute)


;; define event to command mappings for each 'layer'
;; a state demuxer will select the command for an action based on the
;; current layer state

;; state layer mapping for default commands
(defparameter *action-layer-default* (make-hash-table))

(set-hash-multiple
 (('dir-down #'cmd-select-down)
  ('dir-up #'cmd-select-up)
  ('execute #'cmd-execute))
 *action-layer-default*)

;; (setf (gethash 'exit *action-layer-default*) #'cmd-exit)

;; state layer mapping for the user boards window
(defparameter *action-layer-boards* (make-hash-table))

(set-hash-multiple
 (('dir-right #'cmd-enter-layer-lists)
  ('to-layer-boards #'cmd-enter-layer-boards))
 *action-layer-boards*)

;; state layer mapping for lists window
;; TODO
(defparameter *action-layer-lists* (make-hash-table))

(set-hash-multiple
 (('dir-left #'cmd-enter-layer-boards)
  ('dir-right #'cmd-enter-layer-cards))
 *action-layer-lists*)

;; state layer mapping for cards window
;; TODO
(defparameter *action-layer-cards* (make-hash-table))

(set-hash-multiple
 (('dir-left #'cmd-enter-layer-lists)
  ('edit #'cmd-edit-selected))
 *action-layer-cards*)
