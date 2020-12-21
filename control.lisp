(in-package :trel)

;;; internal functions
;;;;;;;;;;;;;;;;;;;;;;

(defun update-boards ()
  "Fetch the boards for the first time or when a refresh is mandated."
  (unless (gethash :loaded-boards *state-collections*)
    (boards-to-collections (get-boards :fields "name,url"))
    ;; sort the new list of boards by ascending name
    (let* ((sorted-boards (sort (alexandria:hash-table-values *trel-boards*)
                                   #'string< :key #'(lambda (board) (name board))))
           (current-id-board (id (car sorted-boards))))
      ;; set the focused and selected boards if they're nil
      (set-if-nil :focused-id-board current-id-board *state-ui*)

      ;; set the new index of the selected board if the selected board
      ;; was not nil previously
      ;; otherwise set it to be the same as the focused board
      (set-if-nil :selected-id-board current-id-board *state-ui*)
      (setf (gethash :selected-board-pos *state-ui*)
            (position (gethash :selected-id-board *state-ui*)
                      sorted-boards
                      :test #'string=
                      :key #'(lambda (board) (id board)))))))

(defun update-board-lists (id-board)
  "Fetch the board's lists for the first time or when a refresh is mandated."
  (with-accessors ((loaded-p loaded-p))
      (gethash id-board *trel-boards*)
    (unless loaded-p
      (lists-to-collections (get-lists id-board))
      (setf loaded-p t))))


;;; command definitions
;;;;;;;;;;;;;;;;;;;;;;;

;; (defun cmd-exit (win event)
;;   (return))

(defun cmd-board-select-up (state-win event)
  "Move the board selection up."
  (progn
    (setf (gethash :selected-board-pos *state-ui*)
          (mod (1- (gethash :selected-board-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-win-boards (gethash :win-boards state-win))))

(defun cmd-board-select-down (state-win event)
  "Move the board selection down."
  (progn
    (setf (gethash :selected-board-pos *state-ui*)
          (mod (1+ (gethash :selected-board-pos *state-ui*))
               (hash-table-count *trel-boards*)))
    (rend-win-boards (gethash :win-boards state-win))))

(defun cmd-list-select-up (state-win event)
  "Move the list selection up."
  (progn
    (setf (gethash :selected-list-pos *state-ui*)
          (mod (1- (gethash :selected-list-pos *state-ui*))
               (hash-table-count
                (gethash
                 (gethash :focused-id-board *state-ui*)
                 *board-to-lists*))))
    (rend-win-lists (gethash :win-lists state-win))))

(defun cmd-list-select-down (state-win event)
  "Move the list selection down."
  (progn
    (setf (gethash :selected-list-pos *state-ui*)
          (mod (1+ (gethash :selected-list-pos *state-ui*))
               (hash-table-count
                (gethash
                 (gethash :focused-id-board *state-ui*)
                 *board-to-lists*))))
    (rend-win-lists (gethash :win-lists state-win))))

(defun cmd-enter-layer-lists (state-win event)
  (setf (gethash :layer *state-ui*) :lists))

(defun cmd-enter-layer-boards (state-win event)
  (setf (gethash :layer *state-ui*) :boards))

(defun cmd-focus-board (state-win event)
  "Focus on the currently selected board. Fetch the board's
lists if they are absent."
  (let ((selected-id-board (id (elt (sort (alexandria:hash-table-values *trel-boards*)
                                   #'string< :key #'(lambda (board) (name board)))
                             (gethash :selected-board-pos *state-ui*)))))
    ;; update the focused board's lists
    (update-board-lists selected-id-board)

    ;; update the focused board to be the selected board
    (setf (gethash :focused-id-board *state-ui*)
          selected-id-board))

  ;; refresh
  (rend-win-boards (gethash :win-boards state-win))
  (rend-win-lists (gethash :win-lists state-win)))

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

(defun execute-event (state-win event)
  "Execute the appropriate command connected to this event."
  (let ((action (gethash event *event-action-map*)))
    ;; return if there's no action mapped to the event
    (unless action (return-from execute-event))

    (funcall (demux-action action (gethash :layer *state-ui*))
             state-win event)))

;; define a mapping between event (keypress) and actions
;; an action is a set of commands that is decoupled from the
;; events that trigger them
;;
;; user defined remaps should modify this mapping
(defparameter *event-action-map* (make-hash-table))

;; (setf (gethash #\q *event-action-map*) 'exit)
(setf (gethash #\j *event-action-map*) 'dir-down)
(setf (gethash #\k *event-action-map*) 'dir-up)
(setf (gethash #\l *event-action-map*) 'dir-right)
(setf (gethash #\b *event-action-map*) 'to-layer-boards)
(setf (gethash #\Newline *event-action-map*) 'execute)


;; define event to command mappings for each 'layer'
;; a state demuxer will select the command for an action based on the
;; current layer state

;; state layer mapping for default commands
(defparameter *action-layer-default* (make-hash-table))

;; (setf (gethash 'exit *action-layer-default*) #'cmd-exit)

;; state layer mapping for the user boards window
(defparameter *action-layer-boards* (make-hash-table))

(setf (gethash 'dir-up *action-layer-boards*) #'cmd-board-select-up)
(setf (gethash 'dir-down *action-layer-boards*) #'cmd-board-select-down)
(setf (gethash 'dir-right *action-layer-boards*) #'cmd-enter-layer-lists)
(setf (gethash 'to-layer-boards *action-layer-boards*) #'cmd-enter-layer-boards)
(setf (gethash 'execute *action-layer-boards*) #'cmd-focus-board)

;; state layer mapping for lists window
;; TODO
(defparameter *action-layer-lists* (make-hash-table))

(setf (gethash 'dir-up *action-layer-lists*) #'cmd-list-select-up)
(setf (gethash 'dir-down *action-layer-lists*) #'cmd-list-select-down)

;; state layer mapping for cards window
;; TODO
(defparameter *action-layer-cards* (make-hash-table))
