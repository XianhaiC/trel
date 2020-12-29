(in-package :trel)

;;; internal functions
;;;;;;;;;;;;;;;;;;;;;;


(defun update-boards ()
  (boards-to-collections (get-boards :fields "name,url")))
;; update the items list of the relevant widget
;; set the focused and selected boards if they're nil
;;(set-hash-if-nil :focused-id-board current-id-board *state-ui*)))

;; (defun update-boards ()
;;   "Fetch the boards for the first time or when a refresh is mandated."
;;   (boards-to-collections (get-boards :fields "name,url"))
;;   ;; sort the new list of boards by ascending name
;;   (let* ((sorted-boards (sort (alexandria:hash-table-values *trel-boards*)
;;                               #'string< :key #'(lambda (board) (name board))))
;;           (current-id-board (id (car sorted-boards))))
;;     ;; set the focused and selected boards if they're nil
;;     (set-hash-if-nil :focused-id-board current-id-board *state-ui*)

;;     ;; set the new index of the selected board if the selected board
;;     ;; was not nil previously
;;     ;; otherwise set it to be the same as the focused board
;;     (set-hash-if-nil :selected-id-board current-id-board *state-ui*)
;;     (setf (gethash :selected-board-pos *state-ui*)
;;           (position (gethash :selected-id-board *state-ui*)
;;                     sorted-boards
;;                     :test #'string=
;;                     :key #'(lambda (board) (id board))))))

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

(defun cmd-select-up (event)
  (move-selection-up (gethash (gethash :layer *state-ui*) *state-wg*)))

(defun cmd-select-down (event)
  (move-selection-down (gethash (gethash :layer *state-ui*) *state-wg*)))


;; (defun cmd-board-select-up (state-wg event)
;;   (cmd-select-next :selected-board-pos
;;                    1-
;;                    (hash-table-count *trel-boards*)
;;                    rend-win-boards
;;                    :win-boards))

;; (defun cmd-board-select-down (state-wg event)
;;   (cmd-select-next :selected-board-pos
;;                    1+
;;                    (hash-table-count *trel-boards*)
;;                    rend-win-boards
;;                    :win-boards))


;; (defun cmd-list-select-up (state-wg event)
;;   (cmd-select-next :selected-list-pos
;;                    1-
;;                    (hash-table-count
;;                     (gethash
;;                      (gethash :focused-id-board *state-ui*)
;;                      *board-to-lists*))
;;                    rend-win-lists
;;                    :win-lists))

;; (defun cmd-list-select-down (state-wg event)
;;   (cmd-select-next :selected-list-pos
;;                    1+
;;                    (hash-table-count
;;                     (gethash
;;                      (gethash :focused-id-board *state-ui*)
;;                      *board-to-lists*))
;;                    rend-win-lists
;;                    :win-lists))

;; ;; TODO: refactor this code so that we have a 'display' object
;; ;; the display object holds the windows that it is responsible
;; ;; for printing to.
;; ;; it should also have a method that updates the screen (render)
;; ;; we can call methods to update its internal state so that the drawn
;; ;; items are differnet
;; (defun cmd-card-select-up (state-wg event)
;;   (cmd-select-next :selected-card-pos
;;                    1-
;;                    (hash-table-count
;;                     (gethash
;;                      (gethash :focused-id-list *state-ui*)
;;                      *list-to-cards*))
;;                    rend-win-cards
;;                    :win-cards))

;; (defun cmd-list-select-down (state-wg event)
;;   (cmd-select-next :selected-card-pos
;;                    1+
;;                    (hash-table-count
;;                     (gethash
;;                      (gethash :focused-id-list *state-ui*)
;;                      *list-to-cards*))
;;                    rend-win-cards
;;                    :win-cards))

;; TODO figureout how to macro the f out of this
;; (defun cmd-board-select-up (state-wg event)
;;   "Move the board selection up."
;;   (progn
;;     (setf (gethash :selected-board-pos *state-ui*)
;;           (mod (1- (gethash :selected-board-pos *state-ui*))
;;                (hash-table-count *trel-boards*)))
;;     (rend-win-boards (gethash :win-boards state-wg))))

;; (defun cmd-board-select-down (state-wg event)
;;   "Move the board selection down."
;;   (progn
;;     (setf (gethash :selected-board-pos *state-ui*)
;;           (mod (1+ (gethash :selected-board-pos *state-ui*))
;;                (hash-table-count *trel-boards*)))
;;     (rend-win-boards (gethash :win-boards state-wg))))

;; (defun cmd-list-select-up (state-wg event)
;;   "Move the list selection up."
;;   (progn
;;     (setf (gethash :selected-list-pos *state-ui*)
;;           (mod (1- (gethash :selected-list-pos *state-ui*))
;;                (hash-table-count
;;                 (gethash
;;                  (gethash :focused-id-board *state-ui*)
;;                  *board-to-lists*))))
;;     (rend-win-lists (gethash :win-lists state-wg))))

;; (defun cmd-list-select-down (state-wg event)
;;   "Move the list selection down."
;;   (progn
;;     (setf (gethash :selected-list-pos *state-ui*)
;;           (mod (1+ (gethash :selected-list-pos *state-ui*))
;;                (hash-table-count
;;                 (gethash
;;                  (gethash :focused-id-board *state-ui*)
;;                  *board-to-lists*))))
;;     (rend-win-lists (gethash :win-lists state-wg))))

(defun cmd-enter-layer-boards (event)
  (setf (gethash :layer *state-ui*) :boards))

(defun cmd-enter-layer-lists (event)
  (setf (gethash :layer *state-ui*) :lists))

(defun cmd-enter-layer-cards (event)
  (setf (gethash :layer *state-ui*) :cards))


(defun cmd-execute (event)
  (declare (ignore event))
  (focus (gethash (gethash :layer *state-ui*) *state-wg*)))

;; (defun cmd-focus-board (state-wg event)
;;   "Focus on the currently selected board. Fetch the board's
;; lists if they are absent."
;;   (let ((selected-id-board (id (elt (sort (alexandria:hash-table-values *trel-boards*)
;;                                       #'string< :key #'(lambda (board) (name board)))
;;                                  (gethash :selected-board-pos *state-ui*)))))
;;     ;; update the focused board's lists
;;     (update-board-lists selected-id-board)

;;     ;; update the focused board to be the selected board
;;     (setf (gethash :focused-id-board *state-ui*)
;;       selected-id-board))

;;   ;; refresh
;;   (rend-win-boards (gethash :win-boards state-wg))
;;   (rend-win-lists (gethash :win-lists state-wg)))

;; (defun cmd-focus-list (state-wg event)
;;   "Focus on the currently selected list. Fetch the list's
;; cards if they are absent."
;;   (let ((selected-id-list (id (elt (sort (alexandria:hash-table-values *trel-lists*)
;;                                      #'string< :key #'(lambda (list) (name list)))
;;                                 (gethash :selected-list-pos *state-ui*)))))
;;     ;; update the focused list's cards
;;     (update-list-cards selected-id-list)

;;     ;; update the focused board to be the selected board
;;     (setf (gethash :focused-id-list *state-ui*)
;;       selected-id-list))

;;   ;; refresh
;;   (rend-win-lists (gethash :win-lists state-wg))
;;   (rend-win-cards (gethash :win-cards state-wg)))

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
  (('dir-left #'cmd-enter-layer-lists))
  *action-layer-cards*)
