(in-package :trel)

;; map collection id's to their render objects
(defparameter *rend-cards* (make-hash-table :test 'equal))

;; generic class to hold rendering information for a collection
(defclass wg-list ()
  ((win
    :initarg :win
    :initform nil
    :accessor win)
   (selected-pos
    :initarg :selected-pos
    :initform 0
    :accessor selected-pos)
   (items
    :initarg :items
    :initform '()
    :accessor items)
   (rerender
    :initarg :rerender
    :initform nil
    :accessor rerender)))

(defclass wg-list-boards (wg-list) ())
(defclass wg-list-lists (wg-list) ())

(defclass wg-list-cards (wg-list)
  ((card-items
    :initarg :card-items
    :initform '()
    :accessor card-items)))


(defgeneric set-rerender (wg)
  (:documentation "Mark the widget for rerendering."))
(defmethod set-rerender ((wg wg-list))
  (setf (slot-value wg 'rerender) t))

(defgeneric clear-rerender (wg)
  (:documentation "Clear the mark for rerendering."))
(defmethod clear-rerender ((wg wg-list))
  (setf (slot-value wg 'rerender) nil))


;;; movement methods
;;;;;;;;;;;;;;;;;;;;

(defmacro select-next (next-ind-func)
  "Macro for move-selection methods"
  `(progn
     (with-accessors ((selected-pos selected-pos)
                      (items items)) wg
       (let ((items-length (length items)))
         (when (> items-length 0)
           ;; set the new selection position
           (setf selected-pos
                 (mod (,next-ind-func selected-pos) items-length))
           ;; refresh the widget
           (set-rerender wg))))))


(defgeneric move-selection-up (wg)
  (:documentation "Move the selection up one item."))

(defmethod move-selection-up ((wg wg-list))
  (select-next 1-))

(defgeneric move-selection-down (wg)
  (:documentation "Move the selection down one item."))

(defmethod move-selection-down ((wg wg-list))
  (select-next 1+))

(defgeneric selected-item (wg)
  (:documentation "Get the selected item"))

(defmethod selected-item ((wg wg-list))
  (with-accessors ((selected-pos selected-pos)
                   (items items)) wg
    (elt items selected-pos)))


;;; grab the collections to display in the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-collection-items (wg)
  (:documentation "Get the collection items associated with the widget"))

(defmethod get-collection-items ((wg wg-list-boards))
  (alexandria:hash-table-values *trel-boards*))

(defmethod get-collection-items ((wg wg-list-lists))
  (mapcar #'(lambda (id) (gethash id *trel-lists*))
          (let ((id-collections (gethash (gethash :focused-id-board *state-ui*) *board-to-lists*)))
            (if id-collections
                (alexandria:hash-table-keys id-collections)
                '()))))

(defmethod get-collection-items ((wg wg-list-cards))
  (mapcar #'(lambda (id) (gethash id *trel-cards*))
          ;; grab the card id's associated with the list, using an empty list
          ;; if the list doesn't map to any cards
          (let ((id-collections (gethash (gethash :focused-id-list *state-ui*) *list-to-cards*)))
            (if id-collections
                (alexandria:hash-table-keys id-collections)
                '()))))

(defgeneric update-items (wg)
  (:documentation "Update a collection associated with the widget."))

(defmethod update-items ((wg wg-list))
  (with-accessors ((selected-pos selected-pos)
                   (items items)) wg
    ;; sort the new list of collections by ascending name
    (setf items (sort (get-collection-items wg)
                      #'string<
                      :key #'(lambda (collection)
                               (name collection))))
    (setf selected-pos (min (1- (length items)) selected-pos))))


;;; render methods
;;;;;;;;;;;;;;;;;;

(defgeneric render-row (wg win row item selected-p)
  (:documentation "Render a row in the list."))

(defmethod render-row ((wg wg-list) win row item selected-p)
  (croatoan:move win row 0)

  ;; highlight the line if it's currently focused
  (when selected-p
    (setf (croatoan:attributes win) '(:reverse)))

  (croatoan:add-string win (name item))

  (setf (croatoan:attributes win) '()))


(defgeneric render (wg)
  (:documentation "Render the widget."))

(defmethod render :around ((wg wg-list))
  "Auxiliary method to handle window clearing/refreshing"

  (with-accessors ((win win)) wg
    ;; clear the window
    (croatoan:clear win)

    ;; draw to the window
    (call-next-method)

    ;; refresh and turn off the rerender flag
    (croatoan:refresh win)
    (clear-rerender wg)))

(defmethod render ((wg wg-list))
  "Render the boards and lists window."
  (with-accessors ((win win)
                   (selected-pos selected-pos)
                   (items items)) wg

    ;; iterate through each board
    (loop
      for row from 0
      for item in items do
        (render-row wg win row item (= selected-pos row)))))

(defmethod render ((wg wg-list-cards))
  "Render the cards window."
  (with-accessors ((win win)
                   (selected-pos selected-pos)
                   (items items)) wg
    (let* ((win-height (croatoan:height win))
           (card-width (gethash :rend-card-width *state-ui*))
           (card-height-max (gethash :rend-card-height-max *state-ui*))
           (card-gap (gethash :rend-card-gap *state-ui*))
           (text-width (- card-width 2))
           (text-height-max (- card-height-max 2)))
      (loop
        for item in items
        for item-ind from 0
        for lines = (wrapped-lines (name item) text-width :pad t)
        for height = (+ (min (length lines) text-height-max) 2)
        and row = 0 then (+ row height card-gap)
        while (<= (+ row height) win-height) do


          ;; highlight the line if it's currently focused
          (when (= item-ind selected-pos)
            (setf (croatoan:attributes win) '(:reverse)))

          (draw-border win row 0 height card-width)
          (loop
            for line in lines
            for line-row from (1+ row) below (+ row (- height 1)) do
              (croatoan:add-string win line :position (list line-row 1)))

          (setf (croatoan:attributes win) '())))))


;;; focus methods
;;;;;;;;;;;;;;;;;

(defgeneric focus (wg)
  (:documentation "Focus on the currently selected item. Fetch the
 item's subcollections if they are absent."))

(defmethod focus ((wg wg-list-boards))
  "Focus on the currently selected board. Fetch the board's
lists if they are absent."
  (with-accessors ((selected-pos selected-pos)
                   (focused-id focused-id)
                   (items items)) wg
    (let ((selected-id-collection (id (selected-item wg)))
          (wg-child (gethash :lists *state-wg*)))
      ;; update the focused board's lists
      ;; TODO: use a macro, this is the only difference
      (update-board-lists selected-id-collection)

      ;; update the focused collection to be the selected collection
      (set-hash :focused-id-board selected-id-collection *state-ui*)

      ;; NOTE: not a good solution, calls method of wg-list-lists
      (update-items wg-child)

      ;; rerender the affected widgets
      (set-rerender wg)
      (set-rerender wg-child))))

(defmethod focus ((wg wg-list-lists))
  "Focus on the currently selected board. Fetch the board's
lists if they are absent."
  (with-accessors ((selected-pos selected-pos)
                   (focused-id focused-id)
                   (items items)) wg
    (let ((selected-id-collection (id (selected-item wg)))
          (wg-child (gethash :cards *state-wg*)))
      ;; update the focused collection's things
      (update-list-cards selected-id-collection)
                                        ;(print *list-to-cards*)

      ;; update the focused collection to be the selected collection
      (set-hash :focused-id-list selected-id-collection *state-ui*)

      ;; NOTE: not a good solution, calls method of wg-list-lists
      (update-items wg-child)

      ;; rerender the affected widgets
      (set-rerender wg)
      (set-rerender wg-child))))


;;; edit methods
;;;;;;;;;;;;;;;;

(defgeneric edit-selected (wg)
  (:documentation "Edit the selected collection"))

(defmethod edit-selected ((wg wg-list-cards))
  (let ((selected (selected-item wg)))
    ;; write the initial contents of the card to the file
    (with-open-file (stream ".EDITTEXT"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (name selected) stream))

    (vom:debug "Editing card: ~a" (id selected))

    ;; we must first save the ncurses mode before shelling out to the editor
    ;; this allows us to return back to the state of the application (via initscr)
    ;; once the user quits out of the editor
    (ncurses:endwin)

    ;; launch the editor
    (uiop:wait-process
     (uiop:launch-program (list "vim" ".EDITTEXT")
                          :output :interactive
                          :error-output :interactive
                          :input :interactive))

    ;; restore the context of the application
    (ncurses:initscr)

    (vom:debug "Finished editing, returning to curses")

    ;; open the file again, reading in the new text
    (with-open-file (stream ".EDITTEXT"
                            :if-does-not-exist :create)
      (let ((new-text (loop for line = (read-line stream nil)
                            while line collect line)))

        ;; perform update request to upstream
        (update-name selected (apply #'concatenate (cons 'string new-text))))))

  ;; rerender the affected card
  (set-rerender wg))
