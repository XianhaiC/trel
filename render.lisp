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
(defclass wg-list-cards (wg-list) ())

(defgeneric set-rerender (wg)
  (:documentation "Mark the widget for rerendering."))
(defmethod set-rerender ((wg wg-list))
  (setf (slot-value wg 'rerender) t))

(defgeneric clear-rerender (wg)
  (:documentation "Clear the mark for rerendering."))
(defmethod clear-rerender ((wg wg-list))
  (setf (slot-value wg 'rerender) nil))

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

      ;; update the focused collection to be the selected collection
      (set-hash :focused-id-list selected-id-collection *state-ui*)

      ;; NOTE: not a good solution, calls method of wg-list-lists
      (update-items wg-child)

      ;; rerender the affected widgets
      (set-rerender wg)
      (set-rerender wg-child))))

(defgeneric get-collection-items (wg)
  (:documentation "Get the collection items associated with the widget"))

(defmethod get-collection-items ((wg wg-list-boards))
  (alexandria:hash-table-values *trel-boards*))

(defmethod get-collection-items ((wg wg-list-lists))
  (mapcar #'(lambda (id) (gethash id *trel-lists*))
    (alexandria:hash-table-keys
      (gethash (gethash :focused-id-board *state-ui*) *board-to-lists*))))

(defmethod get-collection-items ((wg wg-list-cards))
  (mapcar #'(lambda (id) (gethash id *trel-cards*))
    (alexandria:hash-table-keys
      (gethash (gethash :focused-id-list *state-ui*) *list-to-boards*))))

(defgeneric update-items (wg)
  (:documentation "Update a collection associated with the widget."))

(defmethod update-items ((wg wg-list))
  (with-accessors ((items items)) wg
    ;; sort the new list of collections by ascending name
    (setf items (sort (get-collection-items wg)
                  #'string<
                  :key #'(lambda (collection)
                           (name collection))))))

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
  "Render the boards window."
  (with-accessors ((win win)
                    (selected-pos selected-pos)
                    (items items)) wg

    ;; sort the boards into alphabetical order
    (let ((collection-names (mapcar #'(lambda (item) (name item)) items)))
      ;; iterate through each board
      (loop
        for i from 0
        for name in collection-names do
        (croatoan:move win i 0)

        ;; highlight the line if it's currently focused
        (when (= selected-pos i)
          (setf (croatoan:attributes win) '(:reverse)))

        (croatoan:add-string win name)

        (setf (croatoan:attributes win) '())))))

;; (defclass test-class () ())
;; (defclass test-class-sub (test-class) ())

;; (defgeneric aux (obj))
;; (defmethod aux :around ((obj test-class))
;;   (print "before around")
;;   (call-next-method)
;;   (print "after around"))
;; (defmethod aux ((obj test-class))
;;   (print "in primary"))

;; (defmethod aux ((obj test-class-sub))
;;   (print (format nil "res ~a" (+ x y))))

;; (defgeneric only-sub (obj))

;; (defmethod only-sub ((obj test-class-sub))
;;   (print "ONLY SUB"))

;; (defgeneric do-based (obj))
;; (defmethod do-based ((obj test-class))
;;   (print "do-based")
;;   (only-sub obj))


;; (only-sub (make-instance 'test-class-sub))
;; (do-based (make-instance 'test-class-sub))

;; (aux (make-instance 'test-class))

(defmethod render ((wg wg-list-cards))
  "Render the cards window.")
;; ;; TODO
;; (progn
;;   (croatoan:clear win)

;;   ;; sort the lists into alphabetical order
;;   (let ((lists (mapcar #'(lambda (id-card) (gethash id-card *trel-cards*))
;;                  (alexandria:hash-table-keys
;;                    (gethash (gethash :focused-id-list *state-ui*)
;;                      *list-to-cards*)))))
;;     ;; iterate through each card
;;     (loop
;;       for i from 0
;;       for list in lists do
;;       (croatoan:move win i 0)

;;       ;; highlight the line if it's currently focused
;;       (when (= (gethash :selected-list-pos *state-ui*) i)
;;         (setf (croatoan:attributes win) '(:reverse)))

;;       (with-accessors ((name name)) list
;;         (croatoan:add-string win name))

;;       (when (= (gethash :selected-list-pos *state-ui*) i)
;;         (setf (croatoan:attributes win) '()))))

;;   (croatoan:refresh win)))
