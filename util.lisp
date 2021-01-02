(in-package :trel)

(defun contains-char-p (char string) (search char string))

(defun assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro set-hash (key value hash-table)
  "Sets a hash table value."
  `(setf (gethash ,key ,hash-table) ,value))

(defmacro set-hash-if-nil (key value hash-table)
  "Sets a hash table value only if the key maps to nil."
  `(unless (gethash ,key ,hash-table)
     (setf (gethash ,key ,hash-table) ,value)))

(defmacro set-hash-multiple (key-val-pairs hash-table)
  "Set multiple hash table values at once."
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(setf (gethash ,(car pair) ,hash-table)
                      ,(cadr pair)))
         key-val-pairs)))

(defmacro use-otherwise (expr default)
  (with-gensyms (val)
    `(let ((,val ,expr))
       (if ,val
         ,val
         ,default))))

(macroexpand-1 '(use-otherwise (+ 2 3) nil))
(use-otherwise nil '(1))

(defun repeat-str-n (str n)
  (format nil "~v@{~A~:*~}" n str))

(defun wrapped-lines (string max-width &key pad)
  "Break up the given string into multiple lines"
  (let ((str-len (length string)))
    (loop for i from 0 below str-len by max-width collect
      (let ((line (subseq string i (min (+ i max-width) str-len))))
        ;; pad the lines to be the same length
        (if pad
          (concatenate 'string
            line
            (repeat-str-n " " (- max-width (length line))))
          line)))))

(defun draw-border (win y x h w)
  "Draw a border in the window at the specified location"
  (let ((v-ch (croatoan:acs :vertical-line))
         (h-ch (croatoan:acs :horizontal-line))
         (ul-ch (croatoan:acs :upper-left-corner))
         (ll-ch (croatoan:acs :lower-left-corner))
         (ur-ch (croatoan:acs :upper-right-corner))
         (lr-ch (croatoan:acs :lower-right-corner)))
    ;; draw vertical borders
    (loop for row from y below (+ y h) do
      (croatoan:add-char win v-ch :position (list row x))
      (croatoan:add-char win v-ch :position (list row (+ x (1- w)))))
    ;; draw horizontal borders
    (loop for col from x below (+ x w) do
      (croatoan:add-char win h-ch :position (list y col))
      (croatoan:add-char win h-ch :position (list (+ y (1- h)) col)))
    (croatoan:add-char win ul-ch :position (list y x))
    (croatoan:add-char win ll-ch :position (list (+ y (1- h)) x))
    (croatoan:add-char win ur-ch :position (list y (+ x (1- w))))
    (croatoan:add-char win lr-ch :position (list (+ y (1- h)) (+ x (1- w))))))
