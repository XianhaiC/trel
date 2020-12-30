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
