(in-package :trel)

(defun contains-char-p (char string) (search char string))

(defun assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
