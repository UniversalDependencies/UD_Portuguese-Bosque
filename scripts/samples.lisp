
(ql:quickload :cl-conllu)

(in-package :cl-conllu)


(defun take (alist n &optional (res nil))
  "Take n random elements from a list"
  (if (> n 0)
      (let ((pick (nth (random (length alist)) alist)))
	(take (remove pick alist) (- n 1) (cons pick res)))
      (values res alist)))


(let* ((sents (cl-conllu:query '(xpostag "<n>")
			       (read-conllu "/Users/arademaker/work/bosque-UD/documents/"))))
  (multiple-value-bind (common rest)
      (take sents 10)
    (do ((v 0 (+ v 1))
	 (res rest))
	((> v 2))
      (multiple-value-bind (a b)
	  (take res 40)
	(setf res b)
	(cl-conllu:write-conllu (append common a) (format nil "anotador-~a.conllu" v))))))




