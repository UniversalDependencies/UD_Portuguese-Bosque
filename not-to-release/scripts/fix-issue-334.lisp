(ql:quickload :cl-conllu)

(in-package :conllu.user)

(defun f2hash (as)
  (if (equal as "_")
      (make-hash-table :test #'equal)
      (let ((s (split-sequence #\| as)))
	(alexandria:alist-hash-table
	 (mapcar (lambda (x)
		   (handler-case
		       (destructuring-bind (a b)
			   (split-sequence #\= x)
			 (cons a b))
		     (error ()
		       (cons x nil))))
		 s)
	 :test #'equal))))

(defun hash2f (tb)
  (let ((as (sort (alexandria:hash-table-alist tb) #'string-not-greaterp :key #'car)))
    (if (null as)
	"_"
	(format nil "~{~a~^|~}"
		(mapcar (lambda (a)
			  (if (not (null (cdr a)))
			      (format nil "~a=~a" (car a) (cdr a))
			      (format nil "~a" (car a))))
			as)))))


(defun fix (sentences)
  (loop for s in sentences
	do (loop for tk in (sentence-tokens s)
		 do (let ((hmisc (f2hash (token-misc tk)))
			  (hfeat (f2hash (token-feats tk))))
		      (if (gethash "MWEPOS" hmisc)
			  (progn (setf (gethash "ExtPos" hfeat)
				       (gethash "MWEPOS" hmisc))
				 (remhash "MWEPOS" hmisc)))
		      (if (gethash "MWE" hmisc)
			  (remhash "MWE" hmisc))
		      (setf (token-misc tk)  (hash2f hmisc)
			    (token-feats tk) (hash2f hfeat))))
	finally (return sentences)))


(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

