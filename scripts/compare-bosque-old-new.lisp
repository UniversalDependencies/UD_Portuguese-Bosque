(defparameter new (cl-conllu:read-conllu "bosque-ud.conllu"))
(defvar new-array (make-array (length new) :initial-contents new))
(defparameter old (cl-conllu:read-conllu "bosque-old.conllu"))
(defvar old-array (make-array (length old) :initial-contents old))
(defparameter pairs (open "pair.txt"))
(with-open-file (str "compare-sentences.txt"
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	     (let ((n (read pairs)))
	   (dotimes (x n)
	     (let* ((new-id (read pairs))
		   (old-id (read pairs))
		    (new-text (cl-conllu:sentence->text (aref new-array new-id)))
		    (old-text (cl-conllu:sentence->text (aref old-array old-id)))
		    (old-sent-id (cdr (assoc "sent_id" (cl-conllu:sentence-meta (aref old-array old-id)) :test 'equal)))
		    (new-sent-id (cdr (assoc "sent_id" (cl-conllu:sentence-meta (aref new-array new-id)) :test 'equal))))
	       (format str "~a: ~a~%~%" old-sent-id old-text)
	       (format str "~a: ~a~%~%~%" new-sent-id new-text)))))

