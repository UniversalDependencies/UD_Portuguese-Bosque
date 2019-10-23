(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun search-mtoken (start end mtokens)
  (find (cons start end) mtokens :test #'equal))

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun get-forms (start end sentence)
  (mapcar #'token-form 
          (mapcar (lambda (x) (find-token x (sentence-tokens sentence)))
                  (alexandria:iota (1+ (- end start)) :start start))))

(defun collect-next-element (list)
  (loop for (x y z)
     on list collect (list x y z)))

(defparameter *mt* nil)

(defun read-mtokens (mt-file)
  (setf *mt* nil)
  (with-open-file (stream mt-file)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (push (list (split-sequence #\space (car (split-sequence #\, line)))
		  (cadr (split-sequence #\, line))) *mt*))))

(defun fix (sentences)
  (dolist (s sentences)
    (dolist (tks (collect-next-element (sentence-tokens s)))
      (let ((t1 (first tks))
	    (t2 (second tks))
            (t3 (third tks)))
	(when (and t1 t2 t3)
	  (let ((i1 (token-id t1))
		(i2 (token-id t2)))
	    (dolist (mt *mt*)
              ;; (format t "~a ~a~%" (second mt) (token-form t3))
	      (when (and (string= (token-form t1)
				  (first (first mt)))
			 (string= (token-form t2)
				  (second (first mt)))
			 (search (format nil "~a ~a" (second mt) (token-form t3)) (sentence-meta-value s "text"))
			 (not (search-mtoken i1 i2 (sentence-mtokens s))))
                (setf (sentence-mtokens s) 
                      (append (sentence-mtokens s)
                              (list (make-instance 'mtoken
                                                   :start i1
                                                   :end i2
                                                   :form (second mt)))))))))))
    (sort (sentence-mtokens s) #'< :key #'mtoken-start))
  sentences)

(defun run ()
  (read-mtokens #p"scripts/mt.csv")
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

