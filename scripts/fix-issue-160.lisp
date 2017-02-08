(ql:quickload :cl-conllu)

(in-package :cl-conllu)

;; awk '$2 ~ /\-$/ && $4 ~ /VERB/ {vb = $0 ; getline ; if ($4 !~ /PRON/) print vb,"\n",$0,FILENAME }' ../documents/*.conllu

(defun fix-enclisis (sentence)
  (labels ((next-token (tk)
	     (find (1+ (token-id tk))
		   (sentence-tokens sentence)
		   :key #'token-id)))
    (let ((verb-enclisis (remove-if-not (lambda (tk)
					  (and
					   (scan "-$" (token-form tk)) 
					   (equal (token-upostag tk) "VERB")
					   (equal (token-upostag (next-token tk))
						  "PRON")))
					(sentence-tokens sentence))))
      (dolist (tk verb-enclisis)
	(insert-mtoken sentence
		       (make-instance 'mtoken
				      :form (concatenate
					     'string
					     (token-form tk)
					     (token-form (next-token
							  tk)))
				      :start (token-id tk)
				      :end (1+ (token-id tk))))
	(setf (token-form tk)
	      (regex-replace "-$" (token-form tk) "")))
      (if verb-enclisis
	  (values sentence t)
	  (values sentence nil)))))
			 

(defun run ()
  (dolist (fn (directory "documents/C*.conllu"))
    (let* ((path (pathname-directory fn))
	   (fn-out (make-pathname :type "new"
				  :directory path
				  :defaults fn)))
      (write-conllu (mapcar #'fix-enclisis 
			    (read-conllu fn))
		    fn-out))))
