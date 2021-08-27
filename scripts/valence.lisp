

(defun get-table ()
  (loop for s in dados
	with deps = (list "conj" "advmod")
	append (loop for tk in (sentence-tokens s)
		     when (equal (token-upostag tk) "VERB")
		       collect (list 
				(token-lemma tk)
				(token-feats tk)
				(sentence-meta-value s "sent_id")
				(mapcar
				 (lambda (tk)
				   (format nil "~a:~a:~a"
					   (token-lemma tk) (token-upostag tk) (token-deprel tk)))
				 (token-children tk s
						 :fn-filter
						 (lambda (tk)
						   (and (not (equal "PUNCT" (token-upostag tk)))
							(not (member (token-deprel tk) deps :test #'equal))))))))))



(defun print-table ()
  (with-open-file (out "valencias.csv" :if-exists :supersede :direction :output)
    (mapc
     (lambda (res)
       (destructuring-bind (lemma feats sent args)
	   res
	 (format out "~a,~a,~a,~{~a~^ ~}~%" lemma feats sent args)))
     (get-table))))
