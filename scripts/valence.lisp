
(ql:quickload :cl-conllu)

(in-package :conllu.user)

(defun get-table (sents)
  (loop for s in sents
	with deps = (list "conj" "advmod" "cc" "parataxis")
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



(defun report-table (out)
  (mapc
   (lambda (res)
     (destructuring-bind (lemma feats sent args)
	 res
       (format out "~a,~a,~a,~{~a~^ ~}~%" lemma feats sent args)))
   (get-table (cl-conllu:read-conllu "~/work/ud-portuguese-bosque/documents/"))))
