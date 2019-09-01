
(defun get-data (filename &optional (stream *standard-output*))
  (let ((doc (cxml:parse filename (stp:make-builder))))
    (xpath:do-node-set (node (xpath:evaluate "//TIMEX3" doc))
      (format stream "~a|~a|~a|~a|~a~%"
	      (xpath-protocol:local-name node)
	      (cxml-stp:attribute-value node "tid")
	      (cxml-stp:attribute-value node "type")
	      (cxml-stp:attribute-value node "value")
	      (cxml-stp:data (cxml-stp:nth-child 0 node))))))


(with-open-file (out "terms.txt" :direction :output)
  (let ((*standard-output* out))
    (mapcar #'get-data (directory "*.xml"))))

