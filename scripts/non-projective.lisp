(ql:quickload :cl-conllu)

(in-package :conllu.user)

(defun get-problems ()
  (let* ((codes (with-open-file (in "non-projective.txt")
		  (mapcar #'symbol-name
			  (loop for c = (read in nil nil)
				while c
				collect c))))
	 (sents1 (remove-if-not (lambda (s)
				  (member (sentence-id s) codes :test #'equal))
				(read-conllu "../documents/"))))
    (write-conllu sents1 #P"non-projective.conllu")))


#|

for apply the changes, we can use the modify commandline interface. 

grep sent_id non-projective.conllu | head -11 | awk '{print $4}'

test='CF0593 CF0594 CF0595 CF0597 CF0598 CF0599 CF0602 CF0603 CF0605 CF0607 CF0608'; for f in ${=test}; do ~/quicklisp/local-projects/cl-conllu/run-SBCL.lisp modify ../documents/$f.conllu non-projective.conllu > ../documents/$f.new; done

for f in *.new; do mv $f $(basename $f .new).conllu; done

|#



