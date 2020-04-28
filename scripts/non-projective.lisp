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

report.err was produced with

cd ~/work/ud-tools
sh proc-bosque.sh 2> bosque.err
cat bosque.err | grep -v PASSED | grep Line |  awk '{print $4}' | sort | uniq -c > report.err

|#


(defparameter *report*
  (let* ((corpus (read-conllu "~/work/ud-bosque/documents/"))
	 (report (with-open-file (in "~/work/ud-tools/report.err")
		   (loop for e1 = (read in nil nil)
			 for e2 = (read in nil nil)
			 while (and e1 e2)
			 collect (cons e1 e2)))))
    (mapcar (lambda (p) 
	      (let ((obj (find-if (lambda (s) 
				    (equal (sentence-meta-value s "sent_id")
					   (symbol-name (cdr p))))
				  corpus)))
		(list (car p) obj (length (sentence-tokens obj))))) 
	    report)))


(remove-if-not (lambda (p) (< (caddr p) 10)) *report*)
(sort *report* #'< :key #'caddr)

