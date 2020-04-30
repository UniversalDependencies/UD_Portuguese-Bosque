
(ql:quickload '(:cl-conllu :alexandria :cl-ppcre))

(defpackage :working
  (:use :cl :cl-conllu :alexandria :cl-ppcre))

(in-package :working)

(defun read-lines (fn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defun parse-report (pathspec)
  (let (result)
    (dolist (fn (directory pathspec) result)
      (let ((lines (read-lines fn)))
	(if (not (equal "*** PASSED ***" (car lines)))
	    (let ((res))
	      (dolist (line lines)
		(multiple-value-bind (start end begins ends)
		    (scan "\\[Line ([0-9]+) Sent (C[FP][0-9]+-[0-9]+) Node [0-9]+\\]: \\[(L[0-9]+) ([a-zA-Z]+) ([A-Za-z-]+)\\]"
			  line)
		  (if (and start end)
		      (push (mapcar (lambda (v)
				      (subseq line (aref begins v) (aref ends v)))
				    (list 0 1 2 3 4))
			    res))))
	      (push (list (pathname-name fn) res) result)))))))


(defun complete-report (entry)
  (let* ((errors (cadr entry))
	 (fn     (make-pathname :type "conllu" :name (car entry)
				:directory '(:absolute :home "work" "ud-bosque" "documents")))
	 (sents  (read-conllu fn))
	 res)
    (mapcar (lambda (e)
	      (let ((s (find (cadr e) sents :test #'equal :key #'sentence-id)))
		(push (list :file (car entry)
			    :sent s
			    :id   (cadr e)
			    :size (length (sentence-tokens s))
			    :line (car e)
			    :emsg (nth 4 e))
		      res)))
	    (remove-duplicates errors :test #'equal :key #'cadr))))


(defun report (&key (filter nil) (as-string nil))
  (let ((res (reduce (lambda (res e)
		       (append res e)) 
		     (mappend #'complete-report
			      (parse-report "reports/validation/*.report"))
		     :initial-value nil)))
    (if as-string
	(progn (mapc (lambda (r)
		       (format t "- file: ~a id: ~8a size: ~3a line: ~3a emsg: ~a~%"
			       (getf r :file) (getf r :id) (getf r :size) (getf r :line) (getf r :emsg)))
		     (if filter (remove-if-not filter res) res))
	       (values))
	res)))


(remove-if-not (lambda (p) (< (caddr p) 10)) *report*)
(sort *report* #'< :key #'caddr)

