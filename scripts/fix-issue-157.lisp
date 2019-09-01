(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun transfer-mtokens (sent-old sent-new)
  (let ((tokens-old (sentence-tokens sent-old))
	(tokens-new (sentence-tokens sent-new)))
    (if (and (equal (sentence-meta-value sent-old "new_sent_id")
		    (sentence-meta-value sent-new "sent_id"))
	     (equal (length tokens-old)
		    (length tokens-new))
	     (every (lambda (t1 t2)
		      (and (equal (token-form t1)
				  (token-form t2))
			   (equal (token-id t1)
				  (token-id t2))))
		    tokens-old tokens-new))
	(progn
	  (setf (sentence-mtokens sent-new)
		(sort (union (sentence-mtokens sent-old)
			     (sentence-mtokens sent-new) :key #'mtoken-start :test #'equal)
		      #'< :key #'mtoken-start))
	  (values sent-new t))
	(values sent-new nil))))


(defun run ()
  (dolist (fn (directory "documents/old/C*.conllu"))
    (let* ((opath (reverse (cdr (reverse (pathname-directory fn)))))
	   (fn-new (make-pathname :directory opath
				  :defaults fn))
	   (fn-out (make-pathname :type "new"
				  :directory opath
				  :defaults fn)))
      (write-conllu (mapcar #'transfer-mtokens 
			    (read-conllu fn)
			    (read-conllu fn-new))
		    fn-out))))


;; for f in *.new; do awk 'BEGIN {NUM=0} $1 ~ /^[0-9]+\-[0-9]+/ {NUM=NUM+1} END {print FILENAME,NUM}' $f >> new.report; done
;; for f in old/*.conllu; do awk 'BEGIN {NUM=0} $1 ~ /^[0-9]+\-[0-9]+/ {NUM=NUM+1} END {print FILENAME,NUM}' $f >> old.report; done
;; awk '$1 ~ /^[0-9]+\-[0-9]+/ {print $2}' old/*.conllu | sort | uniq -c | sort -nr > old.words
;; awk '$1 ~ /^[0-9]+\-[0-9]+/ {print $2}' *.new | sort | uniq -c | sort -nr > new.words
