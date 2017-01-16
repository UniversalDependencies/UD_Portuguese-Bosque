;; File for improving our dataset using information from the other one
;; e.g. putting multiword tokens in our files

;; See which contractions exist in order to put multiwork tokens where we can't just map directly

;; Perhaps use metadata `text` in order to check if the sentence needs multitokens?

;; (defun execute ()
;;   (sanity-test) ;;

(load "mapping.lisp")

(defun sanity-test ()
  ;; Do distance 0 matches indeed have the same tokens?
  ;; Yes! sanity.report ends up empty
  (with-open-file (out "sanity.report" :direction :output :if-exists :supersede)
    (mapc (lambda (file-new)
	    (let ((new-sentences-list (read-conllu file-new))
		  (old-sentences-list
		   (read-conllu
		    (make-pathname
		     :directory (append (pathname-directory file-new) (list "old"))
		     :name (pathname-name file-new)
		     :type (pathname-type file-new)))))
	      (mapc (lambda (new-sentence old-sentence)
		      (let ((distance (sentence-meta-value old-sentence "distance_from_new")))
			(when (and
			       (equal distance 0)
			       (not (equal
				    (mapcar (lambda (tk) (token-form tk))
					    (sentence-tokens new-sentence))
				    (mapcar (lambda (tk) (token-form tk))
					    (sentence-tokens old-sentence)))))
			  (format out
				  "~a~%Distance: ~a~%~a~%~a~%~%"
				  (sentence-meta-value new-sentence "sent_id")
				  distance
				  (sentence->text new-sentence)
				  (sentence->text old-sentence)))))
		    new-sentences-list
		    old-sentences-list)))
	  (directory #P"../documents/*.conllu"))))
	  
(defun import-mtokens-from-0-distance ()
  (mapc (lambda (file-new)
	  (let ((new-sentences-list (read-conllu file-new))
		(old-sentences-list (read-conllu
				     (make-pathname
				      :directory (append (pathname-directory file-new) (list "old"))
				      :name (pathname-name file-new)
				      :type (pathname-type file-new)))))
	    (mapc (lambda (new-sentence old-sentence)
		    (let ((distance (sentence-meta-value old-sentence "distance_from_new")))
		      (when (and
			     (equal distance 0)
			     (equal ;; Redundant, because sanity test was ok! We can take out this clause
			      (mapcar (lambda (tk) (token-form tk))
				      (sentence-tokens new-sentence))
			      (mapcar (lambda (tk) (token-form tk))
				      (sentence-tokens old-sentence))))			  
			(mapc
			 (lambda (mtk)
			   (unless (find-if (lambda (x)
					      (and
					       (eq (mtoken-start x) (mtoken-start mtk))
					       (eq (mtoken-end x) (mtoken-end mtk))))
					    (sentence-mtokens new-sentence))
			     (push mtk (sentence-mtokens new-sentence))))
			 (sentence-mtokens old-sentence)))))
		  new-sentences-list
		  old-sentences-list)
	    (write-conllu new-sentences-list
			  (make-pathname
			   :directory (append (pathname-directory file-new) (list "modified"))
			   :name (pathname-name file-new)
			   :type (pathname-type file-new)))))
	(directory #P"../documents/*.conllu")))
