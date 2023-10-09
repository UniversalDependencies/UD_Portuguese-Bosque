
;; At this moment, gets "easy" multitoken words from the "old" corpus
;; and generates multitoken words by itself from Palavras' tags for
;; contractions

;; README:
;; Call (run-scripts) in order to import MWE.
;; Call (find-unimported-mwe) to see what is missing

;; Brainstorming: See which contractions exist in order to put
;; multiwork tokens where we can't just map directly Perhaps use
;; metadata `text` in order to check if the sentence needs
;; multitokens? It may be difficult.


(load "mapping.lisp")

(defun sanity-test ()
  ;; Do distance 0 matches indeed have the same tokens?
  ;; Error in CF8-1 at "old" corpus, but that's it.
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
		      (let ((distance (parse-integer (sentence-meta-value old-sentence "distance_from_new")
						     :junk-allowed t)))
			(if distance
			    ;; Distance may be null if new-sentence is single
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
				      (sentence->text old-sentence))))))
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
		    (let ((distance (parse-integer (sentence-meta-value old-sentence "distance_from_new")
						   :junk-allowed t)))
		      (if (and
			     (equal distance 0)
			     (equal ;; Redundant, because sanity test was ok! We can delete this form
			      (mapcar (lambda (tk) (token-form tk))
				      (sentence-tokens new-sentence))
			      (mapcar (lambda (tk) (token-form tk))
				      (sentence-tokens old-sentence))))			  
			(insert-mtokens new-sentence (sentence-mtokens old-sentence)))))
		  new-sentences-list
		  old-sentences-list)
	    (write-conllu new-sentences-list file-new)))
	(directory #P"../documents/*.conllu")))

(defun import-mwe-if-same-tokens ()
  ;; If there is a mtoken which corresponds to tokens t1, ..., tn, and these tokens "are" in new-sentence,
  ;; then we import them (if there's a token whose id and form are the same, we say the token from 'old sentence' "is" there)
  (let ((accent-substitution-list '(("á" "a") ("é" "e") ("í" "i") ("ó" "o") ("ú" "u")
				    ("â" "a") ("ê" "e") ("ô" "o")
				    ("à" "a")
				    ("Á" "A") ("É" "E") ("Í" "I") ("Ó" "O") ("Ú" "U")
				    ("Â" "A") ("Ê" "E") ("Ô" "O")
				    ("À" "A"))))
    (mapc (lambda (file-new)
	  (let ((new-sentences-list (read-conllu file-new))
		(old-sentences-list (read-conllu
				     (make-pathname
				      :directory (append (pathname-directory file-new) (list "old"))
				      :name (pathname-name file-new)
				      :type (pathname-type file-new)))))
	    (mapc (lambda (new-sentence old-sentence)
		    (mapc (lambda (mtk)
			    (let ((found? nil))
			      (dolist (x '(0 1 2 3 -1 -2 -3))
				(unless found?
				  (when (every
					 (lambda (old-tk)
					   (let ((new-tk (find
							  (+ x (token-id old-tk))
							  (sentence-tokens new-sentence)
							  :key 'token-id)))
					     (if new-tk
						 (equal
						  (reduce (lambda (x y) (regex-replace-all (car y) x (cadr y)))
							  accent-substitution-list
							  :initial-value (string-trim "-" (token-form old-tk)))
						  (reduce (lambda (x y) (regex-replace-all (car y) x (cadr y)))
							  accent-substitution-list
							  :initial-value (string-trim "-" (token-form new-tk)))))))
					 (mtoken->tokens old-sentence mtk))
				    (insert-mtoken
				     new-sentence
				     (make-instance 'mtoken
						    :start (+ x (mtoken-start mtk))
						    :end (+ x (mtoken-end mtk))
						    :form (mtoken-form mtk)))
				    (setf found? t))))))
			  (sentence-mtokens old-sentence)))
		  new-sentences-list
		  old-sentences-list)
	    (write-conllu new-sentences-list file-new)))
	(directory #P"../documents/*.conllu"))))

(defun get-contractions ()
  (let ((contraction-list (see-mwe-cases)))
    (mapc (lambda (file-new)
	    (let ((sentence-list (read-conllu file-new)))
	      (mapc (lambda (sentence)
		      (let ((contraction-tokens nil)
			    (contraction-start nil))
			(dolist (tk (sentence-tokens sentence))
			  (if contraction-tokens
			      (and (push (token-form tk) contraction-tokens)
				   (if (member "<-sam>" (split "\\|" (token-xpostag tk)) :test 'equal)
				       (let ((contraction
					      (find (reverse contraction-tokens) contraction-list
						    :key (lambda (x) (getf x :tokens))
						    :test 'equal)))
					 (if contraction
					     (insert-mtoken sentence
							    (make-instance 'mtoken
									   :start contraction-start
									   :end (token-id tk)
									   :form (getf contraction :mtoken))))
					 (setf contraction-tokens nil)
					 (setf contraction-start nil))))
			      (when (member "<sam->" (split "\\|" (token-xpostag tk)) :test 'equal)
				(push (token-form tk) contraction-tokens)
				(setf contraction-start (token-id tk)))))))
		    sentence-list)
	      (write-conllu sentence-list file-new)))
    (directory #P"../documents/*.conllu"))))

(defun find-unimported-mwe ()
  (format t "sent_id ~t mtoken form ~t mtoken start ~t distance ~%")
  (mapc (lambda (file-new)
	  (let ((new-sentences-list (read-conllu file-new))
		(old-sentences-list (read-conllu
				     (make-pathname
				      :directory (append (pathname-directory file-new) (list "old"))
				      :name (pathname-name file-new)
				      :type (pathname-type file-new)))))
	    (mapc (lambda (new-sentence old-sentence)
		    (let ((distance (parse-integer (sentence-meta-value old-sentence "distance_from_new")
						   :junk-allowed t)))
		      (mapc
		       (lambda (mtk)
			 (let ((found? nil))
			   (dolist (y '(0 1 2 3 -1 -2 -3))
			     (unless (or found?
					 (and (find-if (lambda (x)
							 (and
							  (eq (+ y (mtoken-start x))
							      (mtoken-start mtk))
							  (eq (+ y (mtoken-end x))
							      (mtoken-end mtk))))
						       (sentence-mtokens new-sentence))
					      (setf found? t)))))
			   (unless found?
			     (format t "~a~t~a~t~a~t Distance is:~t~a~%"
				     (sentence-meta-value new-sentence "sent_id")
				     (mtoken-form mtk)
				     (mtoken-start mtk)
				     distance))))
		       (sentence-mtokens old-sentence))))
		  new-sentences-list
		  old-sentences-list)))
	(directory #P"../documents/*.conllu"))
  ())

(defun see-mwe-cases ()
  (let ((mwe nil))
    (mapc
     (lambda (file)
       (mapc (lambda (sentence)
	       (mapc (lambda (mtk)
		       (push (list
			      :mtoken (mtoken-form mtk)
			      :tokens (mapcar #'token-form (mtoken->tokens sentence mtk))
			      :file (sentence-meta-value sentence "sent_id"))
			     mwe))
		     (sentence-mtokens sentence)))
	     (read-conllu file)))
     (directory #P"../documents/old/*.conllu"))
    (remove-duplicates mwe :test #'equal :key (lambda (x) (getf x :mtoken)))))

(defun run-scripts ()
  (sanity-test)
  (if (with-open-file (out "sanity.report")
	(read-line out nil))
      (princ "WARNING: Found CONLLu matchings of 0 distance which have different tokens! See sanity.report"))
  (import-mtokens-from-0-distance)
  (import-mwe-if-same-tokens)
  (get-contractions))

    
;; awk '$1 ~ /\-/ { print $2}'  *.conllu | sort | uniq -c
