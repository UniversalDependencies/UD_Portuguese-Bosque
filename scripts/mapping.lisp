(ql:quickload :cl-conllu)
(ql:quickload :cl-store)
(ql:quickload :alexandria)

(defpackage :test
  (:use :cl :cl-ppcre :cl-conllu :cl-store))

(in-package :test)

;; (defun matrix-to-preferences-m (matriz)
;;   (destructuring-bind (rows cols)
;;       (array-dimensions matriz)
;;     (let* ((out (make-array (list rows cols) :initial-element nil))
;; 	   (ids (range cols)))
;;       (dotimes (row rows out)
;; 	(mapc (lambda (a b)
;; 		(setf (aref out row b) (car a)))
;; 	      (sort (loop for col from 0 to (1- cols)
;; 			  collect (cons col (aref matriz row col)))
;; 		    #'< :key #'cdr)
;; 	      ids)))))

;; (defun matrix-to-preferences-w (matriz)
;;   (destructuring-bind (rows cols)
;;       (array-dimensions matriz)
;;     (let ((out (make-array (list cols rows) :initial-element nil))
;; 	  (ids (range rows)))
;;       (dotimes (col cols out)
;; 	(mapc (lambda (a b)
;; 		(setf (aref out col (car a)) b))
;; 	      (sort (loop for row from 0 to (1- rows)
;; 			  collect (cons row (aref matriz row col)))
;; 		    #'< :key #'cdr)
;; 	      ids)))))

;; (defun stable-matching (men women)
;;   (do* ((max-m (1- (array-dimension men 0)))
;; 	(max-w (1- (array-dimension women 0)))
;; 	(free (range max-m))
;; 	(count 0 (1+ count))
;; 	(wife (make-hash-table))	; by man
;; 	(husband (make-hash-table))	; by woman
;; 	(proposal (make-hash-table))	; by man
;; 	(a-man (pop free) (pop free))
;; 	(a-woman (aref men a-man (gethash a-man proposal 0))
;; 		 (aref men a-man (gethash a-man proposal 0))))
;;        ((or (null free)
;; 	    ;; (> count 50)
;; 	    (> (gethash a-man proposal 0) max-w))
;; 	(values husband wife))
;;     ;; (format *standard-output* "~%proposals: ~a ~%husband: ~a ~%"
;;     ;; 	    (alexandria:hash-table-alist proposal)
;;     ;; 	    (alexandria:hash-table-alist husband))
;;     (if (not (gethash a-woman husband nil))
;; 	(progn
;; 	  ;; (format *standard-output* "w:~a single accepted m:~a ~%" a-woman a-man)
;; 	  (setf (gethash a-woman husband) a-man
;; 		(gethash a-man wife) a-woman))
;; 	(let ((partner (gethash a-woman husband)))
;; 	  (if (< (aref women a-woman a-man)
;; 		 (aref women a-woman partner))
;; 	      (progn
;; 		;; (format *standard-output* "w:~a changed m:~a -> m:~a ~%" a-woman partner a-man)
;; 		(push partner free)
;; 		(setf (gethash a-woman husband) a-man
;; 		      (gethash a-man wife) a-woman)
;; 		(remhash partner wife))
;; 	      ;; (format *standard-output* "w:~a rejected m:~a ~%" a-woman a-man)
;; 	      )))
;;     (incf (gethash a-man proposal 0))))


(defun transform-to-mens-preference-matrix (matriz)
  (let* ((dim (array-dimensions matriz)) 
	 (out (make-array dim :initial-element nil)))
    (dotimes (row (car dim) out)
      (mapc (lambda (a b)
	      (setf (aref out row b) (car a)))
	    (sort (loop for col from 0 to (1- (cadr dim))
			collect (cons col (aref matriz row col)))
		  #'< :key #'cdr)
	    (range (cadr dim))))))


(defun mens-preference-matrix->mens-preference-list-of-lists (matriz)
  (let* ((dim (array-dimensions matriz)))
    (flet ((preference-list->man
	       (index pref-list)
	     (let ((pointer pref-list))
	       (cons index (cons pointer pref-list)))))
      (loop for row from 0 to (1- (car dim))
	 collect
	   (preference-list->man
	    row
	    (loop for col from 0 to (1- (cadr dim))
	       collect
		 (aref matriz row col)))))))


(defun transform-to-mens-preferences (matriz)
  (mens-preference-matrix->mens-preference-list-of-lists (transform-to-mens-preference-matrix matriz)))


(defun transform-to-womens-preferences (matriz)
  (let* ((dim (array-dimensions matriz)))
    (flet ((preference-array->woman (index pref-array)
	     (cons index pref-array)))
    (loop for col from 0 to (1- (cadr dim))
       collect
	 (preference-array->woman
	  col
	  (make-array (car dim) :initial-contents
		      (loop for row from 0 to (1- (car dim))
			 collect
			    (aref matriz row col))))))))




;; STABLE MATCHING: BEGIN 
;;
;; See: https://en.wikipedia.org/wiki/Stable_marriage_problem
;; 
;; Data structures:
;;
;; We use men as triples (index, current-most-preferred,
;; preference-list). This preference list represents his preference
;; order of woman (a woman is preferred if she happens earlier in the
;; list). current-most-preferred points to sublist of preference-list
;; beginning at the most preferred woman such that the man hasn't been
;; rejected yet
;;
;; For woman we use a pair (index, preference-array), where
;; preference-array is an array such that a man is preferred if his
;; value at the array is *smaller*
;;
;; For matching we can use a pair of arrays, one indexed by man and
;; the other one by woman. For example, for the one indexed by woman:
;; (aref matching i) is woman i's pair. If nil, she's single.
;;
;; If memory is an issue we could use only an array indexed by woman
;; (if a man doesn't occur as any woman's pair, then he's single).

(defparameter *men* nil)
(defparameter *women* nil)

(defun find-person-with-index (index gender-list)
  (loop for person in gender-list
     do (if (eq (car person) index)
	    (return person))))

(defun get-next-proposal-list (man)
  ;; Gets next woman to whom this man will propose (most preferred
  ;; woman that hasn't rejected him yet)
  (find-person-with-index (caadr man) *women*))


(defun prefers? (woman man1 man2)
  ;; Does woman prefer man 1 to man2?  Remember: we are presupposing
  ;; that preferences are strict (no ties) [at least we'd like to,
  ;; things behave better this way]
  (let ((preferences (cdr woman))
	(man1-index (car man1))
	(man2-index (car man2)))
    (or (null man2) (if man1 (< (aref preferences man1-index)
				(aref preferences man2-index))))))


(defun get-proponent (proposing-men)
  ;; Gets next man yet to propose. We could use, for example, a FIFO
  ;; or FILO structure proposing man: single men not yet rejected by
  ;; every woman (acceptable by him)
  (car proposing-men))


(defun remaining-proposing-men (proposing-men)
  ;; What remains of proposing-men structure after removing the
  ;; proponent for that round
  (cdr proposing-men))


(defun insert-proponent (man proposing-men)
  ;; Returns proposing-men structure with man inserted if man is
  ;; actually a nil (or a woman) [which may happen if the woman was
  ;; single], returns proposing-men
  (if (or (null man) (null (get-next-proposal-list man)))
      ;; A man won't return to the proponent list if i) he isn't a man
      ;; at all; or ii) he has already proposed to every (acceptable)
      ;; woman
      proposing-men
      (cons man proposing-men)))


(defun update-proposal-list! (man1)
  ;; This function is called when man1 is rejected by his
  ;; most-preferred-woman-that-hasn't-rejected-him-yet.  We just need
  ;; to move one step down in his preference order Returns man
  (if man1
      (let ((current-most-preferred (cadr man1)))
	(progn
	  (setf (cadr man1) (cdr current-most-preferred))
	  man1))))


(defun get-womans-match (woman matching)
  ;; Gets woman's pair in this matching
  ;; If there's none, will return nil [or herself?]
  (let ((womans-match-index (aref (cdr matching) (car woman))))
    (if womans-match-index
	(find-person-with-index womans-match-index *men*))))


(defun update-matching! (woman1 man2 matching)
  ;; Updates matching in order to make woman1 matched to man2.  NOTE:
  ;; don't forget to update woman1's current pair (in order to make
  ;; him single) -- man2 will always be single Returns updated
  ;; matching
  (let* ((woman1-index (car woman1))
	 (man2-index (car man2))
	 (woman1-current-match (get-womans-match woman1 matching))
	 (woman1-current-match-index (car woman1-current-match)))
    (progn
      (setf (aref (car matching) man2-index) woman1-index)
      (setf (aref (cdr matching) woman1-index) man2-index)
      (if woman1-current-match
	  (setf (aref (car matching) woman1-current-match-index) nil))
      matching)))


(defun matching-round (proposing-men matching)
  ;; Each call to this function corresponds to a round of the "matching game".
  ;; Input:
  ;;   proposing-men: single men not yet rejected by every woman (acceptable by him)
  ;;   women
  ;;   matching: matching as decided until this point (to be updated)
  ;; Returns:
  ;;   updated proposing-men
  ;;   updated matching
  ;; (actually a list '(updated-proposing-men updated-matching))
  (let* ((proponent (get-proponent proposing-men))
	 (candidate (get-next-proposal-list proponent))
	 (candidate-pair (get-womans-match candidate matching)))
    ;; NOTE:
    ;; - proponent and candidate-pair are men
    ;; - candidate is a woman
    ;; (print "proposing-men / matching") ;; Prints each round
    ;; (print (cons proposing-men matching))
    (if (prefers? candidate proponent candidate-pair)
	(progn
	  (update-matching! candidate proponent matching)
	  (update-proposal-list! candidate-pair)
	  (let ((updated-proposing-men
		 (insert-proponent candidate-pair (remaining-proposing-men proposing-men))))
	    (if (null updated-proposing-men)
		matching
		(matching-round updated-proposing-men matching))))
	;; candidate-pair was rejected my candidate in favour of
	;; proponent, so he returns to the candidate-pair structure
	(progn
	  (update-proposal-list! proponent)
	  (let ((updated-proposing-men
		 (insert-proponent proponent (remaining-proposing-men proposing-men))))
	    (if (null updated-proposing-men)
		matching
		(matching-round updated-proposing-men matching)))))))
;; If proponent was rejected, he'll still be at proposing-men, unless
;; he was already rejected by every (acceptable) woman


(defun empty-matching (men women)
  ;; Returns an empty matching
  (let* ((number-of-men (length men))
	 (number-of-women (length women))
	 (mens-matches (make-array number-of-men :initial-element nil))
	 (womens-matches (make-array number-of-women :initial-element nil)))
    (cons mens-matches womens-matches)))


(defun stable-matching (men women)
  ;; Gale-Shapley algorithm for stable matchings
  (let ((matching (empty-matching men women)))
    (setq *men* men)
    (setq *women* women)
    (matching-round men matching)))


;; STABLE MATCHING: END




(defun difference (new old)
  (let ((filename #P"mapping.dump"))
    (if (probe-file filename)
	(cl-store:restore filename)
	(cl-store:store (diff new old 
			      :test (lambda (a b)
				      (levenshtein a b :test #'equal))
			      :key (lambda (sent)
				     (let ((data (mapcar (lambda (tk) (token-form tk))
							 (sentence-tokens sent))))
				       (make-array (length data) :initial-contents data))))
			filename))))


(defun create-new-sent-id->old-sentence (matching old new distances)
  (let ((new-sent-id->old-sentence-text (make-hash-table :test 'equal))
	(matching-array (car matching))
	(old-array (make-array (length old) :initial-contents old))
	(new-array (make-array (length new) :initial-contents new))) 
    (dotimes (x (length matching-array) new-sent-id->old-sentence-text)
      (setf (gethash (sentence-meta-value (aref new-array x) "sent_id")
		     new-sent-id->old-sentence-text)
	    (if (aref matching-array x)
		(list
		 (aref old-array (aref matching-array x))
		 (aref distances x (aref matching-array x))))))))


(defun write-matched-old-files (tb)
  (dolist (fn (directory (make-pathname :name :wild :type "conllu"
					:defaults #P"../documents/")))
    (write-conllu (mapcar (lambda (sentence)
			    (let* ((id (sentence-meta-value sentence "sent_id"))
				   (ns (or (car (gethash id tb))
					   (make-instance 'sentence)))
				   (dist (or (cadr (gethash id tb))
					     nil)))
			      (push (cons "new_sent_id" id) (sentence-meta ns))
			      (push (cons "distance_from_new" dist) (sentence-meta ns))
			      ns))
			  (read-conllu fn))
		  (make-pathname :directory (append (pathname-directory fn) (list "old"))
				 :defaults fn))))


(defun find-singles (matching)
  (let ((partner (if (> (length (car matching)) (length (cdr matching)))
		     (car matching)
		     (cdr matching))))
    (loop for x below (length partner)
	  when (null (aref partner x)) 
	  collect x)))


(defun write-single (matching new)
  (write-conllu (mapcar (lambda (idx) (nth idx new)) (find-singles matching))
		"../documents/old/single.conllu"))


(defun report-match (new old distances matching)
  (let* ((hist (make-hash-table))
	 (data (mapcar (lambda (v)
			 (let ((m (nth v new))
			       (wix (aref (car matching) v))
			       (w nil)
			       (wid nil)
			       (ws nil)
			       (d -1))
			   (if wix
			       (setf w   (nth wix old)
				     wid (cl-conllu:sentence-meta-value w "sent_id")
				     ws  (sentence->text w)
				     d   (aref distances v wix)))
			   (incf (gethash d hist 0))
			   (list v wix (cl-conllu:sentence-meta-value m "sent_id")
				 wid d
				 (sentence->text m) ws)))
		       (range (1- (length (car matching)))))))
    (with-open-file (out "mapping.report" :direction :output :if-exists :supersede)
      (format out "~:{[~a ~a ~a ~a ~a]~%~a~%~a~%~%~}"
	      (sort (remove-if (lambda (obj) (= (nth 4 obj) 0)) data)
		    #'> :key (lambda (obj) (nth 4 obj))))
      (format out "~a~%~a ~a~%~%" (alexandria:hash-table-alist hist) (length new) (length old)))))



(defun fix-old-ids (prefix sentences)
  (mapc (lambda (s) 
            (setf (cdr (assoc "sent_id" (sentence-meta s) :test #'equal))
                  (format nil "~a#~a" prefix (sentence-meta-value s "sent_id")))) 
	sentences))


(defun prepare-files ()
  (let ((path-old #P"~/work/UD_Portuguese/")
	(path-new #P"~/work/bosque-UD/documents/"))
    (write-conllu
	   (reduce (lambda (a b)
		     (append a (fix-old-ids b (read-conllu (merge-pathnames path-old b)))))
		   '("pt-ud-dev.conllu" "pt-ud-test.conllu" "pt-ud-train.conllu")
		   :initial-value nil)
	   "bosque-old.conllu")
    (write-conllu
	   (reduce (lambda (a b)
		     (append a (read-conllu b)))
		   (directory (merge-pathnames path-new "*.conllu"))
		   :initial-value nil)
	   "bosque-new.conllu")))


(defun execute ()
  (let* ((new (read-conllu #P"bosque-new.conllu")) ; man
	 (old (read-conllu #P"bosque-old.conllu")) ; woman
	 (distances (difference new old))
	 (men (transform-to-mens-preferences distances))
	 (women (transform-to-womens-preferences distances))
	 (matching (stable-matching men women))
	 (tb (create-new-sent-id->old-sentence matching old new distances)))
    (report-match new old distances matching)
    (write-matched-old-files tb)
    (write-single matching new)))

