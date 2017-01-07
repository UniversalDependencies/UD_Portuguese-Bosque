
(ql:quickload :cl-conllu)
(ql:quickload :cl-store)

(defpackage :test
  (:use :cl :cl-ppcre :cl-conllu :cl-store))

(in-package :test)


(defun range (n)
  (loop for x from 0 to (1- n) collect x))


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
  ;;
  (let ((matching (empty-matching men women)))
    (setq *men* men)
    (setq *women* women)
    (matching-round men matching)))


;; STABLE MATCHING: END


(defun test-matching ()
  (let ((men (list (cons 0 (cons '(0 1 2) '(0 1 2)))
		   (cons 1 (cons '(0 1 2) '(0 1 2)))
		   (cons 2 (cons '(0 2 1) '(0 2 1)))))
	(women (list (cons 0 (make-array 3 :initial-contents '(0 1 2)))
		     (cons 1 (make-array 3 :initial-contents '(0 2 1)))
		     (cons 2 (make-array 3 :initial-contents '(0 1 2))))))
    (stable-matching men women)))


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


(defun execute ()
  (let (distances men women old new)
    (setf old (read-conllu #P"../bosque-ud.conllu"))
    (setf new (read-conllu #P"../../UD_Portuguese/bosque-old.conllu"))
    (print "data loaded")
    (setf distances (difference new old))
    (print "distances matrix computed")
    (setf men (transform-to-mens-preferences distances))
    (setf women (transform-to-womens-preferences distances))
    (stable-matching men women)))
