
;; To be added to mapping.lisp
;;
;; STABLE MATCHING: BEGIN - STILL NOT WORKING
;;
;; See: https://en.wikipedia.org/wiki/Stable_marriage_problem
;; 
;;
;;
;;
;; Data structures:
;; We could use men as triples (index, current-most-preferred, preference-list). This preference list represents his preference order of woman (a woman is preferred if she happens earlier in the list). current-most-preferred points to sublist of preference-list beginning at the most preferred woman such that the man hasn't been rejected yet
;; For woman we could use a pair (index, preference-array), where preference-array is an array such that a man is preferred if his value at the array is *smaller*
;; For matching we can use a pair of arrays, one indexed by man and the other one by woman. For example, for the one indexed by woman: (aref matching i) is woman i's pair. If nil, she's single.
;; If memory is an issue we could use only an array indexed by woman (if a man doesn't occur as any woman's pair, then he's single).

(defun stable-matching (men women)
  ;; Gale-Shapley algorithm for stable matchings
  ;;
  (let (matching (empty-matching men women))
    (matching-round men women matching)))

(defun matching-round (proposing-men women matching)
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
	 (candidate-pair (get-womans-match candidate matching 'woman)))
    ;; NOTE:
    ;; - proponent and candidate-pair are men
    ;; - candidate is a woman
    (if (prefers? candidate proponent candidate-pair)
	(progn
	  (update-matching! candidate proponent matching)
	  (update-proposal-list! candidate-pair)
	  (if (eq proposing-men nil)
	      matching
	      (matching-round (insert-proponent candidate-pair (remaining-proposing-men proposing-men)) women matching))) ; candidate-pair was rejected my candidate in favour of proponent, so he returns to the candidate-pair structure
	(progn
	  (update-proposal-list! proponent)
	  (if (eq proposing-men nil)
	      matching
	      (matching-round proposing-men women matching) ;; If proponent was rejected, he'll still be at proposing-men
	      )))))

(defun get-proponent (proposing-men)
  ;; Gets next man yet to propose. We could use, for example, a FIFO or FILO structure
  ;; proposing man: single men not yet rejected by every woman (acceptable by him)
  (car proposing-men))

(defun remaining-proposing-men (proposing-men)
  ;; What remains of proposing-men structure after removing the proponent for that round
  (cdr proposing-men))

(defun insert-proponent (man proposing-men)
  ;; Returns proposing-men structure with man inserted
  ;; if man is actually a nil (or a woman) [which may happen if the woman was single], returns proposing-men
  (cons man proposing-men))

(defun get-next-proposal-list (man)
  ;; Gets next woman to whom this man will propose (most preferred woman that hasn't rejected him yet)
  (car (car (cdr man)))
  )

(defun update-proposal-list! (man1)
  ;; This function is called when man1 is rejected by his most-preferred-woman-that-hasn't-rejected-him-yet.
  ;; We just need to move one step down in his preference order
  ;; Returns man
  (let ((current-most-preferred (car (cdr man1))))
    (progn
      (setf (car (cdr man1)) (cdr current-most-preferred))
      man1))
  )

(defun prefers? (woman man1 man2)
  ;; Does woman prefer man 1 to man2?
  ;; Remember: we are presupposing that preferences are strict (no ties) [at least we'd like to, things behave better this way]
  (let ((preferences (car woman))
	(man1-index (car man1))
	(man2-index (car man2)))
    (> (aref preferences man1-index) (aref preferences man2-index)))
  )

(defun empty-matching (men women)
  ;; Returns an empty matching
  (let* ((number-of-men (length men))
	 (number-of-women (length women))
	 (mens-matches (make-array number-of-men :initial-element nil))
	 (womens-matches (make-array number-of-women :initial-element nil)))
    (cons mens-maches womens-matches)))

(defun get-womans-match (woman matching)
  ;; Gets woman's pair in this matching
  ;; If there's none, will return nil [or herself?]
  (aref (cdr matching) (car woman)))

(defun update-matching! (woman1 man2 matching)
  ;; Updates matching in order to make woman1 matched to man2.
  ;; NOTE: don't forget to update woman1's current pair (in order to make him single) -- man2 will always be single
  ;; Returns updated matching
  (let* ((woman1-index (car woman1))
	 (man2-index (car man2))
	 (woman1-current-match (get-womans-match woman1))
	 (woman1-current-match-index (car woman1-current-match)))
    (progn
      (setf (aref (car matching) man2-index) woman1-index)
      (setf (aref (cdr matching) woman1-index) man2-index)
      matching)))

;; STABLE MATCHING: END
