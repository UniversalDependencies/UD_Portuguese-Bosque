
;; To be added to mapping.lisp
;;
;; STABLE MATCHING: BEGIN - STILL NOT WORKING

;; Data structures:
;; We could use men as triples (index, current-index-at-preference-list, preference-list). This preference list represents his preference order of woman (a woman is preferred if she happens earlier in the list)
;; For woman we could use a pair (index, preference-array), where preference-array is an array such that a man is preferred if his value at the array is *smaller*
;; For matching we can use a pair of arrays, one indexed by man and the other one by woman. For example, for the one indexed by woman: (aref matching i) is woman i's pair. If nil, she's single.
;; If memory is an issue we could use only an array indexed by woman (if a man doesn't occur as any woman's pair, then he's single).

(defun stable-matching (men women)
  ;; Gale-Shapley algorithm for stable matchings
  ;;
  (matching-round men women matching))

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
	 (candidate-pair (get-match candidate matching)))
    ;; NOTE:
    ;; - proponent and candidate-pair are men
    ;; - candidate is a woman
    (if (prefers? candidate proponent candidate-pair)
	(progn
	  (update-matching! candidate proponent matching)
	  (if (eq proposing-men nil)
	      matching
	      (matching-round (insert-proponent candidate-pair proposing-men) women matching))) ; candidate-pair was rejected my candidate in favour of proponent, so he returns to the candidate-pair structure
	(progn
	  (update-proposal-list! proponent)
	  (if (eq proposing-men nil)
	      matching
	      (matching-round proposing-men women matching) ;; If proponent was rejected, he'll still be at proposing-men
	      )))))

(defun get-proponent (proposing-men)
  ;; Gets next man yet to propose. We could use, for example, a FIFO or FILO structure
  ;; proposing man: single men not yet rejected by every woman (acceptable by him)
  ...)

(defun insert-proponent (man proposing-men)
  ;; Returns proposing-men structure with man inserted
  ;; if man is actually a nil (or a woman) [which may happen if the woman was single], returns proposing-men
  ...)

(defun get-next-proposal-list (man)
  ;; Gets next woman to whom this man will propose (most preferred woman that hasn't rejected him yet)
  ...)

(defun get-match (person1 matching)
  ;; Gets person1's pair in this matching.
  ;; If there's none, will return nil (or herself?)
  ...)

(defun update-matching! (person1 person2 matching)
  ;; Updates matching in order to make person1 matched to person2.
  ;; NOTE: don't forget to update person1 and person2's current pair (in order to make them single)
  ...)

(defun update-proposal-list! (man1)
  ;; This function is called when man1 is rejected by his most-preferred-woman-that-hasn't-rejected-him-yet.
  ;; We just need to move one step down in his preference order
  ...)

(defun prefers? (woman man1 man2)
  ;; Does woman prefer man 1 to man2?
  ;; Remember: we are presupposing that preferences are strict (no ties) [at least we'd like to, things behave better this way]
  ...)

;; STABLE MATCHING: END
