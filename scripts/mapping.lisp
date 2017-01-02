
(ql:quickload :cl-conllu)

;; https://common-lisp.net/project/cl-store/
;; (ql:quickload :cl-store)

(in-package :cl-conllu)

(defparameter old (read-conllu "~/Downloads/UD_Portuguese/bosque-old.conllu"))
(defparameter new (read-conllu "bosque.udep.conll"))

(let ((dmatrix (diff new old 
		     :test (lambda (a b)
			     (distance a b :test #'equal))
		     :key (lambda (sent)
			    (let ((data (mapcar (lambda (tk) (slot-value tk 'form))
						(sentence-tokens sent))))
			      (make-array (length data) :initial-contents data))))))
  (cl-store:store dmatrix #P"data.out"))


;; EOF -- below are only tests

;; (defun range (start end)
;;   (loop for x from start to (1- end) collect x))

;; (defun mapping (matriz new old map)
;;   (let ((tab (alexandria:alist-hash-table (mapcar (lambda (p) (cons (car p) (cadr p))) map)
;; 					  :test #'equal))
;; 	(dim (array-dimensions matriz)))
;;     (assert (and (equal (length new) (car dim))
;; 		 (equal (length old) (cdr dim))))
;;     (mapc (lambda (sent pos)
;; 	    (let* ((similars (sort (loop for x from 0 to (cdr dim)
;; 					 collect (list x (aref matriz pos x)))
;; 				   #'< :key #'cadr))
;; 		   (equiv (car (car similars))))
;; 	      (setf (gethash ))))
;; 	  new (range 0 (length new)))))


;; (defun best-match (alist best rest &key test)
;;   (if (null best)
;;       (best-match (cdr alist) (car alist) rest :test test)
;;       (if (null alist)
;; 	  (values best rest)
;; 	  (if (funcall test (caar alist) (caar best))
;; 	      (best-match (cdr alist) (car alist) (cons best rest))
;; 	      (best-match (cdr alist) best (cons (car alist) rest))))))


;; (defun mapping (new old matriz initial-map map)
;;   (if (null new)
;;       (values map old)
;;       (if (member (sentence-meta-value (cdar new) "sent_id")
;; 		  (mapcar #'car initial-map))
;; 	  )
;;       (multiple-value-bind (best rest)
;; 	  (best-match old nil nil
;; 		      :test (lambda (a b) (< (aref matriz (caar new) a)
;; 					     (aref matriz (caar new) b)))
;; 		      :key #'car)
;; 	(mapping (cdr new) rest matriz (cons (cons (car new) best) map)))))





;;;

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
    (flet ((preference-array->woman
	       (index pref-array)
	     (cons index pref-array)))
    (loop for col from 0 to (1- (cadr dim))
       collect
	 (preference-array->woman
	  col
	  (make-array (car dim) :initial-contents
		      (loop for row from 0 to (1- (car dim))
			 collect
			   (aref matriz row col))))))))

(defun execute ()
  (let* ((distance-matrix (cl-store:restore "data.out"))
	 (men (transform-to-mens-preferences distance-matrix))
	 (women (transform-to-womens-preferences distance-matrix)))
    (stable-matching men women)))

(defun test ()
  (let* ((dim '(3 4))
	 (matriz (make-array dim :initial-contents
			      '((4 2 7 10)
				(3 1 0 4)
				(5 10 9 1)) )))
    ;; (mens-preference-matrix->mens-preference-list-of-lists (transform-preferences matriz))))
    (transform-to-womens-preferences matriz)))
