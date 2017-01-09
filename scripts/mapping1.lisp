
(ql:quickload :cl-ppcre)
(ql:quickload :cl-conllu)
(ql:quickload :cl-store)
(ql:quickload :alexandria)

(defpackage :test
  (:use :cl :cl-ppcre :cl-conllu :cl-store))

(in-package :test)


(defun range (n)
  (loop for x from 0 to (1- n) collect x))


(defun matrix-to-preferences-m (matriz)
  (destructuring-bind (rows cols)
      (array-dimensions matriz)
    (let* ((out (make-array (list rows cols) :initial-element nil))
	   (ids (range cols)))
      (dotimes (row rows out)
	(mapc (lambda (a b)
		(setf (aref out row b) (car a)))
	      (sort (loop for col from 0 to (1- cols)
			  collect (cons col (aref matriz row col)))
		    #'< :key #'cdr)
	      ids)))))


(defun matrix-to-preferences-w (matriz)
  (destructuring-bind (rows cols)
      (array-dimensions matriz)
    (let ((out (make-array (list cols rows) :initial-element nil))
	  (ids (range rows)))
      (dotimes (col cols out)
	(mapc (lambda (a b)
		(setf (aref out col (car a)) b))
	      (sort (loop for row from 0 to (1- rows)
			  collect (cons row (aref matriz row col)))
		    #'< :key #'cdr)
	      ids)))))



;; STABLE MATCHING: BEGIN 
;;
;; See: https://en.wikipedia.org/wiki/Stable_marriage_problem

(defun stable-matching (men women)
  (do* ((max-m (1- (array-dimension men 0)))
	(max-w (1- (array-dimension women 0)))
	(free (range max-m))
	(count 0 (1+ count))
	(wife (make-hash-table))	; by man
	(husband (make-hash-table))	; by woman
	(proposal (make-hash-table))	; by man
	(a-man (pop free) (pop free))
	(a-woman (aref men a-man (gethash a-man proposal 0))
		 (aref men a-man (gethash a-man proposal 0))))
       ((or (null free)
	    ;; (> count 50)
	    (> (gethash a-man proposal 0) max-w))
	(values husband wife))
    ;; (format *standard-output* "~%proposals: ~a ~%husband: ~a ~%"
    ;; 	    (alexandria:hash-table-alist proposal)
    ;; 	    (alexandria:hash-table-alist husband))
    (if (not (gethash a-woman husband nil))
	(progn
	  ;; (format *standard-output* "w:~a single accepted m:~a ~%" a-woman a-man)
	  (setf (gethash a-woman husband) a-man
		(gethash a-man wife) a-woman))
	(let ((partner (gethash a-woman husband)))
	  (if (< (aref women a-woman a-man)
		 (aref women a-woman partner))
	      (progn
		;; (format *standard-output* "w:~a changed m:~a -> m:~a ~%" a-woman partner a-man)
		(push partner free)
		(setf (gethash a-woman husband) a-man
		      (gethash a-man wife) a-woman)
		(remhash partner wife))
	      ;; (format *standard-output* "w:~a rejected m:~a ~%" a-woman a-man)
	      )))
    (incf (gethash a-man proposal 0))))


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


(defun execute ()
  (let (distances old new)
    (setf new (read-conllu #P"../bosque-ud.conllu"))
    (setf old (read-conllu #P"../../UD_Portuguese/bosque-old.conllu"))
    (print "data loaded")
    (setf distances (difference new old))
    (print "distances computed")
    (stable-matching (matrix-to-preferences-m distances)
		     (matrix-to-preferences-w distances))))

;; (defun distance-list (a-hash len filename)
;;   ;; From a matching hash (let's say, `wife`), saves in a filenamed `filename` a list of distance values in the matching
;;   (loop for i below 

(defun find-singles (partners-hash length)
  (let ((singles ()))
    (dotimes (x length)
      (if (null (gethash x partners-hash))
	  (push x singles)))
    singles))
