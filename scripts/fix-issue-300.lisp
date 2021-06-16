
(ql:quickload :cl-conllu)
(ql:quickload '(:alexandria :cl-ppcre :cl-fad))

(in-package :conllu.user)


;; (when tokens
;;       (let* ((t1 (car tokens))
;;              (t2 (cdr tokens)))
;; 	(when (and 
;;                t1 t2
;;                (string-equal (slot-value t1 'lemma) "não")
;;                (string-equal (slot-value t2 'lemma) "só")
;;                (string-equal (slot-value t1 'misc) "MWE=não_só"))

;; 	  (setf (slot-value t1 'upostag) "ADV")
;; 	  (setf (slot-value t1 'xpostag) "_")

;; 	  (setf (slot-value t2 'deprel) "fixed")
;; 	  (setf (slot-value t2 'head) (slot-value t1 'id))

;; 	  (setf (slot-value t1 'misc) (format nil "MWE=não_só|MWEPOS=ADV")))))

(defun is (a v)
  (equal (string-downcase a) v))

(defun fix-sentence1 (s flag)
  (loop for tk in (sentence-tokens s)
	for tf = (token-hash-features tk)
	for p = (token-parent tk s)	
	for pf = (when p (token-hash-features p))
	when (and p pf
		  (cl-ppcre:scan "^[AaOo]s?$" (token-form tk))
		  (equal "DET" (token-upostag tk))
		  (equal "NOUN" (token-upostag p))
		  (not (gethash "Typo" tf))
		  (or (not (gethash "Gender" tf))
		      (not (gethash "Number" tf))))
	  do (progn
	       (cond
		 ((is (token-form tk) "as")
		  (token-add-feature tk "Gender" "Fem")
		  (token-add-feature tk "Number" "Plur"))

		 ((is (token-form tk) "a")
		  (token-add-feature tk "Gender" "Fem")
		  (token-add-feature tk "Number" "Sing"))

		 ((is (token-form tk) "os")
		  (token-add-feature tk "Gender" "Masc")
		  (token-add-feature tk "Number" "Plur"))

		 ((is (token-form tk) "o")
		  (token-add-feature tk "Gender" "Masc")
		  (token-add-feature tk "Number" "Sing")))
	       (setf flag t))
	finally (return (values s flag))))


(defun fix-sentence2 (s)
  (loop for tk in (sentence-tokens s)
	for tf = (feats-to-hash (token-feats tk))
	for p = (token-parent tk s)	
	for pf = (when p (feats-to-hash (token-feats p)))
	when (and p pf
		  (cl-ppcre:scan "^[AaOo]s?$" (token-form tk))
		  (equal "DET" (token-upostag tk))
		  (equal "NOUN" (token-upostag p))
		  (or (not (equal (gethash "Gender" tf) (gethash "Gender" pf)))
		      (not (equal (gethash "Number" tf) (gethash "Number" pf)))))
	  do (format t "~a ~a ~a~%~a ~a~%~%" (sentence-meta-value s "sent_id")
		     tk (alexandria:hash-table-alist tf)
		     p (alexandria:hash-table-alist pf))))


(defun fix-file (file)
  (let ((sentences (read-conllu file))
	(changed nil))
    (dolist (s sentences)
      (multiple-value-bind (s flag)
	  (fix-sentence1 s changed)
	(declare (ignore s))
	(if flag
	    (setf changed t))))
    (if changed
	(write-conllu sentences file))))

(defun run (path)
  (mapc #'fix-file (directory path)))
