(ql:quickload :cl-conllu)

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list
            collect (cons x y)))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (sort (append features `(,(if (and key value) 
                                                          (format nil "~a=~a" key value)
                                                          (format nil "~a" value)))) #'string<))))

(defun process (f)
  (let ((sentences (read-conllu f)))
    (dolist (s sentences)
      (let ((current 0)
	    (text (sentence-meta-value s "text")))
	(dolist (tks (collect-next-element (sentence-tokens s)))
	  (let* ((prev (car tks))
		 (tk (cdr tks))
		 (pos (when tk (search (token-form tk) text :start2 current))))
	    (when (and pos (string-equal "PUNCT" (token-upostag tk)) (> pos 0) (not (eq #\space (elt text (- pos 1)))))
	      (setf current (+ (length (token-form tk)) pos))
	      (setf (token-misc prev) (append-feature "SpaceAfter" "No" (token-misc prev))))))))
    sentences))

;; "/home/fcbr/bosque-UD/documents/CF101.conllu"


