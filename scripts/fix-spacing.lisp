(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list collect (list x y)))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun fix (sentences)
  (dolist (s sentences)
    (dolist (tks (collect-next-element (sentence-tokens s)))
      (let ((t1 (first tks))
	    (t2 (second tks)))
	(when (and t1 t2)
	  (let ((f1 (token-form t1))
		(f2 (token-form t2)))
            (when (and (alexandria:ends-with-subseq "-" f1)
                       (search (concatenate 'string f1 f2) (sentence-meta-value s "text")))
              (setf (token-misc t1) (append-feature "SpaceAfter" "No" (token-misc t1)))))))))
  sentences)

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

