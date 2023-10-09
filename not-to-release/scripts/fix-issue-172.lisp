(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
		       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
			     (sort
			      (append features `(,(if (and key value)
						      (format nil "~a=~a" key value)
						      (format nil "~a" value))))
			      #'string-lessp :key (lambda (x) (car (split-sequence #\= x))))
			     :test #'equal))))

(defun fix (tk)
  (let ((xpos (token-xpostag tk))
	(upos (token-upostag tk)))
    (when (and
	   (search "<NUM-ord>" xpos)
	   (string= "ADJ" upos))
      (setf (token-feats tk) (append-feature "NumType" "Ord" (token-feats tk)))
      (setf (token-misc tk) (append-feature "ChangedBy" "Issue172" (token-misc tk))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix (sentence-tokens s))) sentences))

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
