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
						      (format nil "~a" value)))) #'string<)
			     :test #'equal))))

(defun fix (tk)
  (let ((lemma (token-lemma tk))
	(deprel (token-deprel tk))
	(upos (token-upostag tk)))
    (when (and (string= "VERB" upos)
	       (or (string-equal "ser" lemma) (string-equal "estar" lemma))
	       (string= "cop" deprel))
      (setf (token-upostag tk) "AUX")
      (setf (token-misc tk) (append-feature "ChangedBy" "Issue167" (token-misc tk))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix (sentence-tokens s))) sentences))

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
