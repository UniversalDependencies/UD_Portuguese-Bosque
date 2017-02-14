(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list
            collect (cons x y)))

(defun has-feature (key features)
  (unless (string-equal "_" features)
    (find key (split-sequence #\| features) :key (lambda (x) (car (split-sequence #\= x))) :test #'string=)))

(defun sort-features (feats)
  (if (string= "_" feats)
      "_"
      (let* ((features (split-sequence #\| feats)))
	(format nil "~{~a~^|~}" (remove-duplicates
				 (sort features #'string-lessp)
				 :test #'equal)))))

(defun fix (sentences)
  (dolist (s sentences)
    (dolist (tk (sentence-tokens s))
      (setf (token-misc tk) (sort-features (token-misc tk)))
      (setf (token-feats tk) (sort-features (token-feats tk)))))
  sentences)

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu f)) f)))
