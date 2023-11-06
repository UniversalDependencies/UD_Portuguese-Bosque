(ql:quickload :cl-conllu)

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list
            collect (cons x y)))

(defun has-feature (key features)
  (unless (string-equal "_" features)
    (find key (split-sequence #\| features) :key (lambda (x) (car (split-sequence #\= x))) :test #'string=)))

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
    (dolist (tk (sentence-tokens s))
      (let ((feats (token-feats tk)))
	(when (and (equal "NUM" (token-upostag tk))
		   (search "NUM" (token-xpostag tk))
		   (not (has-feature "NumType" feats)))
	  (setf (token-feats tk) (append-feature "NumType" "Card" feats))
	  (setf (token-misc tk) (append-feature "ChangedBy" "Issue168" (token-misc tk)))))))
  sentences)

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu f)) f)))
