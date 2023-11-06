;; PLEASE NOTICE THAT THIS GENERATES A COUPLE OF FALSE POSITIVES,
;; ESPECIALLY WHEN THERE ARE SUBWORDS IN THE SAME PHRASE LIKE "COM"
;; AND "COMO".
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
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun sam? (tk)
  (or (search "<sam->" (token-xpostag tk))
      (search "<-sam>" (token-xpostag tk))))

(defun fix (sentences)
  (dolist (s sentences)
    (let ((text (sentence-meta-value s "text")))
      (dolist (tks (collect-next-element (sentence-tokens s)))
	(let* ((t1 (car tks))
	       (t2 (cdr tks))
	       (combined-token (when (and t1 t2) (concatenate 'string (token-form t1) (token-form t2))))
	       (found (when combined-token (find combined-token (split-sequence #\space text) :test #'equal))))
	  ;; (format t "ct=~a, found=~a.~%" combined-token found)
	  (when (and t1 t2 (not (sam? t1)) (not (sam? t2)) found)
	    (setf (token-misc t1) (append-feature "ChangedBy" "Issue165" (token-misc t1)))
	    (setf (token-misc t1) (append-feature "SpaceAfter" "No" (token-misc t1))))))))
  sentences)

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu f)) f)))
