(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defpackage :join-features
  (:use :cl :split-sequence :alexandria))

(in-package :join-features)

(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))

;; https://groups.google.com/forum/#!topic/comp.lang.lisp/LXG1U7YuILU
(defun mapconcat (list delim)
  (reduce (lambda (x y)
            (concatenate 'string
                         x delim y))
          list))

(defun join-features (features-string)
  (let ((features-map (make-hash-table :test #'equal))
        (features (split-sequence #\| features-string))
        (new-features nil))
    (if (and (not (string-equal "_" features-string)) (> (length features) 0))
        (progn
          (dolist (fstr features)
            (when (> (length fstr) 0)
             (let* ((f (split-sequence #\= fstr))
                    (name (car f))
                    (value (split-sequence #\, (cadr f))))
               (setf (gethash name features-map) (append value (gethash name features-map))))))
          (let ((sorted-keys (sort (hash-table-keys features-map) #'string-lessp)))
            (dolist (k sorted-keys)
              (push (format nil "~a=~a" k 
                            (mapconcat (sort (remove-duplicates (gethash k features-map) 
                                                                :test #'equal) #'string-lessp) ",")) new-features)))
          
          (if new-features (mapconcat (reverse new-features) "|") "_"))
        features-string)))

(defun fix-line (line)
  (let ((cols (split-sequence #\tab line :remove-empty-subseqs nil)))
    (if (= 10 (length cols))
        (progn 
          (let* ((features (elt cols 5))
                 (joined (join-features features)))
            (setf (elt cols 5) joined)
            (dotimes (n (length cols))
              (when (= 0 (length (elt cols n)))
                (setf (elt cols n) "_")))
            (mapconcat cols '(#\tab))))
        line)))

(defun execute (infile outfile)
  (with-open-files ((in infile :direction :input)
		    (out outfile :direction :output :if-exists :supersede))
    (loop for line = (read-line in nil)
	  while line do
	  (write-line (fix-line line) out))))
