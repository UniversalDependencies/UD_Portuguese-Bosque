(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defpackage :join-features
  (:use :cl :split-sequence :alexandria))

(in-package :join-features)

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
    (if (> (length features) 1)
        (progn
          (dolist (fstr features)
            (let ((f (split-sequence #\= fstr)))
              (push (cadr f) (gethash (car f) features-map))))
          (let ((sorted-keys (sort (hash-table-keys features-map) #'string-lessp)))
            (dolist (k sorted-keys)
              (push (format nil "~a=~a" k 
                            (mapconcat (reverse (remove-duplicates (gethash k features-map) 
                                                                   :test #'equal)) ",")) new-features)))

          (mapconcat (reverse new-features) "|"))
        features-string)))

(defun fix-line (line)
  (let ((cols (split-sequence #\tab line :remove-empty-subseqs nil)))
    (if (= 10 (length cols))
        (progn 
          (let* ((features (elt cols 5))
                 (joined (join-features features)))
            (setf (elt cols 5) joined)
            (mapconcat cols '(#\tab))))
        line)))

(defun execute (infile outfile)
  (with-open-file (in infile :direction :input)
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (loop for line = (read-line in nil)
         while line do
           (write-line (fix-line line) out)))))
