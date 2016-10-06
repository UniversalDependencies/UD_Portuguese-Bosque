(ql:quickload :split-sequence)

(defpackage :join-features
  (:use :cl :split-sequence))

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
          (maphash 
           (lambda (k v)
             (push (format nil "~a=~a" k 
                           (mapconcat (reverse (remove-duplicates v :test #'equal)) ",")) new-features))
           features-map)
          (mapconcat new-features "|"))
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
