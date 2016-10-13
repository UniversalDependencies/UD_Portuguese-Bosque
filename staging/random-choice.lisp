(ql:quickload :xmls)
(ql:quickload :alexandria)

(defpackage :random-choice
  (:use :cl :alexandria))

(in-package :random-choice)

;; ¶
(defparameter *overblog*
  (with-open-file (stream "overblog.xml") 
    (xmls:xmlrep-children (xmls:parse stream :compress-whitespace t))))

(defparameter *exts* (make-hash-table :test #'equal))

(dolist (ext *overblog*)
  (let ((id (first (cdaadr ext)))
        (text (third ext)))
    (setf (gethash id *exts*) text)))

(defparameter *keys* (hash-table-keys *exts*))

(defparameter *count* 100)

;; http://stackoverflow.com/questions/158716/how-do-you-efficiently-generate-a-list-of-k-non-repeating-integers-between-0-and

(defun sample (n sequence)
  (let ((length (length sequence))
        (result (subseq sequence 0 n)))
    (loop
       with m = 0
       for i from 0 and u = (random 1.0)
       do (when (< (* (- length i) u) 
                   (- n m))
            (setf (elt result m) (elt sequence i))
            (incf m))
       until (= m n))
    result))

(defun save-key (k)
  (with-open-file (stream (format nil "~a.ext.txt" k) :direction :output)
    (format stream (gethash k *exts*))))

(mapc (lambda (k) (save-key k)) (sample *count* *keys*))

