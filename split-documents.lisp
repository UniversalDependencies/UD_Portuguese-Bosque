(ql:quickload :cl-conllu)


(in-package :cl-conllu)

(defparameter *bosque* nil)
(defparameter *documents* nil)

(defun split-documents (file)
  (flet ((id (s) (cdr (assoc "sent_id" (sentence-meta s) :test #'equal)))
         (doc (id) (car (split-sequence #\- id))))
    (setf *documents* (make-hash-table :test #'equal))
    (setf *bosque* (read-conllu file))
    (mapc (lambda (s)
            (push s (gethash (doc (id s)) *documents*))) *bosque*)
    (maphash (lambda (k v)
               (write-conllu (reverse v)
                             (make-pathname
                              :directory '(:relative "documents")
                              :name k
                              :type "conllu"))) *documents*)))
