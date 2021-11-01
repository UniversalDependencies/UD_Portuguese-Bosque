(ql:quickload :cl-conllu)

(in-package :conllu.user)


(defun fix (sentences)
  (dolist (s sentences sentences)
    (let ((meta (sentence-meta s)))
      (setf (sentence-meta s)
	    (remove-if (lambda (x) (equal "id" (car x))) meta)))))


(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

