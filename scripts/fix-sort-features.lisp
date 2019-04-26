(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)


(defun fix (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (token)
		    (let ((feats (slot-value token 'feats)))
		      (if (> (length (cl-ppcre:split "\\|" feats)) 1)
			  (setf (slot-value token 'feats)
				(format nil "~{~a~^|~}" (sort (cl-ppcre:split "\\|" feats) #'string<=))))))
		  tokens)))
	sentences))

(defun run ()
  (dolist (doc (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix (read-conllu doc)) doc)))
