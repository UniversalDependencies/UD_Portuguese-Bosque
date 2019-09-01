(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)


(defun fix (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (token)
		    (let* ((features (if (string= "_" (token-feats token)) nil
					 (split-sequence #\| (token-feats token)))))
		      (setf (token-feats token)
			    (format nil "~{~a~^|~}" (sort features (lambda (a b) (string< (string-downcase a)
											  (string-downcase b))))))))
		  tokens)))
	sentences))

(defun run ()
  (dolist (doc (directory #P"documents/*.conllu"))
    (write-conllu (fix (read-conllu doc)) doc)))
