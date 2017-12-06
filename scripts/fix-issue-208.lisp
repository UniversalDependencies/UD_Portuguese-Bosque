(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun fix-token (token)
  (let ((patterns '(("<n>"            . "NOUN")
		    ("<n>.*<NUM-ord>" . "ADJ")
		    ("<NUM-ord>.*<n>" . "ADJ")
		    ("<prop>.*<n>"    . "PROPN")
		    ("<n>.*SC"        . "ADJ")))
	(changed nil))
    (dolist (p patterns changed)
      (if (cl-ppcre:scan (car p) (token-xpostag token))
	  (setf (token-upostag token) (cdr p)
		changed t)))))


(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapcar (lambda (x) (fix-token x)) tokens)))
	sentences))

(defun run ()
  (dolist (f (directory #p"~/work/bosque-UD/documents/*.conllu"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
