(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun non-nominal? (pos)
  (member pos '("VERB" "AUX" "ADJ" "ADV") :test #'equal))

(defun fix (sentence)
  (let ((tokens (sentence-tokens sentence)))
    (dolist (tk tokens)
      (when (string= "nmod" (token-deprel tk))
	(let ((head (find-token (token-head tk) tokens)))
	  (when (non-nominal? (token-upostag head))
	    (setf (token-deprel tk) "obl")))))))

(defun fix-corpus (sentences)
  (mapc #'fix sentences))

(defun run ()
  (dolist (f (directory "documents/*.conllu"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
