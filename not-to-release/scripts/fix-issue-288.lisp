(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun fix (sentence)
  (let* ((tokens  (sentence-tokens sentence))
	 (size    (length tokens))
	 (no      (car tokens))
	 (entanto (cadr tokens))
	 (mt (make-instance 'mtoken :lineno -3 :form "No" :start (+ 1 size) :end (+ 2 size)))
	 (em (make-instance 'token :lineno -2 :id (+ 1 size) :form "Em" :lemma "em" :upostag "ADP"
				   :head (token-head no) :deprel (token-deprel no) :misc "MWEPOS=CCONJ"))
	 (o  (make-instance 'token :lineno -1 :id (+ 2 size) :form "o" :lemma "o" :upostag "DET"
				   :feats "Definite=Def|Gender=Masc|Number=Sing|PronType=Art"
				   :head (+ 1 size) :deprel (token-deprel entanto))))
    (format t "Fixing sentence ~a ~%" (sentence-meta-value sentence "sent_id"))
    (setf (token-head entanto)        (+ 1 size)
	  (sentence-tokens sentence)  (append (list em o) (cdr (sentence-tokens sentence)))
	  (sentence-mtokens sentence) (append (list mt) (sentence-mtokens sentence)))
    (adjust-sentence sentence)))


(defun run ()
  (dolist (file (directory "documents/*.conllu"))
    (let* ((all (read-conllu file))
	   (tof (query '(fixed (form "No") (lemma "entanto")) all)))
      (if tof
	  (progn
	    (format t "Fixing file ~a~%" file)
	    (write-conllu (mapc (lambda (s) (if (find s tof) (fix s) s)) all)
			  file))))))



