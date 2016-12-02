;; fix for issue #90

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(in-package :cl-conllu)

(defun fix-misc (token)
  (let ((misc (slot-value token 'misc)))
    (when (and 
           (string-equal (slot-value token 'lemma) "o")
           (string-equal misc "MWE=o_que"))
      (setf (slot-value token 'upostag) "PRON")
      (setf (slot-value token 'xpostag) "_")
      (setf (slot-value token 'misc) "_"))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-misc (sentence-tokens s))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
