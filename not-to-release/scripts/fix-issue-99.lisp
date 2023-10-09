;; fix for issue #99

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (w x y z) 
     on list
     collect (list w x y z)))

(defun fix-misc (tokens)
  (when tokens
   (let ((t1 (first tokens))
         (t2 (second tokens))
         (t3 (third tokens))
         (t4 (fourth tokens)))
     (when (and 
            (string-equal (slot-value t1 'lemma) "a")
            (string-equal (slot-value t2 'lemma) "não")
            (string-equal (slot-value t3 'lemma) "ser")
            (string-equal (slot-value t4 'lemma) "que")
            (string-equal (slot-value t1 'misc) "MWE=a_não_ser_que"))
       (setf (slot-value t1 'misc) (format nil "MWE=a_não_ser_que|MWEPOS=SCONJ"))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-misc (collect-next-element (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
