;; fix for issue #84

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y) 
     on list
     collect (cons x y)))

(defun fix-misc (tokens)
  (when tokens
   (let ((t1 (car tokens))
         (t2 (cdr tokens)))
     (when (and 
            (string-equal (slot-value t1 'lemma) "por")
            (string-equal (slot-value t2 'lemma) "cento")
            (string-equal (slot-value t1 'misc) "MWE=por_cento"))
       (setf (slot-value t1 'upostag) "ADP")
       (setf (slot-value t1 'xpostag) "_")
       (setf (slot-value t1 'misc) (format nil "MWE=por_cento|MWEPOS=NOUN"))
       (setf (slot-value t2 'upostag) "NUM")
       (setf (slot-value t2 'xpostag) "_")
       (setf (slot-value t2 'deprel) "compound")))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-misc (collect-next-element (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
