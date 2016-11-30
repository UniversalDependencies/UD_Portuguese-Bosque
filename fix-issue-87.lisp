;; fix for issue #87

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y) 
     on list
     collect (cons x y)))

(defun fix-misc (tokens)
  (when tokens
   (let* ((t1 (car tokens))
          (t2 (cdr tokens)))
     (when (and 
            (string-equal (slot-value t1 'lemma) "por")
            (string-equal (slot-value t2 'lemma) "exemplo")
            (string-equal (slot-value t1 'misc) "MWE=por_exemplo"))

       (setf (slot-value t1 'deprel) "cc")
       (setf (slot-value t1 'head) (slot-value t2 'head))

       (setf (slot-value t2 'deprel) "mwe")
       (setf (slot-value t2 'head) (slot-value t1 'id))

       (setf (slot-value t1 'misc) (format nil "MWE=por_exemplo|MWEPOS=CONJ"))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-misc (collect-next-element (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
