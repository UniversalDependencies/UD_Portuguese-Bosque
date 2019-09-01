;; note: even though we started writing this fix, since there are only
;; six issues of this problem in the corpus, it is probably not worth
;; making it robust and we should probably just fix the issues
;; manually.  The main issue is that some instances of nao_so have
;; different directions of its arrows.

(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y) 
     on list
     collect (cons x y)))

(defun fix-nao-so (tokens)
  (when tokens
   (let* ((t1 (car tokens))
          (t2 (cdr tokens)))
     (when (and 
            t1 t2
            (string-equal (slot-value t1 'lemma) "não")
            (string-equal (slot-value t2 'lemma) "só")
            (string-equal (slot-value t1 'misc) "MWE=não_só"))

       (setf (slot-value t1 'upostag) "ADV")
       (setf (slot-value t1 'xpostag) "_")

       (setf (slot-value t2 'deprel) "fixed")
       (setf (slot-value t2 'head) (slot-value t1 'id))

       (setf (slot-value t1 'misc) (format nil "MWE=não_só|MWEPOS=ADV"))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-nao-so (collect-next-element (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
