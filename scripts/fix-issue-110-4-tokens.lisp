(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (w x y z) 
     on list
     collect (list w x y z)))

(defun isolated-tree (t1 t2 t3 t4)
  "T1 T2 T3 T4 form an isolated three. In other words, T1 points
outside T2, T3 and T3, and T2, T3, and T4 should either point at T1 or
each other."
  (let ((id1 (slot-value t1 'id))
        (id2 (slot-value t2 'id))
        (id3 (slot-value t3 'id))
        (id4 (slot-value t4 'id))
        (h1 (slot-value t1 'head))
        (h2 (slot-value t2 'head))
        (h3 (slot-value t3 'head))
        (h4 (slot-value t4 'head)))
    (and (not (eq h1 id2))
         (not (eq h1 id3))
         (not (eq h1 id4))
         (or (eq h4 id1) (eq h4 id2) (eq h4 id3))
         (or (eq h3 id1) (eq h3 id2) (eq h3 id4))
         (or (eq h2 id1) (eq h2 id3) (eq h2 id4)))))

(defun fix-mwe (tokens mwe sent-id)
  (when tokens
    (let* ((t1 (first tokens))
           (t2 (second tokens))
           (t3 (third tokens))
           (t4 (fourth tokens))
           (id1 (slot-value t1 'id)))
      (when (and 
             t1 t2 t3 t4
             (string-equal (slot-value t1 'misc) (format nil "MWE=~a" mwe)))

        (if (isolated-tree t1 t2 t3 t4)
            (progn 
              (setf (slot-value t1 'upostag) "NOUN")
              (setf (slot-value t1 'xpostag) "_")
              (setf (slot-value t2 'upostag) "ADP")
              (setf (slot-value t2 'xpostag) "_")
              (setf (slot-value t3 'upostag) "DET")
              (setf (slot-value t3 'xpostag) "_")
              (setf (slot-value t4 'upostag) "NOUN")
              (setf (slot-value t4 'xpostag) "_")
              (setf (slot-value t2 'deprel) "compound")
              (setf (slot-value t3 'deprel) "compound")
              (setf (slot-value t4 'deprel) "compound")
              (setf (slot-value t2 'head) id1)
              (setf (slot-value t3 'head) id1)
              (setf (slot-value t4 'head) id1)
              (setf (slot-value t1 'misc) (format nil "MWE=~a|MWEPOS=NOUN" mwe)))
            (format t "Strange MWE in ~a: ~a.~%" mwe sent-id))))))

(defparameter *mwes* '("café_da_manhã"
                       "caixa_do_correio"
                       "cana_do_nariz"
                       "donos_da_casa"
                       "obra_do_mestre"))

(defun fix-corpus (sentences)
  (dolist (mwe *mwes*)
    (mapc (lambda (s) (mapc (lambda (tks) (fix-mwe tks mwe (sentence-meta-value s "sent_id"))) (collect-next-element (sentence-tokens s)))) sentences))
  sentences)

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
