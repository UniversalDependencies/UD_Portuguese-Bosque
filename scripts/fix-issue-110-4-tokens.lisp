(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (w x y z) 
     on list
     collect (list w x y z)))

;; NB: this doesn't check if t1 t2 t3 actually form a tree, but it
;; works for us since there are other checks later in the pipeline
;; that do check this---and we have the assumption that we are
;; receiving a well formed CoNLL anyway.  Also this could be more
;; elegantly implemented as a lowest common ancestor / search
;; operation, since eventually we'll need an implementation that
;; supports a varying number of nodes.
(defun isolated? (t1 t2 t3 t4)
  "T1 T2 T3 T4 are isolated. In other words, T1 points outside T2, T3,
and T4, and T2, T3, and T4 should either point at T1 or each other."
  (let ((id1 (slot-value t1 'id))
        (id2 (slot-value t2 'id))
        (id3 (slot-value t3 'id))
        (id4 (slot-value t4 'id))
        (h1 (slot-value t1 'head))
        (h2 (slot-value t2 'head))
        (h3 (slot-value t3 'head))
        (h4 (slot-value t4 'head)))
    (and (not (member h1 `(,id2 ,id3 ,id4)))
         (member h2 `(,id1 ,id3 ,id4))
         (member h3 `(,id1 ,id2 ,id4))
         (member h4 `(,id1 ,id2 ,id3)))))

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

        (if (isolated? t1 t2 t3 t4)
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
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
