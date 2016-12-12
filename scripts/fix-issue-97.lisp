(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y z) 
     on list
     collect (list x y z)))

(defun reverse-dependency (t1 t2 dep-t1-id3 dep-t2-t1)
  "Assuming that T1 points to T2 and that T2 points to ID3 via some
dependency relation, this operation reverses the link so that T2 now
points to T1 and T1 now points to ID3.  T1 and T2 should be TOKEN
instances.  DEP-T1-ID3 is the new dependency relation between T1 and
ID3.  DEP-T2-T1 is the new dependency relation between T2 and T1."
  (setf (slot-value t1 'deprel) dep-t1-id3)
  (setf (slot-value t1 'head) (slot-value t2 'head))
  (setf (slot-value t2 'deprel) dep-t2-t1)
  (setf (slot-value t2 'head) (slot-value t1 'id)))

(defun fix-isto-e (tokens)
  (when tokens
    (let* ((t1 (first tokens))
           (t2 (second tokens))
           (t3 (third tokens)))
      (when (and 
             t1 t2 t3
             (string-equal (slot-value t3 'upostag) "PUNCT")
             (string-equal (slot-value t1 'lemma) "isto")
             (string-equal (slot-value t2 'lemma) "ser")
             (string-equal (slot-value t1 'misc) "_"))

        (reverse-dependency t1 t2 "conj" "fixed")
       
        (setf (slot-value t1 'upostag) "PRON")
        (setf (slot-value t1 'xpostag) "_")
        (setf (slot-value t2 'upostag) "VERB")
        (setf (slot-value t2 'xpostag) "_")

        (setf (slot-value t1 'misc) (format nil "MWE=isto_é|MWEPOS=CCONJ"))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-isto-e (collect-next-element (sentence-tokens s)))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
