(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y) 
     on list
     collect (list x y)))

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

(defun append-feature (key value token)
  (let ((features (if (string= "_" (slot-value token 'feats)) nil
                      (split-sequence #\| (slot-value token 'feats)))))
    (format nil "~{~a~^|~}" (sort (append features `(,(format nil "~a=~a" key value))) #'string<))))

(defun fix-ja-nao (tokens)
  (when tokens
    (let* ((t1 (first tokens))
           (t2 (second tokens)))
      (when (and 
             t1 t2
             (string-equal (slot-value t1 'lemma) "já")
             (string-equal (slot-value t2 'lemma) "não")
             (string-equal (slot-value t1 'misc) "MWE=já_não"))

        (reverse-dependency t1 t2 "advmod" "fixed")
       
        (setf (slot-value t1 'upostag) "ADP")
        (setf (slot-value t1 'xpostag) "_")
        (setf (slot-value t2 'upostag) "ADV")
        (setf (slot-value t2 'xpostag) "_")

        (setf (slot-value t2 'feats) (append-feature "Polarity" "Neg" t2))

        (setf (slot-value t1 'misc) (format nil "MWE=já_não|MWEPOS=ADV"))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-ja-nao (collect-next-element (sentence-tokens s)))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
