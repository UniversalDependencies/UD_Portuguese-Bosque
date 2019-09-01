(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun fix-nsubjpass (token)
  (when token
    (when (string= (slot-value token 'deprel) "nsubjpass")
      (setf (slot-value token 'deprel) "nsubj:pass"))))

(defun fix-auxpass (token)
  (when token
    (when (string= (slot-value token 'deprel) "auxpass")
      (setf (slot-value token 'deprel) "aux:pass"))))

(defun fix-csubjpass (token)
  (when token
    (when (string= (slot-value token 'deprel) "csubjpass")
      (setf (slot-value token 'deprel) "csubj:pass"))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-nsubjpass (sentence-tokens s))) sentences)
  (mapc (lambda (s) (mapc #'fix-auxpass (sentence-tokens s))) sentences)
  (mapc (lambda (s) (mapc #'fix-csubjpass (sentence-tokens s))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
