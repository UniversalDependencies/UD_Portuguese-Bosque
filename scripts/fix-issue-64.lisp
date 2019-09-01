(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun fix-sym (token)
  (when token
    (when (member (slot-value token 'lemma) '("CR$" "R$" "US$") :test #'string-equal)
      (setf (slot-value token 'upostag) "SYM")
      (setf (slot-value token 'xpostag) "_"))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-sym (sentence-tokens s))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
