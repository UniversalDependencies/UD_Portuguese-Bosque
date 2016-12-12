(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun fix-case (token)
  (when token
    (unless (member (slot-value token 'upostag) '("SYM" "PROPN") :test #'string=)
      (setf (slot-value token 'lemma) (string-downcase (slot-value token 'lemma))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-case (sentence-tokens s))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
