(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun convert-bosque ()
  (with-open-file (out "bosque.ttl" :direction :output :if-exists :supersede)
    (convert-conll out (read-conllu "bosque.udep.conll")
                   (lambda (s) (cdr (assoc "text" (sentence-meta s) :test #'equal)))
                   (lambda (s) (cdr (assoc "sent_id" (sentence-meta s) :test #'equal))))))
