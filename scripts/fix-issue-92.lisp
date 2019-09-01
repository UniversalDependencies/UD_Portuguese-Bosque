;; fix for issue #92

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(in-package :cl-conllu)

(defun fix-misc (token)
  (let ((misc (slot-value token 'misc)))
    (unless (string-equal misc "_")
      (when (alexandria:starts-with-subseq "MWE:" misc)
        (setf (slot-value token 'misc) (format nil "MWE=~a" (substitute #\_ #\= (subseq misc 4))))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-misc (sentence-tokens s))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
