(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)


(in-package :cl-conllu)


(defparameter *ud-portuguese* "/home/fcbr/repos/ud/UD_Portuguese/")

(defun extract-ids-order (f map)
  (let ((sentences (read-conllu (make-pathname :directory *ud-portuguese* :name (pathname-name f) :type (pathname-type f)))))
    (with-open-file (stream (make-pathname :name (pathname-name f) :type "txt") :direction :output :if-exists :supersede)
      (dolist (s sentences)
        (format stream "~a~%" (gethash (format nil "~a#~a" f (sentence-meta-value s "sent_id")) map))))))

(defun run ()
  (let ((map (make-hash-table :test #'equal)))
   (dolist (f (cl-fad:list-directory #p"documents/old/"))
     (when (and (equal "conllu" (pathname-type f))
                (or (alexandria:starts-with-subseq "CP" (pathname-name f))
                    (alexandria:starts-with-subseq "CF" (pathname-name f))))
       (dolist (s (read-conllu f))
         (setf (gethash (sentence-meta-value s "sent_id") map) (sentence-meta-value s "new_sent_id")))))
   (mapc (lambda (x) (extract-ids-order x map))  '("pt-ud-dev.conllu" "pt-ud-test.conllu" "pt-ud-train.conllu"))))

