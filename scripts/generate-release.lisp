(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun read-file (f)
  (with-open-file (stream f)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun release (dir ids output)
  "Generate the release files using the source directory DIR and the
sentence ids listed in the file IDS, saving the output in OUTPUT."
  (let ((ids (read-file ids))
        (sentences (make-hash-table :test #'equal)))
    (dolist (f (cl-fad:list-directory dir))
      (dolist (s (read-conllu f))
        (setf (gethash (sentence-meta-value s "sent_id") sentences) s)))
    (write-conllu (mapcar (lambda (x) (gethash x sentences)) ids) output)))

(release #p"documents/" #p"ids-dev.txt" #p"pt_bosque-ud-dev.conllu")
(release #p"documents/" #p"ids-test.txt" #p"pt_bosque-ud-test.conllu")
(release #p"documents/" #p"ids-train.txt" #p"pt_bosque-ud-train.conllu")
