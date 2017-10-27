(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun my-read-file (f)
  (with-open-file (stream f)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun remove-feature (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (string-equal key (car (split-sequence #\= x))))
				     (split-sequence #\| features))))

(defun remove-d2d (misc)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (string-equal "d2d" (car (split-sequence #\: x))))
				     (split-sequence #\| misc))))

(defun prepare-for-release (conll)
  "Remove ChangedBy from MISC.  Remove dep2dep annotations from MISC and metadata"
  (dolist (s conll)
    (dolist (tk (sentence-mtokens s))
      (setf (slot-value tk 'misc) (remove-d2d (slot-value tk 'misc)))
      (setf (slot-value tk 'misc) (remove-feature "ChangedBy" (slot-value tk 'misc)))
      (when (or (= 0 (length (slot-value tk 'misc))) (null (slot-value tk 'misc)))
        (setf (slot-value tk 'misc) "_")))
    (dolist (tk (sentence-tokens s))
      (setf (slot-value tk 'misc) (remove-d2d (slot-value tk 'misc)))
      (setf (slot-value tk 'misc) (remove-feature "ChangedBy" (slot-value tk 'misc)))
      (when (or (= 0 (length (slot-value tk 'misc))) (null (slot-value tk 'misc)))
        (setf (slot-value tk 'misc) "_")))
    (setf (sentence-meta s)
          (remove "d2d" (sentence-meta s) :test #'string= :key #'car)))
  conll)

(defun release (dir ids output)
  "Generate the release files using the source directory DIR and the
sentence ids listed in the file IDS, saving the output in OUTPUT."
  (let ((ids (my-read-file ids))
        (sentences (make-hash-table :test #'equal)))
    (dolist (f (cl-fad:list-directory dir))
      (unless (cl-fad:directory-exists-p f)
        (dolist (s (prepare-for-release (read-conllu f)))
          (setf (gethash (sentence-meta-value s "sent_id") sentences) s))))
    (write-conllu (mapcar (lambda (x) (gethash x sentences)) ids) output)))

(release #p"documents/" #p"pt-ud-dev.txt" #p"pt-ud-dev.conllu")
(release #p"documents/" #p"pt-ud-test.txt" #p"pt-ud-test.conllu")
(release #p"documents/" #p"pt-ud-train.txt" #p"pt-ud-train.conllu")
(when (probe-file #p"final.txt")
  (release #p"documents/" #p"final.txt" #p"final.conllu"))

