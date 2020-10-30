(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :conllu.user)

(defun read-lines (f)
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
      (setf (slot-value tk 'xpostag) "_")
      (when (or (= 0 (length (slot-value tk 'misc))) (null (slot-value tk 'misc)))
        (setf (slot-value tk 'misc) "_")))
    (setf (sentence-meta s)
          (remove "d2d" (sentence-meta s) :test #'string= :key #'car)))
  conll)

(defun release (ids output)
  "Generate the release files using the source directory DIR and the
   sentence ids listed in the file IDS, saving the output in OUTPUT."
  (let ((ids (read-lines ids))
        (sentences (make-hash-table :test #'equal)))
    (mapc (lambda (fn)
	    (dolist (s (prepare-for-release (read-conllu fn)))
              (setf (gethash (sentence-id s) sentences) s)))
	  (directory "documents/*.conllu"))
    (write-conllu (mapcar (lambda (x)
			    (assert (gethash x sentences))
			    (gethash x sentences))
			  ids)
		  output)))


(defun main ()
  (release #p"pt_bosque-ud-dev.txt"   #p"pt_bosque-ud-dev.conllu")
  (release #p"pt_bosque-ud-test.txt"  #p"pt_bosque-ud-test.conllu")
  (release #p"pt_bosque-ud-train.txt" #p"pt_bosque-ud-train.conllu"))


;; sbcl --load scripts/generate-release.lisp --eval '(in-package :cl-conllu)' --eval '(main)'
