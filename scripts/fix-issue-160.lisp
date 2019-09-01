(ql:quickload :cl-conllu)

(in-package :cl-conllu)

;; awk '$2 ~ /\-$/ && $4 ~ /VERB/ {vb = $0 ; getline ; if ($4 !~ /PRON/) print vb,"\n",$0,FILENAME }' ../documents/*.conllu

(defun collect-next-element (list)
  (loop for (x y)
     on list collect (cons x y)))

(defun has-feature (key features)
  (unless (string-equal "_" features)
    (find key (split-sequence #\| features) :key (lambda (x) (first (split-sequence #\= x))) :test #'equal)))

(defun remove-feature* (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal key (car (split-sequence #\= x)))) (split-sequence #\| features))))

(defun remove-feature (key value features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal `(,key ,value) (split-sequence #\= x))) (split-sequence #\| features))))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun insert-mtoken (s mt)
  (setf (sentence-mtokens s) (cons mt (sentence-mtokens s))))

(defun fix-enclisis (sentence)
  (labels ((next-token (tk)
	     (find (1+ (token-id tk))
		   (sentence-tokens sentence)
		   :key #'token-id)))
    (let ((verb-enclisis (remove-if-not (lambda (tk)
					  (and
					   (scan "-$" (token-form tk)) 
					   (or (equal (token-upostag tk) "VERB")
                                               (equal (token-upostag tk) "AUX"))
					   (equal (token-upostag (next-token tk)) "PRON")))
					(sentence-tokens sentence))))
      (dolist (tk verb-enclisis)
        (let* ((nt (next-token tk))
               (mt (make-instance 'mtoken
                                  :form (concatenate 'string (token-form tk) (token-form nt))
                                  :start (token-id tk)
                                  :end (token-id nt))))
          (when (has-feature "SpaceAfter" (token-misc tk))
            (setf (token-misc tk) (remove-feature* "SpaceAfter" (token-misc tk)))
            (setf (token-misc tk) (remove-feature "ChangedBy" "Issue165" (token-misc tk)))
            (when (equal "" (token-misc tk))
              (setf (token-misc tk) "_")))
          (when (has-feature "SpaceAfter" (token-misc nt))
            (setf (token-misc nt) (remove-feature* "SpaceAfter" (token-misc nt)))
            (setf (token-misc nt) (remove-feature "ChangedBy" "Issue165" (token-misc nt)))
            (when (equal "" (token-misc nt))
              (setf (token-misc nt) "_"))
            (setf (mtoken-misc mt) (append-feature "SpaceAfter" "No" (mtoken-misc mt))))
          (insert-mtoken sentence mt))
	(setf (token-form tk)
	      (regex-replace "-$" (token-form tk) "")))
      (if verb-enclisis
	  (values sentence t)
	  (values sentence nil)))))
			 
(defun run ()
  (dolist (fn (directory "documents/C*.conllu"))
    (write-conllu (mapcar #'fix-enclisis (read-conllu fn)) fn)))
