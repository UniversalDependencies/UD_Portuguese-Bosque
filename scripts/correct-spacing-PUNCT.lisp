(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list collect (list x y)))

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun has-feature (key value features)
  (unless (string-equal "_" features)
    (find `(,key ,value) (split-sequence #\| features) :key (lambda (x) (split-sequence #\= x)) :test #'equal)))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun remove-feature (key value features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal `(,key ,value) (split-sequence #\= x))) (split-sequence #\| features))))

(defun fix (sentences)
  (dolist (s sentences)
    (let ((text (sentence-meta-value s "text")))
      (dolist (tks (collect-next-element (sentence-tokens s)))
        (let ((t1 (first tks))
              (t2 (second tks)))
          (when (and t1 t2)
            (let* ((f1 (token-form t1))
                   (f2 (token-form t2))
                   (c (concatenate 'string f1 f2))
                   (pos (search c text)))
              ;; (format t "~a: ~a (~a).~%" (concatenate 'string f1 f2) pos (when pos (elt text (+ (length c) pos))))
              (when (and 
                     t1 t2
                     pos
                     (string= "PUNCT" (token-upostag t2))
                     (has-feature "SpaceAfter" "No" (token-misc t2))
                     (< (+ (length c) pos) (length text))
                     (eq #\space (elt text (+ (length c) pos))))
                (setf (token-misc t2) (remove-feature "ChangedBy" "Issue165" (token-misc t2)))
                (setf (token-misc t2) (remove-feature "SpaceAfter" "No" (token-misc t2)))
                (when (equal "" (token-misc t2))
                  (setf (token-misc t2) "_"))
)))))))
  sentences)

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

