(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y)
     on list collect (cons x y)))

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

(defun remove-feature-by-key (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal key (car (split-sequence #\= x)))) (split-sequence #\| features))))

(defun fix (tokens)
  (let ((t1 (car tokens))
	(t2 (cdr tokens)))
    (when (and t1 t2
	       (string= "CCONJ" (token-upostag t1))
	       (string= "VERB" (token-upostag t2))
	       (has-feature "MWE" "ou_seja" (token-misc t1))
	       (string-equal "ou" (token-lemma t1))
	       (string-equal "ser" (token-lemma t2)))
      (let ((feats (token-feats t2)))
	(setf feats (remove-feature "Gender" "Fem" feats))
	(setf feats (remove-feature-by-key "Mood" feats))
	(setf feats (remove-feature-by-key "Person" feats))
	(setf feats (remove-feature-by-key "Tense" feats))
	(setf feats (remove-feature-by-key "VerbForm" feats))
	(setf feats (append-feature "VerbForm" "Fin" feats))
	(setf feats (append-feature "Mood" "Sub" feats))
	(setf feats (append-feature "Person" "3" feats))
	(setf feats (append-feature "Tense" "Pres" feats))
	(setf (token-feats t2) feats)
	(setf (token-misc t2) (append-feature "ChangedBy" "Issue181" (token-misc t2)))))))

(defun fix-sentences (sentences)
  (mapc (lambda (s) (mapc #'fix (collect-next-element (sentence-tokens s)))) sentences))

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix-sentences (read-conllu fn)) fn)))

