(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun search-xpostag (value xpostag)
  (find value (split-sequence #\| xpostag) :test #'string=))

;; note: doesn't add a new feature if there is one with the same name
;; we should enhance it when needed:
;; 1. If adding Foo=Bar and Foo=Bar is already a feature, ignore
;; 2. If adding Foo=Bar and Foo=Baz is already a feature, add Foo=Bar,Baz
(defun append-feature (key value token)
  (let* ((features-str (slot-value token 'misc))
         (features (if (string= "_" features-str) nil
                       (split-sequence #\| features-str))))
    (format nil "~{~a~^|~}" (sort (append features `(,(if (and key value) 
                                                          (format nil "~a=~a" key value)
                                                          (format nil "~a" value)))) #'string<))))

(defun t? (token &key lemma upostag xpostag)
  (and 
   (if (and (atom lemma) lemma)
        (string-equal (slot-value token 'lemma) lemma)
       t)
   (if (and (atom xpostag) xpostag)
       (search-xpostag xpostag (slot-value token 'xpostag))
       t)
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defparameter *exceptions* '("CP401-3" "CF541-1" "CF264-3"))

(defun fix (token id)
  (when (and 
         token
         (not (member id *exceptions* :test #'string-equal))
         (or (t? token :upostag "PRON" :lemma "se" :xpostag "@<ACC-PASS")
             (t? token :upostag "PRON" :lemma "se" :xpostag "@ACC>-PASS")))
    (progn
      (setf (slot-value token 'deprel) "expl")
      (setf (slot-value token 'misc) (append-feature "ChangedBy" "Issue135" token)))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x (sentence-meta-value s "sent_id"))) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
