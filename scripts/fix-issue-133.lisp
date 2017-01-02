(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun head-of (token tokens)
  (when token 
    (find-token (slot-value token 'head) tokens)))

(defun search-feature (key-value features)
  (find key-value (split-sequence #\| features) 
        :test #'string=))

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

(defun t? (token &key lemma upostag xpostag feature deprel)
  (and 
   (if (and (atom deprel) deprel)
       (string-equal (slot-value token 'deprel) deprel)
       t)
   (if (and (atom feature) feature)
       (search-feature feature (slot-value token 'feats))
       t)
   (if (and (atom lemma) lemma)
        (string-equal (slot-value token 'lemma) lemma)
       t)
   (if (and (atom xpostag) xpostag)
       (search-xpostag xpostag (slot-value token 'xpostag))
       t)
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defun fix (token tokens)
  (when tokens
    (let* ((verb (head-of token tokens)))
      (when (and 
             token verb
             (t? token :upostag "PRON" :lemma "se" :deprel "dep" :xpostag "@ACC-PASS"))
        (if (t? verb :upostag "VERB" :feature "Voice=Pass")
            (progn
              (setf (slot-value token 'deprel) "expl")
              (setf (slot-value token 'misc) (append-feature nil "SUBJ_INDEF" token))
              (setf (slot-value token 'misc) (append-feature "ChangedBy" "Issue133" token)))
            (format t "Error: ~a~%" (slot-value verb 'upostag)))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x tokens)) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
