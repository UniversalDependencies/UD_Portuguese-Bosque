(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun head-of (token tokens)
  (when token 
    (find-token (slot-value token 'head) tokens)))

(defun has-feature (key features)
  (unless (string-equal "_" features)
    (find key (split-sequence #\| features) :key (lambda (x) (car (split-sequence #\= x))) :test #'string=)))

(defun search-feature (key-value features)
  (find key-value (split-sequence #\| features) 
        :test #'string=))

(defun search-xpostag (value xpostag)
  (find value (split-sequence #\| xpostag) :test #'string=))

;; note: doesn't add a new feature if there is one with the same name
;; we should enhance it when needed:
;; 1. If adding Foo=Bar and Foo=Bar is already a feature, ignore
;; 2. If adding Foo=Bar and Foo=Baz is already a feature, add Foo=Bar,Baz
(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (sort (append features `(,(if (and key value) 
                                                          (format nil "~a=~a" key value)
                                                          (format nil "~a" value)))) #'string<))))

(defun in-mwe (tokens)
  (some (lambda (x) (has-feature "MWE" (slot-value x 'misc))) tokens))

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

(defun fix (token tokens sid)
  (when tokens
    (let* ((x (head-of token tokens))
           (y (head-of x tokens)))
      (when (and 
             token x y
             (t? token :upostag "ADP" :xpostag "PRP")
             (or (t? token :xpostag "@<ADVL")
                 (t? token :xpostag "@ADVL>"))
             (t? x :deprel "nmod")
             (or (t? x :upostag "NOUN")
                 (t? x :upostag "PROPN")))
        (if (t? y :upostag "VERB")
            (if (not (in-mwe `(,token ,x ,y)))
                (progn
                  (setf (slot-value x 'deprel) "obl")
                  (setf (slot-value x 'misc) (append-feature "ChangedBy" "Issue137" (slot-value x 'misc))))
                (format t "Skipping MWE in ~a (~a).~%" (slot-value token 'lemma) sid))
            (format t "Error: ~a (~a)~%" (slot-value y 'upostag) sid))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x tokens (sentence-meta-value s "sent_id"))) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
