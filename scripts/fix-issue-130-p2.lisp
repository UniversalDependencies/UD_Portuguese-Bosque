(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun head-of (token tokens)
  (when token 
    (find-token (slot-value token 'head) tokens)))

(defun search-feature (key features)
  (find key features :key (lambda (x) (car (split-sequence #\= x))) :test #'string=))

;; note: doesn't add a new feature if there is one with the same name
;; we should enhance it when needed:
;; 1. If adding Foo=Bar and Foo=Bar is already a feature, ignore
;; 2. If adding Foo=Bar and Foo=Baz is already a feature, add Foo=Bar,Baz
(defun append-feature (key value token)
  (let* ((features-str (slot-value token 'feats))
         (features (if (string= "_" features-str) nil
                       (split-sequence #\| features-str))))
    (if (search-feature key features)
        features-str
        (format nil "~{~a~^|~}" (sort (append features `(,(format nil "~a=~a" key value))) #'string<)))))

(defun t? (token &key lemma upostag)
  (and 
   (if (and (atom lemma) lemma)
       (string-equal (slot-value token 'lemma) lemma)
       t)
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defun fix (adp tokens)
  (when tokens
    (let* ((x (head-of adp tokens))
           (y (head-of x tokens)))
      (when (and 
             adp x y
             (t? adp :upostag "ADP")
             (search "PRP|@PASS" (slot-value adp 'xpostag))
             (t? y :upostag "VERB"))
        (setf (slot-value y 'feats) (append-feature "Voice" "Pass" y))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x tokens)) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
