(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun head-of (token tokens)
  (find-token (slot-value token 'head) tokens))

(defun append-feature (key value token)
  (let ((features (if (string= "_" (slot-value token 'feats)) nil
                      (split-sequence #\| (slot-value token 'feats)))))
    (format nil "~{~a~^|~}" (sort (append features `(,(format nil "~a=~a" key value))) #'string<))))

(defun t? (token &key lemma upostag)
  (and 
   (if (and (atom lemma) lemma)
       (string-equal (slot-value token 'lemma) lemma)
       t)
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defun fix (token tokens)
  (when tokens
    (let ((head (head-of token tokens)))
     (when (and 
            (t? token  :upostag "ADP")
            (search "@PASS" (slot-value token 'xpostag)))
       (setf (slot-value head 'deprel) "obl:agent")))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x tokens)) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
