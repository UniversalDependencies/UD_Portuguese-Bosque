(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun head-of (token tokens)
  (find-token (slot-value token 'head) tokens))

(defun search-feature (key-value features)
  (find key-value (split-sequence #\| features) 
        :test #'string=))

(defun t? (token &key lemma upostag feature deprel)
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
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defun fix (token tokens)
  (when tokens
    (let ((head (head-of token tokens)))
     (when (and 
            token head
            (t? token :deprel "nsubj")
            (t? head :upostag "VERB" :feature "Voice=Pass"))
       (setf (slot-value token 'deprel) "nsubj:pass")))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          (let ((tokens (sentence-tokens s)))
            (mapc (lambda (x) (fix x tokens)) tokens))) sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
