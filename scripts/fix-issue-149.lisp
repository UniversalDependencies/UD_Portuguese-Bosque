;; WARNING: not only this got REALLY complicated, we still had to
;; manually fix some of the alterations.  Read the discussion on #149
;; for more details.

(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

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

(defun t? (token &key lemma upostag deprel head)
  (and 
   (if (and (atom lemma) lemma)
        (string-equal (slot-value token 'lemma) lemma)
       t)
   (if (and (atom deprel) deprel)
        (string-equal (slot-value token 'deprel) deprel)
       t)
   (if (and (atom head) head)
        (equal (slot-value token 'head) head)
       t)
   (if (and (atom upostag) upostag)
       (string-equal (slot-value token 'upostag) upostag)
       t)))

(defun conj? (tk) (t? tk :deprel "conj"))

(defun fix (token tokens)
  (flet ((after-conj-or-punct (token tokens)
           "T if either a S/CCONJ or PUNCT appear in the token list before TOKEN."
           (find-if 
            (lambda (x) (< (slot-value x 'id) (slot-value token 'id)))
            (remove-if-not (lambda (x) 
                             (or (t? x :upostag "SCONJ") 
                                 (t? x :upostag "CCONJ") 
                                 (t? x :lemma ","))) tokens)))
         (any-conj-found? (candidates) 
           (some #'identity (mapcar #'conj? candidates))))
    (let ((cc) 
          (conj-candidates) 
          (tid (slot-value token 'id)))
      (dolist (tk tokens)
        (if (t? tk :deprel "cc" :head tid) 
            (setf cc tk)
            (when (t? tk :head tid) (push tk conj-candidates))))
      ;; if there is already a "conj" amongst the candidates, do nothing
      ;; as this will lead to a lot of errors introduced.
      (unless (any-conj-found? conj-candidates)
        (when (and cc (> (length conj-candidates) 0))
          (dolist (candidate conj-candidates)
            (let ((candidate-id (slot-value candidate 'id))
                  (deprel (slot-value candidate 'deprel)))
              (when (and 
                     (> candidate-id tid)
                     (string= (slot-value token 'upostag) (slot-value candidate 'upostag))
                     (after-conj-or-punct candidate tokens)
                     (not (member deprel '("cc" "punct" "flat" "flat:foreign" "flat:name" "fixed" "compound")  :test #'equal)))
                (setf (slot-value candidate 'misc) (append-feature "ChangedBy" "Issue149" candidate))
                (setf (slot-value candidate 'deprel) "conj")))))))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) 
          
          (let ((tokens (sentence-tokens s)))
            ;; we'll skip the first token if it is CCONJ/SCONJ, since
            ;; in this case it is likely a strange/complicated
            ;; structure that will invariably lead to wrong fixes.
            (unless (or (t? (first tokens) :upostag "CCONJ") 
                        (t? (first tokens) :upostag "SCONJ"))
              (mapc (lambda (tk) (fix tk tokens)) tokens)))) 
        sentences))

(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
