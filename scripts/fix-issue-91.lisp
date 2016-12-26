(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)

(in-package :cl-conllu)

(defun collect-next-element2 (list)
  (loop for (x y) 
     on list
     collect (cons x y)))

(defun search-feature (key-value features)
  (find key-value (split-sequence #\| features) 
        :test #'string=))

(defun remove-feature (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (string-equal key (car (split-sequence #\= x)))) (split-sequence #\| features))))

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

(defun add-pcp-suffixes (str)
  (cl-ppcre:regex-replace-all "(.*)\\*" str "\\1(o|a|os|as)"))

(defun in-list (str list)
  (let ((scanner (cl-ppcre:create-scanner (format nil "^~{~a~^|~}$" (mapcar (lambda (x) (format nil "(~a)" (add-pcp-suffixes x))) list)) :case-insensitive-mode t)))
    (cl-ppcre:scan scanner str)))

(defparameter *misidentified-adjectives* '("fixo"
                                           "fixa"
                                           "fixos"
                                           "fixas"
                                           "conhecid*"
                                           "industrializad*"
                                           "sofisticad*"
                                           "difundid*"
                                           "engraçad*"
                                           "divertid*"
                                           "parecid*"
                                           "deprimid*"
                                           "angustiad*"
                                           "assustad*"
                                           "opost*"
                                           "indignad*"
                                           "envelhecid*"
                                           "dispost*"
                                           "civilizad*"
                                           "preocupad*"
                                           "despreocupad*"
                                           "supost*"
                                           "interessad*"
                                           "irritad*"
                                           "sofisticad*"
                                           "distint*"
                                           "cansad*"
                                           "globalizad*"
                                           "generalizad*"
                                           "desesperad*"
                                           "envelhecid*"
                                           "afamad*"
                                           "bem-vind*"
                                           "antiquad*"
                                           "consagrad*"
                                           "sagrad*"
                                           "engajad*"
                                           "juntos"
                                           "empenhad*"
                                           "adequad*"
                                           "casad*"
                                           "divorciad*"
                                           "desempregad*"
                                           "intrigad*"
                                           "aposentad*"
                                           "constrangid*"))

(defun fix-adjectives (token)
  (when token
    (when (and 
           (t? token :upostag "VERB")
           (in-list (slot-value token 'form) *misidentified-adjectives*))
      (let ((feats (slot-value token 'feats)))
        (dolist (f '("VerbForm" "Mood" "Tense" "Aspect" "Voice" "Polarity" "Evident" "Person" "Polite"))
          (setf feats (remove-feature f feats)))
        (setf (slot-value token 'feats) feats))
      (setf (slot-value token 'upostag) "ADJ")
      (setf (slot-value token 'xpostag) "_"))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-adjectives (sentence-tokens s))) sentences))

;; (defun fix-corpus (sentences)
;;   (mapc (lambda (s) (mapc #'fix-adjectives (collect-next-element (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
