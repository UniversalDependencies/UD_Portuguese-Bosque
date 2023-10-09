(ql:quickload :cl-conllu)
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)

(in-package :cl-conllu)

(defun collect-next-element2 (list)
  (loop for (x y) 
     on list
     collect (cons x y)))

(defun collect-next-element3 (list)
  (loop for (x y z) 
     on list
     collect (list x y z)))

(defun search-feature (key-value features)
  (find key-value (split-sequence #\| features) 
        :test #'string=))

(defun search-xpostag (value xpostag)
  (find value (split-sequence #\| xpostag) :test #'string=))

(defun remove-feature (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (string-equal key (car (split-sequence #\= x)))) (split-sequence #\| features))))

(defun remove-verb-features (feats)
  (dolist (f '("VerbForm" "Mood" "Tense" "Aspect" "Voice" "Polarity" "Evident" "Person" "Polite"))
    (setf feats (remove-feature f feats)))
  (if (and feats (> (length feats) 0)) feats "_"))

(defun append-feature (key value feats)
  (let ((features (if (string= "_" feats) nil
                      (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (sort (append features `(,(format nil "~a=~a" key value))) #'string<))))

(defun add-suffixes (str)
  (cl-ppcre:regex-replace-all "(.*)\\*" str "\\1(o|a|os|as)"))

(defun t? (token &key form lemma upostag xpostag feature deprel)
  (and 
   (if (and (atom form) form)
       (scan (add-suffixes form) (slot-value token 'form))
       t)
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

;; following the comments on issue #91: O ideal seria se fosse
;; possível identificar os verbos que tenham a anotação |PCP| na
;; coluna 5.  Porém, ontem você tinha me dito que isso não era
;; possível, então substituí a orientação "VERB com anotação |PCP| na
;; coluna 5" por "VERB terminado em "do", "da", "dos", "das", "to",
;; "ta", "tos", "tas".
(defun is-pcp (token)
  (or (search-xpostag "PCP" (slot-value token 'xpostag))
      (cl-ppcre:scan "^(\w+)(do|da|dos|das|to|ta|tos|tas)$" (slot-value token 'form))))

(defun in-list (str list)
  (let* ((combined-regex (format nil "~{~a~^|~}" (mapcar (lambda (x) (format nil "^~a$" (add-suffixes x))) list)))
         (scanner (cl-ppcre:create-scanner combined-regex :case-insensitive-mode t)))
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

(defun convert-verb-to-adj (token)
  (setf (slot-value token 'feats) (remove-verb-features (slot-value token 'feats)))
  (setf (slot-value token 'misc) (append-feature "ChangedBy" "Issue91" (slot-value token 'misc)))
  (setf (slot-value token 'upostag) "ADJ"))

;; As seguintes palavras anotadas como VERB viram ADJ:
(defun fix-adjectives (token)
  (when token
    (when (and 
           (t? token :upostag "VERB")
           (in-list (slot-value token 'form) *misidentified-adjectives*))
      (convert-verb-to-adj token))))

;;lema "ficar" seguido de VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
;;lema "estar" seguido de VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
(defun fix-ficar/estar (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (or (t? t1 :lemma "ficar")
                   (t? t1 :lemma "estar"))
               (t? t2 :upostag "VERB")
               (is-pcp t2))
      (convert-verb-to-adj t2))))

;; palavra "bem" anotada como ADV seguida de VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
;; palavra "mal" anotada como ADV seguida de VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
(defun fix-bem/mal (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (or (t? t1 :lemma "bem" :upostag "ADV")
                   (t? t1 :lemma "mal" :upostag "ADV"))
               (t? t2 :upostag "VERB")
               (is-pcp t2))
      (convert-verb-to-adj t2))))

;; NOUN seguido da palavra "passad*" anotada como VERB -> VERB vira ADJ
(defun fix-passad* (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (t? t1 :upostag "NOUN")
               (t? t2 :form "passad*" :upostag "VERB"))
      (convert-verb-to-adj t2))))

;; DET seguido da palavra "respeitad*" anotada como VERB seguido de NOUN -> VERB vira ADJ
(defun fix-respeitad* (tokens)
  (let ((t1 (first tokens))
        (t2 (second tokens))
        (t3 (third tokens)))
    (when (and t1 t2 t3
               (t? t1 :upostag "DET")
               (t? t2 :form "respeitad*" :upostag "VERB")
               (t? t3 :upostag "NOUN"))
      (convert-verb-to-adj t2))))

;; DET seguido da palavra "determinad*" anotada como VERB -> VERB vira ADJ
(defun fix-determinad* (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (t? t1 :upostag "DET")
               (t? t2 :form "determinad*" :upostag "VERB"))
      (convert-verb-to-adj t2))))

;; palavra "um" anotada como DET seguida pela apalvra "dado" anotada como VERB -> VERB vira ADJ
(defun fix-um (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (t? t1 :form "um" :upostag "DET")
               (t? t2 :form "dado" :upostag "VERB"))
      (convert-verb-to-adj t2))))

;; palavra "uma" anotada como DET seguida pela apalvra "dada" anotada como VERB -> VERB vira ADJ
(defun fix-uma (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (t? t1 :form "uma" :upostag "DET")
               (t? t2 :form "dada" :upostag "VERB"))
      (convert-verb-to-adj t2))))

;; palavra "tão" anotada como ADV seguida por VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
;; palavra "muito" anotada como ADV seguida por VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
;; palavra "mais" anotada como ADV seguida por VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
(defun fix-tao/muito/mais (tokens)
  (let ((t1 (car tokens))
        (t2 (cdr tokens)))
    (when (and t1 t2
               (or (t? t1 :form "tão" :upostag "ADV")
                   (t? t1 :form "muito" :upostag "ADV")
                   (t? t1 :form "mais" :upostag "ADV"))
               (t? t2 :upostag "VERB")
               (is-pcp t2))
      (convert-verb-to-adj t2))))

;; palavra "até" anotada como ADP seguida por "então" anotada como ADV seguida por VERB terminado em "do", "da", "dos", "das", "to", "ta", "tos", "tas" -> VERB vira ADJ
(defun fix-ate (tokens)
  (let ((t1 (first tokens))
        (t2 (second tokens))
        (t3 (third tokens)))
    (when (and t1 t2 t3
               (t? t1 :form "até" :upostag "ADP")
               (t? t2 :form "então" :upostag "ADV")
               (t? t3 :upostag "ADV")
               (is-pcp t3))
      (convert-verb-to-adj t3))))

(defun fix-corpus (sentences)
  (mapc (lambda (s) (mapc #'fix-adjectives (sentence-tokens s))) sentences)
  (mapc (lambda (s) (mapc #'fix-bem/mal (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-ficar/estar (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-passad* (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-respeitad* (collect-next-element3 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-determinad* (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-um (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-uma (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-tao/muito/mais (collect-next-element2 (sentence-tokens s)))) sentences)
  (mapc (lambda (s) (mapc #'fix-ate (collect-next-element3 (sentence-tokens s)))) sentences))

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
