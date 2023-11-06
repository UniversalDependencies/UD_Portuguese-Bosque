(ql:quickload :cl-conllu)
(ql:quickload :split-sequence)

(in-package :cl-conllu)

(defun has-tag (xpostag tag)
  (member tag (split-sequence:split-sequence #\| xpostag) :test #'equal))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun remove-feature-by-key (key features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal key (car (split-sequence #\= x)))) (split-sequence #\| features))))

;; > qual DET DET								
;; > qual PRON DET

;; --> em ambos, PronType=Rel, EXCETO nos casos de "qual" com a indicação <interr>, que  serão PronType=Int.
(defun fix-qual (token)
  (when (and 
         (string-equal "qual" (token-lemma token))
         (or (string= (token-upostag token) "DET")
             (string= (token-upostag token) "PRON"))
         (has-tag (token-xpostag token) "DET"))
    (setf (token-feats token) (remove-feature-by-key "PronType" (token-feats token)))
    (if (has-tag (token-xpostag token) "<interr>")
        (setf (token-feats token) (append-feature "PronType" "Int" (token-feats token)))
        (setf (token-feats token) (append-feature "PronType" "Rel" (token-feats token))))))


(defun fix-sentences (sentences)
  (mapc (lambda (s) (mapc #'fix-qual (sentence-tokens s))) sentences))

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix-sentences (read-conllu fn)) fn)))
