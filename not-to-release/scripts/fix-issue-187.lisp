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

;; mesmo DET <cjt>
;; mesmo DET DET
;; mesmo DET <diff>
;; mesmo DET <first-cjt>
(defun fix (token)
  (when (and 
         (string-equal "mesmo" (token-lemma token))
         (string= (token-upostag token) "DET")
         (or (has-tag (token-xpostag token) "<cjt>")
             (has-tag (token-xpostag token) "DET")
             (has-tag (token-xpostag token) "<diff>")
             (has-tag (token-xpostag token) "<first-cjt>")))
    (when (token-deprel token) "det"
          (setf (token-deprel token) "amod"))
    (setf (token-upostag token) "ADJ")))

(defun fix-sentences (sentences)
  (mapc (lambda (s) (mapc #'fix (sentence-tokens s))) sentences))

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix-sentences (read-conllu fn)) fn)))
