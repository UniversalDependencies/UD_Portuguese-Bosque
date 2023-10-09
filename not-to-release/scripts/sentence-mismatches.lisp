(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun get-forms (start end sentence)
  (mapcar #'token-form 
          (mapcar (lambda (x) (find-token x (sentence-tokens sentence)))
                  (alexandria:iota (1+ (- end start)) :start start))))

(defparameter *mt* nil)

(defun clean1 (text)
  (remove-if-not #'alpha-char-p text))

(defun clean2 (text)
  (remove #\space text))

;; more flexible, ignore cases and space differences
(defun match/flex (sentences stream)
  (dolist (s sentences)
    (let* ((sid (sentence-meta-value s "sent_id"))
           (stext (sentence->text s))
           (text (sentence-meta-value s "text")))
      (when (not (string-equal (clean1 stext) (clean1 text)))
        (format stream "[~a] ~a~%[~a] ~a~%~a (~a)~%~%" sid text sid stext (mismatch (clean1 stext) (clean1 text)) (subseq (clean1 text) (mismatch (clean1 stext) (clean1 text)))))))
  sentences)

(defun match/strict (sentences stream streamids)
  (dolist (s sentences)
    (let* ((sid (sentence-meta-value s "sent_id"))
           (stext (sentence->text s))
           (text (sentence-meta-value s "text")))
      (when (not (string= stext text))
        (format streamids "~a~%" sid)
        (format stream "[~a] ~a~%[~a] ~a~%{~a}~%~%" sid text sid stext (subseq text (mismatch stext text))))))
  sentences)

(defun run ()
  (with-open-file (stream "mismatches.txt" :direction :output :if-exists :supersede)
    (with-open-file (streamids "mismatches-ids.txt" :direction :output :if-exists :supersede)
      (dolist (fn (directory "documents/*.conllu"))
        (match/strict (read-conllu fn) stream streamids)))))

(run)

;; to validate all "valid" sentences, do the following.
;; cat pt-ud-*.txt | sort > tmp
;; cat mismatches-ids.txt | sort > x && mv x mismatches-ids.txt 
;; comm -23 tmp mismatches-ids.txt > final.txt
;; then exec generate-release.lisp and validate.py --lang pt
