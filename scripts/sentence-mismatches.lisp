(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun get-forms (start end sentence)
  (mapcar #'token-form 
          (mapcar (lambda (x) (find-token x (sentence-tokens sentence)))
                  (alexandria:iota (1+ (- end start)) :start start))))

(defparameter *mt* nil)

;; more flexible, ignore cases and space differences
(defun match/flex (sentences stream)
  (dolist (s sentences)
    (let* ((sid (sentence-meta-value s "sent_id"))
           (stext (sentence->text s))
           (text (sentence-meta-value s "text")))
      (when (not (string-equal (remove #\space stext) (remove #\space text)))
        (format stream "[~a] ~a~%[~a] ~a~%~%" sid text sid stext))))
  sentences)

(defun match/strict (sentences stream)
  (dolist (s sentences)
    (let* ((sid (sentence-meta-value s "sent_id"))
           (stext (sentence->text s))
           (text (sentence-meta-value s "text")))
      (when (not (string= stext text))
        (format stream "[~a] ~a~%[~a] ~a~%~%" sid text sid stext))))
  sentences)

(defun run ()
  (with-open-file (stream "mismatches.txt" :direction :output :if-exists :supersede)
    (dolist (fn (directory "documents/*.conllu"))
      (match/flex (read-conllu fn) stream))))

