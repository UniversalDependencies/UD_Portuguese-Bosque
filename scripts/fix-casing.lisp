(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun get-forms (start end sentence)
  (mapcar #'token-form 
          (mapcar (lambda (x) (find-token x (sentence-tokens sentence)))
                  (alexandria:iota (1+ (- end start)) :start start))))

(defparameter *mt* nil)

(defun delimiterp (c)
  (position c " ,.;/()[]:'«»"))

(defun fix (sentences)
  (dolist (s sentences)
    (let* ((tokens (sentence-tokens s))
          (text (split-sequence-if #'delimiterp (sentence-meta-value s "text") :remove-empty-subseqs nil)))
      (dotimes (i (length tokens))
        (when (< i (length text))
         (let ((t2 (elt text i))
               (t1 (token-form (elt tokens i))))
           (when (and (> (length (elt text i)) 2) (string-equal t1 t2) (not (string= t1 t2)))
             (setf (token-form (elt tokens i)) (elt text i))))))))
  sentences)

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

