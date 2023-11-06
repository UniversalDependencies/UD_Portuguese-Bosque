(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun sam-? (tk)
  (search "<sam->" (token-xpostag tk)))

(defun -sam? (tk)
  (search "<-sam>" (token-xpostag tk)))

(defun search-mtoken (start end mtokens)
  (find (cons start end) mtokens :test #'equal))

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun get-forms (start end sentence)
  (mapcar #'token-form 
          (mapcar (lambda (x) (find-token x (sentence-tokens sentence)))
                  (alexandria:iota (1+ (- end start)) :start start))))

(defparameter *mt* nil)

(defun read-mtokens (mt-file)
  (setf *mt* (make-hash-table :test #'equal))
  (with-open-file (stream mt-file)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (setf (gethash (car (split-sequence #\, line)) *mt*) (cadr (split-sequence #\, line))))))

(defun fix (sentences f)
  (dolist (s sentences)
    (let ((tokens (sentence-tokens s))
          (text (sentence-meta-value s "text"))
          (mtokens (mapcar (lambda (x) (cons (mtoken-start x) (mtoken-end x))) (sentence-mtokens s))) 
          (start)
          (end))
     (dolist (tk tokens)
       (when (sam-? tk)
         (setf end nil)
         (setf start (token-id tk)))
       (when (and start (-sam? tk))
         (setf end (token-id tk))
         (when (and start end (= 1 (- end start)) (not (search-mtoken start end mtokens)))
           (let ((form (gethash (format nil "~a ~a" (first (get-forms start end s))
                                        (second (get-forms start end s))) *mt*)))
             (when (and form (search form text))
               (setf (sentence-mtokens s) (append (sentence-mtokens s) (list (make-instance 'mtoken :start start :end end :form form))))))
           (format f "(~a) ~a-~a (~a) ~a~%" (sentence-meta-value s "sent_id") start end (get-forms start end s) text))
         (setf end nil)
         (setf start nil)))))
  sentences)

(defun run ()
  (read-mtokens #p"scripts/mt.csv")
  (with-open-file (f "ranges.txt" :direction :output :if-exists :supersede)
    (dolist (fn (directory "documents/*.conllu"))
      (write-conllu (fix (read-conllu fn) f) fn))))

