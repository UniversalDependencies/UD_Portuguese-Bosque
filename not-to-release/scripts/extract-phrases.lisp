(ql:quickload :split-sequence)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defpackage :extract-phrases
  (:use :cl :split-sequence :alexandria :cl-ppcre))

(in-package :extract-phrases)

(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))

;; https://groups.google.com/forum/#!topic/comp.lang.lisp/LXG1U7YuILU
(defun mapconcat (list delim)
  (reduce (lambda (x y)
            (concatenate 'string
                         x delim y))
          list))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun read-conll-line (l)
  (split-sequence #\tab l))

(defun extract-words (s)
  (remove nil
          (mapcar 
           (lambda (x) (second (read-conll-line x)))
           (split-sequence #\newline s))))

(defun print-sentence (words out)
  (when words
    (format out "~a~%" (mapconcat words " ") " ")))

(defun execute (infile outfile)
  (let ((sentences (split "\\n\\n" (file-string infile))))
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (length (mapc (lambda (x) (print-sentence x out)) 
                    (mapcar #'extract-words sentences))))))

;; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/tools/src_cmp/mkant/sc.cl
;; https://github.com/froydnj/diff
