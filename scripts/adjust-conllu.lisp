#!/usr/local/bin/sbcl --script

;; Usage: ./adjust-conllu.lisp input-file.conllu output-file.conllu

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun adjust-conllu (input-filename output-filename &key (if-exists :supersede))
  (let ((sentences (read-conllu input-filename)))
    (with-open-file (out output-filename :direction :output :if-exists if-exists)
      (write-conllu-to-stream (mapcar #'adjust-sentence sentences) out))))

(adjust-conllu (cadr sb-ext:*posix-argv*) (caddr sb-ext:*posix-argv*))
