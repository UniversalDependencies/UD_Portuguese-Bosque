#!/usr/local/bin/sbcl --script
;; Usage: (Work in Progress)
;;
;; For selecting sentences from a .conllu file:
;; $ run-cl-conllu select file id-sent
;;
;;
;; For modifying a .conllu:
;; $ run-cl-conllu modify original-file modifying-file add-new
;;
;; If X.conllu is the original file and Y.conllu is a new file,
;; outputs a Z.conllu, which is a modified X file, where, for each
;; sentence S:
;; - if S is in X and Y (checked via sent_id metavalue), use Y's version
;; - if S is only in X, keep it as it is
;; - if S is only in Y, depends on a "add-new" key
;;   - if its value is "t", then add at the end of the file
;;   - if it's value is "nil", then it doesn't appear in Z.

;; For more details, see command-line.lisp


;; Global variable for arguments when calling as a script with sbcl:
;; sb-ext:*posix-argv*
(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-conllu
	      :silent t)

(in-package :cl-conllu)

(cond ((equal (nth 1 sb-ext:*posix-argv*)
	      "select")
       (let ((sent_id
	      (nth 3 sb-ext:*posix-argv*)))
	 (write-selected-sentence (nth 2 sb-ext:*posix-argv*)
				  (if (ppcre:scan "[^\\d]" sent_id)
				      sent_id
				      (parse-integer sent_id)))))
      ((equal (nth 1 sb-ext:*posix-argv*)
	      "modify")
       (write-conllu-to-stream
	(modify-conllu (nth 2 sb-ext:*posix-argv*)
		       (nth 3 sb-ext:*posix-argv*)
		       (if (>= (length sb-ext:*posix-argv*)
			       5)
			   (nth 4 sb-ext:*posix-argv*)
			   t))
	t)))
