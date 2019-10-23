;; move all spaceafter=no to the mtoken.  If there is a mwtoken
;; (I1-In) and in In there is a SpaceAfter=No, we need to move this
;; SpaceAfter=No to the mtoken.
(ql:quickload :cl-conllu)

(in-package :cl-conllu)

(defun find-token (id tokens)
  (find id tokens :key (lambda (tk) (slot-value tk 'id))))

(defun has-feature (key value features)
  (unless (string-equal "_" features)
    (find `(,key ,value) (split-sequence #\| features) :key (lambda (x) (split-sequence #\= x)) :test #'equal)))

(defun append-feature (key value feats)
  (let* ((features (if (string= "_" feats) nil
                       (split-sequence #\| feats))))
    (format nil "~{~a~^|~}" (remove-duplicates
                             (sort
                              (append features `(,(if (and key value) 
                                                      (format nil "~a=~a" key value)
                                                      (format nil "~a" value)))) #'string<)
                             :test #'equal))))

(defun remove-feature (key value features)
  (format nil "~{~a~^|~}" (remove-if (lambda (x) (equal `(,key ,value) (split-sequence #\= x))) (split-sequence #\| features))))

(defun fix (sentences)
  (dolist (s sentences)
    (let ((tokens (sentence-tokens s)))
      (dolist (mtk (sentence-mtokens s))
        (let ((last (find-token (mtoken-end mtk) tokens)))
          (when (has-feature "SpaceAfter" "No" (token-misc last))
            (when (has-feature "ChangedBy" "Issue165" (token-misc last))
              (setf (token-misc last) (remove-feature "ChangedBy" "Issue165" (token-misc last))))
            (setf (token-misc last) (remove-feature "SpaceAfter" "No" (token-misc last)))
            (when (equal "" (token-misc last))
              (setf (token-misc last) "_"))
            (setf (mtoken-misc mtk) (append-feature "SpaceAfter" "No" (mtoken-misc mtk))))))))
  sentences)

(defun run ()
  (dolist (fn (directory "documents/*.conllu"))
    (write-conllu (fix (read-conllu fn)) fn)))

