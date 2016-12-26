;; code that handles various MWE fixes.

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element2 (list)
  (loop for (x y) 
     on list
     collect (list x y)))

(defun collect-next-element3 (list)
  (loop for (x y z) 
     on list
     collect (list x y z)))

;; Check if TOKENS form an isolated set of vertices, where one
;; connects outside, but every other vertice connects to someone
;; inside.  Ideally we need to check if they form a tree, but since we
;; are expecting to deal with valid CoNLL files, we may skip this
;; extra step for now.
(defun isolated? (tokens)
  (flet ((token-id (tk) (slot-value tk 'id))
         (token-head (tk) (slot-value tk 'head)))
    (let ((ids (mapcar #'token-id tokens))
          (parents (mapcar #'token-head tokens)))
      (= 1 (length (remove-if (lambda (tk) (member tk ids)) parents))))))

(defun external-connection (tokens)
  (flet ((token-id (tk) (slot-value tk 'id))
         (token-head (tk) (slot-value tk 'head))
         (token-deprel (tk) (slot-value tk 'deprel)))
    (let ((ids (mapcar #'token-id tokens))
          (parents (mapcar (lambda (tk) (cons (token-head tk) (token-deprel tk))) tokens)))
      (car (remove-if (lambda (tk) (member (car tk) ids)) parents)))))

(defun update-mwe (tokens mwe posmwe)
  (setf (slot-value (first tokens) 'misc) (format nil "MWE=~a|MWEPOS=~a" mwe posmwe)))

(defun update-deprel (tokens deprel)
  (let ((external-connection (external-connection tokens)))
    (setf (slot-value (car tokens) 'head) (car external-connection))
    (setf (slot-value (car tokens) 'deprel) (cdr external-connection)))
  (mapc (lambda (tk)
          (setf (slot-value tk 'head) (slot-value (car tokens) 'id))
          (setf (slot-value tk 'deprel) deprel))
        (cdr tokens)))

(defun update-postag (tokens composition)
  (dotimes (i (length tokens))
    (setf (slot-value (nth i tokens) 'upostag) (if (>= i (length composition)) (car (last composition)) (nth i composition)))
    (setf (slot-value (nth i tokens) 'xpostag) "_")))

(defun fix-mwe (tokens mwe sent-id &key posmwe deprel composition)
  (when 
      (and 
       tokens
       (not (some #'null tokens))
       (string-equal (slot-value (first tokens) 'misc) (format nil "MWE=~a" mwe)))
    (if (isolated? tokens)
        (progn 
          (update-mwe tokens mwe posmwe)
          (update-deprel tokens deprel)
          (update-postag tokens composition))
        (format t "Strange MWE in ~a: ~a.~%" mwe sent-id))))

(defparameter *mwe1* '("commedia_dell'arte"
                       "cash_flow"
                       "network_computing"
                       "wild_cards"
                       "world_class"
                       "body_art"
                       "body_piercing"
                       "fly_bridge"
                       "safety_car"
                       "very_typical"
                       "consejero_de_créacion"
                       "best_seller"
                       "big_band"
                       "milk_shake"
                       "black_man"
                       "death_fuck"
                       "drag_king"
                       "market_makers"
                       "godfather_of_soul"
                       "thin_clients"
                       "hit_parade"
                       "jet_lag"
                       "joint_venture"
                       "jam_sessions"
                       "lana_caprina"
                       "drag_queen"
                       "dream_team"
                       "fax_hotline"
                       "mea_culpa"
                       "pole_position"
                       "shopping_center"
                       "shopping_centers"
                       "shoppings_centers"
                       "status_quo"
                       "strip_tease"
                       "prélude_non-mesuré"
                       "traveler_cheques"
                       "primitive_dream_paintings"
                       "nouveau_réalisme"
                       "direct_mailing"
                       "dolce_far_niente"
                       "top_model"
                       "food_experiences"
                       "search_and_replace"
                       "grand_babou"
                       "jet_ski"
                       "mass_media"
                       "serial_killer"
                       "rock'n_roll"
                       "blues_rap"))

(defparameter *mwe2* '("made_in_England" "made_in_USA" "in_natura"))
(defparameter *mwe3* '("in_loco" "on_line"))

(defun fix-corpus (sentences)
  (flet ((collection-function (str)
           (let ((k (1+ (count #\_ str))))
             (ccase k
                    (2 #'collect-next-element2)
                    (3 #'collect-next-element3)))))
    (dolist (mwe *mwe1*)
      (mapc (lambda (s) 
              (mapc (lambda (tks) 
                      (fix-mwe tks mwe (sentence-meta-value s "sent_id") :posmwe "NOUN" :deprel "flat:foreign" :composition '("X")))
                    (funcall (collection-function mwe) (sentence-tokens s)))) sentences))

    (dolist (mwe *mwe2*)
      (mapc (lambda (s) 
              (mapc (lambda (tks) 
                      (fix-mwe tks mwe (sentence-meta-value s "sent_id") :posmwe "ADJ" :deprel "flat:foreign" :composition '("X")))
                    (funcall (collection-function mwe) (sentence-tokens s)))) sentences))

    (dolist (mwe *mwe3*)
      (mapc (lambda (s) 
              (mapc (lambda (tks) 
                      (fix-mwe tks mwe (sentence-meta-value s "sent_id") :posmwe "ADV" :deprel "flat:foreign" :composition '("X")))
                    (funcall (collection-function mwe) (sentence-tokens s)))) sentences)))
  sentences)

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
