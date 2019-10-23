;; code that handles various MWE fixes.

(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y) 
     on list
     collect (list x y)))

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
    (setf (slot-value (nth i tokens) 'upostag) (nth i composition))
    (setf (slot-value (nth i tokens) 'xpostag) "_")))

(defun fix-mwe (tokens mwe sent-id &key posmwe deprel composition)
  (when 
      (and 
       tokens
       (not (some #'null tokens))
       (= (length tokens) (length composition))
       (string-equal (slot-value (first tokens) 'misc) (format nil "MWE=~a" mwe)))
    (if (isolated? tokens)
        (progn 
          (update-mwe tokens mwe posmwe)
          (update-deprel tokens deprel)
          (update-postag tokens composition))
        (format t "Strange MWE in ~a: ~a.~%" mwe sent-id))))

(defparameter *mwe-115* '("ácido_úrico"
                          "água_mineral"
                          "água_potável"
                          "assistência_médica"
                          "assistência_social"
                          "barra_pesada"
                          "bode_expiatório"
                          "bossa_nova"
                          "braço_direito"
                          "câmara_municipal"
                          "campanha_eleitoral"
                          "capital_social"
                          "capítulo_provincial"
                          "círculo_vicioso"
                          "comunicação_social"
                          "construção_civil"
                          "corpo_docente"
                          "cruzeiros_reais"
                          "dívida_pública"
                          "enfisema_pulmonar"
                          "engenharia_genética"
                          "ensino_primário"
                          "ensino_superior"
                          "escola_básica"
                          "escola_preparatória"
                          "escola_primária"
                          "explosão_demográfica"
                          "fundo_falso"
                          "governador_civil"
                          "greve_geral"
                          "guerra_civil"
                          "guerra_fria"
                          "impressora_matricial"
                          "lei_orgânica"
                          "lençol_freático"
                          "letra_morta"
                          "luz_verde"
                          "mala_direta"
                          "mercado_negro"
                          "metro_quadrado"
                          "música_pimba"
                          "número_inteiro"
                          "opinião_pública"
                          "óculos_escuros"
                          "pão_duro"
                          "pão_francês"
                          "papel_higiênico"
                          "pastilha_elástica"
                          "ponto_final"
                          "ponto_fraco"
                          "relações_públicas"
                          "roda_dentada"
                          "sagrado_matrimónio"
                          "salário_mínimo"
                          "sentido_único"
                          "ser_humano"
                          "seres_humanos"
                          "serviço_militar"
                          "sociedade_civil"
                          "terra_natal"
                          "via_terrestre"
                          "vida_privada"
                          "vidro_fosco"
                          "vila_velha"
                          "bens_imóveis"
                          "bodes_expiatórios"
                          "classes_médias"
                          "espelhos_retrovisores"
                          "funcionários_públicos"
                          "metros_cúbicos"
                          "metros_quadrados"
                          "palavras_cruzadas"
                          "salários_mínimos"))

(defun fix-corpus (sentences)
  (dolist (mwe *mwe-115*)
    (mapc (lambda (s) 
            (mapc (lambda (tks) 
                    (fix-mwe tks mwe (sentence-meta-value s "sent_id") :posmwe "NOUN" :deprel "compound" :composition '("NOUN" "ADJ")))
                  (collect-next-element (sentence-tokens s)))) sentences))
  sentences)

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
