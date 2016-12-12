(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(in-package :cl-conllu)

(defun collect-next-element (list)
  (loop for (x y z) 
     on list
     collect (list x y z)))

;; NB: this doesn't check if t1 t2 t3 actually form a tree, but it
;; works for us since there are other checks later in the pipeline
;; that do check this---and we have the assumption that we are
;; receiving a well formed CoNLL anyway.  Also this could be more
;; elegantly implemented as a lowest common ancestor / search
;; operation, since eventually we'll need an implementation that
;; supports a varying number of nodes.
(defun isolated? (t1 t2 t3)
  "T1 T2 T3 are isolated. In other words, T1 points outside T2 and T3,
and T2 and T3 should either point at T1 or each other."
  (let ((id1 (slot-value t1 'id))
        (id2 (slot-value t2 'id))
        (id3 (slot-value t3 'id))
        (h1 (slot-value t1 'head))
        (h2 (slot-value t2 'head))
        (h3 (slot-value t3 'head)))
    (and (not (member h1 (list id2 id3)))
         (member h2 `(,id1 ,id3))
         (member h3 `(,id1 ,id2)))))

(defun fix-mwe (tokens mwe sent-id)
  (when tokens
    (let* ((t1 (first tokens))
           (t2 (second tokens))
           (t3 (third tokens))
           (id1 (slot-value t1 'id)))
      (when (and 
             t1 t2 t3
             (string-equal (slot-value t1 'misc) (format nil "MWE=~a" mwe)))
        (if (isolated? t1 t2 t3)
            (progn 
              (setf (slot-value t1 'upostag) "NOUN")
              (setf (slot-value t1 'xpostag) "_")
              (setf (slot-value t2 'upostag) "ADP")
              (setf (slot-value t2 'xpostag) "_")
              (setf (slot-value t3 'upostag) "NOUN")
              (setf (slot-value t3 'xpostag) "_")
              (setf (slot-value t2 'deprel) "compound")
              (setf (slot-value t3 'deprel) "compound")
              (setf (slot-value t2 'head) id1)
              (setf (slot-value t3 'head) id1)
              (setf (slot-value t1 'misc) (format nil "MWE=~a|MWEPOS=NOUN" mwe)))
            (format t "Strange MWE in ~a: ~a.~%" mwe sent-id))))))

(defparameter *mwes* '("associação_de_estudantes"
                       "ave_de_rapina"
                       "açúcar_em_pó"
                       "bancos_de_dados"
                       "bilhete_de_identidade"
                       "bilhetes_de_identidade"
                       "bolsas_de_estudo"
                       "bomba_de_gasolina"
                       "calções_de_banho"
                       "carros_de_combate"
                       "cartão_de_crédito"
                       "carvão_de_pedra"
                       "casa_de_banho"
                       "casas_de_banho"
                       "centro_de_mesa"
                       "certidão_de_óbito"
                       "chave_de_ouro"
                       "cheques_em_branco"
                       "conferência_de_imprensa"
                       "controle_de_natalidade"
                       "corrida_de_touros"
                       "custos_de_produção"
                       "estado_de_sítio"
                       "fim_de_semana"
                       "histórias_em_quadrinhos"
                       "horas_de_ponta"
                       "horários_de_trabalho"
                       "junta_de_freguesia"
                       "juntas_de_freguesia"
                       "juros_de_mora"
                       "lençol_de_água"
                       "levado_da_breca"
                       "mercado_de_trabalho"
                       "número_de_contribuinte"
                       "obra_de_arte"
                       "onda_de_choque"
                       "ondas_de_choque"
                       "ontem_de_manhã"
                       "pano_de_fundo"
                       "perigo_de_morte"
                       "ponte_de_safena"
                       "ponto_de_rebuçado"
                       "ponto_de_vista"
                       "pontos_de_vista"
                       "porta_de_saída"
                       "posto_de_trabalho"
                       "postos_de_trabalho"
                       "poços_de_petróleo"
                       "projecto_de_lei"
                       "sala_de_aula"
                       "salto_em_altura"
                       "samba_de_breque"
                       "taxas_de_câmbio"
                       "trabalho_de_sapa"
                       "tratamento_de_choque"
                       "tribunal_de_contas"))

(defun fix-corpus (sentences)
  (dolist (mwe *mwes*)
    (mapc (lambda (s) (mapc (lambda (tks) (fix-mwe tks mwe (sentence-meta-value s "sent_id"))) (collect-next-element (sentence-tokens s)))) sentences))
  sentences)

;; to replicate
;; (write-conllu (fix-corpus (read-conllu "bosque.udep.conll")) "bosque.fixed")
(defun run ()
  (dolist (f (cl-fad:list-directory #p "documents/"))
    (write-conllu (fix-corpus (read-conllu f)) f)))
;; and then concatenate all documents/* into bosque.udep.conll (via
;; join-documents.sh)
