(ql:quickload :cl-conllu)

(in-package :cl-conllu)

;; awk '$2 ~ /\-/ && $4 ~ /VERB/ {print $0 ; getline ; print $0, FILENAME}' ../documents/*.conllu
;; awk '$2 ~ /\-/ && $4 ~ /VERB/ && $6 ~ /VerbForm=Inf/ {print $0 ; getline ; print $0, FILENAME}' ../documents/*.conllu
;; awk '$2 ~ /(á|é|ó|â|ê|ô)\-/ && $4 ~ /VERB/ && $6 ~ /VerbForm=Inf/ {print $0 ; getline ; print $0, FILENAME}' ../documents/*.conllu
;; awk '$2 ~ /(a|e|i|o|u|á|é|í|ó|ú|â|ê|î|ô|û|à|è|ì|ò|ù)\-/ && $4 ~ /VERB/ && $6 ~ /VerbForm=Inf/ {print $0 ; getline ; print $0, FILENAME}' ../documents/*.conllu

;; Idea:
;; if VerbForm=Inf, check if it ends with a vowel. In these cases, we want to substitute this verb form with its lemma
;; However, this won't correct everything ("Fê-lo" is an example)
;;
;; Furthermore, we add the mtoken, if not yet inserted

;; $ awk '$2 ~ /(a|e|i|o|u|á|é|í|ó|ú|â|ê|î|ô|û|à|è|ì|ò|ù)\-/ && $4 ~ /VERB/ && $6 ~ /VerbForm=Inf/ {counter++} END {print counter}' ../documents/*.conllu
;; 50
;; awk '$2 ~ /(a|e|i|o|u|á|é|í|ó|ú|â|ê|î|ô|û|à|è|ì|ò|ù)\-/ && $4 ~ /VERB/ && $6 ~ /VerbForm=Inf/ {printf $2 ; getline ; print $2}' ../documents/*.conllu | sort | uniq -c
;;       1 amá-lo
;;       1 apanhá-los
;;       1 apresentá-lo
;;       1 aprová-lo
;;       1 arrastá-la
;;       1 atendê-los
;;       1 capitalizá-la
;;       1 conduzi-lo
;;       1 convencê-los
;;       1 coordená-la
;;       1 cumprí-la
;;       1 desagradá-la
;;       1 destroçá-la
;;       1 detê-lo
;;       1 distribuí-los
;;       1 divulgá-las
;;       1 dotá-lo
;;       1 encaixá-lo
;;       1 encontrá-la
;;       2 fazê-lo
;;       1 fazê-los
;;       1 ilustrá-lo
;;       1 inalá-lo
;;       1 incorporá-lo
;;       1 interrompê-la
;;       1 lançá-la
;;       1 levá-las
;;       1 libertá-lo
;;       1 localizá-lo
;;       1 mandá-las
;;       1 mantê-lo
;;       1 mantê-los
;;       1 matá-la
;;       1 medi-lo
;;       1 nomeá-lo
;;       1 paralisá-la
;;       1 produzí-lo
;;       1 reintegrá-lo
;;       1 resolvê-la
;;       1 roubá-la
;;       1 sofrê-la
;;       1 substituí-lo
;;       1 tê-lo
;;       1 torná-la
;;       1 torná-lo
;;       1 treiná-lo
;;       1 vaciná-la
;;       1 vende-las
;;       1 visitá-la

;; awk '{f=$2} $2 ~ /(a|e|i|o|u|á|é|í|ó|ú|â|ê|î|ô|û|à|è|ì|ò|ù)\-/ && $4 ~ /VERB/  && $6 ~ /VerbForm=/ && getline && $2 !~ /se/  {printf f ;  print $2, FILENAME}' ../documents/*.conllu
