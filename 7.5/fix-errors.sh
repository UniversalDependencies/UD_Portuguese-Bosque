# fix issue #2
cat bosque_CP.udep.conll | sed -e 's/Gender=None/Gender=Com/' > tmp && mv tmp bosque_CP.udep.conll
cat bosque_CF.udep.conll | sed -e 's/Gender=None/Gender=Com/' > tmp && mv tmp bosque_CF.udep.conll

# fix issue #7
cat bosque_CP.udep.conll | sed -e 's/PronType=Indp/PronType=Ind/' > tmp && mv tmp bosque_CP.udep.conll
cat bosque_CF.udep.conll | sed -e 's/PronType=Indp/PronType=Ind/' > tmp && mv tmp bosque_CF.udep.conll

# fix issues #8 and #19
sbcl --noinform --noprint --non-interactive --load join-features.lisp --eval '(in-package :join-features)' --eval '(execute "bosque_CP.udep.conll" "tmp1")' --eval '(execute "bosque_CF.udep.conll" "tmp2")'

if [ -e "tmp1" ]; then mv tmp1 bosque_CP.udep.conll; fi
if [ -e "tmp2" ]; then mv tmp2 bosque_CF.udep.conll; fi


