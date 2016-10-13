# make sure that the final result is placed on
# bosque_C{P,F}.udep.workaround.conll and does NOT update the original

# fix issue #2
cat bosque_CP.udep.conll | sed -e 's/Gender=None/Gender=Fem,Masc/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.conll | sed -e 's/Gender=None/Gender=Fem,Masc/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue 24
cat bosque_CP.udep.workaround.conll | sed -e 's/Number=None/Number=Plur,Sing/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Number=None/Number=Plur,Sing/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue #31
cat bosque_CP.udep.workaround.conll | sed -e 's/Tense=Pret/Tense=Past/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Tense=Pret/Tense=Past/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue #32
cat bosque_CP.udep.workaround.conll | sed -e 's/Mood=Subj/Mood=Sub/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Mood=Subj/Mood=Sub/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue #33
cat bosque_CP.udep.workaround.conll | sed -e 's/Mood=Cond/Mood=Cnd/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Mood=Cond/Mood=Cnd/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue #34
cat bosque_CP.udep.workaround.conll | sed -e 's/Tense=Plup/Tense=Pqp/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Tense=Plup/Tense=Pqp/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issue #35
cat bosque_CP.udep.workaround.conll | sed -e 's/Mood=Imper/Mood=Imp/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.workaround.conll | sed -e 's/Mood=Imper/Mood=Imp/' > tmp && mv tmp bosque_CF.udep.workaround.conll

# fix issues #19 and #30
sbcl --noinform --noprint --non-interactive --load join-features.lisp --eval '(in-package :join-features)' --eval '(execute "bosque_CP.udep.workaround.conll" "tmp1")' --eval '(execute "bosque_CF.udep.workaround.conll" "tmp2")'
		
if [ -e "tmp1" ]; then mv tmp1 bosque_CP.udep.workaround.conll; fi
if [ -e "tmp2" ]; then mv tmp2 bosque_CF.udep.workaround.conll; fi
