# make sure that the final result is placed on
# bosque_C{P,F}.udep.workaround.conll and does NOT update the original

# fix issue #2
cat bosque_CP.udep.conll | sed -e 's/Gender=None/Gender=Fem,Masc/' > tmp && mv tmp bosque_CP.udep.workaround.conll
cat bosque_CF.udep.conll | sed -e 's/Gender=None/Gender=Fem,Masc/' > tmp && mv tmp bosque_CF.udep.workaround.conll


