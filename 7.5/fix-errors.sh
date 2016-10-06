# fix issue #2
cat bosque_CP.udep.conll | sed -e 's/Gender=None/Gender=Com/' > tmp && mv tmp bosque_CP.udep.conll
cat bosque_CF.udep.conll | sed -e 's/Gender=None/Gender=Com/' > tmp && mv tmp bosque_CF.udep.conll

# fix issue #7
cat bosque_CP.udep.conll | sed -e 's/PronType=Indp/PronType=Ind/' > tmp && mv tmp bosque_CP.udep.conll
cat bosque_CF.udep.conll | sed -e 's/PronType=Indp/PronType=Ind/' > tmp && mv tmp bosque_CF.udep.conll
