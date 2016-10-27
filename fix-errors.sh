# make sure that the final result is placed on
# bosque_C{P,F}.udep.workaround.conll and does NOT update the original

cp bosque_CP.udep.conll bosque_CP.udep.workaround.conll
cp bosque_CF.udep.conll bosque_CF.udep.workaround.conll

# fix issues #19 and #30
sbcl --noinform --noprint --non-interactive --load join-features.lisp --eval '(in-package :join-features)' --eval '(execute "bosque_CP.udep.workaround.conll" "tmp1")' --eval '(execute "bosque_CF.udep.workaround.conll" "tmp2")'
		
if [ -e "tmp1" ]; then mv tmp1 bosque_CP.udep.workaround.conll; fi
if [ -e "tmp2" ]; then mv tmp2 bosque_CF.udep.workaround.conll; fi

cat bosque_CF.udep.workaround.conll | sed 's/^</\n#</' | cat -s > tmp && mv tmp bosque_CF.udep.workaround.conll
cat bosque_CP.udep.workaround.conll | sed 's/^</\n#</' | cat -s > tmp && mv tmp bosque_CP.udep.workaround.conll
