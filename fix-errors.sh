# make sure that the final result is placed on
# bosque.udep.workaround.conll and does NOT update the original

cp bosque.udep.conll bosque.udep.workaround.conll

cat bosque.udep.workaround.conll | sed -e 's/\tPROP\t/\tPROPN\t/' -e 's/\tPRP\t/\tADP\t/' -e 's/\tPU\t/\tPUNCT\t/' > tmp && mv tmp bosque.udep.workaround.conll

# fix issues #19 and #30
sbcl --noinform --noprint --non-interactive --load join-features.lisp --eval '(in-package :join-features)' --eval '(execute "bosque.udep.workaround.conll" "tmp")' 
		
if [ -e "tmp" ]; then mv tmp bosque.udep.workaround.conll; fi

cat bosque.udep.workaround.conll | sed 's/^</\n#</' | cat -s > tmp && mv tmp bosque.udep.workaround.conll
