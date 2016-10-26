# to validate, copy the files in validation/ to the data/ directory
# of the UD tools repository and execute something like this:
# python validate.py --max-err=0 --lang=pt2 <file>

cat bosque_CP.udep.workaround.conll | grep -v "^#" > tmp && cat -s tmp > bosque_CP.udep.validation.conll
cat bosque_CF.udep.workaround.conll | grep -v "^#" > tmp && cat -s tmp > bosque_CF.udep.validation.conll
