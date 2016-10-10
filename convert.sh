#!/bin/bash

pushd bosque-ud
. ./fix-errors.sh
popd

if [ ! -d bosque-ud-fl ]; then mkdir bosque-ud-fl; fi

for f in bosque-ud/*.udep.workaround.conll; do
    perl conll_convert_tags_from_uposf.pl $f > bosque-ud-fl/`basename $f`;
done

if [ -e bosque-ud-fl/bosque-with-tags.conll ]; then rm -f bosque-ud-fl/bosque-with-tags.conll; fi
if [ -e bosque-ud-fl/bosque.conll ]; then rm -f bosque-ud-fl/bosque.conll; fi

for f in bosque-ud-fl/*.udep.conll; do
    cat -s $f >> bosque-ud-fl/bosque-with-tags.conll
done

grep -v "^<" bosque-ud-fl/bosque-with-tags.conll | cat -s - > bosque-ud-fl/bosque.conll
grep "^<s" bosque-ud-fl/bosque-with-tags.conll | sed -e "s/\(.*\)text=\"\(.*\)\">/\2\n/" > bosque-ud-fl/bosque-sentences.txt

python split.py bosque-ud-fl/bosque.conll bosque-ud-fl/devel.conll bosque-ud-fl/train.conll

~/bin/freeling-4.0/bin/analyze -f pt.cfg < bosque-ud-fl/bosque-sentences.txt > bosque-ud-fl/bosque-sentences.freeling.txt

cat bosque-ud-fl/bosque-sentences.freeling.txt | awk '{print $1 " " $2 " " $3}' > bosque-ud-fl/freeling.tokens

cat bosque-ud-fl/bosque.conll | awk '{print $2 " " $3 " " $5}' > bosque-ud-fl/palavras.tokens
