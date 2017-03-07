#!/bin/bash

export LD_LIBRARY_PATH=/usr/local/lib

if [ ! -d bosque-ud-fl ]; then mkdir bosque-ud-fl; fi

# NOTE: you need to concatenate all Bosque documents and put it into a
# bosque-ud/ directory.  To do that you can either use the
# ../scripts/generate-release.lisp file or the
# ../scripts/join-documents.sh file.  It doesn't matter in the end.
# Just make sure the "for" below finds the correct files.

# NOTE: needs interset (see convert.txt, at the bottom, for the
# location of the project).
for f in *.conllu; do
    perl conll_convert_tags_from_uposf.pl $f > bosque-ud-fl/`basename $f`;
done

if [ -e bosque-ud-fl/bosque-with-tags.conll ]; then rm -f bosque-ud-fl/bosque-with-tags.conll; fi
if [ -e bosque-ud-fl/bosque.conll ]; then rm -f bosque-ud-fl/bosque.conll; fi

# NOTE: the same file pattern used above (first "for") needs to be
# repeated here.
for f in bosque-ud-fl/*.conllu; do
    cat -s $f >> bosque-ud-fl/bosque-with-tags.conll
done

grep -v "^#" bosque-ud-fl/bosque-with-tags.conll | cat -s - > bosque-ud-fl/bosque.conll
grep "^# text" bosque-ud-fl/bosque-with-tags.conll | sed -e "s/\# text = \(.*\)/\1\n/" > bosque-ud-fl/bosque-sentences.txt

python3 split2.py bosque-ud-fl/bosque.conll bosque-ud-fl/devel.conll bosque-ud-fl/train.conll

# NOTE: make sure you update this path to your local Freeling installation.
/usr/local/bin/analyze -f pt.cfg < bosque-ud-fl/bosque-sentences.txt > bosque-ud-fl/bosque-sentences.freeling.txt

cat bosque-ud-fl/bosque-sentences.freeling.txt | awk '{print $1 " " $2 " " $3}' > bosque-ud-fl/freeling.tokens

cat bosque-ud-fl/bosque.conll | awk '{print $2 " " $3 " " $5}' > bosque-ud-fl/palavras.tokens
