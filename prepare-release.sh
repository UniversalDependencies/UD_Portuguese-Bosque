#!/bin/bash

if [ ! -e bosque_CP.udep.workaround.conll ]; then
    echo "Missing bosque_CP.udep.workaround.conll; run fix-errors.sh."
    exit 1
fi

if [ ! -e bosque_CF.udep.workaround.conll ]; then
    echo "Missing bosque_CF.udep.workaround.conll; run fix-errors.sh."
    exit 1
fi

# remove </s> tags, and fix the <s> comments.
cat bosque_C{F,P}.udep.workaround.conll | grep -v "^#</s>" | sed -e 's/#<s\(.*\)>/#\1/' | cat -s > pt.conllu

# remove leading whitespace
# http://www.linuxhowtos.org/System/sedoneliner.htm?ref=news.rdf
cat pt.conllu | sed '/./,$!d' > tmp && mv tmp pt.conllu

# remove empty line after the comment (flagged by the validation tool)
python3 fix-comments.py pt.conllu > tmp && mv tmp pt.conllu

# split pt.conllu in dev/test/train, using the percentages specified in the split3.py file
python3 split3.py pt.conllu pt-ud-dev.conllu pt-ud-test.conllu pt-ud-train.conllu
