for f in 7.5/*.udep.conll; do
    perl conll_convert_tags_from_uposf.pl $f > 7.5-fl/`basename $f`;
done

if [ ! -d 7.5-fl ]; then mkdir 7.5-fl; fi

if [ -e 7.5-fl/bosque-with-tags.conll ]; then rm -f 7.5-fl/bosque-with-tags.conll; fi
if [ -e 7.5-fl/bosque.conll ]; then rm -f 7.5-fl/bosque.conll; fi

for f in 7.5-fl/*.udep.conll; do
    cat -s $f >> 7.5-fl/bosque-with-tags.conll
done

grep -v "^<" 7.5-fl/bosque-with-tags.conll | cat -s - > 7.5-fl/bosque.conll
grep "^<s" 7.5-fl/bosque-with-tags.conll | sed -e "s/\(.*\)text=\"\(.*\)\">/\2\n/" > 7.5-fl/bosque-sentences.txt

python split.py 7.5-fl/bosque.conll 7.5-fl/devel.conll 7.5-fl/train.conll

~/bin/freeling-4.0/bin/analyze -f pt.cfg < 7.5-fl/bosque-sentences.txt > 7.5-fl/bosque-sentences.freeling.txt

cat 7.5-fl/bosque-sentences.freeling.txt | awk '{print $1 " " $2 " " $3}' > 7.5-fl/freeling.tokens

cat 7.5-fl/bosque.conll | awk '{print $2 " " $3 " " $5}' > 7.5-fl/palavras.tokens
