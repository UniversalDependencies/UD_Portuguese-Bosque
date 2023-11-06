for f in *.conllu; do
    python ~/work/DepEdit/depedit/depedit.py -c ~/work/bosque-UD/scripts/depedit.test.ini $f > $f.edited
    OUT=$(diff $f $f.edited)
    if [ "$OUT" ]; then
	echo $f;
    else
	rm $f.edited
    fi
done
