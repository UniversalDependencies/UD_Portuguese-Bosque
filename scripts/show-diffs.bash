for f in *.conllu; do
    OUT=$(diff $f $(basename $f .conllu).depedit.edited)
    if [ "$OUT" ]; then
       echo $f;
    fi
done
