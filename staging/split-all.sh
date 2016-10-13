for f in sample/raw/*.txt; do timeout 5m ./split.py $f > sample/split/`basename $f`; done
