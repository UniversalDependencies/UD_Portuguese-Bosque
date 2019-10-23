for f in sample/raw/*.ext.txt; do cat $f | sed -e 's/¶/\n/g' > /tmp/tmp && mv /tmp/tmp sample/raw/$f; done
