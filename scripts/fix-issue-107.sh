#!/bin/bash

# run this over all documents in documents then join-documents.sh
# for f in documents/*; do scripts/fix-issue-107.sh $f; done

cat $1 | sed -e 's/\tname\t/\tflat:name\t/' > /tmp/tmp$$ && mv /tmp/tmp$$ $1
