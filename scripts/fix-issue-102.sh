#!/bin/bash

# run this over all documents in documents then join-documents.sh

cat $1 | sed -e 's/\tCONJ\t/\tCCONJ\t/' | sed -e 's/MWEPOS=CONJ/MWEPOS=CCONJ/' > /tmp/tmp$$ && mv /tmp/tmp$$ $1

cat $1 | sed -e 's/\tdobj\t/\tobj\t/' > /tmp/tmp$$ && mv /tmp/tmp$$ $1

cat $1 | sed -e 's/\tmwe\t/\tfixed\t/' > /tmp/tmp$$ && mv /tmp/tmp$$ $1
