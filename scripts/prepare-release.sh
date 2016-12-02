#!/bin/bash

# join all documents into one

scripts/join-documents.sh > /tmp/tmp-bosque

# split pt.conllu in dev/test/train, using the percentages specified in the split3.py file
python3 scripts/split3.py /tmp/tmp-bosque /tmp/release/pt_bosque-ud-dev.conllu /tmp/release/pt_bosque-ud-test.conllu /tmp/release/pt_bosque-ud-train.conllu seq

