#!/bin/bash

for line in $(find . -path "./documents/*.conllu" | sort -V)
do
    cat $line
    echo
done
