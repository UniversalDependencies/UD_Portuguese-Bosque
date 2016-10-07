#!/bin/bash

LANG=pt

if [ ! -d $LANG ]; then mkdir -p $LANG; fi

cat ../bosque-ud-fl/devel.conll | awk 'NF==0 {print} NF>0 {print $1 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8 " _ _ _ _ _ _" }' > $LANG/$LANG.devel

cat ../bosque-ud-fl/train.conll | awk 'NF==0 {print} NF>0 {print $1 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8 " _ _ _ _ _ _" }' > $LANG/$LANG.train
