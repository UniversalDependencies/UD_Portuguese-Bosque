#!/bin/bash

# set language
LANG=pt
FREELINGDIR=/usr/local
EPOCHS=50

## Create maps for features

if [ ! -d $LANG ]; then mkdir -p $LANG; fi
if [ ! -d $LANG/maps ]; then mkdir -p $LANG/maps; fi
if [ ! -d $LANG/dep1.models ]; then mkdir -p $LANG/dep1.models; fi

cat $LANG/$LANG.train | gawk 'NF>0 {print $2}' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/words.map
cat $LANG/$LANG.train | gawk 'NF>0 {print $3}' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/lemmas.map
cat $LANG/$LANG.train | gawk 'NF>0 {print $4}' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/cpos.map
cat $LANG/$LANG.train | gawk 'NF>0 {print $5}' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/fpos.map
cat $LANG/$LANG.train | gawk 'NF>0 {print $8}' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/dependencies.map
cat $LANG/$LANG.train | gawk 'NF>0 {print $6}' | tr '|' '\n' | sort | uniq -c | sort -nrk 1 | gawk '{print n++,$2,$1}' >$LANG/maps/morphos.map

## create config file for training
CFG=$LANG/dep1.cfg
echo "mod=dep1" > $CFG
echo "L="`cat $LANG/maps/dependencies.map | wc -l` >> $CFG
echo "dict="$LANG/maps >> $CFG
echo "conllx=1" >> $CFG
echo "sentence-end=4" >> $CFG
echo "dependency-end=1" >> $CFG
echo "wavg=1" >> $CFG
echo "wt=1" >> $CFG
echo "T="$EPOCHS >> $CFG
echo "data="$LANG/$LANG.train >> $CFG
echo "val="$LANG/$LANG.devel >> $CFG
echo "dir="$LANG/dep1.models >> $CFG
