#!/bin/bash

# set language
LANG=pt
FREELINGDIR=~/bin/freeling-4.0/

CFG=$LANG/dep1.cfg

$FREELINGDIR/bin/treeler train $CFG
