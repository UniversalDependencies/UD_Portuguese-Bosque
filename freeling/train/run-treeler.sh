#!/bin/bash

# set language
LANG=pt
FREELINGDIR=/usr/local

CFG=$LANG/dep1.cfg

$FREELINGDIR/bin/treeler train $CFG
