#!/usr/bin/python3

import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()
    comment = False
    for l in lines:
        l = l.strip()
        if l != '' and l[0] == '#':
            comment = True
        if comment and l == '':
            comment = False
            continue
        print (l)

            
