# -*- coding: utf-8 -*-

import sys
import regex
from tokenizer.universal_contractions import UniversalContractions
import pdb

language = sys.argv[1]
filepath = sys.argv[2] # One contraction + split words per line.

contraction_splitter = UniversalContractions(language=language)

f = open(filepath)
for line in f:
    line = line.rstrip('\n')
    fields = line.split(' ')
    contraction = unicode(fields[0].decode('utf8'))
    if len(fields) >= 2:
        words = unicode(fields[1].decode('utf8')).split('|')
    else:
        words = [contraction]

    split_contraction = contraction_splitter.split_if_contraction(contraction)

    if split_contraction.split(' ') == words:
        print 'OK: %s %s' % (contraction.encode('utf8'),
                               '|'.join(words).encode('utf8'))
    else:
        print 'FAIL: %s %s' % (contraction.encode('utf8'),
                               '|'.join(words).encode('utf8'))
f.close()
