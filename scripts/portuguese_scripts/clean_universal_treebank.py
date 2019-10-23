# -*- coding: utf-8 -*-

import sys
import regex
import pdb

language = sys.argv[1]
filepath = sys.argv[2]

all_fields = []
f = open(filepath)
for line in f:
    line = line.rstrip('\n')
    if line == '':
        for i, fields in enumerate(all_fields):
            word = unicode(fields[1].decode('utf8'))
            lemma = unicode(fields[2].decode('utf8'))
            # Normalize quotes.
            word = regex.sub(ur'»"', u'»', word) # This happens in pt.
            word = regex.sub(ur'«|»|``|“|”', u'"', word)
            word = regex.sub(ur'`|’|‘|´|‚', u'\'', word)
            fields[1] = word.encode('utf8')
            lemma = regex.sub(ur'«|»|``|“|”', u'"', lemma)
            lemma= regex.sub(ur'`|’|‘|´|‚', u'\'', lemma)
            fields[2] = lemma.encode('utf8')
            # Replicate universal POS tag as fine-grained POS tag.
            fields[4] = fields[3]
            print '\t'.join(fields)
        print
        all_fields = []
    elif line.startswith('#'):
        print line
    else:
        fields = line.split('\t')
        all_fields.append(fields)

f.close()
