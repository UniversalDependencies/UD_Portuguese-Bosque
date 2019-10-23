import sys
import pdb

# CONLL file with tokens like "4-5 del ..." to represent contractions.
filepath = sys.argv[1]
f = open(filepath)

contractions = {}
spans = []
all_fields = []
for line in f:
    line = line.rstrip('\n')
    if line == '':
        for contraction, span in spans:
            if span[0] <= 0 or span[1] > len(all_fields):
                print >> sys.stderr, \
                    'Ignoring contraction span out of bounds: %s' % contraction
                continue
            words = [all_fields[i][1] for i in xrange(span[0]-1, span[1])]
            lemmas = [all_fields[i][2] for i in xrange(span[0]-1, span[1])]
            if contraction not in contractions:
                contractions[contraction] = (words, lemmas)
            else:
                if contractions[contraction] != (words, lemmas):
                    print >> sys.stderr, \
                        "Inconsistent contraction: %s (%s %s vs %s %s)" % \
                        (contraction, '|'.join(contractions[contraction][0]), \
                         '|'.join(contractions[contraction][1]), \
                         '|'.join(words), \
                         '|'.join(lemmas))
        spans = []
        all_fields = []
    elif line.startswith('#'):
        continue
    else:
        fields = line.split('\t')
        if '-' in fields[0]:
            span = map(int, fields[0].split('-'))
            spans.append((fields[1], span))
            assert fields[2:] == ['_'] * (len(fields) - 2), pdb.set_trace()
        else:
            all_fields.append(fields)

f.close()

for contraction in contractions:
    print contraction, '|'.join(contractions[contraction][0]), \
        '|'.join(contractions[contraction][1])
