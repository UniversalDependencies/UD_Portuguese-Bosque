#!/usr/bin/python3

# remove sentences that are invalid and split them into DEVEL and
# TRAIN sets, according the percentages train_frac, devel_frac.

import random
import sys
import math

def valid_sentence(sent):
    lines = sent.split('\n')
    for l in lines:
        if len(l) == 1:
            continue
        
        cols = l.split('\t');
        if len(cols) == 10:
            if cols[4] == '':
                return False
        else:
            return False
    return True

train_frac = .9
devel_frac = .1

if len(sys.argv) != 4:
    print ("Usage:\n\n\tsplit2.py <original> <devel> <train>\n")
    sys.exit(1)

with open(sys.argv[1], 'r') as f:
    txt = f.read()
    sents = txt.split('\n\n')
    valid_sents = []
    # counter = 0
    for sent in sents:
        if valid_sentence(sent):
            valid_sents.append (sent)
    #         counter += 1
    # print(counter, file=sys.stderr)

    devel_len = math.floor(len(valid_sents)*devel_frac)
    shuffled_ids = list(range(0,len(valid_sents)))
    random.shuffle(shuffled_ids)

    devel_ids = shuffled_ids[0:devel_len]
    train_ids = shuffled_ids[devel_len:]

    with open(sys.argv[2], 'w') as devfile:
        for id in devel_ids:
            devfile.write(valid_sents[id])
            devfile.write('\n\n')
            
    with open(sys.argv[3], 'w') as trainfile:
        for id in train_ids:
            trainfile.write(valid_sents[id])
            trainfile.write('\n\n')
