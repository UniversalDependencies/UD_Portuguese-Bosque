#!/usr/bin/python3

# remove sentences that are invalid and split them into TRAIN, DEV and
# TEST sets, according the percentages.  This is different from
# split2.py in which we also need to make sure the comments are
# preserved.  Whereas split2.py only needs the actual sentences to be
# sent to treeler.

import random
import sys
import math

def valid_sentence(sent):
    return len(sent) > 1

dev_frac = .1
test_frac = .1
train_frac = .8

if len(sys.argv) != 5:
    print ("Usage:\n\n\tsplit3.py <original> <dev> <test> <train>\n")
    sys.exit(1)

with open(sys.argv[1], 'r') as f:
    txt = f.read()
    sents = txt.split('\n\n')
    valid_sents = []
    for sent in sents:
        if valid_sentence(sent):
            valid_sents.append (sent)

    dev_len = math.floor(len(valid_sents)*dev_frac)
    test_len = math.floor(len(valid_sents)*test_frac)
    train_len = len(valid_sents) - test_len - dev_len
    
    print ("dev = {} ({}), test = {} ({}), train = {} ({})\n".format(dev_frac, dev_len, test_frac, test_len, train_frac, train_len))

    shuffled_ids = list(range(0,len(valid_sents)))
    random.shuffle(shuffled_ids)

    dev_ids = shuffled_ids[0:dev_len]
    test_ids = shuffled_ids[dev_len:dev_len+test_len]
    train_ids = shuffled_ids[dev_len+test_len:]

    with open(sys.argv[2], 'w') as o:
        for id in dev_ids:
            o.write(valid_sents[id])
            o.write('\n\n')

    with open(sys.argv[3], 'w') as o:
        for id in test_ids:
            o.write(valid_sents[id])
            o.write('\n\n')

    with open(sys.argv[4], 'w') as o:
        for id in train_ids:
            o.write(valid_sents[id])
            o.write('\n\n')
