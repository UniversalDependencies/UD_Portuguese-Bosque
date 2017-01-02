# -*- coding: utf-8 -*-

import operator
import sys
import pdb

from tokenizer.universal_contractions import UniversalContractions
contraction_splitter = UniversalContractions('pt')

def list_all_contractions(filepath):
    contractions = {}
    f = open(filepath)
    all_fields = []
    for line in f:
        line = line.rstrip('\n')
        if line == '':
            # Look for MWEs.
            is_inside_mwe = [False for fields in all_fields]
            for i, fields in enumerate(all_fields):
                if fields[-1].startswith('MWE='):
                    mwe = fields[-1].split('|')[0][4:]
                    mwe_words = mwe.split('_')
                    for j, mwe_word in enumerate(mwe_words):
                        assert mwe_word == all_fields[i+j][1], pdb.set_trace()
                        is_inside_mwe[i+j] = True
            for i, fields in enumerate(all_fields):
                word = unicode(fields[1].decode('utf8'))
                split_words = \
                    contraction_splitter.split_if_contraction(word).split(' ')
                if len(split_words) > 1 and is_inside_mwe[i]:
                    contractions[word] = tuple(split_words)
            all_fields = []
        elif line.startswith('#'):
            pass
        else:
            fields = line.split('\t')
            all_fields.append(fields)
    f.close()
    return contractions

def collect_contraction_statistics(filepath, contractions):
    contraction_maps = {}
    for contraction in contractions:
        split_words = contractions[contraction]
        if len(split_words) not in contraction_maps:
            contraction_maps[len(split_words)] = {}
        assert split_words not in contraction_maps[len(split_words)]
        contraction_maps[len(split_words)][split_words] = contraction
    contraction_statistics = {contraction: {} for contraction in contractions}
    f = open(filepath)
    all_fields = []
    for line in f:
        line = line.rstrip('\n')
        if line == '':
            for i, fields in enumerate(all_fields):
                for l in contraction_maps:
                    if i+l > len(all_fields):
                        continue
                    words = [unicode(all_fields[i+j][1].decode('utf8')) \
                             for j in xrange(l)]
                    words = tuple(words)
                    if words in contraction_maps[l]:
                        # Matched contraction.
                        contraction = contraction_maps[l][words]
                        all_tokens_info = []
                        roots = []
                        for j in xrange(l):
                            # Report if a contraction word is heading other word
                            # in the sentence.
                            for k, ff in enumerate(all_fields):
                                if int(ff[6]) == i+j+1:
                                    if k < i or k >= i+l:
                                        print >> sys.stderr, \
                                            'Contraction word %d/%d head of ' \
                                            'another word (%s).' % \
                                            (j+1, l, contraction)
                            head = int(all_fields[i+j][6])
                            if head < i+1 or head > i+l:
                                if head not in roots:
                                    roots.append(head)
                                head = -(1+roots.index(head)) # -1, -2, ...
                            else:
                                head -= (i+1)
                            token_info = all_fields[i+j][1:6] + [str(head)] + \
                                         all_fields[i+j][7:]
                            token_info = tuple(token_info)
                            all_tokens_info.append(token_info)
                        all_tokens_info = tuple(all_tokens_info)
                        if all_tokens_info in \
                           contraction_statistics[contraction]:
                            contraction_statistics[contraction] \
                                [all_tokens_info] += 1
                        else:
                            contraction_statistics[contraction] \
                                [all_tokens_info] = 1

            all_fields = []
        elif line.startswith('#'):
            pass
        else:
            fields = line.split('\t')
            all_fields.append(fields)
    f.close()
    return contraction_statistics

if __name__ == '__main__':
    filepath = sys.argv[1] # CoNLL-U file.
    # Generate a list of contraction statistics.
    contractions = list_all_contractions(filepath)
    contraction_statistics = collect_contraction_statistics(filepath, \
                                                            contractions)
    print_all = True #False # True
    for contraction in contraction_statistics:
        print contraction.encode('utf8')

        sorted_info = sorted(contraction_statistics[contraction].items(), \
                             key=operator.itemgetter(1))[::-1]
        #pdb.set_trace()
        frequencies = [freq for all_tokens_info, freq in sorted_info]
        total = sum(frequencies)
        for all_tokens_info, freq in sorted_info:
            # Number of times it occurs.
            print '%d/%d' % (freq, total)
            for token_info in all_tokens_info:
                print '\t'.join(list(token_info))
            if not print_all:
                break
            print
        if print_all:
            print '--'
        print
