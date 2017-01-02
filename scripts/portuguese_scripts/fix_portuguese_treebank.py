# -*- coding: utf-8 -*-

import sys
import regex
import pdb

# This function is necessary to create empty contraction tokens to the
# already split contractions (non-MWE words).
def build_contraction_maps():
    from tokenizer.portuguese_contractions import PortugueseContractions
    contraction_splitter = PortugueseContractions()
    contractions = contraction_splitter.contractions
    contraction_maps = {}
    for contraction in contractions:
        split_words = tuple(contractions[contraction][0])
        if len(split_words) not in contraction_maps:
            contraction_maps[len(split_words)] = {}
        assert split_words not in contraction_maps[len(split_words)]
        contraction_maps[len(split_words)][split_words] = contraction
    return contraction_maps

def read_curated_contractions(filepath):
    contraction_words = {}
    f = open(filepath)
    entry_info = []
    for line in f:
        line = line.rstrip('\n')
        if line == '':
            contraction = entry_info[0]
            all_fields = []
            for line in entry_info[2:]:
                fields = line.split('\t')
                all_fields.append(fields)
            contraction_words[contraction] = all_fields
            entry_info = []
        else:
            entry_info.append(line)
    f.close()
    return contraction_words

def get_new_index(map_indices, old_index, offset):
    assert old_index in map_indices, pdb.set_trace()
    start, end = map_indices[old_index]
    assert start >= 0 and end >= 0, pdb.set_trace()
    if offset < 0:
        return end + 1 + offset
    else:
        return start + offset


filepath = sys.argv[1]
filepath_curated_contractions = sys.argv[2]

contraction_words = read_curated_contractions(filepath_curated_contractions)
contraction_maps = build_contraction_maps()

all_fields = []
map_indices = {}
f = open(filepath)
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

        sentence = []
        head_offsets = []
        empty_tokens = {} # For the original contractions.
        for i, fields in enumerate(all_fields):
            index = int(fields[0])
            word = fields[1].decode('utf8')
            lemma = fields[2].decode('utf8')
            head = int(fields[6])
            new_index_start = 1 + len(sentence)
            new_index_end = new_index_start

            #split_words = \
            #    contraction_splitter.split_if_contraction(word).split(' ')
            if word in contraction_words:
                split_words = [contraction_fields[0] \
                               for contraction_fields in contraction_words[word]]
                if not is_inside_mwe[i]:
                    print >> sys.stderr, 'Tried to split word: %s' % word
                    split_words = [word]
                    #pdb.set_trace()
                else:
                    pass
                    #print >> sys.stderr, '_'.join(split_words)
            else:
                split_words = [word]
            if len(split_words) > 1:
                # Write contraction token with empty information.
                new_fields = ['_'] * len(fields)
                new_fields[0] = '%s-%s' % (str(new_index_end),
                                           str(new_index_end + \
                                               len(split_words) - 1))
                new_fields[1] = word
                assert new_index_end not in empty_tokens
                empty_tokens[new_index_end] = new_fields
                for j, split_word in enumerate(split_words):
                    new_fields = ['_'] * len(fields)
                    new_fields[0] = str(new_index_end)
                    new_fields[1:] = contraction_words[word][j]
                    new_head = int(contraction_words[word][j][5])
                    if new_head < 0:
                        # Take the original head of the contraction.
                        new_head = head
                        head_offset = 0
                    else:
                        new_head = i+1 # The first of the contraction, ...
                        head_offset = new_head # ... and its offset.
                    new_fields[6] = str(new_head)
                    # Uncomment the next clause to mark this as a split
                    # contraction.
                    # new_fields[-1] = 'CONTRACTION=%s' % fields[1]
                    sentence.append(new_fields)
                    head_offsets.append(head_offset)
                    new_index_end += 1
                new_index_end -= 1
            else:
                # Check if the word (not coming from a MWE, in principle) starts
                # a list of already split words that jointly form a contraction.
                # If so, then writes an empty contraction token.
                contraction = u''
                for l in reversed(sorted(contraction_maps)):
                    if i+l > len(all_fields):
                        continue
                    words = [unicode(all_fields[i+j][1].decode('utf8')) \
                             for j in xrange(l)]
                    words = tuple(words)
                    if words in contraction_maps[l]:
                        # Matched contraction.
                        contraction = contraction_maps[l][words]
                        # Write contraction token with empty information.
                        new_fields = ['_'] * len(fields)
                        new_fields[0] = '%s-%s' % (str(new_index_end),
                                                   str(new_index_end+l-1))
                        new_fields[1] = contraction.encode('utf8')
                        assert new_index_end not in empty_tokens
                        empty_tokens[new_index_end] = new_fields
                    if contraction != u'':
                        break

                # Now append the word itself.
                fields[0] = str(new_index_end)
                sentence.append(fields)
                head_offsets.append(0)
            map_indices[index] = (new_index_start, new_index_end)

        map_indices[0] = (0, 0)
        for i, fields in enumerate(sentence):
            #index = fields[0]
            if i+1 in empty_tokens:
                # Contraction word.
                print '\t'.join(empty_tokens[i+1])

            # Words whose head was part of a MWE now get as head the head of
            # that MWE.
            # Words that were split in the MWE get need to use the offset
            # and the segment start to determine their new head.
            head = int(fields[6])

            start, end = map_indices[head]
            if end > start:
                if i < start or i > end:
                    # The head of this word was part of a MWE.
                    # Now the word get as head the head of that MWE.
                    # We observed that in the Portuguese dataset the vast
                    # majority of times a contraction word heads another word,
                    # it is the last contraction word (e.g. "o" in "do = de+o").
                    # Therefore we always assume the head of the MWE is its
                    # last word.
                    new_head = get_new_index(map_indices, head, -1)
                    print >> sys.stderr, 'Found contraction heading another ' \
                        'word; using the last contraction word (%s).' % \
                        (all_fields[head-1][1])
                else:
                    # This must be a word that was split.
                    # Words that were split in the MWE need to use the offset
                    # and the segment start to determine their new head.
                    offset = head_offsets[i]
                    new_head = get_new_index(map_indices, head, offset)
            else:
                offset = 0
                new_head = get_new_index(map_indices, head, offset)

            fields[6] = str(new_head)
            print '\t'.join(fields)
        print
        all_fields = []
        map_indices = {}
    elif line.startswith('#'):
        print line
    else:
        fields = line.split('\t')
        all_fields.append(fields)

f.close()
