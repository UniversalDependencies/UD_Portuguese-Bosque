# -*- coding: utf-8 -*-

import sys
import regex
import pdb

def read_curated_segments(filepath):
    segment_words = {}
    f = open(filepath)
    for line in f:
        line = line.rstrip('\n')
        fields = line.split('\t')
        if fields[0] in segment_words:
            assert segment_words[fields[0]] == (fields[1], fields[2])
            continue
        segment_words[fields[0]] = (fields[1], fields[2])
        original_words = fields[0].split('|')
        transformed_words = fields[1].split('|')
        assert len(original_words) == len(transformed_words)
    f.close()
    return segment_words

def replace_segments(filepath, segment_words):
    segment_maps = {}
    for segment in segment_words:
        original_words = tuple(segment.split('|'))
        transformed_words = tuple(segment_words[segment][0].split('|'))
        full_segment = segment_words[segment][1]
        #assert full_segment == ''.join(transformed_words), pdb.set_trace()
        if len(original_words) not in segment_maps:
            segment_maps[len(original_words)] = {}
        assert original_words not in segment_maps[len(original_words)]
        segment_maps[len(original_words)][original_words] = \
            (transformed_words, full_segment)
    f = open(filepath)
    all_fields = []
    empty_tokens = {}
    for line in f:
        line = line.rstrip('\n')
        if line == '':
            for i, fields in enumerate(all_fields):
                for l in reversed(sorted(segment_maps.keys())):
                    if i+l > len(all_fields):
                        continue
                    words = [all_fields[i+j][1] for j in xrange(l)]
                    words = tuple(words)
                    if words in segment_maps[l]:
                        # Matched segment.
                        transformed_words, full_segment = segment_maps[l][words]
                        new_fields = ['_'] * len(fields)
                        index = int(fields[0])
                        new_fields[0] = '%s-%s' % (str(index), str(index+l-1))
                        new_fields[1] = full_segment
                        empty_tokens[i] = new_fields
                        for j in xrange(l):
                            all_fields[i+j][1] = transformed_words[j]
            for i, fields in enumerate(all_fields):
                if i in empty_tokens:
                    # Contraction word.
                    print '\t'.join(empty_tokens[i])
                print '\t'.join(fields)
            print
            all_fields = []
            empty_tokens = {}
        elif line.startswith('#'):
            print line
        else:
            fields = line.split('\t')
            all_fields.append(fields)
    f.close()

if __name__ == '__main__':
    filepath = sys.argv[1] # CoNLL-U file.
    # File with segments and their replacements (e.g. clitic words), e.g.,
    # "vende-|se" -> "vende|-se".
    filepath_curated_segments = sys.argv[2]

    # Read segment words.
    segment_words = read_curated_segments(filepath_curated_segments)
    replace_segments(filepath, segment_words)

