import pdb
import sys

from tokenizer.universal_contractions import UniversalContractions
contraction_splitter = UniversalContractions('pt')

sentence = []
for filepath in sys.argv[1:]:
    f = open(filepath)
    for line in f:
        line = line.rstrip('\n')
        if line == '':
            if len(sentence) > 0:
                words = []
                for i, token_info in enumerate(sentence):
                    (index, word, lemma, tag, feats, head, deprel) = token_info
                    words.append(word)

                for i, word in enumerate(words):
                    context_start = i-2
                    context_end = i+4
                    if context_start < 0:
                        context_start = 0
                    if context_end >= len(words):
                        context_end = len(words) - 1
                    left_context = words[context_start:i]
                    right_context = words[(i+2):context_end]
                    if word[-1] == '-' and word != '--':
                        if i < len(words) - 1:
                            _, next_word, _, _, _, _, _ = sentence[i+1]
                            #pdb.set_trace()
                            verb_clitic_pair = contraction_splitter. \
                                split_if_contraction((word + next_word). \
                                                     decode('utf8')). \
                                encode('utf8').split(' ')
                            if len(verb_clitic_pair) == 2:
                                #pdb.set_trace()
                                print '|'.join([word, next_word]) + '\t' + \
                                    '|'.join([verb_clitic_pair[0], \
                                              verb_clitic_pair[1]]) + '\t' + \
                                    ''.join([word, next_word]) + '\t' + \
                                    '|'.join(left_context + [word, next_word] \
                                             + right_context)
            sentence = []
            continue
        elif line.startswith('#'):
            continue

        fields = line.split('\t')

        # Skip special contraction tokens.
        if '-' in fields[0]:
            continue

        index = int(fields[0])
        word = fields[1]
        lemma = fields[2]
        tag = fields[4]
        morph_info = fields[5]
        head = int(fields[6])
        deprel = fields[7]

        token_info = (index, word, lemma, tag, morph_info, head, deprel)
        sentence.append(token_info)

    f.close()
