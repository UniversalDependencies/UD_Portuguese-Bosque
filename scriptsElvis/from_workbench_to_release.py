import estrutura_ud
import os
import sys

if len(sys.argv) < 2:
    print("Insira o nome do branch final")
    exit()

if not os.system("sh scriptsElvis/1_criar_branch.sh " + sys.argv[1]):
    corpus23 = estrutura_ud.Corpus(recursivo=False)
    corpus23.load('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/bosqueUD_2.3.conllu')
    corpus = estrutura_ud.Corpus(recursivo=False)
    corpus.load('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/' + sys.argv[1] + ".conllu")
    for sentid, sentence in corpus.sentences.items():
        if 'ccomp:parataxis' in sentence.to_str():
            for token in sentence.tokens:
                if 'ccomp:parataxis' == token.deprel:
                    token.deprel = 'parataxis'
        if 'appos:parataxis' in sentence.to_str():
            for n, token in enumerate(sentence.tokens):
                if 'appos:parataxis' == token.deprel:
                    token.deprel = 'nmod' if corpus23.sentences[sentid].tokens[n].deprel == 'nmod' else 'parataxis'

    corpus.save('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/' + sys.argv[1] + ".conllu")
    os.system("sh scriptsElvis/2_commit.sh " + sys.argv[1] + " release_changes")