import estrutura_ud
import os
import sys

if len(sys.argv) < 2:
    print("Insira o nome do branch final")
    exit()

def appos_e_ccomp_parataxis(corpus):
    corpus23 = estrutura_ud.Corpus(recursivo=False)
    corpus23.load('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/bosqueUD_2.3.conllu')
    for sentid, sentence in corpus.sentences.items():
        if 'ccomp:parataxis' in sentence.to_str():
            for token in sentence.tokens:
                if 'ccomp:parataxis' == token.deprel:
                    token.deprel = 'parataxis'
        if 'appos:parataxis' in sentence.to_str():
            for n, token in enumerate(sentence.tokens):
                if 'appos:parataxis' == token.deprel:
                    token.deprel = 'nmod' if corpus23.sentences[sentid].tokens[n].deprel == 'nmod' else 'parataxis'
    return corpus

def loc_verbal_aspectual(corpus):
    for sentence in corpus.sentences.values():
        for token in sentence.tokens:
            if token.deprel == "compound" and token.head_token.upos == "AUX":
                token.head_token.upos = "VERB"
                token.head_token.deprel = token.head_token.head_token.deprel
                token.head_token.head_token.deprel = "xcomp"
                antigoRootVERB = token.head_token.head_token.dephead
                token.head_token.head_token.dephead = token.head_token.id
                antigoRootAUX = token.head_token.dephead
                token.head_token.dephead = antigoRootVERB
                token.dephead = antigoRootAUX
                token.upos = "SCONJ"
                token.deprel = "mark"
    for sentence in corpus.sentences.values():
        for token in sentence.tokens:
            if token.upos == "AUX" and token.head_token.upos == "VERB" and token.lemma not in "ser|estar|ir|ter|haver".split("|"):
                token.deprel = token.head_token.deprel
                token.upos = "VERB"
                token.head_token.deprel = "xcomp"
                token.dephead = token.head_token.dephead
                token.head_token.dephead = token.id

    return corpus


if not os.system("sh scripts/1_criar_branch.sh " + sys.argv[1]):
    corpus = estrutura_ud.Corpus(recursivo=True)
    corpus.load('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/' + sys.argv[1] + ".conllu")
    corpus = appos_e_ccomp_parataxis(corpus)
    corpus = loc_verbal_aspectual(corpus)

    corpus.save('/home/elvis/Dropbox/tronco/comcorhd.tronco.me/UD_Portuguese-Bosque/www/interrogar-ud/conllu/' + sys.argv[1] + ".conllu")
    os.system("sh scripts/2_1-commit.sh " + sys.argv[1] + " release_changes")
