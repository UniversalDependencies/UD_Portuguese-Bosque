# -*- coding: utf-8 -*-

import os
import sys
import estrutura_ud

if len(sys.argv) < 2:
	pasta = input("Diretório onde estão os arquivos de id e a pasta documents: ")
else:
	pasta = sys.argv[1]

for arquivo_id in os.listdir(pasta):
	if 'pt-' in arquivo_id:
		arquivo_ids = pasta + '/' + arquivo_id
		diretorio = arquivo_ids.rsplit('/', 1)[0] + '/'
		arquivo_conllu = arquivo_ids.split('.txt')[0] + '.conllu'

		ids = open(arquivo_ids, 'r').read()

		arquivos_conllu = str()
		for conllu in os.listdir(diretorio + 'documents'):
			if os.path.isfile(diretorio + 'documents/' + conllu):
				arquivos_conllu += open(diretorio + 'documents/' + conllu, 'r').read() + "\n\n"

		corpus = estrutura_ud.Corpus()
		corpus.build(arquivos_conllu)
		for sentid, sentence in corpus.sentences.items():
			for token in sentence.tokens:
				if token.deprel == "ccomp:parataxis": token.deprel = "parataxis"
				if token.deprel == "appos:parataxis": token.deprel = "nmod"
				if (token.upos == "ADJ" or token.upos == "NOUN") and "VerbForm=Ger" in token.feats: token.feats = token.feats.replace("VerbForm=Ger", "").replace("||", "|")
				if (token.upos == "ADJ" or token.upos == "NOUN") and "VerbForm=Inf" in token.feats: token.feats = token.feats.replace("VerbForm=Inf", "").replace("||", "|")
				if (token.upos == "ADJ" or token.upos == "NOUN") and "VerbForm=Part" in token.feats: token.feats = token.feats.replace("VerbForm=Part", "").replace("||", "|")
				if token.feats == "": token.feats = "_"
				if token.feats[0] == "|": token.feats = token.feats[1:]
				if token.feats[-1] == "|": token.feats = token.feats[:-1]
				if token.feats == "": token.feats = "_"

		novo_conllu = list()
		for i, identificador in enumerate(ids.splitlines()):
			if identificador.strip() != '':
				novo_conllu.append(corpus[identificador].to_str())
				print(arquivo_conllu.rsplit('/', 1)[1] + ' - ' + str(i+1) + '/' + str(len(ids.splitlines())) + ': ' + identificador)

		open(arquivo_conllu, 'w').write("\n\n".join(novo_conllu) + '\n\n')