
#$1 = branch
#set -e

if [ $# -eq 0 ]
  then
    git ls-remote
    exit
fi

sh scriptsElvis/1_criar_branch.sh $1
mkdir regras
mkdir regras/$1
ln -nfrs ~/Dropbox/PIBIC/ACDC-UD/estrutura_ud.py regras/$1
cat documents/*.conllu > regras/$1/$1.conllu
cp scriptsElvis/executar_regra.sh regras/$1
cp scriptsElvis/verificar_regra.sh regras/$1
cp scriptsElvis/excluir_regra.sh regras/$1
cd regras/$1
echo "import estrutura_ud
corpus = estrutura_ud.Corpus(recursivo=True)
corpus.load('$1.conllu')

for sentid, sentence in corpus.sentences.items():
	for t, token in enumerate(sentence.tokens):
		#code

print(corpus.to_str())" > $1.py
subl $1.py
echo "Vá para regras/$1"
