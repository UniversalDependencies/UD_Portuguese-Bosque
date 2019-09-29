#deve printar o corpus.to_str() ao final de tudo, e somente isso

set -e

branch=${PWD##*/}
python3 $branch.py > $branch-teste.conllu
diff -U 0 $branch.conllu $branch-teste.conllu | grep -v ^@ | wc -l
meld --diff $branch.conllu $branch-teste.conllu
