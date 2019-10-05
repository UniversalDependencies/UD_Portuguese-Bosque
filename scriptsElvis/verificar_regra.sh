#deve printar o corpus.to_str() ao final de tudo, e somente isso

set -e

branch=${PWD##*/}
python3 $branch.py > $branch-teste.conllu
diff -y --suppress-common-lines $branch.conllu $branch-teste.conllu | wc -l
meld --diff $branch.conllu $branch-teste.conllu
