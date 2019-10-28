# $1: nome da branch

set -e

if [ $# -eq 0 ]
  then
    git ls-remote
    exit
fi

[ ! -d "Repositorio-Branches" ] && mkdir -p "Repositorio-Branches"

pasta=${PWD##*/}

git checkout $1
git pull
cp ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/$1.conllu .
python3 ~/Dropbox/PIBIC/ACDC-UD/split_conllu.py $1.conllu
mv $1.conllu Repositorio-Branches/
git diff
git add -u

if [ $# -eq 1 ]
  then
    git commit
  else
    git commit -m "$2"
fi

git push
git checkout workbench
git pull
