# $1: nome da branch

if [ $# -eq 0 ]
  then
    git -lsremote
    exit
fi

pasta=${PWD##*/}

git checkout workbench
git pull
git branch -D $1
git push --delete origin $1
rm Inalterados/$1.conllu
rm ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/$1.conllu
rm $1.conllu
