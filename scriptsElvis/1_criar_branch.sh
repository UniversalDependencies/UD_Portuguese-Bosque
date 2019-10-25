# $1: nome da branch

set -e

if [ $# -eq 0 ]
  then
    git ls-remote
    exit
fi

[ ! -d "Inalterados" ] && mkdir -p "Inalterados"

pasta=${PWD##*/}

git checkout workbench
git pull
git checkout -b $1 workbench
git push --set-upstream origin $1
cat documents/*.conllu > Inalterados/$1.conllu
sudo chmod a+rwx Inalterados/$1.conllu
if [ $USER != "elvis_desouza99" ]; then
	rsync -av -e 'ssh -i /home/elvis/.ssh/google_compute_engine' Inalterados/$1.conllu elvis_desouza99@35.196.100.118:~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/
	cp Inalterados/$1.conllu ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/
else
	cp Inalterados/$1.conllu ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/
	sudo chmod a+rwx ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/$1.conllu
fi
git checkout workbench
