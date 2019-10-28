#deve printar o corpus.to_str() ao final de tudo, e somente isso
#set -e
branch=${PWD##*/}
python3 $branch.py > $branch-2.conllu
cd ../../
pasta=${PWD##*/}
sudo chmod a+rwx regras/$branch/$branch.conllu
cp regras/$branch/$branch-2.conllu ~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/$branch.conllu
sh scriptsElvis/2_1-commit.sh $branch
mv regras/$branch/$branch-2.conllu regras/$branch/$branch.conllu

if [ $USER != "elvis_desouza99" ];
then
	rsync -av -e 'ssh -i /home/elvis/.ssh/google_compute_engine' regras/$branch/$branch.conllu elvis_desouza99@35.196.100.118:~/Dropbox/tronco/comcorhd.tronco.me/$pasta/www/interrogar-ud/conllu/
fi
