# $1: nome da branch

set -e

cd ..

if [ $# -eq 0 ]
  then
    git branch -l
    exit
fi

git checkout workbench
git pull
git pull origin $1
git push
