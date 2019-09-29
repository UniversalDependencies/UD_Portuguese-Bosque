set -e
branch=${PWD##*/}
cd ..
cd ..
sh 3_apagar_branch.sh $branch
rm -r regras/$branch
