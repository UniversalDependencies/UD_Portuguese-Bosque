set -e
branch=${PWD##*/}
cd ..
cd ..
sh scriptsElvis/3_apagar_branch.sh $branch
rm -r regras/$branch
