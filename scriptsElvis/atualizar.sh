set -e

git checkout workbench
git fetch oficial
git pull oficial workbench
git push
git checkout workbench
