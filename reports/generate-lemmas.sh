#pos=$(mktemp)

for f in $(cat ../../documents/*.conllu | awk -F '\t' '{print $4}' | sort | uniq); do
    cat ../../documents/*.conllu | awk -F '\t' "\$4~/^$f$/ {print \$3}" | sort | uniq -c | sort -rn > lemmas-$f.txt
done
