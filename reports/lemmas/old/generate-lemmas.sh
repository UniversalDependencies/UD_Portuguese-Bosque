#pos=$(mktemp)

for f in $(cat ~/UD_Portuguese/*.conllu | awk -F '\t' '{print $4}' | sort | uniq); do
    cat ~/UD_Portuguese/*.conllu | awk -F '\t' "\$4~/^$f$/ {print \$3}" | sort | uniq -c | sort -rn > lemmas-$f.txt
done
