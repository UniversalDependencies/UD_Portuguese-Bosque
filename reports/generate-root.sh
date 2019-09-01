
cat ../../documents/*.conllu | awk -F '\t' '$8 ~ /^root$/ {print $4}' | sort | uniq -c | sort -rn > root-POS.txt
