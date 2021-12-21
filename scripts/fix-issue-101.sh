for file in documents/*.conllu
do 
    awk '{gsub("SUBJ_INDEF", "_", $10); print $0;}' $file > ./tempFile && mv ./tempFile $file;
done

for file in documents/*.conllu
do
    awk '{gsub("mesoclitic", "_", $10); print $0;}' $file > ./tempFile && mv ./tempFile $file;
done
