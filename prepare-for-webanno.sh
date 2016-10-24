# For some reason WebAnno and BRAT are using the XPOSTAG field to
# extract the POS. Since our XPOSTAG is not really useful for
# anything, we simply replicate the value of the UPOSTAG.

cat - | awk -v OFS='\t' '{print $1, $2, $3, $4, $4, $6, $7, $8, $9, $10}'
