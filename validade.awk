
$0 ~ /^<\/?s/ { next }

$1 == $7 && NF > 1 {
    print "ERROR self reference",FILENAME,NR
}

NF > 0 && NF < 10 {
    print "ERROR number of fields",FILENAME,NR
}
       
    
     
