
$0 ~ /^<\/?s/ { next }

$1 == $7 && NF > 1 {
    print "ERROR in",FILENAME,NR,$0
}

