SYNTAXNET=~/repos/syntaxnet

cat $1 | $SYNTAXNET/models/syntaxnet/syntaxnet/models/parsey_universal/tokenize.sh $SYNTAXNET/models/syntaxnet/Portuguese | $SYNTAXNET/models/syntaxnet/syntaxnet/models/parsey_universal/parse.sh $SYNTAXNET/models/syntaxnet/Portuguese
