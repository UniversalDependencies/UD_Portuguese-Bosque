How to reproduce:

1. Fix all the issues in Bosque UD: in bosque-ud/ execute fix-errors.sh.
   The errors found are listed in bosque-ud/readme.txt

2. Make sure you have perl modules for Freeling 4.0 ready to use.  See
   the Freeling documentation for more details on how to compile it and
   its API modules. 

3. Update the path to your Freeling installation in
   conll_convert_tags_from_uposf.pl

4. Execute convert-7.5.sh

5. The result from that command is four files:

   devel.conll train.conll: to be used in training treeler (see next
   steps)

   freeling.tokens palavras.tokens: to be used in comparing the
   differences between PALAVRAS and Freeling tokenization and PoS
   tagging.

6. If you are unable to execute these steps, the directory
   pregenerated/ contains a version of those files.

7. To train treeler, see train/readme.txt

This script was developed under Ubuntu 14.04, the following are the
programs and their versions used:

awk: GNU Awk 4.0.1
python: Python 2.7.6
perl: perl v5.18.2
bash: GNU bash, version 4.3.11(1)-release
grep: grep (GNU grep) 2.16

Reporting issues we follow:

[1] bosque_CP.udep.conll.gz Bick's version of European Portuguese part of Bosque 7.5 UD annotated, available at http://www.linguateca.pt/Floresta/ficheiros/bosque_CP.udep.conll.gz
[2]bosque_CF.udep.conll.gz ick's version of Brazilian Portuguese part of Bosque 7.5 UD annotated, available at http://www.linguateca.pt/Floresta/ficheiros/bosque_CF.udep.conll.gz
[3] Dan Zeman's version of Bosque CoNLL (7.3), available at https://github.com/UniversalDependencies/UD_Portuguese
[4] Linguateca Version of Bosque CoNLL (7.3), http://www.linguateca.pt/floresta/CoNLL-X/
