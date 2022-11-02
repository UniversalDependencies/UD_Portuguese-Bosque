
# Summary

This Universal Dependencies (UD) Portuguese treebank is based on the
Constraint Grammar converted version of the Bosque, which is part of
the Floresta Sintá(c)tica treebank. It contains both European
(CETEMPúblico) and Brazilian (CETENFolha) variants.

# Introduction

This  Universal Dependencies (UD) Portuguese treebank is  based on
the Constraint Grammar converted version of the Bosque, which is part
of the Floresta Sintá(c)tica treebank.

Eckhard Bick has maintained since 2008 an experimental version of the
dependency Bosque for research, which was not aligned with either the
Linguateca published constituent version or the 7.4 dependency version
of the Bosque.  In 2016, Eckhard Bick wrote UD conversion rules for
Constraint Grammar input, and applied these to the updated version of
the dependency Bosque (Linguateca site version 7.5 of March 2016).

In October 2016, Alexandre Rademaker, Cláudia Freitas, Fabricio
Chalub, Valeria de Paiva and Livy Maria Real Coelho, aiming at full
compatibility with ConLL UD specifications, consistency-checked and
discussed the 7.5 UD Bosque, leading to a further round of manual
treebank corrections and conversion rule changes by Bick. The
conversion grammar used contains some 530 rules. Of these 70 were
simple feature mapping rules, and 130 were local MWE splitting rules,
assigning internal structure, POS and features to MWE's from the
Bosque. The remainder of the rules handle UD-specific dependency and
function label changes in a context-dependent fashion.  The main
issues were raising of copula dependents to subject complements,
inversion of prepositional dependency and a change from syntactic to
semantic verb chain dependency.

The new UD treebank retains the additional tags for NP definiteness
and complex tenses, as well as the original syntactic function tags
and secondary morphological tags of the original Bosque. Thus the
treebank retains its original linguistic focus, in addition to coping
with the machine learning uses targeted by the ConLL UD format.

# Acknowledgments

The UD_Portuguese releases 1.2 to 1.4 were based on a different
conversion of Bosque, used in the CoNLL-X Shared Task in dependency
parsing (2006); the CoNLL version was taken and converted to the
Prague dependency style as a part of HamleDT (since 2011). Later
versions of HamleDT added a conversion to the Stanford dependencies
(2014) and to Universal Dependencies (HamleDT 3.0, 2015).

UD release 1.4 contained two conversions of Bosque: one labeled
UD_Portuguese (via CoNLL 2006 and HamleDT) and another labeled
UD_Portuguese-Bosque (the new conversion described above).

The two versions were merged (and labeled UD_Portuguese) in UD release
2.0.  The merged version is based mostly on the new conversion by Bick
et al.. The conversion by Zeman et al. was used to
cross-validate. After the alignment of the sentences from the two
versions, the data was split in dev, test and train following the
distribution of sentences from Zeman et al.

The conversion was implemented by Eckhard Bick and revised mainly by:

- Alexandre Rademaker
- Claudia Freitas
- Fabricio Chalub

(see other contributors below)

The HamleDT conversion was implemented by Dan Zeman and revised by:

- Martin Popel
- David Mareček
- Daniel Zeman
- Natalia Silveira
- André Martins

# License

See file LICENSE.txt

# How to cite

    @InProceedings{depling-2017,
       author =  {Alexandre Rademaker and Fabricio Chalub and Livy
                  Real and Cláudia Freitas and Eckhard Bick and
                  Valeria de Paiva},
       title = 	 {Universal Dependencies for Portuguese},
       booktitle = {Proceedings of the Fourth International Conference on Dependency
	                Linguistics (Depling)},
       year =	 {2017},
       pages =	 {197-206},
       month =	 {September},
       address =	 {Pisa, Italy},
	   url = {http://aclweb.org/anthology/W17-6523}
    }

# References

- https://github.com/own-pt/bosque-UD (development area of this
  corpus)

- http://visl.sdu.dk/constraint_grammar.html (cg3 compiler used for
  the conversion grammar)

- http://visl.sdu.dk/visl/pt/parsing/automatic/ (PALAVRAS parser used
  to create input trees for the manually revised Bosque treebank)

- http://ufal.mff.cuni.cz/hamledt (HamleDT)

- Afonso, Susana, Eckhard Bick, Renato Haber & Diana Santos (2002),
  Floresta sintá(c)tica: a treebank for Portuguese
  <http://visl.sdu.dk/%7Eeckhard/pdf/AfonsoetalLREC2002.ps.pdf>, In
  /Proceedings of LREC'2002, Las Palmas/. pp. 1698-1703, Paris: ELRA

- Freitas, Cláudia & Rocha, Paulo & Bick, Eckhard (2008), "Floresta
  Sintá(c)tica: Bigger, Thicker and Easier", in: António Teixeira et
  al. (eds.) /Computational Processing of the Portuguese Language/
  (Proceedings of PROPOR 2008, Aveiro, Sept. 8th-10th, 2008),
  pp.216-219. Springer (http://www.linguateca.pt/Floresta/)

- Bick, Eckhard (2014). PALAVRAS, a Constraint Grammar-based Parsing
  System for Portuguese. In: Tony Berber Sardinha & Thelma de Lurdes
  São Bento Ferreira (eds.), /Working with Portuguese Corpora/, pp
  279-302. London/New York:Bloomsburry Academic. ISBN
  978-1-4411-9050-5

# Changelog

2022-11-01 v2.11

  * fixed some verb analysis based on valence analyses
  * fixed some lemmas

2022-05-01 v2.10

  * fixed validation errors according to new tests (goeswith)

2021-11-01 v2.9

  * redistributed of the sentences in dev/test/train (#203, #273)
  * fixed all remain validation errors related to non projective
    caused by punctuation (#265)
  * adopted the ExtPos in the FEATURES instead of the MWEPOS and MWE
    in the MISC field (#334)
  * added the NEWDOC_ID metadata in all documents
  * fixed gender/number agreement between DET/NOUN and ADJ/NOUN (#314,
    #299, #300)
  * annotation of abbreviations in the FEATURES (#301, #312)
  * adding missing FEATURES (#343, #79, #365)
  * removed some duplicated sentences (#126)

2021-05-01 v2.8

  * Fixed many validation errors related to non projective caused by
    punctuation (#265). Issue not closed.

  * Removed Gender=Unsp given discussion in
    https://github.com/UniversalDependencies/docs/issues/780

  * Other issues: #295, #296, #294, #278, #293.

2018-04-15 v2.2
  * Repository renamed from UD_Portuguese to UD_Portuguese-Bosque. The
    'fixed' relations were revised.

2017-11-07 v2.1
  * Minor changes in some sentences.

2017-03-01 v2.0
  * The two conversions merged into UD_Portuguese.
  * Annotation adjusted to UD v2 guidelines.

2016-11-15 v1.4
  * Initial UD release of the new conversion (UD_Portuguese-Bosque).
  * The older conversion (UD_Portuguese) is also part of the release.
  * Annotation of multi-word tokens added by André Martins.

2016-05-15 v1.3
  * Split underscore-glued multi-word tokens.
  * "US$" and similar words are now SYM, not NOUN.
  *  Recognized some determiners that were mistakenly tagged PRON.
  * Copulas with clausal complements are now heads.

2015-11-15 v1.2
  * Initial UD release: Copied from HamleDT 3.0 and slightly improved.


<pre>
=== Machine-readable metadata =================================================
Data available since: UD v1.2
License: CC BY-SA 4.0
Genre: news
Includes text: yes
Lemmas: converted with corrections
UPOS: converted with corrections
XPOS: manual native
Features: converted with corrections
Relations: converted with corrections
Contributors: Rademaker, Alexandre; Freitas, Cláudia; de Souza, Elvis; Silveira, Aline; Cavalcanti, Tatiana; Evelyn, Wograine; Rocha, Luisa; Soares-Bastos, Isabela; Bick, Eckhard; Chalub, Fabricio; Paulino-Passos, Guilherme; Real, Livy; de Paiva, Valeria; Zeman, Daniel; Popel, Martin; Mareček, David; Silveira, Natalia; Martins, André
Contributing: elsewhere
Contact: arademaker@gmail.com
===============================================================================
</pre>
