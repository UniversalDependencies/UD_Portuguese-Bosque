The UD Portuguese treebank is based on the Bosque part of the Floresta Sintá(c)tica project.
The data were used in the CoNLL-X Shared Task in dependency parsing (2006); the CoNLL version
was taken and converted to the Prague dependency style as a part of HamleDT (since 2011).
Later versions of HamleDT added a conversion to the Stanford dependencies (2014) and to
Universal Dependencies (HamleDT 3.0, 2015). The conversion path from the original Bosque still
goes through the CoNLL-X format and the Prague dependencies, which may occasionally lead to
loss of information. The first release of Universal Dependencies that includes this treebank
is UD v1.2 in November 2015. It is essentially the HamleDT conversion but the data is not
identical to HamleDT 3.0 because the conversion procedure has been further improved.

References:

http://www.linguateca.pt/Floresta/principal.html ... Floresta Sintá(c)tica
http://ufal.mff.cuni.cz/hamledt ... HamleDT
http://ufal.mff.cuni.cz/treex ... Treex is the software used for conversion
http://ufal.mff.cuni.cz/interset ... Interset was used to convert POS tags and features

@inproceedings{pt,
  author    = {Susana Afonso and Eckhard Bick and Renato Haber and Diana Santos},
  year      = {2002},
  title     = {{{``}Floresta sint{\'a}(c)tica{''}:} a treebank for {P}ortuguese},
  booktitle = {Proceedings of the 3rd International Conference on Language Resources and Evaluation (LREC)},
  address   = {Las Palmas, Spain},
  pages     = {1698--1703},
  url       = {http://www.lrec-conf.org/proceedings/lrec2002/sumarios/1.htm}
}



Changelog

2016-05-15 v1.3
  * Split underscore-glued multi-word tokens.
  * "US$" and similar words are now SYM, not NOUN.
  *  Recognized some determiners that were mistakenly tagged PRON.
  * Copulas with clausal complements are now heads.
2015-11-15 v1.2
  * Copied from HamleDT 3.0 and slightly improved.



=== Machine-readable metadata =================================================
Documentation status: stub
Data source: automatic
Data available since: UD v1.2
License: CC BY-NC-SA 3.0
Genre: news
Contributors: Zeman, Daniel; Popel, Martin; Mareček, David
===============================================================================
