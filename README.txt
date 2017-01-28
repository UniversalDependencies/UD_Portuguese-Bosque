The CG-converted UD Portuguese treebank is originally based on an
improved and enriched version of the 7.4 dependency version of the
revised Bosque part of the Floresta Sintá(c)tica treebank
(cf. Linguateca.pt). 7.4 was in 2006-2008 aligned with a new live run
with the PALAVRAS parser in order to propagate morphological features
from unambiguous to ambiguous words, and to add what the Floresta team
called "searchables", i.e. tags for features distributed across
several tokens, such as NP definiteness and complex tenses. The public
treebank only used this for the constituent version, which was the one
actively revised by the Floresta team until 2008 (Linguateca.pt
version 8.0).

Since 2008 Eckhard Bick has maintained an experimental version of the
dependency Bosque for semantic and other research, and made further
revisions to it, which were not aligned with either the constituent
version or the published 7.4 dependency version. In the beginning of
2016, Eckhard Bick wrote UD conversion rules for Constraint Grammar
input, and applied these to the updated version of the dependency
Bosque (Linguateca.pt version 7.5 of March 2016).

In a team effort in October 2016, Alexandre Rademaker, Cláudia
Freitas, Fabricio Chalub, Valeria de Paiva and Livy Maria Real Coelho,
aiming at full compatibility with ConLL UD specifications,
consistency-checked and discussed the 7.5 UD Bosque, leading to a
further round of manual treebank corrections and conversion rule
changes by Eckhard Bick. The conversion grammar ultimately used
contained some 530 rules. Of these 70 were simple feature mapping
rules, and 130 were local MWE splitting rules, assigning internal
structure, POS and features to MWE's from Bosque. The remainder of the
rules handle UD-specific dependency and function label changes in a
context-dependent fashion, the main issues being raising of copula
dependents to subject complements, inversion of prepositional
dependency and a change from syntactic to semantic verb chain
dependency.

The new UD treebank retains the additional tags for NP definiteness
and complex tenses, as well as the original syntactic functions tags
and secondary morphological tags. This way, the treebank retains its
original linguistic focus in addition to the machine learning uses
targeted by the ConLL UD format. For instance, conjuncts and roots
still feature a direct function tag (e.g. a verb complement role for a
conjunct or "question" for a root. In cases, where UD does not
distinguish between form and function, e.g. n/np adverbial modifiers,
where UD "duplicates" noun POS as 'nmod' function, the Bosque function
tag for free adverbial, adject or adverbial object is retained in
field 4 (@tags). Finally, some lost valency relations may be recovered
from an underspecified UD tag, e.g. the core clause arguments
"prepositional object" ('gostar de ARG') and valency-bound adverbial
('morar em ARG').

The UD_Portuguese releases 1.2 to 1.4 were based on another conversion
of Bosque, used in the CoNLL-X Shared Task in dependency parsing (2006);
the CoNLL version was taken and converted to the Prague dependency style
as a part of HamleDT (since 2011). Later versions of HamleDT added a
conversion to the Stanford dependencies (2014) and to Universal Dependencies
(HamleDT 3.0, 2015). UD release 1.4 contained two conversions of Bosque:
one labeled UD_Portuguese (via CoNLL 2006 and HamleDT) and another labeled
UD_Portuguese-Bosque (the new conversion described above). The two
versions will be merged (and labeled UD_Portuguese) in UD release 2.0.
The merged version will be based mostly on the new conversion by Bick et al.;
the conversion by Zeman et al. will be used to cross-validate and add
missing features.

CONTRIBUTORS

The conversion was implemented by Eckhard Bick and revised by:

- Claudia Freitas
- Eckhard Bick
- Fabricio Chalub
- Alexandre Rademaker
- Livy Real
- Valeria Paiva

The HamleDT conversion was implemented by Dan Zeman and revised by:

- Martin Popel
- David Mareček
- Daniel Zeman
- Natalia Silveira
- André Martins

CHANGELOG

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

LICENSE

See file LICENSE.txt

REFERENCES

- https://github.com/own-pt/bosque-UD (development of this corpus)

- http://www.linguateca.pt/Floresta/ (Floresta Treebank repository)

- http://visl.sdu.dk/tagset_cg_general.pdf (non-UD tags in field 4)

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
  pp.216-219. Springer

- Bick, Eckhard (2014). PALAVRAS, a Constraint Grammar-based Parsing
  System for Portuguese. In: Tony Berber Sardinha & Thelma de Lurdes
  São Bento Ferreira (eds.), /Working with Portuguese Corpora/, pp
  279-302. London/New York:Bloomsburry Academic. ISBN
  978-1-4411-9050-5

=== Machine-readable metadata =================================================
Documentation status: partial
Data source: semi-automatic
Data available since: UD v1.2
License: CC BY-SA 4.0
Genre: news blog
Contributors: Freitas, Cláudia; Bick, Eckhard; Chalub, Fabricio; Rademaker, Alexandre; Real, Livy; de Paiva, Valeria; Zeman, Daniel; Popel, Martin; Mareček, David; Silveira, Natalia; Martins, André
Contact: arademaker@gmail.com
===============================================================================
