---
layout: feature
title: 'Tense'
shortdef: 'tense'
---

Tense is typically a feature of [verbs](u-pos/VERB).
It may also occur with other parts of speech
([nouns](u-pos/NOUN), [adjectives](u-pos/ADJ), [adverbs](u-pos/ADV)),
depending on whether borderline word forms such as gerunds and participles
are classified as verbs or as the other category.

Tense is a feature that specifies the time when the action took /
takes / will take place, in relation to the current moment or to
another action in the utterance. In some languages (e.g. English and Portuguese),
some tenses are actually combinations of tense and
[aspect](Aspect). 

Note that we are defining features that apply to a single word. If a
tense is constructed periphrastically (two or more words,
e.g. auxiliary verb indicative + participle of the main verb) and none
of the participating words are specific to this tense, then the
features will probably not directly reveal the tense. For instance, _Eu <b>tinha ido</b> lá_ is past perfect (pluperfect - _Eu fora lá_) tense,
formed periphrastically by the impeferct past tense of the auxiliary _ter_ and the past participle of the main verb _ir_. The auxiliary
will be tagged `VerbForm=Fin|Mood=Ind|Tense=Imp` and the participle
will have `VerbForm=Part|Tense=Past` **or VerbForm=Part|Voice=Pass**; none of the two will have
`Tense=Pqp`. On the other hand, Portuguese can form the pluperfect
morphologically as just one word, such as _estivera_, which will thus be tagged
`VerbForm=Fin|Mood=Ind|Tense=Pqp`.

### `Past`: past tense / preterite / aorist

The past tense denotes actions that happened before the current
moment. In Portuguese, this is the simple past form. In German, this is
the Präteritum. 

#### Examples

*  _Ele <b>foi</b> para casa_

### `Pres`: present tense

The present tense denotes actions that are happening right now or that
usually happen.

#### Examples

* _Ele <b>vai</b> para casa_

### `Fut`: future tense

The future tense denotes actions that will happen after the current
moment.

#### Examples

*  _Ele <b>irá</b> para casa_ "

### `Imp`: imperfect

Used in e.g. Portuguese, Bulgarian and Croatian, imperfect is a special case of
the past tense. Note that, unfortunately, imperfect tense is not
always the same as past tense + imperfective aspect. For instance, in
Bulgarian, there is lexical aspect, inherent in verb meaning, and
grammatical aspect, which does not necessarily always match the
lexical one. In main clauses, imperfective verbs can have imperfect
tense and perfective verbs have perfect tense. However, both rules can
be violated in embedded clauses. **Portuguese is similar to Bulgarian?**

*  _Ele <b>ia</b> para casa_ "

### `Pqp`: pluperfect

The pluperfect denotes action that happened before another action in
past. This value does not apply to English where the pluperfect (past
perfect) is constructed analytically. It applies e.g. to Portuguese.

*  _Ele <b>fora</b> para casa_ "
