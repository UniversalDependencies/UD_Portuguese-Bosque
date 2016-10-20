---
layout: postag
title: 'PART'
shortdef: 'particle'
---

In Portuguese, `PART` is used to tag prefixes that form complex words, but not compounds.  In `ex-presidente, anti-capitalista, vice-diretor, pós-graduação`, the morphemes `ex-, anti-, vice-, pós-` should be tagged as `PART`. Note that when one uses one of those prefixes alone (in a sentence as `Minha pós não acaba nunca. (My post never ends.)`) "pós" still stands for "pós-graduação". Differently from compound words, as `norte-americano, meio-campo, porta-voz`, in which there is no particle and one cannot use only the prefix to recall the entire sense of the compound. Weekdays names, as `segunda-feira`, are analysed as compound words. Words as `fim-de-semana, a partir de, de novo` are `MWE` and their elements should not be tagged as `PART`.

It means that these prefixed words should be split in the tokenization step. Note that hyphenation is still a big issue here, since many of those complex words formed by particles not necessarily would be split by a hyphen. Hyphen is discussed in the new Regulation of Portuguese Orthography (2009) and some specific cases are ruled: vice- and ex- always come with hyphen. But not all cases are specified and many dictionaries (and old corpora as well) bring forms as `anti-capitalista` and `anticapitalista`.

`Part` is also used for negative particles, as `não, nem` in predicative contexts. Note that negative adverbs, as `nunca, jamais` are still tagged as `ADV`. 

Examples:

Negative particles: não, nem 

Prefixes: anti-, ex-, pós-, vice-, primeiro-, pró-, infra-
