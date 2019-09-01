# -*- coding: utf-8 -*-
# Natural Language Toolkit: Tokenizers
#
# Copyright (C) 2001-2012 NLTK Project
# Author: André Martins <andre.martins@unbabel.com>
#
# URL: <http://nltk.sourceforge.net>
# For license information, see LICENSE.TXT

r"""

Universal Word Tokenizer

The universal word tokenizer uses regular expressions to tokenize text as in
the Universal Dependencies Treebanks (http://universaldependencies.org/).
Currently supported languages: en, es, fr, it, pt.
"""

import regex
from nltk.tokenize.api import TokenizerI
from universal_contractions import UniversalContractions

class UniversalWordTokenizer(TokenizerI):
    """
    This is a version of the Treebank tokenizer
    (nltk.tokenize.TreebankWordTokenizer) for the Universal Dependencies
    Treebank. It assumes the sentence is unicode formatted. The goal is to
    match the tokenization decisions of those treebanks.

    UniversalWordTokenizer assumes that the
    text has already been segmented into sentences, e.g. using
    ``sent_tokenize()``.

    This tokenizer performs the following steps:

    - split standard contractions (e.g. clitic pronouns and contractions of
    prepositions and determiners, e.g. ``dos`` -> ``de_ os``.
    - treat most punctuation characters as separate tokens
    - split off commas and single quotes, when followed by whitespace
    - normalize quotes.
    - separate periods that appear at the end of line

    >>> import nltk
    >>> from universal_word_tokenizer import UniversalWordTokenizer
    >>> sent_tokenizer = nltk.data.load('tokenizers/punkt/portuguese.pickle')
    >>> word_tokenizer = UniversalWordTokenizer(language='pt')
    >>> s = u'''Um pastel de Belém custa 1,05 EUR em Lisboa. Queria dois pastéis desses, com açúcar e canela. Obrigado!''' 
    >>> sentences = sent_tokenizer.tokenize(s)
    >>> for sentence in sentences: print word_tokenizer.tokenize(sentence)
    [u'Um', u'pastel', u'de', u'Bel\xe9m', u'custa', u'1,05', u'EUR', u'em', u'Lisboa', u'.']
    [u'Queria', u'dois', u'past\xe9is', u'de', u'esses', u',', u'com', u'a\xe7\xfacar', u'e', u'canela', u'.']
    [u'Obrigado', u'!']
    NB. this tokenizer assumes that the text is presented as one sentence per
    line, where each line is delimited with a newline character.
    The only periods to be treated as separate tokens are those appearing
    at the end of a line.
    """
    def __init__(self, language):
        self.language = language
        self.contraction_splitter = UniversalContractions(language=language)

    def tokenize(self, text):
        """
        Return a tokenized copy of *s*.

        :rtype: list of str
        """
        # Replace non-breaking spaces by spaces.
        # Note: the Portuguese sentence tokenizer should also do this!!
        text = regex.sub(ur'\u00A0', ' ', text)

        # Replace tabs by spaces [ATM 3/12/2014].
        text = regex.sub(ur'\t', ' ', text)

        # Replace U+0096 by dashes.
        text = regex.sub(ur'\u0096', ' -- ', text)

        if self.language == u'pt-cintil':
            # Replace all parenthesis by single quotes.
            # This looks a really terrible idea. However, since there are
            # no sentences with parenthesis in the CINTIL corpus (I don't
            # know why), further processing units in the pipeline (such as
            # a POS tagger or a parser) trained on that corpora would get
            # confused and so stupid things. Pretending everything is a
            # single quote seems to be the least of all evils.
            text = regex.sub(r'\(|\)', '\'', text)

        if self.language == u'pt-cintil':
            # Replace all quotes by single quotes.
            # This looks a terrible idea, but necessary for consistency with
            # the CINTIL corpus.
            text = regex.sub(ur'"|«|»|``|“|”|\'|`', '\' ', text)
        else:
            # starting quotes.
            text = regex.sub(ur'«', r'``', text) # Non-ASCII starting quotes.
            text = regex.sub(ur'»', r'"', text) # Non-ASCII ending quotes.
            text = regex.sub(ur'“', r'``', text) # Non-ASCII starting quotes.
            text = regex.sub(ur'”', r'"', text) # Non-ASCII ending quotes.
            text = regex.sub(r'^\"', r'``', text)
            text = regex.sub(r'(``)', r' \1 ', text)
            text = regex.sub(r'([ (\[{<])"', r'\1 `` ', text)

            # I added these for single quotes -- to avoid things like
            # "o 'apartheid social ' ".
            # However, we excluded this for English for now to handle well
            # contractions such as "can't -> ca + n't".
            if self.language not in ['en', 'en-ptb', 'fr']:
                text = regex.sub(ur'\'', '\' ', text)
            else:
                text = regex.sub(ur'([^\p{IsAlpha}])\'', ur"\1' ", text)
                text = regex.sub(ur'^\'', ur"' ", text)

        if self.language != 'en-ptb':
            # No special coding of starting quotes.
            text = regex.sub(ur' `` ', r' " ', text)
            text = regex.sub(ur" '' ", r' " ', text)

        # Punctuation.
        text = regex.sub(ur'([:,])([^\d])', ur' \1 \2', text)
        text = regex.sub(ur'\.\.\.', ur' ... ', text)
        text = regex.sub(ur'[;@#$%&]', ur' \g<0> ', text)
        text = regex.sub(ur'([^\.])(\.)([\]\)}>"\']*)\s*$', ur'\1 \2\3 ', text)
        text = regex.sub(ur'[?!]', ur' \g<0> ', text)

        if self.language in ['en', 'en-ptb']:
            text = regex.sub(ur"([^'])' ", ur"\1 ' ", text)
        else:
            text = regex.sub(ur"([^'])'", ur"\1' ", text)
            text = regex.sub(ur"([^\p{IsAlpha}])' ", ur"\1 ' ", text)

        # Parens, brackets, etc.
        text = regex.sub(ur'[\]\[\(\)\{\}\<\>]', ur' \g<0> ', text)
        text = regex.sub(ur'([^-])---([^-])', ur'\1 -- \2', text)
        text = regex.sub(ur'([^-])--([^-])', ur' -- ', text)

        # Add extra space to make things easier.
        text = " " + text + " "

        # Ending quotes.
        if self.language == 'en-ptb':
            text = regex.sub(r'"', " '' ", text)
        else:
            text = regex.sub(r'"', ' " ', text)
        text = regex.sub(r'(\S)(\'\')', r'\1 \2 ', text)

        # Split on contractions and clitics.
        words = text.split(' ')
        words = map(self.contraction_splitter.split_if_contraction, words)
        text = ' '.join(words)

        text = regex.sub(" +", " ", text)
        text = text.strip()

        # Add space at end to match up with MacIntyre's output (for debugging).
        if text != "":
            text += " "

        return text.split()

