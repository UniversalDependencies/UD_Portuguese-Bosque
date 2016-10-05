To train treeler:

1. make sure you have files ../7.5-fl/devel.conll and
   ../7.5-fl/train.conll.

2. Execute reformat.sh

3. Execute prepare-pt.sh

4. Execute run-treeler.sh

The following is an email exchange with Lluis Padro with some
information about what to expect while training treeler:

- - -

The output will be the folder $LANG/maps with all the maps, and a sent
of models in dep1.models

  To deploy the parser, you need to:

      - copy folder $LANG/maps to freeling/data/$LANG/dep_treeler/
      - copy parameters.XXX.gz to freeling/data/$LANG/dep
      - create file freeling/data/$LANG/dep/config.dat that specifies which parameter file to use in the "wt" parameter

Among all parameters.XXX.gz files, you must elect the epoch with
better score in development corpus (the traces of the trainer give the
score on devel after each training epoch).

- - -

Your corpus looks nice, except for a couple of things

    - Column 4 should be coarse-POS (e.g. NC, VMI, AQ, etc), and
      column 5 should be fine-pos (i.e. all eagles digits).

      If the goal of the corpus is to train FreeLing, there is no use
      in having UD tags there, and you are loosing the space to feed
      some other information to the parser

    - A general comment:

         - The goal of the corpus is to train the parser in conditions
          as similar as possible to those where it will be asked to
          perform.  So, the traning data should follow the same
          criteria than will be produced by freeling steps previous to
          the parser (so the parser is performing in the same
          conditions it was trained.).  A couple of examples about
          this:

           - Proper nouns have gender and number in the corpus, but
             freeling does not provide this information.

           - Percentages are splitted in two parts (e.g.  86_Z0
              %_NCMP000 ), but freeling provides them toghether and
              with a different tag(86_% Zp)

           - Plain numbers are tagged with Z (not Z0) if I recall
             properly.

            These small differences will cause that the parser may
            learn to behave under different assumptions than those it
            will find in reality (e.g. it may learn to rely on
            gender&number for NPs to take some decision, or it will
            not know anything about tag Zp for percentages or Z for
            plain numbers).

            So, to make sure the parser performs properly once
            integrated in FreeLing, the training corpus should keep to
            the maximum possible extent the same conventions than the
            output of FreeLing morpho+tagger steps.

- - -

> Quick question: which value should I use for $EPOCHS for the first
  time I execute this?

  Just set it to 50 or so.
  
  At each epoch, it reports the accuracy on the development set.

  If you see that it peaks and then goes down for 5 or 6 epochs, you
  can just kill it.

  If it reaches 50 and it is still rising, launch it again with a
  larger number of epochs until you find the peak

  Btw, it takes a long time to train... expect it to take a few days
  on an average desktop machine
